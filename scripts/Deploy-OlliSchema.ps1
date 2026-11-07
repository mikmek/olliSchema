[CmdletBinding()]
param(
    [ValidateSet('Build', 'Deploy', 'Status', 'Rollback')]
    [string]$Action = 'Build',
    [string]$Server = 'olli-server',
    [string]$JavaHome,
    [string]$ReleaseDirectory,
    [switch]$RollbackSafe
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest
$repository = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$eventCore = (Resolve-Path (Join-Path $repository '..\olliEventCore')).Path
if ($Server -notmatch '^[a-zA-Z0-9_.@-]+$') { throw 'Invalid SSH host name.' }

function Invoke-Checked {
    param([string]$Program, [Parameter(ValueFromRemainingArguments=$true)][string[]]$Arguments)
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) { throw "$Program failed with exit code $LASTEXITCODE" }
}

function Get-Commit {
    param([string]$Directory)
    # Legacy EventCore tracks generated target files. Never treat build output as source edits.
    $dirty = & git -C $Directory status --porcelain --untracked-files=normal -- . ':(exclude)target/**' ':(exclude)project/target/**' ':(exclude)project/project/**'
    if ($LASTEXITCODE -ne 0) { throw "Cannot inspect repository $Directory" }
    if ($dirty) { throw "Commit or otherwise resolve local changes first: $Directory" }
    $commit = & git -C $Directory rev-parse HEAD
    if ($LASTEXITCODE -ne 0 -or $commit -notmatch '^[0-9a-f]{40}$') { throw 'Invalid Git commit.' }
    return $commit
}

if ($Action -in @('Status', 'Rollback')) {
    Invoke-Checked ssh @('-t', $Server, "sudo /usr/local/lib/olli-deploy/schema.py $($Action.ToLowerInvariant())")
    return
}

if (-not $ReleaseDirectory) {
    $sourceCommit = Get-Commit $repository
    $coreCommit = Get-Commit $eventCore
    if (-not $JavaHome) {
        $candidates = @(
            (Join-Path $env:USERPROFILE '.jdks\corretto-21.0.9'),
            (Join-Path $env:USERPROFILE '.jdks\corretto-21.0.7'),
            $env:JAVA_HOME
        )
        $JavaHome = $candidates | Where-Object { $_ -and (Test-Path (Join-Path $_ 'bin\javac.exe')) } | Select-Object -First 1
    }
    if (-not $JavaHome) { throw 'Supply -JavaHome pointing to JDK 21.' }
    $release = (Get-Date).ToUniversalTime().ToString('yyyyMMddTHHmmssZ') + '-' + $sourceCommit.Substring(0, 12)
    $image = "olli-schema:$release"
    $ReleaseDirectory = Join-Path $repository "target\deploy\$release"
    New-Item -ItemType Directory -Path $ReleaseDirectory | Out-Null
    $oldJavaHome = $env:JAVA_HOME
    $oldPath = $env:PATH
    $transcript = $false
    try {
        Start-Transcript -Path (Join-Path $ReleaseDirectory 'build.log') | Out-Null
        $transcript = $true
        $env:JAVA_HOME = $JavaHome
        $env:PATH = (Join-Path $JavaHome 'bin') + ';' + $oldPath
        $javaDetails = Get-Content -LiteralPath (Join-Path $JavaHome 'release') -Raw
        if ($javaDetails -notmatch 'JAVA_VERSION="21\.') { throw 'This deployment requires JDK 21.' }
        Write-Host "Release: $release; source: $sourceCommit; EventCore: $coreCommit"
        Invoke-Checked docker @('info', '--format', '{{.ServerVersion}}')
        Push-Location $eventCore
        try { Invoke-Checked sbt @('test', 'publishAll') } finally { Pop-Location }
        Push-Location $repository
        try {
            Invoke-Checked sbt @("-Dolli.build.version=$release", "-Dolli.source.commit=$sourceCommit", "-Dolli.eventcore.commit=$coreCommit", 'test', 'Docker/publishLocal')
        } finally { Pop-Location }
        # A source edit during the build invalidates provenance.
        if ((Get-Commit $repository) -ne $sourceCommit -or (Get-Commit $eventCore) -ne $coreCommit) {
            throw 'Source changed during build. Release rejected.'
        }
        $imageInfoRaw = & docker image inspect $image
        if ($LASTEXITCODE -ne 0) { throw 'Built image is missing.' }
        $imageInfo = @($imageInfoRaw | ConvertFrom-Json)[0]
        if ($imageInfo.Config.Labels.'org.opencontainers.image.revision' -ne $sourceCommit) { throw 'Image provenance label missing.' }
        $plain = Join-Path $ReleaseDirectory 'image.tar'
        $compressed = Join-Path $ReleaseDirectory 'image.tar.gz'
        Invoke-Checked docker @('save', '--output', $plain, $image)
        $inputStream = [IO.File]::OpenRead($plain)
        $outputStream = [IO.File]::Create($compressed)
        $gzip = New-Object IO.Compression.GZipStream($outputStream, [IO.Compression.CompressionLevel]::Optimal)
        try { $inputStream.CopyTo($gzip) } finally { $gzip.Dispose(); $outputStream.Dispose(); $inputStream.Dispose() }
        # Only the exact generated intermediate file is removed; no recursive cleanup.
        Remove-Item -LiteralPath $plain
        $manifest = [ordered]@{
            release = $release; image = $image; sourceCommit = $sourceCommit
            eventCoreCommit = $coreCommit; dirty = $false; localImageId = $imageInfo.Id
            archiveSha256 = (Get-FileHash -LiteralPath $compressed -Algorithm SHA256).Hash.ToLowerInvariant()
            createdAt = (Get-Date).ToUniversalTime().ToString('o'); rollbackSafe = [bool]$RollbackSafe
        }
        [IO.File]::WriteAllText((Join-Path $ReleaseDirectory 'manifest.json'), ($manifest | ConvertTo-Json), (New-Object Text.UTF8Encoding($false)))
    } finally {
        $env:JAVA_HOME = $oldJavaHome
        $env:PATH = $oldPath
        if ($transcript) { Stop-Transcript | Out-Null }
    }
}

$ReleaseDirectory = (Resolve-Path $ReleaseDirectory).Path
$manifestPath = Join-Path $ReleaseDirectory 'manifest.json'
$manifest = Get-Content -LiteralPath $manifestPath -Raw | ConvertFrom-Json
$release = $manifest.release
if ($release -notmatch '^\d{8}T\d{6}Z-[0-9a-f]{12}$') { throw 'Invalid release ID.' }
$archive = Join-Path $ReleaseDirectory 'image.tar.gz'
if ((Get-FileHash -LiteralPath $archive).Hash.ToLowerInvariant() -ne $manifest.archiveSha256) { throw 'Local archive checksum mismatch.' }
Write-Host "Release ready: $ReleaseDirectory"
if ($Action -eq 'Build') { return }

Invoke-Checked ssh @($Server, "mkdir -p /srv/olli/incoming/schema/$release")
Invoke-Checked scp @($archive, $manifestPath, "${Server}:/srv/olli/incoming/schema/$release/")
# Interactive SSH/sudo is deliberate: no password storage and no blanket passwordless sudo.
& ssh -t $Server "sudo /usr/local/lib/olli-deploy/schema.py apply $release" | Tee-Object -FilePath (Join-Path $ReleaseDirectory 'deployment.log') | Write-Output
$remoteExit = $LASTEXITCODE
if ($remoteExit -ne 0) { throw "Remote deployment failed with exit code $remoteExit. Inspect deployment.log." }
Write-Host "Deployment completed: $release"

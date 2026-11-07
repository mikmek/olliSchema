$ErrorActionPreference = 'Stop'
$repository = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$tokens = $null
$parseErrors = $null
$ast = [System.Management.Automation.Language.Parser]::ParseFile(
    (Join-Path $repository 'scripts\Deploy-OlliSchema.ps1'), [ref]$tokens, [ref]$parseErrors)
if ($parseErrors.Count) { throw 'Deployment script parse failure.' }
$definition = $ast.Find({ param($node)
    $node -is [System.Management.Automation.Language.FunctionDefinitionAst] -and $node.Name -eq 'Invoke-Checked'
}, $true)
. ([scriptblock]::Create($definition.Extent.Text))
$folder = Join-Path $repository 'target\deploy-tests'
New-Item -ItemType Directory -Path $folder -Force | Out-Null
$probe = Join-Path $folder 'argument probe.py'
@'
import sys
assert sys.argv[1:] == ['olli-server', 'mkdir -p /srv/olli/incoming/schema/example', 'path with spaces/archive.tar.gz'], repr(sys.argv)
print('Native argument forwarding passed.')
'@ | Set-Content -LiteralPath $probe -Encoding ASCII
Invoke-Checked -Program python -Arguments @($probe, 'olli-server', 'mkdir -p /srv/olli/incoming/schema/example', 'path with spaces/archive.tar.gz')

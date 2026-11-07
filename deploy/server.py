#!/usr/bin/env python3
"""Root-owned server-side release tool; accepts only olliSchema releases."""
import argparse
import datetime as dt
import fcntl
import hashlib
import ipaddress
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time
import urllib.request

BASE = Path('/srv/olli/deploy/schema')
ARCHIVES = Path('/srv/olli-data/releases/olli-schema')
INCOMING = Path('/srv/olli/incoming/schema')
CONFIG = Path('/srv/olli/config/schema')
REDIS = 'olli-migration/image-b6636bae9624:backup'
STATE = BASE / 'state.json'
RELEASE_PATTERN = r'[0-9]{8}T[0-9]{6}Z-[0-9a-f]{12}'
LOG = None


def run(*args, capture=False):
    if capture:
        return subprocess.check_output(args, text=True).strip()
    process = subprocess.Popen(args, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    for line in process.stdout:
        print(line.rstrip(), flush=True)
        if LOG is not None:
            with LOG.open('a') as stream:
                stream.write(line)
    if process.wait() != 0:
        raise subprocess.CalledProcessError(process.returncode, args)


def sha256(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix('.tmp')
    temporary.write_text(json.dumps(value, indent=2) + '\n')
    temporary.replace(path)


def state():
    return json.loads(STATE.read_text()) if STATE.exists() else {}


def bind_address():
    for line in Path('/srv/olli/config/infrastructure.env').read_text().splitlines():
        if line.startswith('OLLI_BIND_ADDRESS='):
            value = line.split('=', 1)[1].strip().strip("'\"")
            ipaddress.IPv4Address(value)
            if value == '0.0.0.0':
                raise RuntimeError('A private, explicit bind address is required.')
            return value
    raise RuntimeError('OLLI_BIND_ADDRESS missing in infrastructure.env')


def preflight():
    if os.geteuid() != 0:
        raise RuntimeError('Run this installed tool with sudo.')
    run('findmnt', '--mountpoint', '/srv/olli-data')
    if shutil.disk_usage('/').free < 3 * 1024**3:
        raise RuntimeError('Less than 3 GiB free on system SSD.')
    run('docker', 'network', 'inspect', 'infrastructure_default', capture=True)
    run('docker', 'image', 'inspect', REDIS, capture=True)
    bind_address()


def compose_file(release):
    return BASE / 'releases' / release / 'compose.yaml'


def compose(release, *args, capture=False):
    return run('docker', 'compose', '-p', 'olli-schema', '-f',
               str(compose_file(release)), *args, capture=capture)


def make_compose(release, image):
    config = CONFIG / 'releases' / release / 'olli-schema.conf'
    # $$ escapes Compose interpolation; Bash receives a single $ at runtime.
    probe = ("exec 3<>/dev/tcp/127.0.0.1/8110; "
             "printf 'GET /health/ready HTTP/1.0\\r\\nHost: localhost\\r\\n\\r\\n' >&3; "
             'IFS= read -r status <&3; [[ "$$status" == *" 200 "* ]]')
    return {
        'name': 'olli-schema',
        'services': {
            'schema-redis': {
                'image': REDIS, 'pull_policy': 'never', 'restart': 'unless-stopped',
                'networks': ['infra'],
                'command': ['redis-server', '--appendonly', 'yes', '--appendfsync', 'everysec'],
                'volumes': ['schema-redis-data:/data'],
                'healthcheck': {'test': ['CMD', 'redis-cli', 'ping'],
                                'interval': '5s', 'timeout': '3s', 'retries': 20}},
            'schema': {
                'image': image, 'pull_policy': 'never', 'restart': 'unless-stopped',
                'networks': ['infra'], 'mem_limit': '1536m',
                'depends_on': {'schema-redis': {'condition': 'service_healthy'}},
                'environment': {'JAVA_OPTS': '-Dconfig.file=/etc/olli/olli-schema.conf'},
                'volumes': [f'{config}:/etc/olli/olli-schema.conf:ro'],
                'ports': [f'{bind_address()}:8110:8110'],
                'healthcheck': {'test': ['CMD', 'bash', '-c', probe],
                                'interval': '10s', 'timeout': '5s', 'retries': 30,
                                'start_period': '30s'}}},
        'networks': {'infra': {'external': True, 'name': 'infrastructure_default'}},
        'volumes': {'schema-redis-data': {'name': 'olli-schema-redis-data', 'external': True}}}


def check_http():
    with urllib.request.urlopen(f'http://{bind_address()}:8110/health/ready', timeout=15) as response:
        body = json.load(response)
        if response.status != 200 or body.get('status') != 'ready':
            raise RuntimeError('Schema readiness did not return ready.')
        print('HTTP readiness:', json.dumps(body), flush=True)


def activate(release):
    compose(release, 'config', '--quiet')
    compose(release, 'up', '-d', '--pull', 'never', '--wait', '--wait-timeout', '300')
    check_http()


def load_release(release):
    manifest = json.loads((ARCHIVES / release / 'manifest.json').read_text())
    image = manifest['image']
    exists = subprocess.run(['docker', 'image', 'inspect', image],
                            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode == 0
    if not exists:
        archive = ARCHIVES / release / 'image.tar.gz'
        if sha256(archive) != manifest['archiveSha256']:
            raise RuntimeError('Stored archive checksum mismatch.')
        run('docker', 'load', '--input', str(archive))


def apply(release):
    global LOG
    source = INCOMING / release
    manifest = json.loads((source / 'manifest.json').read_text())
    if manifest.get('release') != release or manifest.get('image') != f'olli-schema:{release}':
        raise RuntimeError('Release or image name mismatch.')
    for key in ['sourceCommit', 'eventCoreCommit']:
        if not re.fullmatch(r'[0-9a-f]{40}', manifest.get(key, '')):
            raise RuntimeError('Invalid source provenance.')
    if manifest.get('dirty') is not False:
        raise RuntimeError('Only clean, committed source releases may be deployed.')
    destination = ARCHIVES / release
    if destination.exists():
        raise RuntimeError('Release already exists; use a new release or explicit rollback.')
    archive = source / 'image.tar.gz'
    if not re.fullmatch(r'[0-9a-f]{64}', manifest.get('archiveSha256', '')):
        raise RuntimeError('Invalid archive checksum.')
    if shutil.disk_usage(ARCHIVES.parent).free < archive.stat().st_size * 2:
        raise RuntimeError('Not enough archive disk space.')
    destination.mkdir(parents=True, mode=0o750)
    LOG = destination / 'deployment.log'
    shutil.copyfile(archive, destination / 'image.tar.gz')
    if sha256(destination / 'image.tar.gz') != manifest['archiveSha256']:
        raise RuntimeError('Transfer checksum mismatch. No containers changed.')
    write_json(destination / 'manifest.json', manifest)
    run('docker', 'load', '--input', str(destination / 'image.tar.gz'))
    info = json.loads(run('docker', 'image', 'inspect', manifest['image'], capture=True))[0]
    labels = info['Config'].get('Labels') or {}
    if labels.get('org.opencontainers.image.revision') != manifest['sourceCommit']:
        raise RuntimeError('Loaded image source label mismatch.')
    if labels.get('de.bizgrid.eventcore.revision') != manifest['eventCoreCommit']:
        raise RuntimeError('Loaded image dependency label mismatch.')
    if labels.get('de.bizgrid.release') != release:
        raise RuntimeError('Loaded image release label mismatch.')
    manifest['serverImageId'] = info['Id']
    write_json(destination / 'manifest.json', manifest)
    config = CONFIG / 'releases' / release / 'olli-schema.conf'
    config.parent.mkdir(parents=True, mode=0o750)
    shutil.copyfile(CONFIG / 'olli-schema.conf', config)
    os.chmod(config, 0o644)
    write_json(compose_file(release), make_compose(release, manifest['image']))
    run('docker', 'volume', 'create', 'olli-schema-redis-data')
    before = state()
    previous = before.get('current')
    try:
        activate(release)
    except Exception:
        try:
            compose(release, 'logs', '--no-color', '--tail', '100', 'schema')
        except Exception as log_error:
            print('Could not collect container logs:', log_error, flush=True)
        if previous and manifest.get('rollbackSafe') is True:
            print('Activation failed; restoring previous application/configuration.', flush=True)
            load_release(previous)
            activate(previous)
        else:
            compose(release, 'stop', 'schema')
            write_json(STATE, {**before, 'failedDeployment': release})
            print('Schema stopped; Redis/data retained. No automatic rollback authorized.', flush=True)
        raise
    write_json(STATE, {'current': release, 'previous': previous,
                      'activatedAt': dt.datetime.now(dt.timezone.utc).isoformat()})
    print('DEPLOYMENT SUCCESS:', release, flush=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('action', choices=['apply', 'rollback', 'status'])
    parser.add_argument('release', nargs='?')
    args = parser.parse_args()
    if args.action == 'apply' and not re.fullmatch(RELEASE_PATTERN, args.release or ''):
        parser.error('Expected release ID YYYYMMDDTHHMMSSZ-<12 hex>.')
    preflight()
    BASE.mkdir(parents=True, exist_ok=True)
    ARCHIVES.mkdir(parents=True, mode=0o750, exist_ok=True)
    with (BASE / 'deployment.lock').open('w') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        if args.action == 'apply':
            apply(args.release)
        elif args.action == 'rollback':
            before = state()
            previous = before.get('current') if before.get('failedDeployment') else before.get('previous')
            if not previous:
                raise RuntimeError('No successful previous release available.')
            load_release(previous)
            activate(previous)
            write_json(STATE, {'current': previous, 'previous': before['current'],
                              'activatedAt': dt.datetime.now(dt.timezone.utc).isoformat()})
            print('ROLLBACK SUCCESS:', previous)
        else:
            current = state().get('current')
            print(json.dumps(state(), indent=2))
            if current:
                compose(current, 'ps')
                check_http()


if __name__ == '__main__':
    try:
        main()
    except Exception as error:
        print('DEPLOYMENT FAILED:', str(error), file=sys.stderr, flush=True)
        sys.exit(1)

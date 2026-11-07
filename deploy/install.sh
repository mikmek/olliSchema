#!/usr/bin/env bash
set -euo pipefail
if [ "$(id -u)" != 0 ]; then
  echo 'Run with sudo bash install.sh' >&2
  exit 1
fi
source_dir=$(cd -- "$(dirname -- "$0")" && pwd)
findmnt --mountpoint /srv/olli-data
install -d -m 755 /usr/local/lib/olli-deploy
install -m 755 "$source_dir/server.py" /usr/local/lib/olli-deploy/schema.py
install -d -o frank -g frank -m 750 /srv/olli/incoming/schema
install -d -m 750 /srv/olli/config/schema
if [ ! -e /srv/olli/config/schema/olli-schema.conf ]; then
  install -m 644 "$source_dir/olli-schema.conf" /srv/olli/config/schema/olli-schema.conf
else
  echo 'Existing runtime configuration retained.'
fi
python3 -m py_compile /usr/local/lib/olli-deploy/schema.py
echo 'Deployment helper installed. No application containers started.'

# SPDX-License-Identifier: AGPL-3.0-or-later

from pathlib import Path
import re
import subprocess

REPO_ROOT = Path(__file__).resolve().parent.parent
CHART_INIT = REPO_ROOT / 'deploy/helm/fluxer/files/seaweedfs-init.sh'
COMPOSE = REPO_ROOT / 'fluxer_rexctl/internal/stack/files/docker-compose.yml'
STARTUP = "trap 'exit 0' TERM INT\n"
KEEP_ALIVE = '''        touch /tmp/fluxer-buckets-ready
        while :; do
          sleep 86400 &
          wait $!
        done
'''

source = CHART_INIT.read_text()
if not source.startswith(STARTUP) or source.count(KEEP_ALIVE) != 1:
    raise ValueError('The chart initializer startup or keepalive contract changed')
subprocess.run(['sh', '-n'], input=source, text=True, check=True)
body = source[len(STARTUP):].replace(KEEP_ALIVE, '        exit 0\n')
for original, replacement in [
    ('if ! nc -z 127.0.0.1 9333 2>/dev/null; then',
     'if ! nc -z seaweedfs 9333 2>/dev/null; then'),
    ('for _attempt in $(seq 1 "$FLUXER_SEAWEEDFS_INIT_ATTEMPTS"); do',
     'for attempt in $(seq 1 $FLUXER_SEAWEEDFS_INIT_ATTEMPTS); do'),
    ('"^[[:space:]]*${b}[[:space:]]"', '"^[[:space:]]*$b[[:space:]]"'),
]:
    if body.count(original) != 1:
        raise ValueError(f'The chart initializer command changed: {original}')
    body = body.replace(original, replacement)
body, master_count = re.subn(r'(?<= )-master=127\.0\.0\.1:9333(?= |$)',
                             '-master=seaweedfs:9333', body, flags=re.MULTILINE)
if master_count == 0:
    raise ValueError('The chart initializer has no local master arguments')
lines = []
for line in body.splitlines():
    statement = line.strip()
    if statement.startswith(('if ', 'for ', 'while ')):
        if not statement.endswith(('; then', '; do')):
            raise ValueError(f'Unsupported multiline shell command: {statement}')
        suffix = ''
    elif statement == 'else':
        suffix = ''
    elif not statement or statement.endswith(('\\', ';', '&')):
        raise ValueError(f'Unsupported shell statement: {statement}')
    else:
        suffix = ';'
    lines.append(line + suffix)
script = '\n'.join(lines) + '\n'
subprocess.run(['sh', '-n'], input=script, text=True, check=True)
rendered = ''.join('        ' + line.replace('$', '$$') + '\n' for line in lines)
compose = COMPOSE.read_text()
services = list(re.finditer(r'^  seaweedfs-init:\n.*?(?=^  [^ \n][^\n]*:\n|\Z)',
                            compose, re.MULTILINE | re.DOTALL))
if len(services) != 1:
    raise ValueError('Expected exactly one Compose seaweedfs-init service')
service = services[0]
entrypoints = list(re.finditer(
    r'^    entrypoint:\n      - /bin/sh\n      - -c\n      - >\n(?P<script>(?:        [^\n]*\n)+)',
    service.group(), re.MULTILINE))
if len(entrypoints) != 1 or service.group().count('\n    entrypoint:') != 1:
    raise ValueError('Expected exactly one folded shell initializer entrypoint')
entrypoint = entrypoints[0]
start = service.start() + entrypoint.start('script')
end = service.start() + entrypoint.end('script')
COMPOSE.write_text(compose[:start] + rendered + compose[end:])

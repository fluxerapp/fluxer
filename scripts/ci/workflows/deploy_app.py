#!/usr/bin/env python3

import pathlib
import sys

sys.path.append(str(pathlib.Path(__file__).resolve().parents[1]))

from ci_steps import (
    ADD_KNOWN_HOSTS_SCRIPT,
    INSTALL_DOCKER_PUSSH_SCRIPT,
    INSTALL_RCLONE_SCRIPT,
    record_deploy_commit_script,
    rclone_config_script,
    set_build_timestamp_script,
)
from ci_workflow import parse_step_env_args
from ci_utils import run_step


STEPS: dict[str, str] = {
    "install_dependencies": """
set -euo pipefail
cd fluxer_app
pnpm install --frozen-lockfile
""",
    "run_lingui": """
set -euo pipefail
cd fluxer_app
pnpm lingui:extract
pnpm lingui:compile --strict
""",
    "record_deploy_commit": record_deploy_commit_script(
        include_env=True,
        include_sentry=False,
    ),
    "install_wasm_pack": """
set -euo pipefail
if ! command -v wasm-pack >/dev/null 2>&1; then
  cargo install wasm-pack --version 0.13.1
fi
""",
    "generate_wasm": """
set -euo pipefail
cd fluxer_app
pnpm wasm:codegen
""",
    "add_known_hosts": ADD_KNOWN_HOSTS_SCRIPT,
    "fetch_deployment_config": """
set -euo pipefail
if [[ "${RELEASE_CHANNEL}" == "canary" ]]; then
  CONFIG_PATH="/etc/fluxer/config.canary.json"
else
  CONFIG_PATH="/etc/fluxer/config.stable.json"
fi
ssh "${SERVER}" "cat ${CONFIG_PATH}" > fluxer_app/config.json
""",
    "build_application": """
set -euo pipefail
cd fluxer_app
pnpm build
node -e "const fs = require('fs'); const {execSync} = require('child_process'); const cfg = JSON.parse(fs.readFileSync(process.env.FLUXER_CONFIG, 'utf8')); const app = cfg.app_public || {}; let sha = app.build_sha || ''; if (!sha) { try { sha = execSync('git rev-parse --short HEAD', {stdio:['ignore','pipe','ignore']}).toString().trim(); } catch {} } const timestamp = Number(app.build_timestamp ?? Math.floor(Date.now() / 1000)); const buildNumber = Number(app.build_number ?? 0); const env = app.project_env ?? cfg.sentry?.release_channel ?? cfg.env ?? ''; const payload = { sha, buildNumber, timestamp, env }; fs.writeFileSync('dist/version.json', JSON.stringify(payload, null, 2));"
""",
    "install_rclone": INSTALL_RCLONE_SCRIPT,
    "upload_assets": rclone_config_script(
        endpoint="https://s3.us-east-va.io.cloud.ovh.us",
        acl="public-read",
        expand_vars=True,
    )
    + """
rclone copy fluxer_app/dist/assets ovh:fluxer-static/assets \
  --transfers 32 \
  --checkers 16 \
  --size-only \
  --fast-list \
  --s3-upload-concurrency 8 \
  --s3-chunk-size 16M \
  -v
""",
    "set_build_timestamp": set_build_timestamp_script(),
    "install_docker_pussh": INSTALL_DOCKER_PUSSH_SCRIPT,
    "push_and_deploy": """
set -euo pipefail

docker pussh "${IMAGE_TAG}" "${SERVER}"

ssh "${SERVER}" \
  "IMAGE_TAG=${IMAGE_TAG} SERVICE_NAME=${SERVICE_NAME} COMPOSE_STACK=${COMPOSE_STACK} RELEASE_CHANNEL=${RELEASE_CHANNEL} APP_REPLICAS=${APP_REPLICAS} bash" << 'REMOTE_EOF'
set -euo pipefail
if [[ "${RELEASE_CHANNEL}" == "canary" ]]; then
  CONFIG_PATH="/etc/fluxer/config.canary.json"
else
  CONFIG_PATH="/etc/fluxer/config.stable.json"
fi
read -r CADDY_APP_DOMAIN <<EOF
$(python3 - <<'PY' "${CONFIG_PATH}"
import sys, json
from urllib.parse import urlparse
path = sys.argv[1]
with open(path, 'r') as f:
    cfg = json.load(f)
domain = cfg.get('domain', {})
overrides = cfg.get('endpoint_overrides', {})

def build_url(scheme, base_domain, port, path=''):
    standard = (scheme == 'http' and port == 80) or (scheme == 'https' and port == 443) or (scheme == 'ws' and port == 80) or (scheme == 'wss' and port == 443)
    port_part = f":{port}" if port and not standard else ""
    return f"{scheme}://{base_domain}{port_part}{path}"

def derive_domain(key):
    if key == 'cdn':
        return domain.get('cdn_domain') or domain.get('base_domain')
    if key == 'invite':
        return domain.get('invite_domain') or domain.get('base_domain')
    if key == 'gift':
        return domain.get('gift_domain') or domain.get('base_domain')
    return domain.get('base_domain')

public_scheme = domain.get('public_scheme', 'https')
public_port = domain.get('public_port', 443 if public_scheme == 'https' else 80)

derived_app = build_url(public_scheme, derive_domain('app'), public_port)
app_url = (overrides.get('app') or derived_app).strip()
parsed_app = urlparse(app_url)
app_host = parsed_app.netloc or parsed_app.path
print(app_host)
PY
)
EOF
if [[ "${RELEASE_CHANNEL}" == "canary" ]]; then
  API_TARGET="fluxer-api-canary_app"
else
  API_TARGET="fluxer-api_app"
fi
sudo mkdir -p "/opt/${SERVICE_NAME}"
sudo chown -R "${USER}:${USER}" "/opt/${SERVICE_NAME}"
cd "/opt/${SERVICE_NAME}"

cat > compose.yaml << COMPOSEEOF
x-deploy-base: &deploy_base
  restart_policy:
    condition: on-failure
    delay: 5s
    max_attempts: 3
  update_config:
    parallelism: 1
    delay: 10s
    order: start-first
  rollback_config:
    parallelism: 1
    delay: 10s

x-common-caddy-headers: &common_caddy_headers
  caddy.header.Strict-Transport-Security: "max-age=31536000; includeSubDomains; preload"
  caddy.header.X-Xss-Protection: "1; mode=block"
  caddy.header.X-Content-Type-Options: "nosniff"
  caddy.header.Referrer-Policy: "strict-origin-when-cross-origin"
  caddy.header.X-Frame-Options: "DENY"
  caddy.header.Cache-Control: "no-store, no-cache, must-revalidate"
  caddy.header.Pragma: "no-cache"
  caddy.header.Expires: "0"

x-env-base: &env_base
  FLUXER_CONFIG: /etc/fluxer/config.json

x-healthcheck: &healthcheck
  test: ['CMD', 'curl', '-f', 'http://localhost:8080/_health']
  interval: 30s
  timeout: 10s
  retries: 3
  start_period: 40s

services:
  app:
    image: ${IMAGE_TAG}
    volumes:
      - ${CONFIG_PATH}:/etc/fluxer/config.json:ro
    deploy:
      <<: *deploy_base
      replicas: ${APP_REPLICAS}
      labels:
        <<: *common_caddy_headers
        caddy: ${CADDY_APP_DOMAIN}
        caddy.redir: "/.well-known/fluxer /api/.well-known/fluxer 301"
        caddy.handle_path_0: /api*
        caddy.handle_path_0.reverse_proxy: "http://${API_TARGET}:8080"
        caddy.reverse_proxy: "{{upstreams 8080}}"
    environment:
      <<: *env_base
    networks: [fluxer-shared]
    healthcheck: *healthcheck

networks:
  fluxer-shared:
    external: true
COMPOSEEOF

docker stack deploy \
  --with-registry-auth \
  --detach=false \
  --resolve-image never \
  -c compose.yaml \
  "${COMPOSE_STACK}"
REMOTE_EOF
""",
}


def main() -> int:
    args = parse_step_env_args(include_server_ip=True)
    run_step(STEPS, args.step)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

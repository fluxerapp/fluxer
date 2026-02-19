#!/usr/bin/env python3

import pathlib
import sys

sys.path.append(str(pathlib.Path(__file__).resolve().parents[1]))

from deploy_workflow import build_standard_deploy_steps, run_deploy_workflow


PUSH_AND_DEPLOY_SCRIPT = """
set -euo pipefail

docker pussh "${IMAGE_TAG_APP}" "${SERVER}"

if [[ "${IS_CANARY}" == "true" ]]; then
  docker pussh "${IMAGE_TAG_WORKER}" "${SERVER}"
fi

ssh "${SERVER}" \
  "IMAGE_TAG_APP=${IMAGE_TAG_APP} IMAGE_TAG_WORKER=${IMAGE_TAG_WORKER} STACK=${STACK} WORKER_STACK=${WORKER_STACK} CANARY_WORKER_REPLICAS=${CANARY_WORKER_REPLICAS} IS_CANARY=${IS_CANARY} CADDY_DOMAIN=${CADDY_DOMAIN} RELEASE_CHANNEL=${RELEASE_CHANNEL} SENTRY_RELEASE=${SENTRY_RELEASE} SENTRY_BUILD_SHA=${SENTRY_BUILD_SHA} SENTRY_BUILD_NUMBER=${SENTRY_BUILD_NUMBER} SENTRY_BUILD_TIMESTAMP=${SENTRY_BUILD_TIMESTAMP} bash" << 'REMOTE_EOF'
set -euo pipefail

if [[ "${IS_CANARY}" == "true" ]]; then
  CONFIG_PATH="/etc/fluxer/config.canary.json"
else
  CONFIG_PATH="/etc/fluxer/config.stable.json"
fi
CANARY_WORKER_REPLICAS="${CANARY_WORKER_REPLICAS:-3}"
if [[ "${IS_CANARY}" == "true" ]]; then
  API_REPLICAS=6
else
  API_REPLICAS=20
fi
BLUESKY_KEYS_DIR="/etc/fluxer/keys"
sudo mkdir -p "${BLUESKY_KEYS_DIR}"
sudo chown root:65534 "${BLUESKY_KEYS_DIR}"
sudo chmod 0750 "${BLUESKY_KEYS_DIR}"
shopt -s nullglob
KEY_FILES=("${BLUESKY_KEYS_DIR}"/*.pem)
if [[ ${#KEY_FILES[@]} -gt 0 ]]; then
  sudo chown root:65534 "${KEY_FILES[@]}"
  sudo chmod 0440 "${KEY_FILES[@]}"
fi
shopt -u nullglob

deploy_api_stack() {
  sudo mkdir -p "/opt/${STACK}"
  sudo chown -R "${USER}:${USER}" "/opt/${STACK}"
  cd "/opt/${STACK}"

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

x-healthcheck: &healthcheck
  test: ['CMD', 'curl', '-f', 'http://localhost:8080/_health']
  interval: 30s
  timeout: 10s
  retries: 3
  start_period: 40s

services:
  app:
    image: ${IMAGE_TAG_APP}
    command: ['npm', 'run', 'start']
    environment:
      - FLUXER_CONFIG=/etc/fluxer/config.json
    volumes:
      - ${CONFIG_PATH}:/etc/fluxer/config.json:ro
      - ${BLUESKY_KEYS_DIR}:${BLUESKY_KEYS_DIR}:ro
      - /opt/geoip/GeoLite2-City.mmdb:/data/GeoLite2-City.mmdb:ro
    deploy:
      <<: *deploy_base
      replicas: ${API_REPLICAS}
      labels:
        - "caddy=${CADDY_DOMAIN}"
        - 'caddy.reverse_proxy={{upstreams 8080}}'
        - 'caddy.header.Strict-Transport-Security="max-age=31536000; includeSubDomains; preload"'
        - 'caddy.header.X-Xss-Protection="1; mode=block"'
        - 'caddy.header.X-Content-Type-Options=nosniff'
        - 'caddy.header.Referrer-Policy=strict-origin-when-cross-origin'
        - 'caddy.header.X-Frame-Options=DENY'
        - 'caddy.header.Expect-Ct="max-age=86400, report-uri=\\"https://o4510149383094272.ingest.us.sentry.io/api/4510205804019712/security/?sentry_key=bb16e8b823b82d788db49a666b3b4b90\\""'
    networks:
      - fluxer-shared
    healthcheck: *healthcheck

networks:
  fluxer-shared:
    external: true
COMPOSEEOF

  docker stack deploy --with-registry-auth --detach=false --resolve-image never -c compose.yaml "${STACK}"
}

deploy_worker_stack() {
  sudo mkdir -p "/opt/${WORKER_STACK}"
  sudo chown -R "${USER}:${USER}" "/opt/${WORKER_STACK}"
  cd "/opt/${WORKER_STACK}"

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

services:
  worker:
    image: ${IMAGE_TAG_WORKER}
    command: ['npm', 'run', 'start:worker']
    environment:
      - FLUXER_CONFIG=/etc/fluxer/config.json
      - SENTRY_RELEASE=${SENTRY_RELEASE}
      - SENTRY_BUILD_SHA=${SENTRY_BUILD_SHA}
      - SENTRY_BUILD_NUMBER=${SENTRY_BUILD_NUMBER}
      - SENTRY_BUILD_TIMESTAMP=${SENTRY_BUILD_TIMESTAMP}
    volumes:
      - ${CONFIG_PATH}:/etc/fluxer/config.json:ro
    deploy:
      <<: *deploy_base
      replicas: ${CANARY_WORKER_REPLICAS}
    networks:
      - fluxer-shared

networks:
  fluxer-shared:
    external: true
COMPOSEEOF

  docker stack deploy --with-registry-auth --detach=false --resolve-image never -c compose.yaml "${WORKER_STACK}"
}

deploy_api_stack

if [[ "${IS_CANARY}" == "true" ]]; then
  deploy_worker_stack
fi
REMOTE_EOF
"""

STEPS = build_standard_deploy_steps(
    push_and_deploy_script=PUSH_AND_DEPLOY_SCRIPT,
    include_sentry=True,
    include_build_timestamp=False,
)


def main() -> int:
    return run_deploy_workflow(STEPS)


if __name__ == "__main__":
    raise SystemExit(main())

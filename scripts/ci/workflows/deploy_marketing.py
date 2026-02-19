#!/usr/bin/env python3

import pathlib
import sys

sys.path.append(str(pathlib.Path(__file__).resolve().parents[1]))

from deploy_workflow import build_standard_deploy_steps, run_deploy_workflow


PUSH_AND_DEPLOY_SCRIPT = """
set -euo pipefail

docker pussh "${IMAGE_TAG}" "${SERVER}"

ssh "${SERVER}" \
  "IMAGE_TAG=${IMAGE_TAG} STACK=${STACK} IS_CANARY=${IS_CANARY} CADDY_DOMAIN=${CADDY_DOMAIN} RELEASE_CHANNEL=${RELEASE_CHANNEL} APP_REPLICAS=${APP_REPLICAS} bash" << 'REMOTE_EOF'
set -euo pipefail

if [[ "${IS_CANARY}" == "true" ]]; then
  CONFIG_PATH="/etc/fluxer/config.canary.json"
else
  CONFIG_PATH="/etc/fluxer/config.stable.json"
fi

sudo mkdir -p "/opt/${STACK}"
sudo chown -R "${USER}:${USER}" "/opt/${STACK}"
cd "/opt/${STACK}"

cat > compose.yaml << COMPOSEEOF
services:
  app:
    image: ${IMAGE_TAG}
    environment:
      - FLUXER_CONFIG=/etc/fluxer/config.json
    volumes:
      - ${CONFIG_PATH}:/etc/fluxer/config.json:ro
      - /opt/geoip/GeoLite2-City.mmdb:/data/GeoLite2-City.mmdb:ro
    deploy:
      replicas: ${APP_REPLICAS}
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
      labels:
        caddy: "${CADDY_DOMAIN}"
        caddy.reverse_proxy: "{{upstreams 8080}}"
        caddy.header.Strict-Transport-Security: "max-age=31536000; includeSubDomains; preload"
        caddy.header.X-Xss-Protection: "1; mode=block"
        caddy.header.X-Content-Type-Options: "nosniff"
        caddy.header.Referrer-Policy: "strict-origin-when-cross-origin"
        caddy.header.X-Frame-Options: "DENY"
COMPOSEEOF

if [[ "${IS_CANARY}" == "true" ]]; then
  cat >> compose.yaml << 'COMPOSEEOF'
        caddy.header.X-Robots-Tag: "noindex, nofollow, nosnippet, noimageindex"
        caddy.@channels.path: "/channels /channels/*"
        caddy.redir: "@channels https://web.canary.fluxer.app{uri}"
        caddy.redir_0: "/.well-known/fluxer https://api.canary.fluxer.app/.well-known/fluxer 301"
COMPOSEEOF
else
  cat >> compose.yaml << 'COMPOSEEOF'
        caddy.redir_0: "/channels/* https://web.fluxer.app{uri}"
        caddy.redir_1: "/channels https://web.fluxer.app{uri}"
        caddy.redir_2: "/delete-my-account /help/delete-account 302"
        caddy.redir_3: "/delete-my-data /help/data-deletion 302"
        caddy.redir_4: "/export-my-data /help/data-export 302"
        caddy.redir_5: "/bugs /help/report-bug 302"
        caddy_1: "www.fluxer.app"
        caddy_1.redir: "https://fluxer.app{uri}"
        caddy_3: "fluxer.gg"
        caddy_3.@fluxer_gg_root.path: "/"
        caddy_3.redir_0: "@fluxer_gg_root https://fluxer.app"
        caddy_3.redir_1: "https://web.fluxer.app/invite{uri}"
        caddy_4: "fluxer.gift"
        caddy_4.@fluxer_gift_root.path: "/"
        caddy_4.redir_0: "@fluxer_gift_root https://fluxer.app"
        caddy_4.redir_1: "https://web.fluxer.app/gift{uri}"
        caddy_5: "fluxerapp.com"
        caddy_5.redir: "https://fluxer.app{uri}"
        caddy_6: "www.fluxerapp.com"
        caddy_6.redir: "https://fluxer.app{uri}"
        caddy_7: "fluxer.dev"
        caddy_7.redir: "https://docs.fluxer.app{uri}"
        caddy_8: "www.fluxer.dev"
        caddy_8.redir: "https://docs.fluxer.app{uri}"
        caddy.redir_9: "/.well-known/fluxer https://api.fluxer.app/.well-known/fluxer 301"
COMPOSEEOF
fi

cat >> compose.yaml << 'COMPOSEEOF'
    networks:
      - fluxer-shared
    healthcheck:
      test: ['CMD', 'curl', '-f', 'http://localhost:8080/']
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s

networks:
  fluxer-shared:
    external: true
COMPOSEEOF

docker stack deploy \
  --with-registry-auth \
  --detach=false \
  --resolve-image never \
  -c compose.yaml \
  "${STACK}"
REMOTE_EOF
"""

STEPS = build_standard_deploy_steps(
    push_and_deploy_script=PUSH_AND_DEPLOY_SCRIPT,
)


def main() -> int:
    return run_deploy_workflow(STEPS)


if __name__ == "__main__":
    raise SystemExit(main())

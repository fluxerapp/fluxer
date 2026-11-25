#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

[ "$#" -eq 1 ] || { echo "usage: image-smoke.sh IMAGE" >&2; exit 2; }
image="$1"
env_file="$(cd "$(dirname "$0")/../../deploy/docker" && pwd)/fluxer.env"
name="fluxer-smoke-$$"
volume="fluxer-smoke-$$"
refused="fluxer-smoke-refused-$$"

cleanup() {
    docker rm -f "$name" "$refused" >/dev/null 2>&1 || true
    docker volume rm "$volume" >/dev/null 2>&1 || true
}
trap cleanup EXIT

fail() {
    echo "image-smoke: $*" >&2
    docker logs --tail 80 "$name" >&2 2>&1 || true
    exit 1
}

user="$(docker image inspect -f '{{.Config.User}}' "$image")"
[ "$user" = "65532:65532" ] || fail "$image runs as $user, expected 65532:65532"

shipped="$(docker run --rm --entrypoint ls "$image" /opt/fluxer/current/bin)"
for bin in weed valkey-server valkey-cli; do
    if grep -qx "$bin" <<<"$shipped"; then fail "$image ships $bin"; fi
done

status=0
out="$(timeout 120 docker run --rm --name "$refused" --env-file "$env_file" "$image" 2>&1)" || status=$?
docker rm -f "$refused" >/dev/null 2>&1 || true
if [ "$status" -eq 0 ] || [ "$status" -eq 124 ]; then
    fail "$image started with the placeholder domain from fluxer.env"
fi
grep -q 'FLUXER_DOMAIN is still chat.example.com' <<<"$out" || fail "the placeholder domain was refused with: $out"
echo "refused the placeholder domain"

docker run -d --name "$name" --stop-timeout 130 --env-file "$env_file" -e FLUXER_DOMAIN=fluxer.local \
    -v "$volume:/var/lib/fluxer" "$image" >/dev/null
ready=""
for _ in $(seq 1 120); do
    if docker exec "$name" fluxerd health >/dev/null 2>&1; then
        ready=1
        break
    fi
    [ "$(docker inspect -f '{{.State.Running}}' "$name")" = true ] || fail "$image exited before it was ready"
    sleep 5
done
[ -n "$ready" ] || fail "$image was not ready after 10 minutes"
echo "ready"

token="$(docker exec "$name" fluxerd setup-token)"
[ "${#token}" -ge 16 ] || fail "fluxerd setup-token printed ${#token} characters"
echo "setup token printed"

start="$(date +%s)"
docker stop "$name" >/dev/null
took="$(($(date +%s) - start))"
[ "$took" -lt 130 ] || fail "docker stop took ${took}s"
logs="$(docker logs "$name" 2>&1)"
grep -q 'database system is shut down' <<<"$logs" || fail "PostgreSQL did not shut down cleanly"
echo "stopped in ${took}s with a clean PostgreSQL shutdown"

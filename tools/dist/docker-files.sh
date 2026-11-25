#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

die() {
    echo "docker-files: $*" >&2
    exit 1
}

[ "$#" -eq 2 ] || die "usage: docker-files.sh IMAGE:VERSION OUT_DIR"
image="$1"
out="$2"
[[ "$image" =~ ^[a-z0-9./-]+:[0-9A-Za-z][0-9A-Za-z._-]*$ ]] || die "$image is not an image reference with a tag"

src="$(cd "$(dirname "$0")/../../deploy/docker" && pwd)"
mkdir -p "$out"
cp "$src/fluxer.env" "$out/fluxer.env"

pattern='^    image: ghcr\.io/fluxerapp/fluxer:\${FLUXER_VERSION:?[^}]*}$'
[ "$(grep -c "$pattern" "$src/compose.yaml")" -eq 1 ] || die "$src/compose.yaml needs exactly one image line for ghcr.io/fluxerapp/fluxer:\${FLUXER_VERSION:?...}"
sed "s#$pattern#    image: $image#" "$src/compose.yaml" >"$out/compose.yaml"

config="$(docker compose --project-directory "$out" -f "$out/compose.yaml" config --format json)"
got="$(printf '%s' "$config" | python3 -c 'import json, sys; c = json.load(sys.stdin); print(" ".join(s["image"] for s in c["services"].values()))')"
[ "$got" = "$image" ] || die "the stamped compose.yaml runs $got, expected $image"
echo "$out/compose.yaml runs $image"

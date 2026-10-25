#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Downloads livekit-server (linux amd64) into .cache/ if missing, then prints its path.
set -euo pipefail

VERSION=1.12.0
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/.cache"
BIN="$DIR/livekit-server-$VERSION"

if [ ! -x "$BIN" ]; then
	mkdir -p "$DIR"
	TMP="$(mktemp -d)"
	trap 'rm -rf "$TMP"' EXIT
	curl -fsSL "https://github.com/livekit/livekit/releases/download/v${VERSION}/livekit_${VERSION}_linux_amd64.tar.gz" -o "$TMP/lk.tar.gz"
	tar -xzf "$TMP/lk.tar.gz" -C "$TMP" livekit-server
	mv "$TMP/livekit-server" "$BIN"
	chmod +x "$BIN"
fi

echo "$BIN"

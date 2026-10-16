#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

SOCKET="${DOCKER_SOCKET:-/var/run/docker.sock}"
USER_NAME="$(id -un)"

if [ ! -S "$SOCKET" ]; then
	echo "fix-docker-socket: no socket at $SOCKET; skipping (Docker-in-devcontainer will not work)"
	exit 0
fi

if docker version --format '{{.Server.Version}}' >/dev/null 2>&1; then
	echo "fix-docker-socket: $SOCKET is already usable as $USER_NAME"
	exit 0
fi

sudo setfacl --modify "user:${USER_NAME}:rw" "$SOCKET"

if docker version --format '{{.Server.Version}}' >/dev/null 2>&1; then
	echo "fix-docker-socket: $SOCKET is now usable as $USER_NAME"
else
	echo "fix-docker-socket: $SOCKET is still unreachable as $USER_NAME" >&2
	exit 1
fi

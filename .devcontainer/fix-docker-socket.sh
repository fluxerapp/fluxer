#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -uo pipefail

SOCKET="${DOCKER_SOCKET:-/var/run/docker.sock}"
USER_NAME="${USER:-vscode}"

if [ ! -S "$SOCKET" ]; then
	echo "fix-docker-socket: no socket at $SOCKET; skipping (Docker-in-devcontainer will not work)"
	exit 0
fi

if docker version --format '{{.Server.Version}}' >/dev/null 2>&1; then
	echo "fix-docker-socket: $SOCKET is already usable as $USER_NAME"
	exit 0
fi

socket_gid="$(stat -c '%g' "$SOCKET" 2>/dev/null || echo "")"
if [ -z "$socket_gid" ]; then
	echo "fix-docker-socket: could not stat $SOCKET; skipping" >&2
	exit 0
fi

sudo sh -c '
	set -e
	gid="$1"
	user="$2"
	socket="$3"
	if ! getent group "$gid" >/dev/null 2>&1; then
		groupadd --gid "$gid" docker-host
	fi
	group_name="$(getent group "$gid" | cut -d: -f1)"
	usermod --append --groups "$group_name" "$user"
	chgrp "$gid" "$socket"
	chmod g+rw "$socket"
' sh "$socket_gid" "$USER_NAME" "$SOCKET" || {
	echo "fix-docker-socket: could not adjust $SOCKET; run docker with sudo" >&2
	exit 0
}

if docker version --format '{{.Server.Version}}' >/dev/null 2>&1; then
	echo "fix-docker-socket: $SOCKET is now usable as $USER_NAME (gid $socket_gid)"
else
	echo "fix-docker-socket: $SOCKET still unreachable as $USER_NAME; run docker with sudo" >&2
fi

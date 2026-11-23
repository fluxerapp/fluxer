#!/usr/bin/env sh

# SPDX-License-Identifier: AGPL-3.0-or-later

set -eu

root="$(CDPATH='' cd "$(dirname "$0")/.." && pwd -P)"

"$root/scripts/nif.sh"
cd "$root"
if [ "${1:-}" != test ]; then
	exec gleam "$@"
fi

export FLUXER_GATEWAY_PORT="${FLUXER_GATEWAY_PORT:-0}"
export FLUXER_GATEWAY_STATUS_PORT="${FLUXER_GATEWAY_STATUS_PORT:-0}"
scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT INT TERM
{
	status=0
	gleam "$@" || status=$?
	echo "$status" >"$scratch/status"
} 2>&1 | tee "$scratch/output"

status="$(cat "$scratch/status" 2>/dev/null || echo 1)"
if [ "$status" != 0 ]; then
	echo "gleam test: exited with status $status." >&2
	exit "$status"
fi
if ! grep -Eq '[0-9]+ passed, no failures' "$scratch/output"; then
	echo 'gleam test: the run ended without its summary line, so the VM died partway through.' >&2
	exit 1
fi

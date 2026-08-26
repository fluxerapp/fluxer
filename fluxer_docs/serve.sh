#!/usr/bin/env bash
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV="$HERE/.venv"
REQUIREMENTS_STAMP="$VENV/.requirements.sha256"
ADDR="${ZENSICAL_DEV_ADDR:-0.0.0.0:8000}"
LOG="${ZENSICAL_LOG:-/tmp/zensical-serve.log}"
PORT="${ADDR##*:}"

cd "$HERE"

ensure_env() {
	if [ ! -x "$VENV/bin/python" ]; then
		python3 -m venv "$VENV"
		"$VENV/bin/python" -m pip install --quiet --upgrade pip
	fi
	python_fingerprint="$(python3 -c 'import platform, sys; print(f"{sys.implementation.cache_tag}:{platform.machine()}")')"
	requirements_hash="$(printf '%s\0%s\n' "$python_fingerprint" "$(sha256sum "$HERE/requirements.txt" | cut -d' ' -f1)" | sha256sum | cut -d' ' -f1)"
	if [ ! -f "$REQUIREMENTS_STAMP" ] || [ "$(cat "$REQUIREMENTS_STAMP")" != "$requirements_hash" ]; then
		"$VENV/bin/python" -m pip install --quiet --require-virtualenv -r "$HERE/requirements.txt"
		printf '%s\n' "$requirements_hash" >"$REQUIREMENTS_STAMP"
	fi
}

case "${1:-serve}" in
--bootstrap)
	ensure_env
	;;
--daemon)
	ensure_env
	if curl -sf -o /dev/null "http://127.0.0.1:${PORT}/" 2>/dev/null; then
		echo "zensical already serving on ${ADDR}"
		exit 0
	fi
	setsid "$VENV/bin/zensical" serve -a "$ADDR" >"$LOG" 2>&1 &
	disown 2>/dev/null || true
	echo "zensical serving on ${ADDR} (logs: ${LOG})"
	;;
serve)
	ensure_env
	exec "$VENV/bin/zensical" serve -a "$ADDR"
	;;
*)
	ensure_env
	exec "$VENV/bin/zensical" "$@"
	;;
esac

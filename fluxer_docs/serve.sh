#!/usr/bin/env bash
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV="$HERE/.venv"
ADDR="${ZENSICAL_DEV_ADDR:-0.0.0.0:8000}"
LOG="${ZENSICAL_LOG:-/tmp/zensical-serve.log}"
HOST="${ADDR%:*}"
PORT="${ADDR##*:}"

case "$HOST" in
	0.0.0.0) READY_HOST="127.0.0.1" ;;
	"[::]") READY_HOST="[::1]" ;;
	*) READY_HOST="$HOST" ;;
esac
READY_URL="http://${READY_HOST}:${PORT}/"

cd "$HERE"

ensure_env() {
	if [ ! -x "$VENV/bin/python" ]; then
		python3 -m venv "$VENV"
		"$VENV/bin/python" -m pip install --quiet --upgrade pip
	fi
	"$VENV/bin/python" -m pip install --quiet --require-virtualenv -r "$HERE/requirements.txt"
}

is_ready() {
	curl --fail --silent --output /dev/null --noproxy '*' --connect-timeout 1 --max-time 2 "$READY_URL"
}

stop_daemon() {
	local pid="$1"
	kill "$pid" 2>/dev/null || true
	for _ in {1..50}; do
		if ! kill -0 "$pid" 2>/dev/null; then
			wait "$pid" 2>/dev/null || true
			return
		fi
		sleep 0.1
	done
	kill -KILL "$pid" 2>/dev/null || true
	wait "$pid" 2>/dev/null || true
}

case "${1:-serve}" in
	--bootstrap)
		ensure_env
		;;
	--daemon)
		ensure_env
		if is_ready; then
			echo "zensical already serving on ${ADDR}"
			exit 0
		fi
		setsid "$VENV/bin/zensical" serve -a "$ADDR" </dev/null >"$LOG" 2>&1 &
		pid=$!
		deadline=$((SECONDS + 30))
		while ((SECONDS < deadline)); do
			if is_ready; then
				disown "$pid" 2>/dev/null || true
				echo "zensical serving on ${ADDR} (logs: ${LOG})"
				exit 0
			fi
			if ! kill -0 "$pid" 2>/dev/null; then
				status=0
				wait "$pid" || status=$?
				echo "zensical exited during startup with status ${status} (logs: ${LOG})" >&2
				tail -n 20 "$LOG" >&2 || true
				exit 1
			fi
			sleep 0.25
		done
		stop_daemon "$pid"
		echo "timed out waiting for zensical on ${ADDR} (logs: ${LOG})" >&2
		tail -n 20 "$LOG" >&2 || true
		exit 1
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

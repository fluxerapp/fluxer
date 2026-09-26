#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

BUDGET="${BENCH_REGRESSION_BUDGET_PERCENT:-5}"
MEASUREMENT_TIME="${BENCH_MEASUREMENT_TIME:-5}"
WARM_UP_TIME="${BENCH_WARM_UP_TIME:-2}"

log() { printf '[check-regression] %s\n' "$*" >&2; }
fail() {
	log "ERROR: $*"
	exit 1
}

if [ "$#" -ne 2 ]; then
	fail "usage: $0 <crate-path> <bench-name>"
fi

CRATE_PATH="$1"
BENCH_NAME="$2"

[ -d "$CRATE_PATH" ] || fail "crate path not a directory: $CRATE_PATH"
[ -f "$CRATE_PATH/Cargo.toml" ] || fail "no Cargo.toml in $CRATE_PATH"
BASELINE="$CRATE_PATH/benches/baseline.json"
[ -f "$BASELINE" ] || fail "no baseline.json at $BASELINE"
command -v cargo >/dev/null || fail "cargo not on PATH"
command -v python3 >/dev/null || fail "python3 not on PATH"

LOG_FILE="$(mktemp -t bench-regression.XXXXXX.log)"
trap 'rm -f "$LOG_FILE"' EXIT

log "running cargo bench --bench $BENCH_NAME in $CRATE_PATH"
(
	cd "$CRATE_PATH"
	cargo bench --bench "$BENCH_NAME" -- \
		--warm-up-time "$WARM_UP_TIME" \
		--measurement-time "$MEASUREMENT_TIME"
) >"$LOG_FILE" 2>&1 || {
	tail -50 "$LOG_FILE" >&2
	fail "cargo bench failed; see log above"
}

BUDGET_PERCENT="$BUDGET" BASELINE_PATH="$BASELINE" \
	python3 "$(dirname "$0")/_compare_criterion.py" "$LOG_FILE"

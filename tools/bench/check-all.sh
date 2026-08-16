#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

DEFAULT_CRATES=$(cat <<'ENTRIES'
fluxer_desktop/native/rt-thread:tick
fluxer_desktop/native/audio-mix:mix
fluxer_desktop/native/screen-frame-bus:staging
fluxer_desktop/native/screen-frame-bus:frame_pool
fluxer_desktop/native/encoder-ring:ring
fluxer_desktop/native/linux-audio-capture:end_to_end
fluxer_desktop/native/linux-screen-capture:pipewire_callback
fluxer_desktop/native/rust:native_core
ENTRIES
)

ENTRIES="${BENCH_CRATES:-$DEFAULT_CRATES}"

log() { printf '[check-all] %s\n' "$*" >&2; }

pass_count=0
fail_count=0
failed_names=()

while IFS= read -r entry; do
  [ -n "$entry" ] || continue
  case "$entry" in
    \#*) continue;;
  esac
  crate="${entry%%:*}"
  bench="${entry##*:}"
  log "==> $crate :: $bench"
  if "$SCRIPT_DIR/check-regression.sh" "$REPO_ROOT/$crate" "$bench"; then
    pass_count=$((pass_count + 1))
  else
    fail_count=$((fail_count + 1))
    failed_names+=("$crate::$bench")
  fi
done <<< "$ENTRIES"

printf '\n[check-all] summary: %d passed, %d failed\n' "$pass_count" "$fail_count"

if [ "$fail_count" -gt 0 ]; then
  printf '[check-all] failed entries:\n'
  for name in "${failed_names[@]}"; do
    printf '  - %s\n' "$name"
  done
  exit 1
fi

exit 0

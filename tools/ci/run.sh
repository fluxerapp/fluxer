#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

if [ -x "${FLUXER_CI_BIN:-}" ]; then
	exec "$FLUXER_CI_BIN" "$@"
fi

script_dir="$(cd "$(dirname "$0")" && pwd)"
exec cargo run --quiet --manifest-path "$script_dir/Cargo.toml" -- "$@"

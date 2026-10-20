#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../.." && pwd)"
cargo_bin="${CARGO_BIN:-cargo}"
cargo_deny_bin="${CARGO_DENY_BIN:-cargo-deny}"

cd "$repo_root"

failed=0

while IFS= read -r lockfile; do
	manifest="${lockfile%Cargo.lock}Cargo.toml"

	if [ ! -f "$manifest" ]; then
		printf '%s has no Cargo.toml next to it\n' "$lockfile" >&2
		failed=1
		continue
	fi

	workspace_root="$("$cargo_bin" locate-project --manifest-path "$manifest" --workspace --message-format plain)"
	if [ "$workspace_root" != "$repo_root/$manifest" ]; then
		printf '%s is unused because %s belongs to the workspace at %s\n' "$lockfile" "$manifest" "${workspace_root#"$repo_root/"}" >&2
		failed=1
		continue
	fi

	printf 'Advisories: %s\n' "$lockfile"
	if ! "$cargo_deny_bin" --manifest-path "$manifest" --locked check advisories; then
		failed=1
	fi
done < <(git ls-files '*Cargo.lock')

exit "$failed"

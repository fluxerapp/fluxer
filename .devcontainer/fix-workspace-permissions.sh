#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

owner="$(id -u):$(id -g)"

repair_tree() {
	local path="$1"
	local unwritable
	if [ ! -d "$path" ]; then
		echo "fix-workspace-permissions: expected mount is missing: $path" >&2
		exit 1
	fi
	unwritable="$(find "$path" -xdev \( -type d -o -type f \) ! -writable -print -quit 2>/dev/null || true)"
	if [ ! -w "$path" ] || [ -n "$unwritable" ]; then
		sudo find "$path" -xdev \( -type d -o -type f \) -exec chown "$owner" {} +
	fi
	unwritable="$(find "$path" -xdev \( -type d -o -type f \) ! -writable -print -quit 2>/dev/null || true)"
	if [ ! -w "$path" ] || [ -n "$unwritable" ]; then
		echo "fix-workspace-permissions: $path is not writable as $(id -un)" >&2
		exit 1
	fi
}

for path in \
	/workspaces/fluxer/target \
	/home/vscode/.cargo/registry \
	/home/vscode/.cargo/git \
	/home/vscode/.local \
	/home/vscode/.local/share/pnpm/store \
	/workspaces/fluxer/fluxer_docs/.venv; do
	repair_tree "$path"
done

while IFS= read -r -d '' path; do
	if mountpoint -q "$path"; then
		repair_tree "$path"
	fi
done < <(find /workspaces/fluxer -maxdepth 4 -type d -name node_modules -prune -print0)

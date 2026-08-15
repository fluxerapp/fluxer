#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

initialize_command="git -c submodule.fluxer_marketing.update=checkout submodule update --init -- fluxer_marketing"

if ! git -c submodule.fluxer_marketing.update=checkout submodule update --init -- fluxer_marketing; then
	printf '%s\n' \
		"Unable to initialize the private fluxerapp/marketing repository." \
		"Confirm that your GitHub account has access and that your existing Git credentials can authenticate, then run:" \
		"  $initialize_command" >&2
	exit 1
fi

if [[ ! -f fluxer_marketing/Cargo.toml ]]; then
	printf '%s\n' \
		"The marketing submodule initialized without its required Cargo.toml." \
		"Remove the inconsistent checkout and run:" \
		"  $initialize_command" >&2
	exit 1
fi

printf '%s\n' \
	"Private marketing source is initialized at fluxer_marketing." \
	"Install its dependencies explicitly with:" \
	"  pnpm --dir fluxer_marketing install --frozen-lockfile"

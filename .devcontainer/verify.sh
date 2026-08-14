#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -uo pipefail

QUICK=0
SKIP_INSTALL=0
for arg in "$@"; do
	case "$arg" in
		--quick) QUICK=1 ;;
		--skip-install) SKIP_INSTALL=1 ;;
		-h|--help) sed -n '2,25p' "$0"; exit 0 ;;
		*) echo "unknown argument: $arg" >&2; exit 2 ;;
	esac
done

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT" || exit 1

FAILURES=()
PASSED=0

stage() {
	local name="$1"
	shift
	echo
	echo "=== ${name} ==="
	echo "+ $*"
	local started
	started=$SECONDS
	if "$@"; then
		PASSED=$((PASSED + 1))
		echo "PASS ${name} ($((SECONDS - started))s)"
	else
		local status=$?
		FAILURES+=("${name} (exit ${status})")
		echo "FAIL ${name} (exit ${status}, $((SECONDS - started))s)"
	fi
}

echo "repository: ${REPO_ROOT}"
echo "node:       $(node --version 2>&1)"
echo "pnpm:       $(pnpm --version 2>&1)"
echo "rustc:      $(rustc --version 2>&1)"
echo "python3:    $(python3 --version 2>&1)"
echo "clang:      $(clang --version 2>&1 | head -1)"
echo "CC_wasm32_unknown_unknown=${CC_wasm32_unknown_unknown:-<unset>}"
echo "AR_wasm32_unknown_unknown=${AR_wasm32_unknown_unknown:-<unset>}"

stage "fonts: build_fonts.py --verify" python3 tools/fonts/build_fonts.py --verify

stage "docker: usable as the remote user" docker version --format '{{.Server.Version}}'

if [ "$SKIP_INSTALL" -eq 0 ]; then
	stage "pnpm install" pnpm install --frozen-lockfile
else
	echo
	echo "=== pnpm install === (skipped)"
fi

stage "wasm: pnpm --filter fluxer_app wasm:codegen" pnpm --filter fluxer_app wasm:codegen

stage "app: typecheck" pnpm --filter fluxer_app typecheck
stage "app: unit tests" pnpm --filter fluxer_app exec vitest run

if [ "$QUICK" -eq 0 ]; then
	stage "app: production build" pnpm --filter fluxer_app build
fi

stage "rust: fmt" cargo fmt --all -- --check
if [ "$QUICK" -eq 0 ]; then
	stage "rust: clippy (workspace)" cargo clippy --workspace --all-targets -- -D warnings
else
	stage "rust: clippy (servers)" cargo clippy -p fluxer_app_proxy -p fluxer_admin --all-targets -- -D warnings
fi
stage "rust: app proxy tests" cargo test -p fluxer_app_proxy

echo
echo "---------------------------------------------"
echo "${PASSED} stages passed, ${#FAILURES[@]} failed"
for failure in "${FAILURES[@]:-}"; do
	[ -n "$failure" ] && echo "  FAILED: ${failure}"
done
[ "${#FAILURES[@]}" -eq 0 ] || exit 1
echo "Devcontainer verification passed."

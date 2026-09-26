#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../.." && pwd)"
native_root="$repo_root/fluxer_desktop/native"
cargo_bin="${CARGO_BIN:-cargo}"
cargo_deny_bin="${CARGO_DENY_BIN:-cargo-deny}"
native_target_dir="${DESKTOP_NATIVE_CARGO_TARGET_DIR:-$repo_root/target/desktop-native-workspaces}"
mode="${1:-}"

linux_workspaces=(
	"audio-apm/Cargo.toml"
	"audio-mix/Cargo.toml"
	"audio-timing/Cargo.toml"
	"encoder-ring/Cargo.toml"
	"gpu-rebuild/Cargo.toml"
	"hardware-encoder/Cargo.toml"
	"linux-audio-capture/Cargo.toml"
	"linux-evdev/Cargo.toml"
	"linux-input-hook/Cargo.toml"
	"linux-notifications/Cargo.toml"
	"linux-portals/Cargo.toml"
	"linux-screen-capture/Cargo.toml"
	"platform-info/Cargo.toml"
	"rt-thread/Cargo.toml"
	"rust/Cargo.toml"
	"screen-frame-bus/Cargo.toml"
	"system-hunspell/Cargo.toml"
	"webauthn/Cargo.toml"
)

fuzz_workspaces=(
	"rust/fuzz/Cargo.toml"
)

macos_workspaces=(
	"mac-app-audio/Cargo.toml"
	"mac-clipboard/Cargo.toml"
	"mac-screen-capture/Cargo.toml"
	"mac-sysctl/Cargo.toml"
	"mac-tcc/Cargo.toml"
	"macos-input-hook/Cargo.toml"
)

windows_workspaces=(
	"win-clipboard/Cargo.toml"
	"win-game-capture/Cargo.toml"
	"win-process-loopback/Cargo.toml"
	"win-shell/Cargo.toml"
	"win-toast/Cargo.toml"
	"windows-input-hook/Cargo.toml"
)

fail() {
	printf '%s\n' "$1" >&2
	exit 1
}

if [ "$#" -ne 1 ]; then
	fail "Usage: $0 dependencies|fmt|clippy|test"
fi

case "$mode" in
dependencies | fmt | clippy | test) ;;
*) fail "Unknown desktop native workspace check: $mode" ;;
esac

manifest_list="$(mktemp)"
discovered_inventory="$(mktemp)"
expected_inventory="$(mktemp)"

cleanup() {
	rm -f "$manifest_list" "$discovered_inventory" "$expected_inventory"
}

trap cleanup EXIT

workspace_category() {
	local relative_manifest="$1"
	local classified_manifest

	for classified_manifest in "${linux_workspaces[@]}"; do
		if [ "$relative_manifest" = "$classified_manifest" ]; then
			printf 'linux\n'
			return
		fi
	done

	for classified_manifest in "${fuzz_workspaces[@]}"; do
		if [ "$relative_manifest" = "$classified_manifest" ]; then
			printf 'fuzz\n'
			return
		fi
	done

	for classified_manifest in "${macos_workspaces[@]}"; do
		if [ "$relative_manifest" = "$classified_manifest" ]; then
			printf 'macos\n'
			return
		fi
	done

	for classified_manifest in "${windows_workspaces[@]}"; do
		if [ "$relative_manifest" = "$classified_manifest" ]; then
			printf 'windows\n'
			return
		fi
	done

	printf 'unknown\n'
}

run_dependencies() {
	local manifest="$1"

	"$cargo_deny_bin" \
		--manifest-path "$manifest" \
		--all-features \
		--locked \
		check \
		-D warnings \
		-A unmatched-skip \
		-A unnecessary-skip \
		-D duplicate
}

run_format() {
	local manifest="$1"

	"$cargo_bin" fmt --manifest-path "$manifest" --all -- --check
}

run_clippy() {
	local manifest="$1"

	CARGO_TARGET_DIR="$native_target_dir" \
		"$cargo_bin" clippy \
		--manifest-path "$manifest" \
		--workspace \
		--all-targets \
		--all-features \
		--locked \
		-- \
		-D warnings
}

run_tests() {
	local manifest="$1"

	CI=true CARGO_TARGET_DIR="$native_target_dir" \
		"$cargo_bin" test \
		--manifest-path "$manifest" \
		--workspace \
		--all-features \
		--locked
}

find "$native_root" -type d -name target -prune -o -type f -name Cargo.toml -print | LC_ALL=C sort >"$manifest_list"

if [ ! -s "$manifest_list" ]; then
	fail "No independent desktop native workspaces found"
fi

linux_count=0
fuzz_count=0
macos_count=0
windows_count=0

while IFS= read -r manifest; do
	if ! grep -Eq '^[[:space:]]*\[workspace\][[:space:]]*$' "$manifest"; then
		fail "Desktop native manifest is not an independent workspace: $manifest"
	fi

	manifest_dir="${manifest%/Cargo.toml}"
	if [ ! -f "$manifest_dir/Cargo.lock" ]; then
		fail "Desktop native workspace has no adjacent Cargo.lock: $manifest"
	fi

	relative_manifest="${manifest#"$native_root/"}"
	category="$(workspace_category "$relative_manifest")"
	case "$category" in
	linux) linux_count=$((linux_count + 1)) ;;
	fuzz) fuzz_count=$((fuzz_count + 1)) ;;
	macos) macos_count=$((macos_count + 1)) ;;
	windows) windows_count=$((windows_count + 1)) ;;
	*) fail "Unclassified desktop native workspace: $relative_manifest" ;;
	esac
	printf '%s\n' "$relative_manifest" >>"$discovered_inventory"
done <"$manifest_list"

printf '%s\n' \
	"${linux_workspaces[@]}" \
	"${fuzz_workspaces[@]}" \
	"${macos_workspaces[@]}" \
	"${windows_workspaces[@]}" |
	LC_ALL=C sort >"$expected_inventory"

if ! diff -u "$expected_inventory" "$discovered_inventory"; then
	fail "Desktop native workspace inventory does not match discovery"
fi

if [ "$mode" = clippy ] || [ "$mode" = test ]; then
	host_kernel="$(uname -s)"
	if [ "$host_kernel" != Linux ]; then
		fail "Desktop native $mode checks require a Linux host, found: $host_kernel"
	fi
fi

checked_count=0

cd "$native_root"

while IFS= read -r manifest; do
	relative_manifest="${manifest#"$native_root/"}"
	category="$(workspace_category "$relative_manifest")"

	case "$mode" in
	dependencies)
		printf 'Dependencies: %s\n' "$relative_manifest"
		run_dependencies "$manifest"
		;;
	fmt)
		printf 'Format: %s\n' "$relative_manifest"
		run_format "$manifest"
		;;
	clippy)
		case "$category" in
		linux) printf 'Clippy full Linux/shared coverage: %s\n' "$relative_manifest" ;;
		fuzz) printf 'Clippy fuzz-target coverage: %s\n' "$relative_manifest" ;;
		macos) printf 'Clippy Linux stub/common coverage; macOS backend not validated: %s\n' "$relative_manifest" ;;
		windows) printf 'Clippy Linux stub/common coverage; Windows backend not validated: %s\n' "$relative_manifest" ;;
		esac
		run_clippy "$manifest"
		;;
	test)
		case "$category" in
		linux) printf 'Test full Linux/shared coverage: %s\n' "$relative_manifest" ;;
		fuzz) printf 'Test command is a no-op for test=false fuzz binaries: %s\n' "$relative_manifest" ;;
		macos) printf 'Test Linux stub/common coverage; macOS backend not validated: %s\n' "$relative_manifest" ;;
		windows) printf 'Test Linux stub/common coverage; Windows backend not validated: %s\n' "$relative_manifest" ;;
		esac
		run_tests "$manifest"
		;;
	esac
	checked_count=$((checked_count + 1))
done <"$manifest_list"

discovered_count=$((linux_count + fuzz_count + macos_count + windows_count))
printf 'Desktop native workspace categories: discovered=%s linux-full=%s fuzz=%s macos-stub-common=%s windows-stub-common=%s\n' "$discovered_count" "$linux_count" "$fuzz_count" "$macos_count" "$windows_count"
printf 'Desktop native %s summary: checked=%s\n' "$mode" "$checked_count"

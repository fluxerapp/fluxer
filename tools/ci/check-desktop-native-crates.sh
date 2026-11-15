#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "$script_dir/../.." && pwd)"
native_root="$repo_root/fluxer_desktop/native"
workspace_manifest="$repo_root/Cargo.toml"
cargo_bin="${CARGO_BIN:-cargo}"
native_target_dir="${DESKTOP_NATIVE_CARGO_TARGET_DIR:-$repo_root/target/desktop-native-crates}"
windows_backend_target="x86_64-pc-windows-msvc"
mode="${1:-}"

linux_crates=(
	"app-store:fluxer_app_store"
	"gateway-socket:fluxer_gateway_socket"
	"hardware-encoder:fluxer_hardware_encoder"
	"linux-audio-capture:fluxer_linux_audio_capture"
	"linux-evdev:fluxer_linux_evdev"
	"linux-input-hook:fluxer_linux_input_hook"
	"linux-notifications:fluxer_linux_notifications"
	"linux-portals:fluxer_linux_portals"
	"platform-info:fluxer_platform_info"
	"rt-thread:fluxer_rt_thread"
	"sample-pool:fluxer_sample_pool"
	"system-hunspell:fluxer_system_hunspell"
	"webauthn:fluxer_webauthn"
)

macos_crates=(
	"mac-app-audio:fluxer_mac_app_audio"
	"mac-clipboard:fluxer_mac_clipboard"
	"mac-screen-capture:fluxer_mac_screen_capture"
	"mac-sysctl:fluxer_mac_sysctl"
	"mac-tcc:fluxer_mac_tcc"
	"macos-input-hook:fluxer_macos_input_hook"
)

windows_crates=(
	"win-clipboard:fluxer_win_clipboard"
	"win-game-capture:fluxer_win_game_capture"
	"win-process-loopback:fluxer_win_process_loopback"
	"win-shell:fluxer_win_shell"
	"windows-input-hook:fluxer_windows_input_hook"
)

fail() {
	printf '%s\n' "$1" >&2
	exit 1
}

if [ "$#" -ne 1 ]; then
	fail "Usage: $0 inventory|clippy"
fi

case "$mode" in
inventory | clippy) ;;
*) fail "Unknown desktop native crate check: $mode" ;;
esac

discovered_inventory="$(mktemp)"
expected_inventory="$(mktemp)"

cleanup() {
	rm -f "$discovered_inventory" "$expected_inventory"
}

trap cleanup EXIT

find "$native_root" -mindepth 2 -maxdepth 2 -name Cargo.toml -print |
	sed -e "s|^$native_root/||" -e 's|/Cargo.toml$||' |
	LC_ALL=C sort >"$discovered_inventory"

for entry in "${linux_crates[@]}" "${macos_crates[@]}" "${windows_crates[@]}"; do
	printf '%s\n' "${entry%%:*}"
done | LC_ALL=C sort >"$expected_inventory"

if ! diff -u "$expected_inventory" "$discovered_inventory"; then
	fail "Desktop native crate inventory does not match discovery"
fi

while IFS= read -r crate_dir; do
	manifest="$native_root/$crate_dir/Cargo.toml"
	if grep -Eq '^[[:space:]]*\[workspace\][[:space:]]*$' "$manifest"; then
		fail "Desktop native crate declares its own workspace: $manifest"
	fi
	if [ -e "$native_root/$crate_dir/Cargo.lock" ]; then
		fail "Desktop native crate has its own Cargo.lock, the root Cargo.lock is the only one: $crate_dir"
	fi
	if ! grep -Fq "\"fluxer_desktop/native/$crate_dir\"" "$workspace_manifest"; then
		fail "Desktop native crate is not a member of the root workspace: $crate_dir"
	fi
done <"$discovered_inventory"

if [ "$mode" = inventory ]; then
	printf 'Desktop native crates: %s, all in the root workspace\n' "$(wc -l <"$discovered_inventory" | tr -d ' ')"
	exit 0
fi

host_kernel="$(uname -s)"
if [ "$host_kernel" != Linux ]; then
	fail "Desktop native clippy checks require a Linux host, found: $host_kernel"
fi

run_clippy() {
	local package="$1"
	shift

	CARGO_TARGET_DIR="$native_target_dir" \
		"$cargo_bin" clippy \
		--manifest-path "$workspace_manifest" \
		-p "$package" \
		--all-targets \
		--all-features \
		--locked \
		"$@" \
		-- \
		-D warnings
}

cd "$native_root"

for entry in "${linux_crates[@]}"; do
	printf 'Clippy full Linux/shared coverage: %s\n' "${entry%%:*}"
	run_clippy "${entry#*:}"
done

for entry in "${macos_crates[@]}"; do
	printf 'Clippy Linux stub/common coverage; macOS backend not validated: %s\n' "${entry%%:*}"
	run_clippy "${entry#*:}"
done

for entry in "${windows_crates[@]}"; do
	printf 'Clippy Linux stub/common coverage: %s\n' "${entry%%:*}"
	run_clippy "${entry#*:}"
	printf 'Clippy Windows backend coverage for %s: %s\n' "$windows_backend_target" "${entry%%:*}"
	run_clippy "${entry#*:}" --target "$windows_backend_target"
done

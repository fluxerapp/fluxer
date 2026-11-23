#!/usr/bin/env sh

# SPDX-License-Identifier: AGPL-3.0-or-later

set -eu

root="$(CDPATH='' cd "$(dirname "$0")/.." && pwd -P)"
target="$root/priv/gateway_nif.so"

stale() {
	[ ! -f "$target" ] && return 0
	[ -n "$(find "$root/rust" -path "$root/rust/target" -prune -o -newer "$target" -print | head -n 1)" ] && return 0
	return 1
}

if [ "${1:-}" != "--force" ] && ! stale; then
	exit 0
fi

echo 'nif: rust sources are newer than priv/gateway_nif.so, rebuilding.' >&2
cargo build --release -p gateway_nif --manifest-path "$root/rust/Cargo.toml"

case "$(uname -s)" in
Darwin) library="$root/rust/target/release/libgateway_nif.dylib" ;;
*) library="$root/rust/target/release/libgateway_nif.so" ;;
esac

if [ ! -f "$library" ]; then
	echo "nif: $library was not built." >&2
	exit 1
fi

mkdir -p "$root/priv"
cp "$library" "$target.tmp"
mv "$target.tmp" "$target"

#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CRATE_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

require_pkg_config() {
	if ! command -v pkg-config >/dev/null 2>&1; then
		echo "error: pkg-config is required but was not found in PATH." >&2
		echo "       Install via: brew install pkg-config   (macOS)" >&2
		echo "                    apt-get install pkg-config (Debian/Ubuntu)" >&2
		exit 1
	fi
}

assert_webrtc_pkgconfig() {
	if ! pkg-config --exists webrtc-audio-processing-2; then
		echo "error: pkg-config cannot locate webrtc-audio-processing-2." >&2
		echo "       See fluxer_desktop/native/audio-apm/README.md for install steps." >&2
		exit 1
	fi
}

assert_absl_pkgconfig() {
	if ! pkg-config --exists absl_base; then
		echo "error: pkg-config cannot locate absl_base." >&2
		echo "       Install via: brew install abseil   (macOS)" >&2
		echo "                    apt-get install libabsl-dev (Debian/Ubuntu)" >&2
		exit 1
	fi
}

inject_absl_include_path() {
	local absl_includes
	absl_includes="$(pkg-config --cflags-only-I absl_base)"
	if [[ -z "${absl_includes}" ]]; then
		echo "error: pkg-config returned no include path for absl_base." >&2
		exit 1
	fi

	local prev_cxxflags="${CXXFLAGS-}"
	local prev_bindgen="${BINDGEN_EXTRA_CLANG_ARGS-}"

	export CXXFLAGS="${absl_includes} ${prev_cxxflags}"
	export BINDGEN_EXTRA_CLANG_ARGS="${absl_includes} ${prev_bindgen}"
}

main() {
	cd "${CRATE_DIR}"

	local cargo_subcmd="build"
	if [[ $# -gt 0 && "$1" != -* && "$1" != "--" ]]; then
		cargo_subcmd="$1"
		shift
	fi

	require_pkg_config
	assert_webrtc_pkgconfig
	assert_absl_pkgconfig
	inject_absl_include_path

	exec cargo "${cargo_subcmd}" --features real-apm "$@"
}

main "$@"

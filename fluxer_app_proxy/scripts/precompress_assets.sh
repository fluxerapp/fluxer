#!/usr/bin/env sh
# SPDX-License-Identifier: AGPL-3.0-or-later

set -eu

if [ "${PRECOMPRESS_WORKER:-}" = "1" ]; then
	source_file="$1"
	source_size="$(wc -c <"$source_file")"

	if brotli -q "$PRECOMPRESS_BROTLI_QUALITY" -f -o "$source_file.br" "$source_file" 2>/dev/null; then
		if [ "$(wc -c <"$source_file.br")" -ge "$source_size" ]; then
			rm -f "$source_file.br"
		fi
	else
		rm -f "$source_file.br"
	fi

	if gzip -n "-$PRECOMPRESS_GZIP_LEVEL" -c "$source_file" >"$source_file.gz" 2>/dev/null; then
		if [ "$(wc -c <"$source_file.gz")" -ge "$source_size" ]; then
			rm -f "$source_file.gz"
		fi
	else
		rm -f "$source_file.gz"
	fi

	exit 0
fi

root="${1:?usage: precompress_assets.sh <asset-root>}"

case "$0" in
/*) script="$0" ;;
*) script="$PWD/$0" ;;
esac

PRECOMPRESS_WORKER=1
PRECOMPRESS_BROTLI_QUALITY="${PRECOMPRESS_BROTLI_QUALITY:-11}"
PRECOMPRESS_GZIP_LEVEL="${PRECOMPRESS_GZIP_LEVEL:-9}"
export PRECOMPRESS_WORKER PRECOMPRESS_BROTLI_QUALITY PRECOMPRESS_GZIP_LEVEL

min_size="${PRECOMPRESS_MIN_SIZE_BYTES:-1024}"
jobs="${PRECOMPRESS_JOBS:-$(nproc 2>/dev/null || echo 1)}"

find "$root" -type f \
	! -name '*.br' \
	! -name '*.gz' \
	-size +"${min_size}"c \
	\( \
	-name '*.css' -o \
	-name '*.html' -o \
	-name '*.js' -o \
	-name '*.json' -o \
	-name '*.map' -o \
	-name '*.mjs' -o \
	-name '*.svg' -o \
	-name '*.txt' -o \
	-name '*.wasm' -o \
	-name '*.webmanifest' -o \
	-name '*.xml' \
	\) \
	-print0 |
	xargs -0 -r -n 1 -P "$jobs" "$script"

#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later

set -eu

here=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)
manifest="$here/spellcheck-dictionaries.sha256"
dest=${1:-"$here/desktop/spellcheck/dictionaries"}

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT INT TERM

check() {
	(cd "$1" && sha256sum -c "$manifest") >"$tmp/check" 2>&1
}

if [ -d "$dest" ] && check "$dest"; then
	exit 0
fi

mkdir "$tmp/out"
for pkg in $(awk '{ sub("/.*", "", $2); print $2 }' "$manifest" | sort -u); do
	name=${pkg%@*}
	version=${pkg##*@}
	curl -fsSL --retry 3 -o "$tmp/$pkg.tgz" "https://registry.npmjs.org/$name/-/$name-$version.tgz"
	mkdir "$tmp/$pkg" "$tmp/out/$pkg"
	tar -xzf "$tmp/$pkg.tgz" -C "$tmp/$pkg"
	for file in $(awk -v prefix="$pkg/" 'index($2, prefix) == 1 { print substr($2, length(prefix) + 1) }' "$manifest"); do
		case $file in
		LICENSE) source=license ;;
		*) source=$file ;;
		esac
		cp "$tmp/$pkg/package/$source" "$tmp/out/$pkg/$file"
	done
done

if ! check "$tmp/out"; then
	grep -v ': OK$' "$tmp/check" >&2
	echo "Spellcheck dictionaries do not match $manifest" >&2
	exit 1
fi

mkdir -p "$dest"
cp -R "$tmp/out/." "$dest/"

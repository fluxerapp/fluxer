#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

die() {
    printf 'valkey.sh: %s\n' "$*" >&2
    exit 1
}

[ "$#" -eq 1 ] || {
    printf 'usage: valkey.sh <dist-dir>\n' >&2
    exit 64
}

mkdir -p "$1"
dist="$(cd "$1" && pwd)"
tarball="$("$here/fetch-thirdparty.sh" fetch valkey any)"
version="$(basename "$tarball" .tar.gz)"
jobs="${JOBS:-$(nproc)}"

case "$(uname -m)" in
x86_64 | amd64) lg_page=12 ;;
*) lg_page=16 ;;
esac

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

tar -xzf "$tarball" -C "$work" --strip-components=1
grep -qF 'cd jemalloc && ./configure ' "$work/deps/Makefile" \
    || die "deps/Makefile no longer configures the bundled jemalloc the expected way"
sed -i "s!cd jemalloc && ./configure !&--with-lg-page=${lg_page} --with-lg-hugepage=21 !" "$work/deps/Makefile"

make -C "$work" -j"$jobs" BUILD_TLS=no USE_SYSTEMD=no USE_FAST_FLOAT=yes all

install -D -m 0755 "$work/src/valkey-server" "$dist/bin/valkey-server"
install -D -m 0755 "$work/src/valkey-cli" "$dist/bin/valkey-cli"
"$dist/bin/valkey-server" --version | grep -qF " v=${version} " \
    || die "built valkey-server does not report version ${version}"

licenses="$dist/share/LICENSES/valkey"
rm -rf "$licenses"
install -D -m 0644 "$work/COPYING" "$licenses/COPYING"
while IFS= read -r file; do
    install -D -m 0644 "$file" "$licenses/${file#"$work/"}"
done < <(find "$work/deps" -maxdepth 2 -type f \( -iname 'COPYING*' -o -iname 'LICENSE*' \) | sort)

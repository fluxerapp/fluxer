#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOCK="$here/thirdparty.lock"
CACHE="${FLUXER_DIST_CACHE:-${XDG_CACHE_HOME:-$HOME/.cache}/fluxer-dist}"
tmp=""
trap '[ -z "$tmp" ] || rm -rf "$tmp"' EXIT

log() { printf '==> %s\n' "$*" >&2; }

die() {
    printf 'fetch-thirdparty: %s\n' "$*" >&2
    exit 1
}

usage() {
    printf '%s\n' \
        "usage: fetch-thirdparty.sh install <amd64|arm64> <dist-dir>" \
        "       fetch-thirdparty.sh fetch <name> <amd64|arm64|any>" >&2
    exit 64
}

sha256_of() {
    sha256sum "$1" | cut -d' ' -f1
}

fetch_url() {
    local sha="$1" url="$2" dest
    dest="$CACHE/$sha/$(basename "$url")"
    if [ -f "$dest" ] && [ "$(sha256_of "$dest")" = "$sha" ]; then
        printf '%s\n' "$dest"
        return
    fi
    mkdir -p "$(dirname "$dest")"
    log "downloading $url"
    curl -fsSL --retry 3 --retry-delay 2 -o "$dest.part" "$url"
    local actual
    actual="$(sha256_of "$dest.part")"
    if [ "$actual" != "$sha" ]; then
        rm -f "$dest.part"
        die "sha256 mismatch for $url: expected $sha, got $actual"
    fi
    mv "$dest.part" "$dest"
    printf '%s\n' "$dest"
}

entries_for() {
    awk -v a="$1" 'NF && ($2 == a || $2 == "any")' "$LOCK"
}

install_all() {
    local arch="$1" dist="$2" name entry_arch kind sha url member path
    tmp="$(mktemp -d)"
    while read -r name entry_arch kind sha url member; do
        path="$(fetch_url "$sha" "$url")"
        case "$kind" in
        bin)
            install -D -m 0755 "$path" "$dist/bin/$name"
            ;;
        tgz)
            tar -xzf "$path" -C "$tmp" "$member"
            install -D -m 0755 "$tmp/$member" "$dist/bin/$name"
            rm -f "$tmp/$member"
            ;;
        license)
            install -D -m 0644 "$path" "$dist/share/LICENSES/$name"
            ;;
        src) ;;
        *)
            die "unknown kind $kind for $name ($entry_arch) in $LOCK"
            ;;
        esac
    done < <(entries_for "$arch")
}

fetch_one() {
    local name="$1" arch="$2" matches
    matches="$(awk -v n="$name" -v a="$arch" '$1 == n && ($2 == a || $2 == "any")' "$LOCK")"
    [ -n "$matches" ] || die "no entry for $name ($arch) in $LOCK"
    [ "$(printf '%s\n' "$matches" | wc -l)" -eq 1 ] || die "more than one entry for $name ($arch) in $LOCK"
    local sha url
    read -r _ _ _ sha url _ <<<"$matches"
    fetch_url "$sha" "$url"
}

[ "$#" -ge 1 ] || usage
case "$1" in
install)
    [ "$#" -eq 3 ] || usage
    case "$2" in amd64 | arm64) ;; *) usage ;; esac
    mkdir -p "$3"
    install_all "$2" "$(cd "$3" && pwd)"
    ;;
fetch)
    [ "$#" -eq 3 ] || usage
    fetch_one "$2" "$3"
    ;;
*)
    usage
    ;;
esac

#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$here/../.." && pwd)"

RUST_PARTS=(server media-proxy)
declare -A RUST_CRATE=(
    [server]=fluxer-server
    [media-proxy]=fluxer-media-proxy
)
ALL_PARTS=(fluxerd "${RUST_PARTS[@]}" gateway api app valkey thirdparty share)
STATIC_ENTRIES=(avatars badges desktop email emoji web LICENSE THIRD_PARTY_LICENSES.md)
TOP_LEVEL=(bin lib gateway api app share tsconfigs)

log() { printf '==> %s\n' "$*" >&2; }

die() {
    printf 'build.sh: %s\n' "$*" >&2
    exit 1
}

usage() {
    cat >&2 <<EOF
usage: build.sh [--only part,...] [--into dist-dir] [--no-pack] <out-dir>

parts: ${ALL_PARTS[*]} rust
--into rebuilds the chosen parts inside an existing unpacked dist directory.
Without --into the dist directory is <out-dir>/fluxer-linux-<arch>, rebuilt from scratch.
The tarball fluxer-<version>-linux-<arch>.tar.zst and SHA256SUMS are written to <out-dir>.

environment: BUILD_VERSION, PROFILE (release or release-fast), FLUXER_TOOLCHAIN,
FLUXER_NATIVE_PREFIX, PNPM_STORE_DIR, JOBS or CARGO_BUILD_JOBS, ZSTD_LEVEL
EOF
    exit 64
}

need() {
    local tool
    for tool in "$@"; do
        command -v "$tool" >/dev/null 2>&1 || die "$tool is required but is not on PATH"
    done
}

only=""
into=""
pack=1
while [ "$#" -gt 0 ]; do
    case "$1" in
    --only)
        only="${2:?--only needs a list of parts}"
        shift 2
        ;;
    --only=*)
        only="${1#*=}"
        shift
        ;;
    --into)
        into="${2:?--into needs a directory}"
        shift 2
        ;;
    --into=*)
        into="${1#*=}"
        shift
        ;;
    --no-pack)
        pack=0
        shift
        ;;
    -h | --help)
        usage
        ;;
    -*)
        die "unknown option $1"
        ;;
    *)
        break
        ;;
    esac
done
[ "$#" -eq 1 ] || usage

declare -A want=()
if [ -z "$only" ]; then
    for part in "${ALL_PARTS[@]}"; do want[$part]=1; done
else
    IFS=, read -ra requested <<<"$only"
    for part in "${requested[@]}"; do
        if [ "$part" = rust ]; then
            for p in "${RUST_PARTS[@]}"; do want[$p]=1; done
            continue
        fi
        [[ " ${ALL_PARTS[*]} " == *" $part "* ]] || die "unknown part $part, expected one of: ${ALL_PARTS[*]} rust"
        want[$part]=1
    done
fi

selected() {
    local part
    for part in "$@"; do
        [ -z "${want[$part]:-}" ] || return 0
    done
    return 1
}

[ "$(uname -s)" = Linux ] || die "build.sh builds Linux releases only"
case "$(uname -m)" in
x86_64 | amd64) arch=amd64 ;;
aarch64 | arm64) arch=arm64 ;;
*) die "unsupported architecture $(uname -m)" ;;
esac

profile="${PROFILE:-release}"
case "$profile" in
dev | test | bench) die "PROFILE=$profile is not a release profile" ;;
esac
target_dir="$repo/target/$profile"

if [ -n "${FLUXER_TOOLCHAIN:-}" ]; then
    [ -d "$FLUXER_TOOLCHAIN/bin" ] || die "no toolchain in $FLUXER_TOOLCHAIN, run make toolchain first"
    export RUSTUP_HOME="$FLUXER_TOOLCHAIN/rustup" CARGO_HOME="$FLUXER_TOOLCHAIN/cargo"
    export PATH="$FLUXER_TOOLCHAIN/bin:$CARGO_HOME/bin:$PATH"
fi
native="${FLUXER_NATIVE_PREFIX:-}"
export CARGO_INCREMENTAL=0 ELECTRON_SKIP_BINARY_DOWNLOAD=1
export JOBS="${JOBS:-${CARGO_BUILD_JOBS:-$(nproc)}}"
store_args=()
[ -z "${PNPM_STORE_DIR:-}" ] || store_args=(--store-dir "$PNPM_STORE_DIR")

mkdir -p "$1"
out="$(cd "$1" && pwd)"
if [ -n "$into" ]; then
    [ -d "$into" ] || die "$into is not an unpacked dist directory"
    dist="$(cd "$into" && pwd)"
else
    dist="$out/fluxer-linux-$arch"
    rm -rf "$dist"
    mkdir -p "$dist"
fi

version="${BUILD_VERSION:-}"
if [ -z "$version" ] && [ -f "$dist/share/VERSION" ]; then
    version="$(cat "$dist/share/VERSION")"
fi
if [ -z "$version" ]; then
    read -r year monthday hms < <(date -u '+%Y %-m%d %H%M%S')
    version="$year.$monthday.$((10#$hms))"
fi
[[ "$version" =~ ^[0-9A-Za-z][0-9A-Za-z._+~-]*$ ]] || die "version $version is not usable in a file name"
if [ -n "$into" ] && [ -f "$dist/share/VERSION" ] && ! selected app; then
    built="$(cat "$dist/share/VERSION")"
    [ "$version" = "$built" ] || die "$dist holds release $built and its app bundle has that version, add app to --only to build $version"
fi
export BUILD_VERSION="$version"

run() {
    local name="$1" start=$SECONDS
    shift
    log "$name"
    "$@"
    log "$name done in $((SECONDS - start))s"
}

build_ci() {
    need cargo
    (cd "$repo" && cargo build --profile "$profile" --locked -p fluxer-ci --no-default-features)
    export FLUXER_CI_BIN="$target_dir/fluxer-ci"
}

pnpm_install() {
    need node pnpm
    (cd "$repo" && pnpm install --frozen-lockfile --config.confirm-modules-purge=false "${store_args[@]}")
}

require_native() {
    [ -n "$native" ] || die "set FLUXER_NATIVE_PREFIX to the install-native-deps.sh prefix"
    [ -f "$native/lib/pkgconfig/libheif.pc" ] || die "no libheif in $native, run make native-deps first"
    need pkg-config ldd
    local prefix
    prefix="$(PKG_CONFIG_PATH="$native/lib/pkgconfig" pkg-config --variable=prefix libheif)"
    [ "$prefix" = "$native" ] || die "libheif resolves to $prefix instead of $native"
}

check_media_ldd() {
    local listing bad
    listing="$(LD_LIBRARY_PATH="$dist/lib" ldd "$dist/bin/fluxer-media-proxy")"
    printf '%s\n' "$listing" >&2
    if grep -q 'not found' <<<"$listing"; then
        die "fluxer-media-proxy has unresolved libraries"
    fi
    grep -qF "libheif.so.1 => $dist/lib/libheif.so.1 " <<<"$listing" \
        || die "libheif.so.1 does not resolve from $dist/lib"
    bad="$(grep -E '^[[:space:]]*lib(heif|av[a-z]+|sw[a-z]+)\.so' <<<"$listing" | grep -vF " => $dist/lib/" || true)"
    [ -z "$bad" ] || die "media libraries resolve outside $dist/lib: $bad"
}

check_admin_assets() {
    local out_dir
    for out_dir in "$target_dir"/build/fluxer_admin-*/out/static; do
        [ -s "$out_dir/app.css" ] || continue
        [ "$(find "$out_dir/fonts" -maxdepth 1 -name '*.woff2' | wc -l)" -eq 16 ] || continue
        compgen -G "$out_dir/fonts/fonts.*.css" >/dev/null || continue
        return 0
    done
    die "fluxer_admin was built without its Tailwind CSS or its 16 bundled fonts"
}

build_rust() {
    local part pkgs=()
    for part in "${RUST_PARTS[@]}"; do
        selected "$part" && pkgs+=(-p "${RUST_CRATE[$part]}")
    done
    need cargo
    if selected media-proxy; then
        require_native
        (cd "$repo" && PKG_CONFIG_PATH="$native/lib/pkgconfig:$native/lib64/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}" \
            LD_LIBRARY_PATH="$native/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
            cargo build --profile "$profile" --locked "${pkgs[@]}")
    else
        (cd "$repo" && cargo build --profile "$profile" --locked "${pkgs[@]}")
    fi
    for part in "${RUST_PARTS[@]}"; do
        selected "$part" || continue
        install -D -m 0755 "$target_dir/${RUST_CRATE[$part]}" "$dist/bin/fluxer-$part"
    done
    if selected server; then
        check_admin_assets
    fi
    if selected media-proxy; then
        rm -rf "${dist:?}/lib"
        mkdir -p "$dist/lib"
        cp -a "$native"/lib/*.so.* "$dist/lib/"
        check_media_ldd
    fi
}

build_gateway() {
    need rebar3 erl cargo
    local release
    (
        cd "$repo/fluxer_gateway"
        export REBAR_SKIP_PROJECT_PLUGINS=1
        rebar3 compile --deps_only
        cp config/sys.config.template config/sys.config
        rebar3 as prod release
        rm -f config/sys.config
    )
    release="$repo/fluxer_gateway/_build/prod/rel/fluxer_gateway"
    compgen -G "$release/releases/*/vm.args.native.src" >/dev/null \
        || die "the gateway release has no vm.args.native.src, check the prod overlay in rebar.config"
    compgen -G "$release/releases/*/sys.native.config" >/dev/null \
        || die "the gateway release has no sys.native.config, check the prod overlay in rebar.config"
    rm -rf "$dist/gateway"
    cp -a "$release" "$dist/gateway"
    mv "$dist/gateway/bin/fluxer_gateway" "$dist/gateway/bin/fluxer_gateway.real"
    install -m 0755 "$repo/fluxer_gateway/scripts/fluxer_gateway_release_wrapper.sh" "$dist/gateway/bin/fluxer_gateway"
}

build_api() {
    need node pnpm
    local node_bin
    (cd "$repo" && pnpm --filter fluxer_api run build)
    rm -rf "$dist/api" "$dist/tsconfigs"
    (cd "$repo" && pnpm deploy --legacy --filter=fluxer_api --prod --config.allow-unused-patches=true "${store_args[@]}" "$dist/api")
    rm -rf "$dist/api/pkgs" "$dist/api/dist" "$dist/api/bin"
    cp -a "$repo/fluxer_api/dist" "$dist/api/dist"
    cp -a "$repo/fluxer_api/bin" "$dist/api/bin"
    cp -a "$repo/tsconfigs" "$dist/tsconfigs"
    remove_escaping_links "$dist/api"
    node_bin="$(readlink -f "$(command -v node)")"
    install -D -m 0755 "$node_bin" "$dist/bin/node"
    install -D -m 0644 "$(dirname "$(dirname "$node_bin")")/LICENSE" "$dist/share/LICENSES/node/LICENSE"
}

escaping_links() {
    local link target root
    root="$(realpath "$dist")"
    while IFS= read -r link; do
        target="$(realpath -m "$link")"
        [[ "$target" == "$root"/* ]] || printf '%s\n' "$link"
    done < <(find "$@" -type l)
}

remove_escaping_links() {
    local link
    while IFS= read -r link; do
        log "removing $link, it points at $(readlink "$link") outside the dist"
        rm "$link"
    done < <(escaping_links "$1")
}

build_app() {
    need node pnpm brotli
    local static="$dist/app/static" entry
    (cd "$repo" && FLUXER_SELF_HOSTED=true PUBLIC_ASSET_BASE_URL="" BUNDLE_LOCAL_ASSETS=true pnpm --filter fluxer_app build)
    rm -rf "$dist/app"
    mkdir -p "$dist/app"
    cp -a "$repo/fluxer_app/dist" "$static"
    "$repo/fluxer_app_proxy/scripts/precompress_assets.sh" "$static"
    for entry in "${STATIC_ENTRIES[@]}"; do
        [ ! -e "$static/$entry" ] || die "the app build already has $entry, refusing to overwrite it with fluxer_static/$entry"
        cp -a "$repo/fluxer_static/$entry" "$static/$entry"
    done
    "$repo/fluxer_static/fetch-spellcheck-dictionaries.sh" "$static/desktop/spellcheck/dictionaries"
}

build_valkey() {
    need make cc
    "$here/valkey.sh" "$dist"
}

build_thirdparty() {
    need curl sha256sum
    "$here/fetch-thirdparty.sh" install "$arch" "$dist"
}

build_fluxerd() {
    need go
    local licenses="$dist/share/LICENSES/fluxerd" module dir file
    (cd "$repo/fluxer_native" && CGO_ENABLED=0 GOTOOLCHAIN=local go build -trimpath -ldflags='-s -w' -o "$dist/bin/fluxerd" ./cmd/fluxerd)
    rm -rf "$licenses"
    install -D -m 0644 "$(go env GOROOT)/LICENSE" "$licenses/go/LICENSE"
    while read -r module dir; do
        [ "$dir" != "$repo/fluxer_native" ] || continue
        while IFS= read -r file; do
            install -D -m 0644 "$file" "$licenses/$module/$(basename "$file")"
        done < <(find "$dir" -maxdepth 1 -type f \( -iname 'LICENSE*' -o -iname 'COPYING*' -o -iname 'NOTICE*' \))
    done < <(cd "$repo/fluxer_native" && GOTOOLCHAIN=local go list -deps -f '{{with .Module}}{{.Path}} {{.Dir}}{{end}}' ./cmd/fluxerd | sort -u | sed '/^$/d')
}

build_share() {
    install -D -m 0755 "$repo/packaging/install.sh" "$dist/share/install.sh"
    install -D -m 0644 "$repo/packaging/runtime-packages" "$dist/share/runtime-packages"
    install -D -m 0644 "$repo/packaging/systemd/fluxerd.service" "$dist/share/systemd/fluxerd.service"
    install -D -m 0644 "$repo/LICENSE" "$dist/share/LICENSES/fluxer/LICENSE"
}

verify_layout() {
    local path missing=()
    local required=(
        bin/fluxerd bin/node bin/valkey-server bin/valkey-cli bin/weed bin/meilisearch bin/livekit-server
        lib/libheif.so.1
        gateway/bin/fluxer_gateway gateway/bin/fluxer_gateway.real
        api/package.json api/node_modules api/dist/AppEntrypoint.js api/bin
        app/static/index.html app/static/avatars app/static/web
        share/install.sh share/runtime-packages share/systemd/fluxerd.service share/LICENSES/fluxer/LICENSE
        tsconfigs
    )
    for part in "${RUST_PARTS[@]}"; do
        required+=("bin/fluxer-$part")
    done
    for path in "${required[@]}"; do
        [ -e "$dist/$path" ] || missing+=("$path")
    done
    [ "${#missing[@]}" -eq 0 ] || die "$dist is incomplete, missing: ${missing[*]}"
    missing=()
    while IFS= read -r path; do missing+=("${path#"$dist"/}"); done < <(escaping_links "${TOP_LEVEL[@]/#/$dist/}")
    [ "${#missing[@]}" -eq 0 ] || die "these links point outside $dist, installers refuse them: ${missing[*]}"
}

pack() {
    need tar zstd sha256sum
    local name="fluxer-$version-linux-$arch.tar.zst"
    printf '%s\n' "$version" >"$dist/share/VERSION"
    verify_layout
    tar -C "$dist" --sort=name --owner=0 --group=0 --numeric-owner \
        --use-compress-program "zstd -T0 -${ZSTD_LEVEL:-10}" \
        -cf "$out/$name.part" "${TOP_LEVEL[@]}"
    mv "$out/$name.part" "$out/$name"
    (cd "$out" && sha256sum fluxer-*-linux-*.tar.zst >SHA256SUMS.part && mv SHA256SUMS.part SHA256SUMS)
    log "wrote $out/$name ($(du -h "$out/$name" | cut -f1))"
}

log "building fluxer $version for linux/$arch with profile $profile into $dist"
if selected gateway app; then run "fluxer-ci" build_ci; fi
if selected api app server; then run "pnpm install" pnpm_install; fi
if selected "${RUST_PARTS[@]}"; then run "rust services" build_rust; fi
if selected gateway; then run "gateway" build_gateway; fi
if selected app; then run "app" build_app; fi
if selected api; then run "api" build_api; fi
if selected valkey; then run "valkey" build_valkey; fi
if selected thirdparty; then run "third-party binaries" build_thirdparty; fi
if selected fluxerd; then run "fluxerd" build_fluxerd; fi
if selected share; then run "share" build_share; fi
if [ "$pack" = 1 ]; then run "tarball" pack; fi

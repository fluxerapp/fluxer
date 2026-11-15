#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

RUST_VERSION=1.98.1
RUSTUP_VERSION=1.29.3
WASM_BINDGEN_VERSION=0.2.128
NODE_VERSION=26.8.1
PNPM_VERSION=11.27.0
OTP_VERSION=28.5.0.8
OTP_SHA256=8a696e15bcbe5d57197b1f4c03c92d415f9fd9af70f9222a05911c8a5463cba6
REBAR3_VERSION=3.27.3
REBAR3_SHA256=f1d87d78decfc56b87d971c75ee6355698ec0c49fadd57f2c056043b7ca291b5
GO_VERSION=1.27.3

APT_PACKAGES=(
    build-essential pkg-config cmake nasm yasm git curl ca-certificates xz-utils zstd python3 brotli clang llvm
    libssl-dev zlib1g-dev libcurl4-openssl-dev
    libvips-dev libwebp-dev liblcms2-dev libyuv-dev libde265-dev libdav1d-dev libaom-dev
)
OTP_APT_PACKAGES=(libncurses-dev)

log() { printf '==> %s\n' "$*" >&2; }

die() {
    printf 'toolchain.sh: %s\n' "$*" >&2
    exit 1
}

[ "$#" -eq 1 ] || {
    printf 'usage: toolchain.sh <prefix>\n' >&2
    exit 64
}
[ "$(uname -s)" = Linux ] || die "toolchain.sh installs Linux toolchains only"

case "$(uname -m)" in
x86_64 | amd64)
    RUST_TRIPLE=x86_64-unknown-linux-gnu
    RUSTUP_SHA256=b1c8cb14e30889e7e2999e6b69e62d9e9b9ffc0f4df8180564d306d9680e6c55
    NODE_ARCH=x64
    NODE_SHA256=3e301118d7df53d563b7e96c1617545f26e2f76f9724be668d6cab65c15dda5d
    GO_ARCH=amd64
    GO_SHA256=65efbc80fdc1fa2cd2631cad5b481777a68e8725f34e89f50fc6ac714e379b40
    ;;
aarch64 | arm64)
    RUST_TRIPLE=aarch64-unknown-linux-gnu
    RUSTUP_SHA256=9f608c142e0e38a78957d0661ba1dfaf0b6fb37184cfaa4b9a0aa57af6184282
    NODE_ARCH=arm64
    NODE_SHA256=23c1b4d19e2f12a7d06fe8aa3d6e0e4923cf77a47e13c5ccdf32fadaa33960f2
    GO_ARCH=arm64
    GO_SHA256=a06e99309dbcbdca1a8ccd2282c3ee146e55930886e26b4f1193269587165f49
    ;;
*)
    die "unsupported architecture $(uname -m)"
    ;;
esac

mkdir -p "$1"
PREFIX="$(cd "$1" && pwd)"
JOBS="${JOBS:-$(nproc)}"
export RUSTUP_HOME="$PREFIX/rustup" CARGO_HOME="$PREFIX/cargo"
export PATH="$PREFIX/bin:$CARGO_HOME/bin:$PATH"

work="$(mktemp -d "$PREFIX/.work.XXXXXX")"
trap 'rm -rf "$work"' EXIT

fetch() {
    local url="$1" sha="$2" dest actual
    dest="$work/$(basename "$url")"
    log "downloading $url"
    curl -fsSL --retry 3 --retry-delay 2 -o "$dest" "$url"
    actual="$(sha256sum "$dest" | cut -d' ' -f1)"
    [ "$actual" = "$sha" ] || die "sha256 mismatch for $url: expected $sha, got $actual"
    printf '%s\n' "$dest"
}

otp_version() {
    erl -noshell -eval '{ok, V} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:put_chars(string:trim(V)), halt().' 2>/dev/null || true
}

missing_packages() {
    local pkg
    for pkg in "$@"; do
        [ "$(dpkg-query -W -f='${db:Status-Status}' "$pkg" 2>/dev/null)" = installed ] || printf '%s\n' "$pkg"
    done
}

install_apt_packages() {
    command -v dpkg-query >/dev/null 2>&1 || die "install these Debian packages first: $*"
    local missing
    mapfile -t missing < <(missing_packages "$@")
    [ "${#missing[@]}" -gt 0 ] || return 0
    log "installing ${missing[*]}"
    local sudo=()
    [ "$(id -u)" = 0 ] || sudo=(sudo)
    "${sudo[@]}" apt-get update
    "${sudo[@]}" env DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends "${missing[@]}"
}

install_rust() {
    if [ ! -x "$CARGO_HOME/bin/rustup" ]; then
        local init
        init="$(fetch "https://static.rust-lang.org/rustup/archive/${RUSTUP_VERSION}/${RUST_TRIPLE}/rustup-init" "$RUSTUP_SHA256")"
        chmod 0755 "$init"
        "$init" -y --no-modify-path --profile minimal --default-toolchain "$RUST_VERSION"
    fi
    rustup toolchain install "$RUST_VERSION" --profile minimal --no-self-update
    rustup target add --toolchain "$RUST_VERSION" wasm32-unknown-unknown
    rustup default "$RUST_VERSION"
    if [ "$(wasm-bindgen --version 2>/dev/null || true)" != "wasm-bindgen $WASM_BINDGEN_VERSION" ]; then
        cargo "+$RUST_VERSION" install wasm-bindgen-cli --version "$WASM_BINDGEN_VERSION" --locked --force
    fi
}

install_node() {
    local dir="$PREFIX/node" tarball
    if [ "$("$dir/bin/node" --version 2>/dev/null || true)" != "v$NODE_VERSION" ]; then
        tarball="$(fetch "https://nodejs.org/dist/v${NODE_VERSION}/node-v${NODE_VERSION}-linux-${NODE_ARCH}.tar.xz" "$NODE_SHA256")"
        rm -rf "$dir"
        mkdir -p "$dir"
        tar -xJf "$tarball" -C "$dir" --strip-components=1
    fi
    if [ "$("$dir/bin/pnpm" --version 2>/dev/null || true)" != "$PNPM_VERSION" ]; then
        PATH="$dir/bin:$PATH" npm install -g --prefix "$dir" --no-audit --no-fund "pnpm@$PNPM_VERSION"
    fi
    local tool
    for tool in node npm npx pnpm; do
        ln -sfn "../node/bin/$tool" "$PREFIX/bin/$tool"
    done
}

install_go() {
    local tarball
    if ! "$PREFIX/go/bin/go" version 2>/dev/null | grep -qF "go$GO_VERSION "; then
        tarball="$(fetch "https://go.dev/dl/go${GO_VERSION}.linux-${GO_ARCH}.tar.gz" "$GO_SHA256")"
        rm -rf "$PREFIX/go"
        tar -xzf "$tarball" -C "$PREFIX"
    fi
    ln -sfn ../go/bin/go "$PREFIX/bin/go"
    ln -sfn ../go/bin/gofmt "$PREFIX/bin/gofmt"
}

install_otp() {
    local tarball src="$work/otp" tool
    tarball="$(fetch "https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/otp_src_${OTP_VERSION}.tar.gz" "$OTP_SHA256")"
    rm -rf "$PREFIX/otp"
    mkdir -p "$src"
    tar -xzf "$tarball" -C "$src" --strip-components=1
    (
        cd "$src"
        ./configure --prefix="$PREFIX/otp" --without-javac --without-jinterface --without-wx --without-odbc
        make -j"$JOBS"
        make install
    )
    for tool in "$PREFIX"/otp/bin/*; do
        ln -sfn "../otp/bin/$(basename "$tool")" "$PREFIX/bin/$(basename "$tool")"
    done
}

install_rebar3() {
    local script
    if [ -x "$PREFIX/bin/rebar3" ] && [ "$(sha256sum "$PREFIX/bin/rebar3" | cut -d' ' -f1)" = "$REBAR3_SHA256" ]; then
        return
    fi
    script="$(fetch "https://github.com/erlang/rebar3/releases/download/${REBAR3_VERSION}/rebar3" "$REBAR3_SHA256")"
    install -m 0755 "$script" "$PREFIX/bin/rebar3"
}

mkdir -p "$PREFIX/bin"

need_otp=0
[ "$(otp_version)" = "$OTP_VERSION" ] || need_otp=1

if [ "$need_otp" = 1 ]; then
    install_apt_packages "${APT_PACKAGES[@]}" "${OTP_APT_PACKAGES[@]}"
else
    install_apt_packages "${APT_PACKAGES[@]}"
fi

install_rust
install_node
install_go
if [ "$need_otp" = 1 ]; then
    log "building OTP ${OTP_VERSION} into $PREFIX/otp"
    install_otp
fi
[ "$(otp_version)" = "$OTP_VERSION" ] || die "erl on PATH does not report OTP ${OTP_VERSION}"
install_rebar3

log "toolchain ready in $PREFIX"
log "rust $(rustc --version), node $(node --version), pnpm $(pnpm --version), $(go version), OTP $(otp_version), $(rebar3 version)"

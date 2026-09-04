#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

PREFIX="${1:-/usr/local}"

FFMPEG_VERSION=9.0
FFMPEG_SHA256=7f607a00dd0d28a729d5a4811205812eef01cf6ef6155025febb6f36a9062d52
FFMPEG_URL="https://ffmpeg.org/releases/ffmpeg-${FFMPEG_VERSION}.tar.xz"

LIBHEIF_VERSION=1.23.1
LIBHEIF_SHA256=0de0327f60fcd47de90d5654c6fe152232738d60d84fe084ec3e0f35e03b166a
LIBHEIF_URL="https://github.com/strukturag/libheif/releases/download/v${LIBHEIF_VERSION}/libheif-${LIBHEIF_VERSION}.tar.gz"

LIBAVFILTER_FLOOR=11.0.100
LIBHEIF_FLOOR=1.23.0
VIPS_FLOOR=8.13.0
LIBWEBP_FLOOR=1.1.0
LCMS2_FLOOR=2.8

LIBHEIF_FLOOR_REASON="src/native_shim/heif_pixels.c compiles a different HEIF decode path below 1.21 and below 1.23, so every environment must be at least ${LIBHEIF_FLOOR}"

FFMPEG_BUILD_MODULES="zlib dav1d aom"
LIBHEIF_BUILD_MODULES="libde265 dav1d aom"
REQUIRED_DECODERS="apng gif h264 hevc vp8 vp9 libdav1d libaom-av1 mjpeg png webp bmp"
REQUIRED_ENCODERS="gif"
REQUIRED_DEMUXERS="apng bmp_pipe gif"

log() { printf '==> %s\n' "$*" >&2; }

die() {
    printf 'install-native-deps: %s\n' "$*" >&2
    exit 1
}

pc() {
    PKG_CONFIG_PATH="${PREFIX}/lib/pkgconfig:${PREFIX}/lib64/pkgconfig:${PKG_CONFIG_PATH:-}" pkg-config "$@"
}

need_command() {
    command -v "$1" >/dev/null 2>&1 || die "$1 is required but is not on PATH"
}

atleast() {
    pc --atleast-version="$2" "$1" || die "$1 >= $2 is required: $3"
}

installed_here() {
    local reported
    reported="$(pc --variable=prefix "$1" 2>/dev/null)" || return 1
    [ "$reported" = "$PREFIX" ]
}

verify_sha256() {
    local file="$1" expected="$2" actual
    if command -v sha256sum >/dev/null 2>&1; then
        actual="$(sha256sum "$file" | cut -d' ' -f1)"
    else
        actual="$(shasum -a 256 "$file" | cut -d' ' -f1)"
    fi
    [ "$actual" = "$expected" ] || die "sha256 mismatch for ${file}: expected ${expected}, got ${actual}"
}

homebrew_prefix() {
    if [ -n "${HOMEBREW_PREFIX:-}" ]; then
        printf '%s\n' "$HOMEBREW_PREFIX"
        return 0
    fi
    need_command brew
    brew --prefix
}

verify_image_floors() {
    local vips_hint="$1" webp_hint="$2" lcms2_hint="$3"
    atleast vips "$VIPS_FLOOR" "$vips_hint"
    atleast libwebp "$LIBWEBP_FLOOR" "$webp_hint"
    atleast lcms2 "$LCMS2_FLOOR" "$lcms2_hint"
}

verify_libyuv_toolchain() {
    need_command cc
    local probe_dir probe status=0
    probe_dir="$(mktemp -d)"
    probe="${probe_dir}/libyuv_probe.c"
    printf '%s\n' \
        '#include <libyuv.h>' \
        'int main(void) { return I420ToABGR(0, 0, 0, 0, 0, 0, 0, 0, 0, 0); }' \
        >"$probe"
    cc -std=c11 -I"${PREFIX}/include" "$probe" -L"${PREFIX}/lib" -lyuv -o "${probe_dir}/libyuv_probe" \
        >/dev/null 2>&1 || status=$?
    rm -rf "$probe_dir"
    [ "$status" = 0 ] || die "libyuv.h and -lyuv must be usable from C11: $1"
}

verify_macos_floors() {
    need_command pkg-config
    atleast libavfilter "$LIBAVFILTER_FLOOR" "FFmpeg >= 8.0, run 'brew install ffmpeg'"
    atleast libheif "$LIBHEIF_FLOOR" "${LIBHEIF_FLOOR_REASON}, run 'brew install libheif'"
    verify_image_floors \
        "run 'brew install vips'" \
        "run 'brew install webp'" \
        "run 'brew install little-cms2'"
    local prefix
    prefix="$(homebrew_prefix)"
    [ -f "${prefix}/include/libyuv.h" ] \
        || die "libyuv header missing at ${prefix}/include/libyuv.h, run 'brew install libyuv'"
    [ -f "${prefix}/lib/libyuv.dylib" ] || [ -f "${prefix}/lib/libyuv.a" ] \
        || die "libyuv library missing under ${prefix}/lib, run 'brew install libyuv'"
}

verify_linux_floors() {
    need_command pkg-config
    verify_image_floors \
        "install libvips-dev" \
        "install libwebp-dev" \
        "install liblcms2-dev"
    verify_libyuv_toolchain "install libyuv-dev"
}

require_build_modules() {
    local module
    for module in $1; do
        pc --exists "$module" \
            || die "build dependency ${module} is missing: install zlib1g-dev libdav1d-dev libaom-dev libde265-dev"
    done
}

fetch_source() {
    local url="$1" sha256="$2" archive="$3" destination="$4"
    log "downloading ${url}"
    curl -fsSL --retry 3 --retry-delay 2 "$url" -o "$archive"
    verify_sha256 "$archive" "$sha256"
    mkdir -p "$destination"
    tar -xf "$archive" -C "$destination" --strip-components=1
}

build_ffmpeg() {
    need_command curl
    need_command tar
    need_command make
    require_build_modules "$FFMPEG_BUILD_MODULES"
    fetch_source "$FFMPEG_URL" "$FFMPEG_SHA256" "${WORKDIR}/ffmpeg.tar.xz" "${WORKDIR}/ffmpeg"
    (
        cd "${WORKDIR}/ffmpeg"
        ./configure \
            --prefix="$PREFIX" \
            --disable-debug --disable-doc --disable-static --enable-shared --enable-pic \
            --disable-programs --enable-ffmpeg \
            --enable-libaom --enable-libdav1d
        make -j"$JOBS"
        make install
    )
}

build_libheif() {
    need_command curl
    need_command tar
    need_command cmake
    require_build_modules "$LIBHEIF_BUILD_MODULES"
    fetch_source "$LIBHEIF_URL" "$LIBHEIF_SHA256" "${WORKDIR}/libheif.tar.gz" "${WORKDIR}/libheif"
    cmake -S "${WORKDIR}/libheif" -B "${WORKDIR}/libheif/build" \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX="$PREFIX" \
        -DBUILD_SHARED_LIBS=ON \
        -DBUILD_TESTING=OFF \
        -DBUILD_DOCUMENTATION=OFF \
        -DBUILD_DEVELOPMENT_TOOLS=OFF \
        -DENABLE_COVERAGE=OFF \
        -DENABLE_PLUGIN_LOADING=OFF \
        -DENABLE_MULTITHREADING_SUPPORT=ON \
        -DENABLE_PARALLEL_TILE_DECODING=ON \
        -DWITH_EXAMPLES=OFF \
        -DWITH_EXAMPLE_HEIF_THUMB=OFF \
        -DWITH_EXAMPLE_HEIF_VIEW=OFF \
        -DWITH_FUZZERS=OFF \
        -DWITH_GDK_PIXBUF=OFF \
        -DWITH_REDUCED_VISIBILITY=ON \
        -DWITH_HEADER_COMPRESSION=OFF \
        -DWITH_UNCOMPRESSED_CODEC=OFF \
        -DWITH_WEBCODECS=OFF \
        -DWITH_LIBSHARPYUV=OFF \
        -DWITH_LIBSHARPYUV_INTERNAL=OFF \
        -DWITH_LIBDE265=ON \
        -DWITH_DAV1D=ON \
        -DWITH_AOM_DECODER=ON \
        -DWITH_AOM_ENCODER=OFF \
        -DWITH_X265=OFF \
        -DWITH_X264=OFF \
        -DWITH_OpenH264_DECODER=OFF \
        -DWITH_KVAZAAR=OFF \
        -DWITH_UVG266=OFF \
        -DWITH_VVDEC=OFF \
        -DWITH_VVENC=OFF \
        -DWITH_SvtEnc=OFF \
        -DWITH_RAV1E=OFF \
        -DWITH_JPEG_DECODER=OFF \
        -DWITH_JPEG_ENCODER=OFF \
        -DWITH_OpenJPEG_DECODER=OFF \
        -DWITH_OpenJPEG_ENCODER=OFF \
        -DWITH_OPENJPH_ENCODER=OFF \
        -DWITH_FFMPEG_DECODER=OFF
    cmake --build "${WORKDIR}/libheif/build" -j "$JOBS"
    cmake --install "${WORKDIR}/libheif/build"
}

register_library_path() {
    [ "$(id -u)" = "0" ] || return 0
    [ -d /etc/ld.so.conf.d ] || return 0
    printf '%s\n' "${PREFIX}/lib" >/etc/ld.so.conf.d/fluxer-media-native.conf
    ldconfig
}

ffmpeg_listing() {
    LD_LIBRARY_PATH="${PREFIX}/lib:${PREFIX}/lib64:${LD_LIBRARY_PATH:-}" "$1" -hide_banner "$2"
}

require_listed() {
    local listing="$1" kind="$2" names="$3" name
    for name in $names; do
        grep -qE "^[[:space:]]*[A-Z.]+[[:space:]]+${name}([[:space:]]|\$)" <<<"$listing" \
            || die "FFmpeg ${FFMPEG_VERSION} under ${PREFIX} is missing the ${name} ${kind}"
    done
}

verify_ffmpeg_codecs() {
    local binary="${PREFIX}/bin/ffmpeg" listing
    [ -x "$binary" ] || die "expected an ffmpeg binary at ${binary}"
    listing="$(ffmpeg_listing "$binary" -decoders)"
    require_listed "$listing" decoder "$REQUIRED_DECODERS"
    listing="$(ffmpeg_listing "$binary" -encoders)"
    require_listed "$listing" encoder "$REQUIRED_ENCODERS"
    listing="$(ffmpeg_listing "$binary" -demuxers)"
    require_listed "$listing" demuxer "$REQUIRED_DEMUXERS"
}

if [ "$(uname -s)" = "Darwin" ]; then
    log "macOS: verifying Homebrew native floors instead of source-building"
    verify_macos_floors
    log "Homebrew native floors satisfied"
    exit 0
fi

verify_linux_floors

JOBS="$(nproc 2>/dev/null || echo 4)"
WORKDIR="$(mktemp -d)"
trap 'rm -rf "$WORKDIR"' EXIT

if installed_here libavfilter && pc --atleast-version="$LIBAVFILTER_FLOOR" libavfilter && [ -x "${PREFIX}/bin/ffmpeg" ]; then
    log "libavfilter $(pc --modversion libavfilter) already installed under ${PREFIX}"
else
    log "building FFmpeg ${FFMPEG_VERSION} into ${PREFIX}"
    build_ffmpeg
fi

if installed_here libheif && pc --exact-version="$LIBHEIF_VERSION" libheif; then
    log "libheif ${LIBHEIF_VERSION} already installed under ${PREFIX}"
else
    log "building libheif ${LIBHEIF_VERSION} into ${PREFIX}"
    build_libheif
fi

register_library_path

installed_here libavfilter || die "libavfilter must resolve from ${PREFIX}"
atleast libavfilter "$LIBAVFILTER_FLOOR" "FFmpeg >= 8.0 must resolve from ${PREFIX}"
installed_here libheif || die "libheif must resolve from ${PREFIX}"
pc --exact-version="$LIBHEIF_VERSION" libheif \
    || die "libheif ${LIBHEIF_VERSION} must resolve from ${PREFIX}, got $(pc --modversion libheif): ${LIBHEIF_FLOOR_REASON}"
verify_ffmpeg_codecs

log "native media dependencies ready under ${PREFIX}"

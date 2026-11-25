#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

PREFIX="${1:-/usr/local}"
BUNDLE="${FLUXER_NATIVE_BUNDLE:-0}"

FFMPEG_VERSION=9.0.2
FFMPEG_SHA256=8c3850283eb25fa026482078a04051e0be17347b09ef81a0849bec15a96e002e
FFMPEG_URL="https://ffmpeg.org/releases/ffmpeg-${FFMPEG_VERSION}.tar.xz"

LIBHEIF_VERSION=1.23.7
LIBHEIF_SHA256=f7483e818503ea6224c8863e4a56ac02c7089021a98472c811c12aa19faf2475
LIBHEIF_URL="https://github.com/strukturag/libheif/releases/download/v${LIBHEIF_VERSION}/libheif-${LIBHEIF_VERSION}.tar.gz"

DAV1D_VERSION=1.5.3
DAV1D_SHA256=e099f53253f6c247580c554d53a13f1040638f2066edc3c740e4c2f15174ce22
DAV1D_URL="https://code.videolan.org/videolan/dav1d/-/archive/${DAV1D_VERSION}/dav1d-${DAV1D_VERSION}.tar.bz2"

HIGHWAY_VERSION=1.3.0
HIGHWAY_SHA256=07b3c1ba2c1096878a85a31a5b9b3757427af963b1141ca904db2f9f4afe0bc2
HIGHWAY_URL="https://github.com/google/highway/archive/refs/tags/${HIGHWAY_VERSION}.tar.gz"

LIBJXL_VERSION=0.11.2
LIBJXL_SHA256=ab38928f7f6248e2a98cc184956021acb927b16a0dee71b4d260dc040a4320ea
LIBJXL_URL="https://github.com/libjxl/libjxl/archive/refs/tags/v${LIBJXL_VERSION}.tar.gz"

CGIF_VERSION=0.5.4
CGIF_SHA256=83a70a15bc2da41f081a44ebc58ee48e2e1d524a6d3fdb4a24064afa08d5ad4d
CGIF_URL="https://github.com/dloebl/cgif/archive/refs/tags/v${CGIF_VERSION}.tar.gz"

VIPS_VERSION=8.18.7
VIPS_SHA256=5baaead3b0bb20ffdb9e9ff09aa9fda08620923df77b63b436654cb5e0b3bf94
VIPS_URL="https://github.com/libvips/libvips/releases/download/v${VIPS_VERSION}/vips-${VIPS_VERSION}.tar.xz"

LIBAVFILTER_FLOOR=11.0.100
LIBHEIF_FLOOR=1.23.0
VIPS_FLOOR=8.13.0
LIBWEBP_FLOOR=1.1.0
LCMS2_FLOOR=2.8

LIBHEIF_FLOOR_REASON="src/native_shim/heif_pixels.c compiles a different HEIF decode path below 1.21 and below 1.23, so every environment must be at least ${LIBHEIF_FLOOR}"

FFMPEG_BUILD_MODULES="zlib dav1d aom"
VIPS_BUILD_MODULES="glib-2.0 expat libjpeg libpng libwebp libtiff-4 libexif librsvg-2.0 lcms2 imagequant"
LIBHEIF_BUILD_MODULES="libde265 dav1d aom"
REQUIRED_DECODERS="apng gif h264 hevc vp8 vp9 libdav1d libaom-av1 mjpeg png webp bmp aac mp3float vorbis opus flac alac pcm_s16le"
REQUIRED_ENCODERS="gif"
REQUIRED_DEMUXERS="apng bmp_pipe gif mov,mp4,m4a,3gp,3g2,mj2 matroska,webm avi flv mpegts mpeg asf mp3 wav flac aac aiff ogg"
REQUIRED_MUXERS="3gp mp4 mov webm matroska avi flv mpegts mpeg asf mp3 wav flac adts aiff ogg"

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
            || die "build dependency ${module} is missing: run tools/dist/toolchain.sh, or install zlib1g-dev libdav1d-dev libaom-dev libde265-dev"
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

meson_build() {
    local name="$1"
    shift
    meson setup "${WORKDIR}/${name}/build" "${WORKDIR}/${name}" \
        --prefix="$PREFIX" --libdir=lib --buildtype=release --default-library=shared "$@"
    ninja -C "${WORKDIR}/${name}/build" -j "$JOBS"
    ninja -C "${WORKDIR}/${name}/build" install
}

cmake_build() {
    local name="$1"
    shift
    cmake -S "${WORKDIR}/${name}" -B "${WORKDIR}/${name}/build" \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX="$PREFIX" \
        -DCMAKE_INSTALL_LIBDIR=lib \
        -DCMAKE_PREFIX_PATH="$PREFIX" \
        -DBUILD_SHARED_LIBS=ON \
        -DBUILD_TESTING=OFF \
        "$@"
    cmake --build "${WORKDIR}/${name}/build" -j "$JOBS"
    cmake --install "${WORKDIR}/${name}/build"
}

build_dav1d() {
    fetch_source "$DAV1D_URL" "$DAV1D_SHA256" "${WORKDIR}/dav1d.tar.bz2" "${WORKDIR}/dav1d"
    meson_build dav1d -Denable_tools=false -Denable_examples=false -Denable_tests=false -Denable_docs=false
}

build_highway() {
    fetch_source "$HIGHWAY_URL" "$HIGHWAY_SHA256" "${WORKDIR}/highway.tar.gz" "${WORKDIR}/highway"
    cmake_build highway -DHWY_ENABLE_CONTRIB=OFF -DHWY_ENABLE_EXAMPLES=OFF -DHWY_ENABLE_TESTS=OFF
}

build_libjxl() {
    fetch_source "$LIBJXL_URL" "$LIBJXL_SHA256" "${WORKDIR}/libjxl.tar.gz" "${WORKDIR}/libjxl"
    cmake_build libjxl \
        -DJPEGXL_FORCE_SYSTEM_BROTLI=ON \
        -DJPEGXL_FORCE_SYSTEM_HWY=ON \
        -DJPEGXL_FORCE_SYSTEM_LCMS2=ON \
        -DJPEGXL_ENABLE_SKCMS=OFF \
        -DJPEGXL_ENABLE_TOOLS=OFF \
        -DJPEGXL_ENABLE_JPEGLI=OFF \
        -DJPEGXL_ENABLE_DOXYGEN=OFF \
        -DJPEGXL_ENABLE_MANPAGES=OFF \
        -DJPEGXL_ENABLE_BENCHMARK=OFF \
        -DJPEGXL_ENABLE_EXAMPLES=OFF \
        -DJPEGXL_ENABLE_JNI=OFF \
        -DJPEGXL_ENABLE_SJPEG=OFF \
        -DJPEGXL_ENABLE_OPENEXR=OFF
}

build_cgif() {
    fetch_source "$CGIF_URL" "$CGIF_SHA256" "${WORKDIR}/cgif.tar.gz" "${WORKDIR}/cgif"
    meson_build cgif -Dexamples=false -Dtests=false
}

build_vips() {
    require_build_modules "$VIPS_BUILD_MODULES"
    fetch_source "$VIPS_URL" "$VIPS_SHA256" "${WORKDIR}/vips.tar.xz" "${WORKDIR}/vips"
    meson_build vips \
        -Ddeprecated=false -Dexamples=false -Dcplusplus=false -Ddocs=false -Dcpp-docs=false \
        -Dmodules=disabled -Dintrospection=disabled -Dvapi=false \
        -Dcgif=enabled -Dexif=enabled -Dheif=enabled -Dimagequant=enabled -Djpeg=enabled -Djpeg-xl=enabled \
        -Dlcms=enabled -Dpng=enabled -Drsvg=enabled -Dtiff=enabled -Dwebp=enabled -Dzlib=enabled -Dhighway=enabled \
        -Dnsgif=true -Dspng=disabled -Dquantizr=disabled -Dorc=disabled -Dfontconfig=disabled -Dpangocairo=disabled \
        -Darchive=disabled -Dcfitsio=disabled -Dfftw=disabled -Dmagick=disabled -Dmatio=disabled -Dnifti=disabled \
        -Dopenexr=disabled -Dopenjpeg=disabled -Dopenslide=disabled -Dpdfium=disabled -Dpoppler=disabled \
        -Draw=disabled -Duhdr=disabled
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
            --enable-libaom --enable-libdav1d \
            --disable-avdevice --disable-xlib --disable-libxcb
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
    listing="$(ffmpeg_listing "$binary" -muxers)"
    require_listed "$listing" muxer "$REQUIRED_MUXERS"
}

if [ "$(uname -s)" = "Darwin" ]; then
    log "macOS: verifying Homebrew native floors instead of source-building"
    verify_macos_floors
    log "Homebrew native floors satisfied"
    exit 0
fi

if [ "$BUNDLE" != 1 ]; then
    verify_linux_floors
fi

JOBS="$(nproc 2>/dev/null || echo 4)"
WORKDIR="$(mktemp -d)"
trap 'rm -rf "$WORKDIR"' EXIT
export PKG_CONFIG_PATH="${PREFIX}/lib/pkgconfig:${PREFIX}/lib64/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"

if [ "$BUNDLE" = 1 ]; then
    need_command meson
    need_command ninja
    if installed_here dav1d && pc --exact-version="$DAV1D_VERSION" dav1d; then
        log "dav1d ${DAV1D_VERSION} already installed under ${PREFIX}"
    else
        log "building dav1d ${DAV1D_VERSION} into ${PREFIX}"
        build_dav1d
    fi
fi

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

if [ "$BUNDLE" = 1 ]; then
    if installed_here libhwy && pc --exact-version="$HIGHWAY_VERSION" libhwy; then
        log "highway ${HIGHWAY_VERSION} already installed under ${PREFIX}"
    else
        log "building highway ${HIGHWAY_VERSION} into ${PREFIX}"
        build_highway
    fi
    if installed_here libjxl && pc --exact-version="$LIBJXL_VERSION" libjxl; then
        log "libjxl ${LIBJXL_VERSION} already installed under ${PREFIX}"
    else
        log "building libjxl ${LIBJXL_VERSION} into ${PREFIX}"
        build_libjxl
    fi
    if installed_here cgif && pc --exact-version="$CGIF_VERSION" cgif; then
        log "cgif ${CGIF_VERSION} already installed under ${PREFIX}"
    else
        log "building cgif ${CGIF_VERSION} into ${PREFIX}"
        build_cgif
    fi
    if installed_here vips && pc --exact-version="$VIPS_VERSION" vips; then
        log "libvips ${VIPS_VERSION} already installed under ${PREFIX}"
    else
        log "building libvips ${VIPS_VERSION} into ${PREFIX}"
        build_vips
    fi
    installed_here vips || die "libvips must resolve from ${PREFIX}"
    verify_linux_floors
fi

register_library_path

installed_here libavfilter || die "libavfilter must resolve from ${PREFIX}"
atleast libavfilter "$LIBAVFILTER_FLOOR" "FFmpeg >= 8.0 must resolve from ${PREFIX}"
installed_here libheif || die "libheif must resolve from ${PREFIX}"
pc --exact-version="$LIBHEIF_VERSION" libheif \
    || die "libheif ${LIBHEIF_VERSION} must resolve from ${PREFIX}, got $(pc --modversion libheif): ${LIBHEIF_FLOOR_REASON}"
verify_ffmpeg_codecs

log "native media dependencies ready under ${PREFIX}"

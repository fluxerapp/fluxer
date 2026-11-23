#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

CHANNEL="${1:?channel must be set}"
VERSION="${2:?version must be set}"
PUB_DATE="${3:?publication date must be set}"
X64_TARBALL="${4:?x64 release tarball must be set}"
ARM64_TARBALL="${5:?arm64 release tarball must be set}"
OUT="${6:?output directory must be set}"
BUILD="${7:-pacman_build}"

PACKAGER="${PACMAN_PACKAGER:-Fluxer Platform AB <support@fluxer.com>}"
BUILDER_IMAGE="${PACMAN_BUILDER_IMAGE:?PACMAN_BUILDER_IMAGE must be set}"
PACKAGING="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "${CHANNEL}" in
stable)
	pkgname=fluxer
	product=Fluxer
	;;
canary)
	pkgname=fluxer-canary
	product=Fluxer-Canary
	;;
*)
	echo "unsupported channel '${CHANNEL}'" >&2
	exit 1
	;;
esac

rm -rf "${BUILD}"
mkdir -p "${OUT}"
declare -A tarballs=([x86_64]="${X64_TARBALL}" [aarch64]="${ARM64_TARBALL}")
for carch in x86_64 aarch64; do
	source="${tarballs[${carch}]}"
	[ -f "${source}" ] || {
		echo "release tarball ${source} does not exist" >&2
		exit 1
	}
	mkdir -p "${BUILD}/src/${carch}"
	cp "${source}" "${BUILD}/src/${carch}/${pkgname}-${VERSION}-${carch}.tar.gz"
done

docker run --rm --platform linux/amd64 \
	--env "FLUXER_CHANNEL=${CHANNEL}" \
	--env "FLUXER_VERSION=${VERSION}" \
	--env "FLUXER_PUB_DATE=${PUB_DATE}" \
	--env "FLUXER_PACKAGER=${PACKAGER}" \
	--env "FLUXER_CARCH_LIST=x86_64 aarch64" \
	--env "HOST_UID=$(id -u)" \
	--env "HOST_GID=$(id -g)" \
	--volume "$(cd "${BUILD}" && pwd):/build" \
	--volume "${PACKAGING}:/packaging:ro" \
	"${BUILDER_IMAGE}" /packaging/build-pacman.sh

for carch in x86_64 aarch64; do
	package="${BUILD}/out/${carch}/${pkgname}-${VERSION}-1-${carch}.pkg.tar.zst"
	[ -f "${package}" ] || {
		echo "makepkg produced no ${carch} package" >&2
		exit 1
	}
	asset="${OUT}/${product}-${VERSION}-pacman-${carch}.pkg.tar.zst"
	cp "${package}" "${asset}"
	zstd -q -t "${asset}"
	printf 'built %s %s for %s into %s\n' "${pkgname}" "${VERSION}" "${carch}" "${asset}"
done

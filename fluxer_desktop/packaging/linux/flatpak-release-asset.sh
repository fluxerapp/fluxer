#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

CHANNEL="${1:?channel must be set}"
VERSION="${2:?version must be set}"
PUB_DATE="${3:?publication date must be set}"
MACHINE="${4:?target machine must be set}"
TARBALL="${5:?release tarball must be set}"
OUT="${6:?output directory must be set}"
BUILD="${7:-flatpak_build}"

RUNTIME_BRANCH="${FLATPAK_RUNTIME_BRANCH:-25.08}"
BUILDER_IMAGE="${FLATPAK_BUILDER_IMAGE:?FLATPAK_BUILDER_IMAGE must be set}"
PACKAGING="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "${CHANNEL}" in
stable)
	app_id=app.fluxer.Fluxer
	product=Fluxer
	;;
canary)
	app_id=app.fluxer.FluxerCanary
	product=Fluxer-Canary
	;;
*)
	echo "unsupported channel '${CHANNEL}'" >&2
	exit 1
	;;
esac
case "${MACHINE}" in
x86_64 | aarch64) ;;
*)
	echo "unsupported flatpak target machine '${MACHINE}'" >&2
	exit 1
	;;
esac
[ -f "${TARBALL}" ] || {
	echo "release tarball ${TARBALL} does not exist" >&2
	exit 1
}
release_date="${PUB_DATE%%T*}"

rm -rf "${BUILD}"
mkdir -p "${BUILD}" "${OUT}"
tarball_name="$(basename "${TARBALL}")"
cp "${TARBALL}" "${BUILD}/${tarball_name}"
tarball_sha256="$(sha256sum "${BUILD}/${tarball_name}" | cut -d' ' -f1)"

for name in "${app_id}.sh" "${app_id}.desktop" "${app_id}.svg"; do
	cp "${PACKAGING}/${name}" "${BUILD}/${name}"
done

metainfo="${PACKAGING}/${app_id}.metainfo.xml"
grep -q '</component>' "${metainfo}" || {
	echo "${metainfo} has no closing </component> element" >&2
	exit 1
}
awk -v version="${VERSION}" -v date="${release_date}" '
	/<releases[[:space:]]*\/>/ { next }
	/<releases[[:space:]>]/ { stripping = 1 }
	stripping {
		if (/<\/releases>/) stripping = 0
		next
	}
	/<\/component>/ && !done {
		printf "    <releases>\n        <release version=\"%s\" date=\"%s\"/>\n    </releases>\n", version, date
		done = 1
	}
	{ print }
' "${metainfo}" >"${BUILD}/${app_id}.metainfo.xml"
grep -q "<release version=\"${VERSION}\" date=\"${release_date}\"/>" "${BUILD}/${app_id}.metainfo.xml" || {
	echo "the rendered metainfo does not list ${VERSION}" >&2
	exit 1
}
[ "$(grep -c '<release ' "${BUILD}/${app_id}.metainfo.xml")" = 1 ] || {
	echo "the rendered metainfo lists more than one release" >&2
	exit 1
}

manifest="${PACKAGING}/${app_id}.yml"
grep -q '^    sources:$' "${manifest}" || {
	echo "${manifest} has no sources list to append the release archive to" >&2
	exit 1
}
if grep -q 'type: archive' "${manifest}"; then
	echo "${manifest} already pins an archive source" >&2
	exit 1
fi
{
	cat "${manifest}"
	printf '      - type: archive\n        path: %s\n        sha256: %s\n        archive-type: tar-gzip\n        strip-components: 1\n        dest: app\n' \
		"${tarball_name}" "${tarball_sha256}"
} >"${BUILD}/${app_id}.yml"

docker run --rm --privileged \
	--env "FLATPAK_APP_ID=${app_id}" \
	--env "FLATPAK_TARGET_ARCH=${MACHINE}" \
	--env "FLATPAK_RUNTIME_BRANCH=${RUNTIME_BRANCH}" \
	--env "HOST_UID=$(id -u)" \
	--env "HOST_GID=$(id -g)" \
	--volume "$(cd "${BUILD}" && pwd):/build" \
	--volume "${PACKAGING}:/packaging:ro" \
	"${BUILDER_IMAGE}" /packaging/build-flatpak.sh

[ -d "${BUILD}/repo/objects" ] || {
	echo "flatpak-builder produced no ostree objects in ${BUILD}/repo" >&2
	exit 1
}
asset="${OUT}/${product}-${VERSION}-flatpak-${MACHINE}.ostree.tar.zst"
tar --sort=name --owner=0 --group=0 --numeric-owner -C "${BUILD}" -cf - repo | zstd -q -T0 -19 -o "${asset}"
zstd -q -t "${asset}"
printf 'built %s %s for %s into %s\n' "${app_id}" "${VERSION}" "${MACHINE}" "${asset}"

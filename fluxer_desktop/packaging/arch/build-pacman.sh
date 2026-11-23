#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
set -euo pipefail

: "${FLUXER_CHANNEL:?FLUXER_CHANNEL must be set}"
: "${FLUXER_VERSION:?FLUXER_VERSION must be set}"
: "${FLUXER_PUB_DATE:?FLUXER_PUB_DATE must be set}"
: "${FLUXER_PACKAGER:?FLUXER_PACKAGER must be set}"
: "${FLUXER_CARCH_LIST:?FLUXER_CARCH_LIST must be set}"
: "${HOST_UID:?HOST_UID must be set}"
: "${HOST_GID:?HOST_GID must be set}"

sed -i 's/^COMPRESSZST=.*/COMPRESSZST=(zstd -c -T0 -19 -)/' /etc/makepkg.conf
sed -i "s|^#\?PKGEXT=.*|PKGEXT='.pkg.tar.zst'|" /etc/makepkg.conf
grep -q "^PKGEXT='.pkg.tar.zst'$" /etc/makepkg.conf || printf "PKGEXT='.pkg.tar.zst'\n" >>/etc/makepkg.conf
sed -i "s|^#\?PACKAGER=.*|PACKAGER=\"${FLUXER_PACKAGER}\"|" /etc/makepkg.conf
grep -q '^PACKAGER=' /etc/makepkg.conf || printf 'PACKAGER="%s"\n' "${FLUXER_PACKAGER}" >>/etc/makepkg.conf

SOURCE_DATE_EPOCH=$(date -u -d "${FLUXER_PUB_DATE}" +%s)
export SOURCE_DATE_EPOCH

id -u build >/dev/null 2>&1 || useradd -m build

pkgname=fluxer
if [[ ${FLUXER_CHANNEL} == canary ]]; then
	pkgname=fluxer-canary
fi

for carch in ${FLUXER_CARCH_LIST}; do
	sed -i "s/^CARCH=.*/CARCH=\"${carch}\"/" /etc/makepkg.conf

	work="/build/work/${carch}"
	out="/build/out/${carch}"
	tarball="${pkgname}-${FLUXER_VERSION}-${carch}.tar.gz"

	rm -rf "${work}" "${out}"
	install -d -o build -g build "${work}"
	install -d "${out}"
	install -m644 -o build -g build /packaging/PKGBUILD "${work}/PKGBUILD"
	install -m644 -o build -g build "/build/src/${carch}/${tarball}" "${work}/${tarball}"

	su build -c "cd '${work}' && SOURCE_DATE_EPOCH='${SOURCE_DATE_EPOCH}' FLUXER_CHANNEL='${FLUXER_CHANNEL}' FLUXER_VERSION='${FLUXER_VERSION}' makepkg --nodeps --noconfirm --clean"

	package=$(printf '%s\n' "${work}"/*.pkg.tar.zst | head -n1)
	test -f "${package}"
	expected="${pkgname}-${FLUXER_VERSION}-1-${carch}.pkg.tar.zst"
	if [[ $(basename "${package}") != "${expected}" ]]; then
		echo "makepkg wrote $(basename "${package}"), expected ${expected}" >&2
		exit 1
	fi
	appledouble=$(bsdtar -tf "${package}" | grep -c '/\._' || true)
	if [[ ${appledouble} -gt 0 ]]; then
		echo "${package} has ${appledouble} macOS AppleDouble entries and was repacked on a Mac" >&2
		exit 1
	fi
	pkginfo_arch=$(bsdtar -xOf "${package}" .PKGINFO | sed -n 's/^arch = //p')
	if [[ ${pkginfo_arch} != "${carch}" ]]; then
		echo "${package} declares arch ${pkginfo_arch}, expected ${carch}" >&2
		exit 1
	fi
	cp "${package}" "${out}/"
	rm -rf "${work}"
done

rm -rf /build/work
chown -R "${HOST_UID}:${HOST_GID}" /build

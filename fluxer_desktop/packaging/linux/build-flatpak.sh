#!/bin/sh
# SPDX-License-Identifier: AGPL-3.0-or-later
set -eu

: "${FLATPAK_APP_ID:?FLATPAK_APP_ID must be set}"
: "${FLATPAK_TARGET_ARCH:?FLATPAK_TARGET_ARCH must be set}"
: "${FLATPAK_RUNTIME_BRANCH:?FLATPAK_RUNTIME_BRANCH must be set}"
: "${HOST_UID:?HOST_UID must be set}"
: "${HOST_GID:?HOST_GID must be set}"

dnf install -y --setopt=install_weak_deps=False flatpak flatpak-builder

flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install -y --noninteractive --arch="${FLATPAK_TARGET_ARCH}" flathub \
	"org.freedesktop.Platform//${FLATPAK_RUNTIME_BRANCH}" \
	"org.freedesktop.Sdk//${FLATPAK_RUNTIME_BRANCH}" \
	"org.electronjs.Electron2.BaseApp//${FLATPAK_RUNTIME_BRANCH}"

cd /build
rm -rf repo builddir .fb-state

flatpak-builder \
	--arch="${FLATPAK_TARGET_ARCH}" \
	--repo=repo \
	--force-clean \
	--disable-rofiles-fuse \
	--state-dir=.fb-state \
	builddir "${FLATPAK_APP_ID}.yml"

rm -rf builddir .fb-state
ostree --repo=repo refs
rm -rf repo/tmp repo/state repo/extensions repo/.lock
chown -R "${HOST_UID}:${HOST_GID}" repo

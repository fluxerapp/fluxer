// SPDX-License-Identifier: AGPL-3.0-or-later

import {BUILD_CHANNEL, type BuildChannel} from '@electron/common/BuildChannel';

type DesktopDownloadArch = 'x64' | 'arm64';

function getDesktopDownloadArch(arch: NodeJS.Architecture): DesktopDownloadArch {
	return arch === 'arm64' ? 'arm64' : 'x64';
}

const DESKTOP_DOWNLOAD_ARCH = getDesktopDownloadArch(process.arch);
const PACKAGE_ORIGIN_ENV = 'FLUXER_DESKTOP_PACKAGE_ORIGIN';
const CHANNEL_PACKAGE_ORIGINS: Record<BuildChannel, string> = {
	stable: 'https://pkgs.fluxer.com',
	canary: 'https://pkgs.fluxer.com',
	development: 'http://localhost:48780',
};

export function resolveDesktopPackageOrigin(): string {
	const override = process.env?.[PACKAGE_ORIGIN_ENV];
	if (override == null || override.trim().length === 0) {
		return CHANNEL_PACKAGE_ORIGINS[BUILD_CHANNEL];
	}
	return override.trim().replace(/\/+$/u, '');
}

export function getUpdateBaseUrl(platform: NodeJS.Platform = process.platform): string {
	return `${resolveDesktopPackageOrigin()}/desktop/${BUILD_CHANNEL}/${platform}/${DESKTOP_DOWNLOAD_ARCH}`;
}

export const MANUAL_DESKTOP_FORMATS = ['setup', 'dmg', 'zip', 'appimage', 'deb', 'rpm', 'tar_gz'] as const;

export type ManualDesktopFormat = (typeof MANUAL_DESKTOP_FORMATS)[number];

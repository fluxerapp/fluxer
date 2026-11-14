// SPDX-License-Identifier: AGPL-3.0-or-later

import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import {DOWNLOAD_PAGE_URLS} from '@electron/common/Constants';
import {getUpdateBaseUrl, type ManualDesktopFormat} from '@electron/main/ShellDownloadFormats';

export const UPDATE_BASE_URL = getUpdateBaseUrl();
export const DOWNLOAD_PAGE_URL = DOWNLOAD_PAGE_URLS[BUILD_CHANNEL];

export type ManualLatestFile = {url: string; sha256: string | null};

export type ManualLatestInfo = {
	version: string;
	pubDate: string | null;
	files: Partial<Record<ManualDesktopFormat, ManualLatestFile>>;
};

function getManualDownloadFormatPreference(): Array<ManualDesktopFormat> {
	if (process.platform === 'darwin') {
		return ['dmg', 'zip'];
	}
	if (process.platform === 'win32') {
		return ['setup'];
	}
	return [];
}

export function getManualDownloadFile(info: ManualLatestInfo): ManualLatestFile {
	for (const format of getManualDownloadFormatPreference()) {
		const file = info.files[format];
		if (file?.url) {
			return file;
		}
	}
	return {url: DOWNLOAD_PAGE_URL, sha256: null};
}

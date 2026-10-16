// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {openExternalDeduped} from '@electron/main/OpenExternal';
import {onSplashOpenDownload, SplashAction, SplashStatus, setSplashState} from '@electron/main/SplashWindow';
import {DOWNLOAD_PAGE_URL} from '@electron/main/UpdaterDownloads';
import {app} from 'electron';

const logger = createChildLogger('ShellUpdateSplash');

let disarmDownloadListener: (() => void) | null = null;

function openDownloadPage(): void {
	void openExternalDeduped(DOWNLOAD_PAGE_URL).then(
		() => {
			logger.info('Opened the download for a required shell update, quitting', {url: DOWNLOAD_PAGE_URL});
			app.quit();
		},
		(error: unknown) => {
			logger.error('Failed to open the download for a required shell update, quitting anyway', {
				url: DOWNLOAD_PAGE_URL,
				error,
			});
			app.quit();
		},
	);
}

export function armBlockedShellUpdate(): void {
	disarmDownloadListener?.();
	disarmDownloadListener = onSplashOpenDownload(openDownloadPage);
	setSplashState({status: SplashStatus.BLOCKED_SHELL_UPDATE, action: SplashAction.DOWNLOAD});
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {app} from 'electron';

const logger = createChildLogger('RecentDocuments');

export function recordRecentDeepLink(url: string): void {
	if (process.platform !== 'win32' && process.platform !== 'darwin') return;
	try {
		app.addRecentDocument(url);
	} catch (error) {
		logger.debug('addRecentDocument failed', {error});
	}
}

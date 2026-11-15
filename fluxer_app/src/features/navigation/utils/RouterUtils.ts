// SPDX-License-Identifier: AGPL-3.0-or-later

import {tryInterceptChannelNavigationPath} from '@app/features/navigation/utils/ChannelNavigationGuard';
import {createBrowserHistory} from '@app/features/platform/components/router/RouterHistory';
import type {HistoryAdapter} from '@app/features/platform/components/router/RouterTypes';
import {resolveDocumentURLFromRoot} from '@app/features/platform/URLOriginUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('RouterUtils');
export const history: HistoryAdapter | null = createBrowserHistory();

export function getCurrentPath(): string {
	const url = history?.getLocation().url;
	if (!url) return '';
	return url.pathname + url.search + url.hash;
}

export function transitionTo(path: string) {
	logger.debug('transitionTo', path);
	if (history) {
		const current = getCurrentPath();
		if (current === path) return;
		const url = resolveDocumentURLFromRoot(path);
		if (tryInterceptChannelNavigationPath(path)) return;
		history.push(url);
	}
}

export function replaceWith(path: string) {
	logger.debug('replaceWith', path);
	if (history) {
		const current = getCurrentPath();
		if (current === path) return;
		const url = resolveDocumentURLFromRoot(path);
		if (tryInterceptChannelNavigationPath(path)) return;
		history.replace(url);
	}
}

export function getHistory() {
	return history;
}

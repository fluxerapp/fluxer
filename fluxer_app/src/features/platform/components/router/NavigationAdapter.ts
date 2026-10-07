// SPDX-License-Identifier: AGPL-3.0-or-later

import {derivePathStack} from '@app/app/HistoryBootstrap';
import {getBrowserNavigation} from '@app/features/platform/components/router/BrowserNavigation';

function canGoBack(): boolean {
	const navigation = getBrowserNavigation();
	if (typeof navigation?.canGoBack === 'boolean') return navigation.canGoBack;
	if (typeof window === 'undefined') return false;
	return window.history.length > 1;
}

export function goBackOr(fallbackPath: string): void {
	if (typeof window === 'undefined') return;
	if (canGoBack()) {
		window.history.back();
		return;
	}
	const parents = derivePathStack(window.location.pathname);
	const target = fallbackPath || parents[parents.length - 1] || '/';
	const currentPath = window.location.pathname + window.location.search + window.location.hash;
	if (target === currentPath) return;
	window.history.replaceState(null, '', target);
	window.history.pushState(null, '', currentPath);
	window.history.back();
}

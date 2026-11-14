// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {tryInterceptChannelNavigationPath} from '@app/features/navigation/utils/ChannelNavigationGuard';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {resolveDocumentURLFromRoot} from '@app/features/platform/URLOriginUtils';

function getStateWithoutMobileOverlayMarkers(state: unknown): unknown {
	if (!state || typeof state !== 'object') {
		return state ?? null;
	}
	const nextState = {...(state as Record<string, unknown>)};
	delete nextState.bottomSheet;
	delete nextState.modal;
	return Object.keys(nextState).length > 0 ? nextState : null;
}

function replaceSilent(p: string): void {
	if (typeof window !== 'undefined') {
		window.history.replaceState(getStateWithoutMobileOverlayMarkers(window.history.state), '', p);
	}
}

let inProgress = false;

function computeBasePath(url: string): string | null {
	const pathname = resolveDocumentURLFromRoot(url).pathname;
	if (Routes.isDMRoute(pathname) && pathname !== Routes.ME) {
		return Routes.ME;
	}
	if (Routes.isGuildChannelRoute(pathname)) {
		const parts = pathname.split('/');
		if (parts.length === 4) {
			return Routes.guildChannel(parts[2]);
		}
	}
	return null;
}

export function navigateToWithMobileHistory(url: string, isMobile: boolean): void {
	if (!isMobile) {
		inProgress = false;
		RouterUtils.replaceWith(url);
		return;
	}
	if (inProgress) {
		return;
	}
	const current = RouterUtils.getCurrentPath();
	if (current === url) {
		return;
	}
	if (tryInterceptChannelNavigationPath(url)) {
		return;
	}
	const base = computeBasePath(url);
	if (!base) {
		RouterUtils.replaceWith(url);
		return;
	}
	inProgress = true;
	try {
		replaceSilent(base);
		RouterUtils.transitionTo(url);
	} finally {
		inProgress = false;
	}
}

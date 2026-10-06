// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {tryInterceptChannelNavigationPath} from '@app/features/navigation/utils/ChannelNavigationGuard';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';

export interface Navigator {
	replace: (path: string) => void;
	push: (path: string) => void;
	getPath: () => string;
	replaceSilent: (path: string) => void;
}

function getStateWithoutMobileOverlayMarkers(state: unknown): unknown {
	if (!state || typeof state !== 'object') {
		return state ?? null;
	}
	const nextState = {...(state as Record<string, unknown>)};
	delete nextState.bottomSheet;
	delete nextState.modal;
	return Object.keys(nextState).length > 0 ? nextState : null;
}

const defaultNavigator: Navigator = {
	replace: (p: string) => RouterUtils.replaceWith(p),
	push: (p: string) => RouterUtils.transitionTo(p),
	getPath: () => {
		const location = RouterUtils.getHistory()?.location;
		if (!location) return '';
		return location.pathname + location.search + location.hash;
	},
	replaceSilent: (p: string) => {
		if (typeof window !== 'undefined') {
			window.history.replaceState(getStateWithoutMobileOverlayMarkers(window.history.state), '', p);
		}
	},
};

let inProgress = false;

function computeBasePath(url: string): string | null {
	const pathname = new URL(url, window.location.origin).pathname;
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

export function navigateToWithMobileHistory(url: string, isMobile: boolean, nav: Navigator = defaultNavigator): void {
	if (!isMobile) {
		inProgress = false;
		nav.replace(url);
		return;
	}
	if (inProgress) {
		return;
	}
	const current = nav.getPath();
	if (current === url) {
		return;
	}
	if (tryInterceptChannelNavigationPath(url)) {
		return;
	}
	const base = computeBasePath(url);
	if (!base) {
		nav.replace(url);
		return;
	}
	inProgress = true;
	try {
		nav.replaceSilent(base);
		nav.push(url);
	} finally {
		inProgress = false;
	}
}

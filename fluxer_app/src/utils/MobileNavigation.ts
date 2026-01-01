/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Routes} from '~/Routes';
import * as RouterUtils from '~/utils/RouterUtils';

export interface Navigator {
	replace: (path: string) => void;
	push: (path: string) => void;
	getPath: () => string;
}

const defaultNavigator: Navigator = {
	replace: (p: string) => RouterUtils.replaceWith(p),
	push: (p: string) => RouterUtils.transitionTo(p),
	getPath: () => RouterUtils.getHistory()?.location.pathname ?? '',
};

let inProgress = false;
let pendingTarget: string | null = null;

function computeBasePath(url: string): string | null {
	if (Routes.isDMRoute(url) && url !== Routes.ME) {
		return Routes.ME;
	}
	if (Routes.isGuildChannelRoute(url) && url.split('/').length === 4) {
		const parts = url.split('/');
		const guildId = parts[2];
		return Routes.guildChannel(guildId);
	}
	return null;
}

export function navigateToWithMobileHistory(url: string, isMobile: boolean, nav: Navigator = defaultNavigator): void {
	if (!isMobile) {
		inProgress = false;
		pendingTarget = null;
		nav.replace(url);
		return;
	}

	if (inProgress && (pendingTarget === url || pendingTarget !== null)) {
		return;
	}

	const base = computeBasePath(url);
	if (!base) {
		nav.replace(url);
		return;
	}

	const current = nav.getPath();
	if (current === base) {
		inProgress = true;
		pendingTarget = url;
		nav.push(url);
		inProgress = false;
		pendingTarget = null;
		return;
	}

	inProgress = true;
	pendingTarget = url;
	nav.replace(base);
	setTimeout(() => {
		try {
			if (pendingTarget === url) {
				nav.push(url);
			}
		} finally {
			inProgress = false;
			pendingTarget = null;
		}
	}, 0);
}

export function __resetMobileNavigationGuardsForTests() {
	inProgress = false;
	pendingTarget = null;
}

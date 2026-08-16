// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';

const GUILD_ROOT_PATH_SEGMENT_COUNT = 3;

export interface MobileBottomNavVisibilityQuery {
	readonly mobileLayoutEnabled: boolean;
	readonly pathname: string;
}

export interface AppLoadingSkeletonVisibilityQuery {
	readonly booting: boolean;
	readonly isAuthenticated: boolean;
	readonly isAuthSessionInitialized: boolean;
	readonly shouldBypassGateway: boolean;
}

const BOTTOM_NAV_ROOT_ROUTES = new Set<string>([Routes.ME, Routes.FAVORITES, Routes.NOTIFICATIONS, Routes.YOU]);

export function shouldShowMobileBottomNav({mobileLayoutEnabled, pathname}: MobileBottomNavVisibilityQuery): boolean {
	if (!mobileLayoutEnabled) {
		return false;
	}
	if (BOTTOM_NAV_ROOT_ROUTES.has(pathname)) {
		return true;
	}
	if (!Routes.isGuildChannelRoute(pathname)) {
		return false;
	}
	return isGuildRootRoute(pathname);
}

export function shouldShowLoadingSkeleton({
	booting,
	isAuthenticated,
	isAuthSessionInitialized,
	shouldBypassGateway,
}: AppLoadingSkeletonVisibilityQuery): boolean {
	if (shouldBypassGateway) {
		return false;
	}
	if (!isAuthSessionInitialized) {
		return true;
	}
	if (!isAuthenticated) {
		return true;
	}
	return booting;
}

function isGuildRootRoute(pathname: string): boolean {
	return pathname.split('/').length === GUILD_ROOT_PATH_SEGMENT_COUNT;
}

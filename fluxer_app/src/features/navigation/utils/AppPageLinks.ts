// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {DIRECT_MESSAGES_DESCRIPTOR, FAVORITES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Navigation from '@app/features/navigation/state/Navigation';
import {isInternalChannelHost} from '@app/features/navigation/utils/DeepLinkUtils';
import * as PlutoniumPageCommands from '@app/features/premium/commands/PlutoniumPageCommands';
import PlutoniumPageRollout from '@app/features/premium/state/PlutoniumPageRollout';
import {getPremiumProductFullName, shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {APP_PROTOCOL_SCHEME, isAppProtocolUrl} from '@app/features/ui/utils/AppProtocol';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {ChatsCircleIcon, CompassIcon, CrownIcon, type Icon, StarIcon} from '@phosphor-icons/react';

export type AppPageId = 'discovery' | 'plutonium' | 'favorites' | 'direct_messages';

interface AppPageDefinition {
	id: AppPageId;
	path: string;
	icon: Icon;
	appHostOnly: boolean;
}

const DISCOVERY_PAGE_DESCRIPTOR = msg({
	message: 'Discovery',
	comment: 'Name of the Discovery page, shown in an inline chat link that opens it.',
});

const APP_PAGES: ReadonlyArray<AppPageDefinition> = [
	{id: 'discovery', path: Routes.DISCOVER, icon: CompassIcon, appHostOnly: false},
	{id: 'plutonium', path: Routes.PLUTONIUM, icon: CrownIcon, appHostOnly: true},
	{id: 'favorites', path: Routes.FAVORITES, icon: StarIcon, appHostOnly: false},
	{id: 'direct_messages', path: Routes.ME, icon: ChatsCircleIcon, appHostOnly: false},
];

const OFFICIAL_WEB_APP_HOSTS = ['web.fluxer.app', 'web.canary.fluxer.app'];

function normalizePath(pathname: string): string {
	const trimmed = pathname.length > 1 ? pathname.replace(/\/+$/, '') : pathname;
	try {
		return decodeURIComponent(trimmed);
	} catch {
		return trimmed;
	}
}

function isWebAppHost(host: string): boolean {
	const normalized = host.toLowerCase();
	if (typeof location !== 'undefined' && normalized === location.host.toLowerCase()) return true;
	try {
		if (normalized === new URL(RuntimeConfig.webAppBaseUrl).host.toLowerCase()) return true;
	} catch {}
	return OFFICIAL_WEB_APP_HOSTS.includes(normalized);
}

function isPageAvailable(page: AppPageDefinition): boolean {
	if (page.id === 'plutonium') return shouldShowPremiumFeatures();
	if (page.id === 'discovery') return !RuntimeConfig.singleCommunityEnabled;
	if (page.id === 'direct_messages') return !RuntimeConfig.directMessagesDisabled;
	return true;
}

function findPageByPath(pathname: string): AppPageDefinition | null {
	const path = normalizePath(pathname);
	return APP_PAGES.find((page) => page.path === path && isPageAvailable(page)) ?? null;
}

export function parseAppPagePath(path: string): AppPageId | null {
	if (!PlutoniumPageRollout.enabled) return null;
	const pathname = path.split(/[?#]/, 1)[0] ?? '';
	return findPageByPath(pathname)?.id ?? null;
}

export function parseAppPageLink(rawUrl: string): AppPageId | null {
	if (!PlutoniumPageRollout.enabled) return null;
	let parsed: URL;
	try {
		parsed = new URL(rawUrl);
	} catch {
		return null;
	}
	if (isAppProtocolUrl(rawUrl)) {
		if (parsed.protocol.toLowerCase() !== APP_PROTOCOL_SCHEME) return null;
		const host = parsed.hostname;
		const path = host && host !== '-' ? `/${host}${parsed.pathname}` : parsed.pathname;
		return findPageByPath(path.startsWith('/') ? path : `/${path}`)?.id ?? null;
	}
	if (parsed.protocol !== 'https:' && parsed.protocol !== 'http:') return null;
	const page = findPageByPath(parsed.pathname);
	if (!page) return null;
	const internal = page.appHostOnly ? isWebAppHost(parsed.host) : isInternalChannelHost(parsed.host);
	return internal ? page.id : null;
}

export function getAppPageLabel(i18n: I18n, page: AppPageId): string {
	switch (page) {
		case 'discovery':
			return i18n._(DISCOVERY_PAGE_DESCRIPTOR);
		case 'plutonium':
			return getPremiumProductFullName();
		case 'favorites':
			return i18n._(FAVORITES_DESCRIPTOR);
		case 'direct_messages':
			return i18n._(DIRECT_MESSAGES_DESCRIPTOR);
	}
}

export function getAppPageIcon(page: AppPageId): Icon {
	return APP_PAGES.find((definition) => definition.id === page)?.icon ?? CompassIcon;
}

export function navigateToAppPage(page: AppPageId): void {
	switch (page) {
		case 'discovery':
			Navigation.navigateToDiscover();
			return;
		case 'plutonium':
			PlutoniumPageCommands.openPlutoniumPage();
			return;
		case 'favorites':
			Navigation.navigateToFavorites();
			return;
		case 'direct_messages':
			Navigation.navigateToDM();
			return;
	}
}

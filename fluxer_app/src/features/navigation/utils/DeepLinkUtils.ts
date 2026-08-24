// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Authentication from '@app/features/auth/state/Authentication';
import * as GiftCommands from '@app/features/gift/commands/GiftCommands';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import {setPathQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {APP_PROTOCOL_SCHEME, isAppProtocolUrl} from '@app/features/ui/utils/AppProtocol';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {
	parseUserSettingsDeepLinkPath,
	type UserSettingsDeepLinkTarget,
} from '@app/features/user/components/settings_utils/UserSettingsDeepLinks';
import {ME} from '@fluxer/constants/src/AppConstants';
import {isProbablyAValidSnowflake} from '@fluxer/snowflake/src/SnowflakeUtils';
import {createElement} from 'react';

const logger = new Logger('DeepLinkUtils');
const ROUTE_PATH_BLOCKLIST = /["'<>\\|\t\r\n]/;

type DeepLinkTarget =
	| {
			type: 'invite';
			code: string;
			preferLogin: boolean;
	  }
	| {
			type: 'gift';
			code: string;
			preferLogin: boolean;
	  }
	| {
			type: 'user';
			userId: string;
	  }
	| {
			type: 'route';
			path: string;
	  }
	| {
			type: 'user_settings';
			target: UserSettingsDeepLinkTarget;
	  };

function normalizeAppRoutePath(rawUrl: string): string | null {
	if (ROUTE_PATH_BLOCKLIST.test(rawUrl)) return null;
	if (rawUrl.startsWith('/')) return rawUrl;
	if (!isAppProtocolUrl(rawUrl)) return null;
	try {
		const parsed = new URL(rawUrl);
		if (parsed.protocol.toLowerCase() !== APP_PROTOCOL_SCHEME) return null;
		const host = parsed.hostname;
		const path = host && host !== '-' ? `/${host}${parsed.pathname}` : parsed.pathname || '/';
		return `${path.startsWith('/') ? path : `/${path}`}${parsed.search}${parsed.hash}`;
	} catch {
		const path = rawUrl.slice(APP_PROTOCOL_SCHEME.length);
		return path.startsWith('/') ? path : `/${path}`;
	}
}

function isRoutableDeepLinkPath(path: string): boolean {
	return (
		path === Routes.ME ||
		path.startsWith('/channels/') ||
		path.startsWith('/invite/') ||
		path.startsWith('/gift/') ||
		path.startsWith('/theme/') ||
		path.startsWith('/users/')
	);
}

export function parseUserSettingsDeepLink(rawUrl: string): UserSettingsDeepLinkTarget | null {
	const normalized = normalizeAppRoutePath(rawUrl);
	if (!normalized) return null;
	return parseUserSettingsDeepLinkPath(normalized);
}

export const parseDeepLink = (rawUrl: string): DeepLinkTarget | null => {
	const directUserId = parseUserProfileUrl(rawUrl);
	if (directUserId) {
		return {type: 'user', userId: directUserId};
	}
	const tryFromSegments = (segments: Array<string>, search?: string): DeepLinkTarget | null => {
		const [first, second, third] = segments.filter(Boolean);
		const preferLogin = third === 'login' || search?.includes('login=1') || search?.includes('action=login') || false;
		if (first === 'invite' && second) {
			return {type: 'invite', code: second, preferLogin};
		}
		if (first === 'gift' && second) {
			return {type: 'gift', code: second, preferLogin};
		}
		if (first === 'users' && second) {
			return {type: 'user', userId: second};
		}
		return null;
	};
	const appRoutePath = normalizeAppRoutePath(rawUrl);
	if (appRoutePath) {
		const [pathPart, searchPart] = appRoutePath.split('?');
		const target = tryFromSegments(pathPart.split('/'), searchPart ? `?${searchPart}` : undefined);
		if (target) return target;
		const settingsTarget = parseUserSettingsDeepLinkPath(appRoutePath);
		if (settingsTarget) return {type: 'user_settings', target: settingsTarget};
		if (isRoutableDeepLinkPath(pathPart)) return {type: 'route', path: appRoutePath};
	}
	try {
		const parsed = new URL(rawUrl);
		const segments = [parsed.host, ...parsed.pathname.split('/')];
		return tryFromSegments(segments, parsed.search);
	} catch {
		return null;
	}
};

function openUserSettingsDeepLink(target: UserSettingsDeepLinkTarget): void {
	ModalCommands.push(
		ModalCommands.modal(() =>
			createElement(UserSettingsModal, {
				initialTab: target.tab,
				initialSubtab: target.section,
			}),
		),
	);
	ComponentBus.dispatchOrBuffer('USER_SETTINGS_TAB_SELECT', {tab: target.tab, section: target.section});
}

const navigateForTarget = (target: DeepLinkTarget) => {
	const isAuthenticated = Authentication.isAuthenticated;
	if (target.type === 'gift' && RuntimeConfig.isSelfHosted()) {
		return;
	}
	if (isAuthenticated) {
		if (target.type === 'invite') {
			void InviteCommands.openAcceptModal(target.code);
			RouterUtils.transitionTo(Routes.ME);
		} else if (target.type === 'gift') {
			void GiftCommands.openAcceptModal(target.code);
			RouterUtils.transitionTo(Routes.ME);
		} else if (target.type === 'user') {
			navigateToLinkedUserProfile(target.userId);
		} else if (target.type === 'user_settings') {
			openUserSettingsDeepLink(target.target);
		} else if (target.type === 'route') {
			RouterUtils.transitionTo(target.path);
		}
		return;
	}
	if (target.type === 'user') {
		RouterUtils.transitionTo(setPathQueryParams(Routes.LOGIN, {redirect_to: Routes.userProfile(target.userId)}));
		return;
	}
	if (target.type === 'user_settings') {
		RouterUtils.transitionTo(Routes.LOGIN);
		return;
	}
	if (target.type === 'invite') {
		const dest = target.preferLogin ? Routes.inviteLogin(target.code) : Routes.inviteRegister(target.code);
		RouterUtils.transitionTo(dest);
		return;
	}
	if (target.type === 'route') {
		RouterUtils.transitionTo(target.path);
		return;
	}
	const dest = target.preferLogin ? Routes.giftLogin(target.code) : Routes.giftRegister(target.code);
	RouterUtils.transitionTo(dest);
};

export function handleDeepLinkUrl(rawUrl: string): boolean {
	const target = parseDeepLink(rawUrl);
	if (!target) return false;
	navigateForTarget(target);
	return true;
}

export function handleRpcNavigation(path: string): void {
	RouterUtils.transitionTo(path);
}

let listenerStarted = false;

export async function startDeepLinkHandling(): Promise<void> {
	if (listenerStarted) return;
	const electronApi = getElectronAPI();
	if (electronApi) {
		listenerStarted = true;
		try {
			const initialUrl = await electronApi.getInitialDeepLink();
			if (initialUrl) {
				handleDeepLinkUrl(initialUrl);
			}
		} catch (error) {
			logger.error(' Failed to get initial deep link', error);
		}
		electronApi.onDeepLink((url: string) => {
			try {
				handleDeepLinkUrl(url);
			} catch (error) {
				logger.error(' Failed to handle URL', url, error);
			}
		});
		if (typeof electronApi.onRpcNavigate === 'function') {
			electronApi.onRpcNavigate((path: string) => {
				try {
					handleRpcNavigation(path);
				} catch (error) {
					logger.error(' Failed to handle RPC navigation', path, error);
				}
			});
		} else {
			logger.warn(' onRpcNavigate not available on this host version');
		}
		return;
	}
}

const OFFICIAL_INTERNAL_APP_HOSTS = ['fluxer.app', 'canary.fluxer.app', 'web.fluxer.app', 'web.canary.fluxer.app'];
const getNormalizedWebAppHost = (): string => {
	try {
		return new URL(RuntimeConfig.webAppBaseUrl).host.toLowerCase();
	} catch {
		return '';
	}
};

export function isInternalChannelHost(host: string): boolean {
	if (!host) return false;
	const normalizedHost = host.toLowerCase();
	if (typeof location !== 'undefined' && normalizedHost === location.host.toLowerCase()) {
		return true;
	}
	if (RuntimeConfig.marketingHost && normalizedHost === RuntimeConfig.marketingHost.toLowerCase()) {
		return true;
	}
	const webAppHost = getNormalizedWebAppHost();
	if (webAppHost && normalizedHost === webAppHost) {
		return true;
	}
	return OFFICIAL_INTERNAL_APP_HOSTS.includes(normalizedHost);
}

export function parseChannelUrl(url: string): string | null {
	try {
		const parsed = new URL(url);
		const isInternal = isInternalChannelHost(parsed.host) && parsed.pathname.startsWith('/channels/');
		if (!isInternal) return null;
		const normalizedPath = parsed.pathname;
		const segments = normalizedPath.split('/').filter(Boolean);
		if (segments[0] !== 'channels') return null;
		const [, scope, channelId, messageId] = segments;
		const segmentCount = segments.length;
		const isSnowflake = (value?: string) => isProbablyAValidSnowflake(value ?? null);
		const isDmScope = scope === ME;
		let isValid = false;
		if (isDmScope) {
			if (segmentCount === 2) {
				isValid = true;
			} else if (segmentCount === 3 && isSnowflake(channelId)) {
				isValid = true;
			} else if (segmentCount === 4 && isSnowflake(channelId) && isSnowflake(messageId)) {
				isValid = true;
			}
		} else {
			if (segmentCount === 3 && isSnowflake(scope) && isSnowflake(channelId)) {
				isValid = true;
			} else if (segmentCount === 4 && isSnowflake(scope) && isSnowflake(channelId) && isSnowflake(messageId)) {
				isValid = true;
			}
		}
		if (isValid) {
			return normalizedPath;
		}
	} catch {
		return null;
	}
	return null;
}

export function parseUserProfileUrl(url: string): string | null {
	try {
		const parsed = new URL(url);
		if (!isInternalChannelHost(parsed.host) || !parsed.pathname.startsWith('/users/')) {
			return null;
		}
		const segments = parsed.pathname.split('/').filter(Boolean);
		if (segments.length !== 2 || segments[0] !== 'users') {
			return null;
		}
		const userId = segments[1];
		return isProbablyAValidSnowflake(userId) ? userId : null;
	} catch {
		return null;
	}
}

export function navigateToLinkedUserProfile(
	userId: string,
	{
		replace = false,
		guildId,
		autoFocusNote,
	}: {
		replace?: boolean;
		guildId?: string;
		autoFocusNote?: boolean;
	} = {},
): void {
	const navigate = replace ? RouterUtils.replaceWith : RouterUtils.transitionTo;
	navigate(Routes.ME);
	if (!isProbablyAValidSnowflake(userId)) {
		return;
	}
	void UserProfileCommands.openLinkedUserProfile(userId, guildId, autoFocusNote);
}

export interface ChannelJumpLink {
	scope: string;
	channelId: string;
}

export interface MessageJumpLink extends ChannelJumpLink {
	messageId: string;
}

const getChannelSegments = (url: string): Array<string> | null => {
	const channelPath = parseChannelUrl(url);
	if (!channelPath) return null;
	return channelPath.split('/').filter(Boolean);
};

export function parseChannelJumpLink(url: string): ChannelJumpLink | null {
	const segments = getChannelSegments(url);
	if (!segments || segments.length < 3) return null;
	const [, scope, channelId] = segments;
	if (!scope || !channelId) return null;
	return {
		scope,
		channelId,
	};
}

export function parseMessageJumpLink(url: string): MessageJumpLink | null {
	const segments = getChannelSegments(url);
	if (!segments || segments.length !== 4) return null;
	const [, scope, channelId, messageId] = segments;
	if (!messageId || !isProbablyAValidSnowflake(messageId)) {
		return null;
	}
	return {
		scope,
		channelId,
		messageId,
	};
}

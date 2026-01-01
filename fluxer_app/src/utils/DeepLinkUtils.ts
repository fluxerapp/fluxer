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

import React from 'react';
import * as GiftActionCreators from '~/actions/GiftActionCreators';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {ME} from '~/Constants';
import {UserProfileModal} from '~/components/modals/UserProfileModal';
import {Routes} from '~/Routes';
import AuthenticationStore from '~/stores/AuthenticationStore';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import UserStore from '~/stores/UserStore';
import * as RouterUtils from '~/utils/RouterUtils';
import SnowflakeUtil from '~/utils/SnowflakeUtil';
import {APP_PROTOCOL_PREFIX} from './appProtocol';
import {getElectronAPI} from './NativeUtils';

type DeepLinkTarget =
	| {type: 'invite'; code: string; preferLogin: boolean}
	| {type: 'gift'; code: string; preferLogin: boolean}
	| {type: 'user'; userId: string};

const parseDeepLink = (rawUrl: string): DeepLinkTarget | null => {
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

	try {
		const parsed = new URL(rawUrl);
		const segments = [parsed.host, ...parsed.pathname.split('/')];
		const target = tryFromSegments(segments, parsed.search);
		if (target) return target;
	} catch {}

	const protocolPattern = new RegExp(`^${APP_PROTOCOL_PREFIX.replace(/[-/\\^$*+?.()|[\]{}]/g, '\\$&')}`);
	const sanitized = rawUrl.replace(protocolPattern, '').replace(/^\/+/, '');
	const [pathPart, searchPart] = sanitized.split('?');
	const target = tryFromSegments(pathPart.split('/'), searchPart ? `?${searchPart}` : undefined);
	return target;
};

const navigateForTarget = (target: DeepLinkTarget) => {
	const isAuthenticated = AuthenticationStore.isAuthenticated;

	if (target.type === 'gift' && RuntimeConfigStore.isSelfHosted()) {
		return;
	}

	if (isAuthenticated) {
		if (target.type === 'invite') {
			void InviteActionCreators.openAcceptModal(target.code);
		} else {
			if (target.type === 'gift') {
				void GiftActionCreators.openAcceptModal(target.code);
			} else if (target.type === 'user') {
				void openUserProfile(target.userId);
			}
		}
		RouterUtils.transitionTo(Routes.ME);
		return;
	}

	if (target.type === 'user') {
		RouterUtils.transitionTo(Routes.LOGIN);
		return;
	}

	if (target.type === 'invite') {
		const dest = target.preferLogin ? Routes.inviteLogin(target.code) : Routes.inviteRegister(target.code);
		RouterUtils.transitionTo(dest);
		return;
	}

	const dest = target.preferLogin ? Routes.giftLogin(target.code) : Routes.giftRegister(target.code);
	RouterUtils.transitionTo(dest);
};

export const handleDeepLinkUrl = (rawUrl: string): boolean => {
	const target = parseDeepLink(rawUrl);
	if (!target) return false;
	navigateForTarget(target);
	return true;
};

export const handleRpcNavigation = (path: string): void => {
	RouterUtils.transitionTo(path);
};

let listenerStarted = false;

export const startDeepLinkHandling = async (): Promise<void> => {
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
			console.error('[DeepLink] Failed to get initial deep link', error);
		}

		electronApi.onDeepLink((url) => {
			try {
				handleDeepLinkUrl(url);
			} catch (error) {
				console.error('[DeepLink] Failed to handle URL', url, error);
			}
		});

		electronApi.onRpcNavigate((path) => {
			try {
				handleRpcNavigation(path);
			} catch (error) {
				console.error('[DeepLink] Failed to handle RPC navigation', path, error);
			}
		});

		return;
	}
};

const EXTRA_INTERNAL_CHANNEL_HOSTS = ['fluxer.app', 'canary.fluxer.app'];

export const isInternalChannelHost = (host: string): boolean => {
	if (!host) return false;
	if (typeof location !== 'undefined' && host === location.host) {
		return true;
	}
	if (RuntimeConfigStore.marketingHost && host === RuntimeConfigStore.marketingHost) {
		return true;
	}
	return EXTRA_INTERNAL_CHANNEL_HOSTS.includes(host);
};

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
		const isSnowflake = (value?: string) => SnowflakeUtil.isProbablyAValidSnowflake(value ?? null);
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
	if (!messageId || !SnowflakeUtil.isProbablyAValidSnowflake(messageId)) {
		return null;
	}

	return {
		scope,
		channelId,
		messageId,
	};
}

const openUserProfile = async (userId: string, guildId?: string) => {
	try {
		await UserProfileActionCreators.fetch(userId, guildId);
	} catch (error) {
		console.error('[DeepLink] Failed to fetch user profile', userId, error);
	}

	const user = UserStore.getUser(userId);
	ModalActionCreators.pushWithKey(
		modal(() =>
			React.createElement(UserProfileModal, {
				userId,
				guildId,
				previewUser: user ?? undefined,
			}),
		),
		`user-profile-${userId}-${guildId ?? 'global'}`,
	);
};

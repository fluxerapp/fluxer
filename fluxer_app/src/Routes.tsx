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

import {marketingUrl} from '~/utils/UrlUtils';

export const Routes = {
	HOME: '/',
	LOGIN: '/login',
	REGISTER: '/register',
	FORGOT_PASSWORD: '/forgot',
	RESET_PASSWORD: '/reset',
	VERIFY_EMAIL: '/verify',
	AUTHORIZE_IP: '/authorize-ip',
	EMAIL_REVERT: '/wasntme',
	PENDING_VERIFICATION: '/pending',
	OAUTH_AUTHORIZE: '/oauth2/authorize',

	INVITE_REGISTER: '/invite/:code',
	INVITE_LOGIN: '/invite/:code/login',
	GIFT_REGISTER: '/gift/:code',
	GIFT_LOGIN: '/gift/:code/login',
	THEME_REGISTER: '/theme/:themeId',
	THEME_LOGIN: '/theme/:themeId/login',

	ME: '/channels/@me',
	FAVORITES: '/channels/@favorites',
	BOOKMARKS: '/bookmarks',
	MENTIONS: '/mentions',
	NOTIFICATIONS: '/notifications',
	YOU: '/you',
	REPORT: '/report',
	PREMIUM_CALLBACK: '/premium-callback',

	terms: () => marketingUrl('terms'),
	privacy: () => marketingUrl('privacy'),
	guidelines: () => marketingUrl('guidelines'),
	careers: () => marketingUrl('careers'),
	partners: () => marketingUrl('partners'),
	bugs: () => marketingUrl('bugs'),
	plutonium: () => marketingUrl('plutonium'),
	plutoniumVisionary: () => marketingUrl('plutonium#visionary'),
	help: () => marketingUrl('help'),

	dmChannel: (channelId: string) => `/channels/@me/${channelId}`,
	favoritesChannel: (channelId: string) => `/channels/@favorites/${channelId}`,
	guildChannel: (guildId: string, channelId?: string) =>
		channelId ? `/channels/${guildId}/${channelId}` : `/channels/${guildId}`,
	channelMessage: (guildId: string, channelId: string, messageId: string) =>
		`${Routes.guildChannel(guildId, channelId)}/${messageId}`,
	dmChannelMessage: (channelId: string, messageId: string) => `${Routes.dmChannel(channelId)}/${messageId}`,
	favoritesChannelMessage: (channelId: string, messageId: string) =>
		`${Routes.favoritesChannel(channelId)}/${messageId}`,
	inviteRegister: (code: string) => `/invite/${code}`,
	inviteLogin: (code: string) => `/invite/${code}/login`,
	giftRegister: (code: string) => `/gift/${code}`,
	giftLogin: (code: string) => `/gift/${code}/login`,
	theme: (themeId: string) => `/theme/${themeId}`,
	themeRegister: (themeId: string) => `/theme/${themeId}`,
	themeLogin: (themeId: string) => `/theme/${themeId}/login`,

	isSpecialPage: (pathname: string) =>
		pathname === Routes.BOOKMARKS ||
		pathname === Routes.MENTIONS ||
		pathname === Routes.NOTIFICATIONS ||
		pathname === Routes.YOU,

	isDMRoute: (pathname: string) => pathname.startsWith('/channels/@me'),
	isFavoritesRoute: (pathname: string) => pathname.startsWith('/channels/@favorites'),
	isChannelRoute: (pathname: string) => pathname.startsWith('/channels/'),
	isGuildChannelRoute: (pathname: string) =>
		pathname.startsWith('/channels/') &&
		!pathname.startsWith('/channels/@me') &&
		!pathname.startsWith('/channels/@favorites'),
} as const;

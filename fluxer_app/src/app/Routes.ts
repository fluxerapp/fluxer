// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {marketingUrl} from '@app/features/messaging/utils/MessagingUrlUtils';

function hostedMarketingUrl(path: string): string | null {
	return RuntimeConfig.isSelfHosted() ? null : marketingUrl(path);
}

export const Routes = {
	HOME: '/',
	APP: '/app',
	LOGIN: '/login',
	REGISTER: '/register',
	FORGOT_PASSWORD: '/forgot',
	RESET_PASSWORD: '/reset',
	RECOVER_ACCOUNT: '/recover',
	VERIFY_EMAIL: '/verify',
	AUTHORIZE_IP: '/authorize-ip',
	EMAIL_REVERT: '/wasntme',
	PENDING: '/pending',
	OAUTH_AUTHORIZE: '/oauth2/authorize',
	SSO_CALLBACK: '/auth/sso/callback',
	INVITE_REGISTER: '/invite/:code',
	INVITE_LOGIN: '/invite/:code/login',
	GIFT_REGISTER: '/gift/:code',
	GIFT_LOGIN: '/gift/:code/login',
	THEME_REGISTER: '/theme/:themeId',
	THEME_LOGIN: '/theme/:themeId/login',
	ME: '/channels/@me',
	FAVORITES: '/channels/@favorites',
	DISCOVER: '/channels/@discover',
	BOOKMARKS: '/bookmarks',
	MENTIONS: '/mentions',
	NOTIFICATIONS: '/notifications',
	YOU: '/you',
	REPORT: '/report',
	PREMIUM_CALLBACK: '/premium-callback',
	AGE_VERIFICATION_CALLBACK: '/age-verification-callback',
	CONNECTION_CALLBACK: '/connection-callback',
	USER_PROFILE: '/users/:userId',
	THEME_STUDIO: '/theme-studio',
	PLUTONIUM: '/channels/@premium',
	LEGACY_PLUTONIUM: '/plutonium',
	terms: () => marketingUrl('terms'),
	privacy: () => marketingUrl('privacy'),
	guidelines: (): string | null => RuntimeConfig.guidelinesUrl ?? hostedMarketingUrl('guidelines'),
	careers: () => marketingUrl('careers'),
	partners: () => marketingUrl('partners'),
	bugs: () => marketingUrl('help/report-bug'),
	plutonium: () => marketingUrl('plutonium'),
	help: (): string | null => hostedMarketingUrl('help'),
	helpArticle: (slug: string): string | null => hostedMarketingUrl(`help/${slug}`),
	dmChannel: (channelId: string) => `/channels/@me/${channelId}`,
	favoritesChannel: (channelId: string) => `/channels/@favorites/${channelId}`,
	guildMembers: (guildId: string) => `/channels/${guildId}/members`,
	guildChannel: (guildId: string, channelId?: string) =>
		channelId ? `/channels/${guildId}/${channelId}` : `/channels/${guildId}`,
	channelMessage: (guildId: string, channelId: string, messageId: string) =>
		`${Routes.guildChannel(guildId, channelId)}/${messageId}`,
	threadPanel: (guildId: string, channelId: string, threadId: string, messageId?: string) =>
		`${Routes.guildChannel(guildId, channelId)}/threads/${threadId}${messageId ? `/${messageId}` : ''}`,
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
	userProfile: (userId: string) => `/users/${userId}`,
	isSpecialPage: (pathname: string) =>
		pathname === Routes.BOOKMARKS ||
		pathname === Routes.MENTIONS ||
		pathname === Routes.NOTIFICATIONS ||
		pathname === Routes.YOU,
	isDMRoute: (pathname: string) => pathname.startsWith('/channels/@me'),
	isFavoritesRoute: (pathname: string) => pathname.startsWith('/channels/@favorites'),
	isDiscoverRoute: (pathname: string) => pathname.startsWith('/channels/@discover'),
	isPlutoniumRoute: (pathname: string) => pathname === '/channels/@premium',
	isChannelRoute: (pathname: string) => pathname.startsWith('/channels/') && !pathname.startsWith('/channels/@premium'),
	isGuildChannelRoute: (pathname: string) =>
		pathname.startsWith('/channels/') &&
		!pathname.startsWith('/channels/@me') &&
		!pathname.startsWith('/channels/@favorites') &&
		!pathname.startsWith('/channels/@discover') &&
		!pathname.startsWith('/channels/@premium'),
} as const;

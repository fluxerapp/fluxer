// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {marketingUrl} from '@app/features/messaging/utils/MessagingUrlUtils';

function hostedMarketingUrl(path: string): string | null {
	return RuntimeConfig.isSelfHosted() ? null : marketingUrl(path);
}

const CHANNEL_SCOPE_ROOTS: ReadonlySet<string> = new Set(['@me', '@favorites', '@explore', '@discover']);

function channelsRoot(pathname: string): string | null {
	return pathname.startsWith('/channels/') ? (pathname.split('/')[2] ?? '') : null;
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
	DISCOVER: '/channels/@explore',
	LEGACY_DISCOVER: '/channels/@discover',
	BOOKMARKS: '/bookmarks',
	MENTIONS: '/mentions',
	NOTIFICATIONS: '/notifications',
	YOU: '/you',
	REPORT: '/report',
	PREMIUM_CALLBACK: '/premium-callback',
	CONNECTION_CALLBACK: '/connection-callback',
	USER_PROFILE: '/users/:userId',
	PLUTONIUM: '/channels/@premium',
	LEGACY_PLUTONIUM: '/plutonium',
	OPERATOR_PASS: '/channels/@operator-pass',
	CHECKOUT: '/channels/@checkout/:product',
	terms: (): string | null => RuntimeConfig.termsUrl ?? hostedMarketingUrl('terms'),
	privacy: (): string | null => RuntimeConfig.privacyUrl ?? hostedMarketingUrl('privacy'),
	guidelines: (): string | null => RuntimeConfig.guidelinesUrl ?? hostedMarketingUrl('guidelines'),
	operatorPassInfo: () => marketingUrl('pass'),
	help: (): string | null => hostedMarketingUrl('help'),
	helpArticle: (slug: string): string | null => hostedMarketingUrl(`help/${slug}`),
	patchNotes: (): string | null => hostedMarketingUrl('patch-notes'),
	feedback: (): string | null => (RuntimeConfig.isSelfHosted() ? null : 'https://feedback.fluxer.com'),
	donate: (): string | null => hostedMarketingUrl('donate'),
	operatorPassTerms: () => marketingUrl('operator-pass-terms'),
	operatorPassPrivacy: () => marketingUrl('operator-pass-privacy'),
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
	isDiscoverRoute: (pathname: string) =>
		pathname.startsWith(Routes.DISCOVER) || pathname.startsWith(Routes.LEGACY_DISCOVER),
	isChannelRoute: (pathname: string) => {
		const root = channelsRoot(pathname);
		return root !== null && (!root.startsWith('@') || CHANNEL_SCOPE_ROOTS.has(root));
	},
	isGuildChannelRoute: (pathname: string) => {
		const root = channelsRoot(pathname);
		return root !== null && !root.startsWith('@');
	},
} as const;

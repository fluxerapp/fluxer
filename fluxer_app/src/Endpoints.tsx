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

import {ME} from '~/Constants';

export const Endpoints = {
	INSTANCE: '/instance',

	AUTH_LOGIN: '/auth/login',
	AUTH_LOGIN_MFA_TOTP: '/auth/login/mfa/totp',
	AUTH_LOGIN_MFA_SMS_SEND: '/auth/login/mfa/sms/send',
	AUTH_LOGIN_MFA_SMS: '/auth/login/mfa/sms',
	AUTH_LOGIN_MFA_WEBAUTHN_OPTIONS: '/auth/login/mfa/webauthn/authentication-options',
	AUTH_LOGIN_MFA_WEBAUTHN: '/auth/login/mfa/webauthn',
	AUTH_WEBAUTHN_OPTIONS: '/auth/webauthn/authentication-options',
	AUTH_WEBAUTHN_AUTHENTICATE: '/auth/webauthn/authenticate',
	AUTH_LOGOUT: '/auth/logout',
	AUTH_REGISTER: '/auth/register',
	AUTH_USERNAME_SUGGESTIONS: '/auth/username-suggestions',
	AUTH_REDEEM_BETA_CODE: '/auth/redeem-beta-code',
	AUTH_SESSIONS: '/auth/sessions',
	AUTH_SESSIONS_LOGOUT: '/auth/sessions/logout',
	AUTH_HANDOFF_INITIATE: '/auth/handoff/initiate',
	AUTH_HANDOFF_COMPLETE: '/auth/handoff/complete',
	AUTH_HANDOFF_STATUS: (code: string) => `/auth/handoff/${code}/status`,
	AUTH_HANDOFF_CANCEL: (code: string) => `/auth/handoff/${code}`,
	AUTH_FORGOT_PASSWORD: '/auth/forgot',
	AUTH_RESET_PASSWORD: '/auth/reset',
	AUTH_EMAIL_REVERT: '/auth/email-revert',
	AUTH_VERIFY_EMAIL: '/auth/verify',
	AUTH_RESEND_VERIFICATION: '/auth/verify/resend',
	AUTH_AUTHORIZE_IP: '/auth/authorize-ip',
	AUTH_IP_AUTHORIZATION_RESEND: '/auth/ip-authorization/resend',
	AUTH_IP_AUTHORIZATION_STREAM: (ticket: string) =>
		`/auth/ip-authorization/stream?ticket=${encodeURIComponent(ticket)}`,

	SUDO_MFA_METHODS: '/users/@me/sudo/mfa-methods',
	SUDO_SMS_SEND: '/users/@me/sudo/mfa/sms/send',
	SUDO_WEBAUTHN_OPTIONS: '/users/@me/sudo/webauthn/authentication-options',

	OAUTH_AUTHORIZE: '/oauth2/authorize',
	OAUTH_CONSENT: '/oauth2/authorize/consent',

	OAUTH_APPLICATIONS: '/oauth2/applications',
	OAUTH_APPLICATIONS_LIST: '/oauth2/applications/@me',
	OAUTH_APPLICATION: (applicationId: string) => `/oauth2/applications/${applicationId}`,
	OAUTH_APPLICATION_BOT_TOKEN_RESET: (applicationId: string) => `/oauth2/applications/${applicationId}/bot/reset-token`,
	OAUTH_APPLICATION_CLIENT_SECRET_RESET: (applicationId: string) =>
		`/oauth2/applications/${applicationId}/client-secret/reset`,
	OAUTH_APPLICATION_BOT_PROFILE: (applicationId: string) => `/oauth2/applications/${applicationId}/bot`,
	OAUTH_PUBLIC_APPLICATION: (applicationId: string) => `/oauth2/applications/${applicationId}/public`,

	OAUTH_AUTHORIZATIONS: '/oauth2/@me/authorizations',
	OAUTH_AUTHORIZATION: (applicationId: string) => `/oauth2/@me/authorizations/${applicationId}`,

	CHANNEL: (channelId: string) => `/channels/${channelId}`,
	CHANNEL_ATTACHMENTS: (channelId: string) => `/channels/${channelId}/attachments`,
	CHANNEL_INVITES: (channelId: string) => `/channels/${channelId}/invites`,
	CHANNEL_RECIPIENT: (channelId: string, userId: string) => `/channels/${channelId}/recipients/${userId}`,
	CHANNEL_MESSAGES: (channelId: string) => `/channels/${channelId}/messages`,
	CHANNEL_MESSAGE_SCHEDULE: (channelId: string) => `/channels/${channelId}/messages/schedule`,
	CHANNEL_MESSAGE: (channelId: string, messageId: string) => `/channels/${channelId}/messages/${messageId}`,
	CHANNEL_MESSAGE_ATTACHMENT: (channelId: string, messageId: string, attachmentId: string) =>
		`/channels/${channelId}/messages/${messageId}/attachments/${attachmentId}`,
	CHANNEL_MESSAGE_ACK: (channelId: string, messageId: string) => `/channels/${channelId}/messages/${messageId}/ack`,
	CHANNEL_MESSAGE_REACTION: (channelId: string, messageId: string, emoji: string) =>
		`/channels/${channelId}/messages/${messageId}/reactions/${emoji}`,
	CHANNEL_MESSAGE_REACTION_QUERY: (channelId: string, messageId: string, emoji: string, query = ME) =>
		`/channels/${channelId}/messages/${messageId}/reactions/${emoji}/${query}`,
	CHANNEL_MESSAGE_REACTIONS: (channelId: string, messageId: string) =>
		`/channels/${channelId}/messages/${messageId}/reactions`,
	CHANNEL_MESSAGES_ACK: (channelId: string) => `/channels/${channelId}/messages/ack`,
	CHANNEL_PIN: (channelId: string, messageId: string) => `/channels/${channelId}/pins/${messageId}`,
	CHANNEL_PINS: (channelId: string) => `/channels/${channelId}/messages/pins`,
	CHANNEL_PINS_ACK: (channelId: string) => `/channels/${channelId}/pins/ack`,
	CHANNEL_TYPING: (channelId: string) => `/channels/${channelId}/typing`,
	CHANNEL_WEBHOOKS: (channelId: string) => `/channels/${channelId}/webhooks`,
	CHANNEL_RTC_REGIONS: (channelId: string) => `/channels/${channelId}/rtc-regions`,
	CHANNEL_CALL: (channelId: string) => `/channels/${channelId}/call`,
	CHANNEL_CALL_RING: (channelId: string) => `/channels/${channelId}/call/ring`,
	CHANNEL_CALL_STOP_RINGING: (channelId: string) => `/channels/${channelId}/call/stop-ringing`,

	GUILDS: '/guilds',
	GUILD: (guildId: string) => `/guilds/${guildId}`,
	GUILD_CHANNELS: (guildId: string) => `/guilds/${guildId}/channels`,
	GUILD_MEMBER: (guildId: string, query = ME) => `/guilds/${guildId}/members/${query}`,
	GUILD_MEMBERS: (guildId: string) => `/guilds/${guildId}/members`,
	GUILD_MEMBER_ROLE: (guildId: string, userId: string, roleId: string) =>
		`/guilds/${guildId}/members/${userId}/roles/${roleId}`,
	GUILD_BAN: (guildId: string, userId: string) => `/guilds/${guildId}/bans/${userId}`,
	GUILD_BANS: (guildId: string) => `/guilds/${guildId}/bans`,
	GUILD_ROLE: (guildId: string, roleId: string) => `/guilds/${guildId}/roles/${roleId}`,
	GUILD_ROLES: (guildId: string) => `/guilds/${guildId}/roles`,
	GUILD_ROLE_HOIST_POSITIONS: (guildId: string) => `/guilds/${guildId}/roles/hoist-positions`,
	GUILD_DELETE: (guildId: string) => `/guilds/${guildId}/delete`,
	GUILD_TRANSFER_OWNERSHIP: (guildId: string) => `/guilds/${guildId}/transfer-ownership`,
	GUILD_TEXT_CHANNEL_FLEXIBLE_NAMES: (guildId: string) => `/guilds/${guildId}/text-channel-flexible-names`,
	GUILD_DETACHED_BANNER: (guildId: string) => `/guilds/${guildId}/detached-banner`,
	GUILD_DISALLOW_UNCLAIMED_ACCOUNTS: (guildId: string) => `/guilds/${guildId}/disallow-unclaimed-accounts`,
	GUILD_EMOJI: (guildId: string, emojiId: string) => `/guilds/${guildId}/emojis/${emojiId}`,
	GUILD_EMOJIS: (guildId: string) => `/guilds/${guildId}/emojis`,
	GUILD_STICKER: (guildId: string, stickerId: string) => `/guilds/${guildId}/stickers/${stickerId}`,
	GUILD_STICKERS: (guildId: string) => `/guilds/${guildId}/stickers`,
	GUILD_INVITES: (guildId: string) => `/guilds/${guildId}/invites`,
	GUILD_VANITY_URL: (guildId: string) => `/guilds/${guildId}/vanity-url`,
	GUILD_WEBHOOKS: (guildId: string) => `/guilds/${guildId}/webhooks`,
	GUILD_AUDIT_LOGS: (guildId: string) => `/guilds/${guildId}/audit-logs`,

	INVITE: (code: string) => `/invites/${code}`,

	GIFT: (code: string) => `/gifts/${code}`,
	GIFT_REDEEM: (code: string) => `/gifts/${code}/redeem`,
	USER_GIFTS: '/users/@me/gifts',

	PREMIUM_VISIONARY_SLOTS: '/premium/visionary/slots',
	PREMIUM_VISIONARY_REJOIN: '/premium/visionary/rejoin',
	PREMIUM_OPERATOR_REJOIN: '/premium/operator/rejoin',
	PREMIUM_PRICE_IDS: '/premium/price-ids',
	PREMIUM_CUSTOMER_PORTAL: '/premium/customer-portal',
	PREMIUM_CANCEL_SUBSCRIPTION: '/premium/cancel-subscription',
	PREMIUM_REACTIVATE_SUBSCRIPTION: '/premium/reactivate-subscription',
	STRIPE_CHECKOUT_SUBSCRIPTION: '/stripe/checkout/subscription',
	STRIPE_CHECKOUT_GIFT: '/stripe/checkout/gift',

	SWISH_AVAILABLE: '/swish/available',
	SWISH_PRICES: '/swish/prices',
	SWISH_CHECKOUT: '/swish/checkout',
	SWISH_PAYMENT: (paymentId: string) => `/swish/payments/${paymentId}`,
	SWISH_PAYMENTS: '/swish/payments',

	READ_STATES_ACK_BULK: '/read-states/ack-bulk',

	DSA_REPORT_EMAIL_SEND: '/reports/dsa/email/send',
	DSA_REPORT_EMAIL_VERIFY: '/reports/dsa/email/verify',
	DSA_REPORT_CREATE: '/reports/dsa',

	TENOR_FEATURED: '/tenor/featured',
	TENOR_REGISTER_SHARE: '/tenor/register-share',
	TENOR_SEARCH: '/tenor/search',
	TENOR_SUGGEST: '/tenor/suggest',
	TENOR_TRENDING_GIFS: '/tenor/trending-gifs',

	USER_BETA_CODE: (code: string) => `/users/@me/beta-codes/${code}`,
	USER_BETA_CODES: '/users/@me/beta-codes',
	USER_CHANNELS: '/users/@me/channels',
	USER_CHANNEL_PIN: (channelId: string) => `/users/@me/channels/${channelId}/pin`,
	USER_GUILDS_LIST: '/users/@me/guilds',
	USER_GUILDS: (guildId: string) => `/users/@me/guilds/${guildId}`,
	USER_ME: '/users/@me',
	USER_MENTION: (messageId: string) => `/users/@me/mentions/${messageId}`,
	USER_MENTIONS: '/users/@me/mentions',
	USER_MFA_BACKUP_CODES: '/users/@me/mfa/backup-codes',
	USER_MFA_TOTP_DISABLE: '/users/@me/mfa/totp/disable',
	USER_MFA_TOTP_ENABLE: '/users/@me/mfa/totp/enable',
	USER_MFA_SMS_ENABLE: '/users/@me/mfa/sms/enable',
	USER_MFA_SMS_DISABLE: '/users/@me/mfa/sms/disable',
	USER_MFA_WEBAUTHN_CREDENTIALS: '/users/@me/mfa/webauthn/credentials',
	USER_MFA_WEBAUTHN_REGISTRATION_OPTIONS: '/users/@me/mfa/webauthn/credentials/registration-options',
	USER_MFA_WEBAUTHN_CREDENTIAL: (credentialId: string) => `/users/@me/mfa/webauthn/credentials/${credentialId}`,
	USER_PHONE_SEND_VERIFICATION: '/users/@me/phone/send-verification',
	USER_PHONE_VERIFY: '/users/@me/phone/verify',
	USER_PHONE: '/users/@me/phone',
	USER_EMAIL_CHANGE_START: '/users/@me/email-change/start',
	USER_EMAIL_CHANGE_RESEND_ORIGINAL: '/users/@me/email-change/resend-original',
	USER_EMAIL_CHANGE_VERIFY_ORIGINAL: '/users/@me/email-change/verify-original',
	USER_EMAIL_CHANGE_REQUEST_NEW: '/users/@me/email-change/request-new',
	USER_EMAIL_CHANGE_RESEND_NEW: '/users/@me/email-change/resend-new',
	USER_EMAIL_CHANGE_VERIFY_NEW: '/users/@me/email-change/verify-new',
	USER_DISABLE: '/users/@me/disable',
	USER_DELETE: '/users/@me/delete',
	USER_BULK_DELETE_MESSAGES: '/users/@me/messages/delete',
	USER_BULK_DELETE_MESSAGES_TEST: '/users/@me/messages/delete/test',
	USER_HARVEST: '/users/@me/harvest',
	USER_HARVEST_LATEST: '/users/@me/harvest/latest',
	USER_HARVEST_STATUS: (harvestId: string) => `/users/@me/harvest/${harvestId}`,
	USER_PRELOAD_MESSAGES: '/users/@me/preload-messages',
	USER_NOTE: (userId: string) => `/users/@me/notes/${userId}`,
	USER_CHECK_TAG: '/users/check-tag',
	USER_PROFILE: (query = ME) => `/users/${query}/profile`,
	USER_RELATIONSHIP: (userId: string) => `/users/@me/relationships/${userId}`,
	USER_RELATIONSHIPS: '/users/@me/relationships',
	USER_THEMES: '/users/@me/themes',
	USER_SAVED_MESSAGE: (messageId: string) => `/users/@me/saved-messages/${messageId}`,
	USER_SAVED_MESSAGES: '/users/@me/saved-messages',
	USER_SCHEDULED_MESSAGES: '/users/@me/scheduled-messages',
	USER_SCHEDULED_MESSAGE: (messageId: string) => `/users/@me/scheduled-messages/${messageId}`,
	USER_FAVORITE_MEMES: (query = ME) => `/users/${query}/memes`,
	USER_FAVORITE_MEME: (query = ME, memeId: string) => `/users/${query}/memes/${memeId}`,
	CHANNEL_MESSAGE_FAVORITE_MEMES: (channelId: string, messageId: string) =>
		`/channels/${channelId}/messages/${messageId}/memes`,
	STREAM_PREVIEW: (streamKey: string) => `/streams/${streamKey}/preview`,
	USER_SETTINGS: '/users/@me/settings',
	USER_GUILD_SETTINGS_ME: '/users/@me/guilds/@me/settings',
	USER_GUILD_SETTINGS: (guildId: string) => `/users/@me/guilds/${guildId}/settings`,
	USER_PUSH_SUBSCRIBE: '/users/@me/push/subscribe',
	USER_PUSH_SUBSCRIPTIONS: '/users/@me/push/subscriptions',
	USER_PUSH_SUBSCRIPTION: (subscriptionId: string) => `/users/@me/push/subscriptions/${subscriptionId}`,
	PACKS: '/packs',
	PACK: (packId: string) => `/packs/${packId}`,
	PACK_CREATE: (packType: 'emoji' | 'sticker') => `/packs/${packType}`,
	PACK_INSTALL: (packId: string) => `/packs/${packId}/install`,
	PACK_EMOJIS: (packId: string) => `/packs/emojis/${packId}`,
	PACK_EMOJI: (packId: string, emojiId: string) => `/packs/emojis/${packId}/${emojiId}`,
	PACK_EMOJI_BULK: (packId: string) => `/packs/emojis/${packId}/bulk`,
	PACK_STICKERS: (packId: string) => `/packs/stickers/${packId}`,
	PACK_STICKER: (packId: string, stickerId: string) => `/packs/stickers/${packId}/${stickerId}`,
	PACK_STICKERS_BULK: (packId: string) => `/packs/stickers/${packId}/bulk`,
	PACK_INVITES: (packId: string) => `/packs/${packId}/invites`,

	WEBHOOK: (webhookId: string) => `/webhooks/${webhookId}`,

	REPORT_MESSAGE: '/reports/message',
	REPORT_USER: '/reports/user',
	REPORT_GUILD: '/reports/guild',
} as const;

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

import type {RouteRateLimitConfig} from '~/middleware/RateLimitMiddleware';

export const UserRateLimitConfigs = {
	USER_GET: {
		bucket: 'user:read::user_id',
		config: {limit: 100, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_GET_PROFILE: {
		bucket: 'user:profile::target_id',
		config: {limit: 100, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_CHECK_TAG: {
		bucket: 'user:check_tag',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_UPDATE_SELF: {
		bucket: 'user:update',
		config: {limit: 20, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_EMAIL_CHANGE_START: {
		bucket: 'user:email_change:start',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_EMAIL_CHANGE_RESEND_ORIGINAL: {
		bucket: 'user:email_change:resend_original',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_EMAIL_CHANGE_VERIFY_ORIGINAL: {
		bucket: 'user:email_change:verify_original',
		config: {limit: 20, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_EMAIL_CHANGE_REQUEST_NEW: {
		bucket: 'user:email_change:request_new',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_EMAIL_CHANGE_RESEND_NEW: {
		bucket: 'user:email_change:resend_new',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_EMAIL_CHANGE_VERIFY_NEW: {
		bucket: 'user:email_change:verify_new',
		config: {limit: 20, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_ACCOUNT_DISABLE: {
		bucket: 'user:account:disable',
		config: {limit: 5, windowMs: 3600000},
	} as RouteRateLimitConfig,

	USER_ACCOUNT_DELETE: {
		bucket: 'user:account:delete',
		config: {limit: 5, windowMs: 3600000},
	} as RouteRateLimitConfig,

	USER_DATA_HARVEST: {
		bucket: 'user:data:harvest',
		config: {limit: 1, windowMs: 3600000},
	} as RouteRateLimitConfig,

	USER_PRELOAD_MESSAGES: {
		bucket: 'user:preload_messages',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_BULK_MESSAGE_DELETE: {
		bucket: 'user:messages:bulk_delete',
		config: {limit: 6, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_SETTINGS_GET: {
		bucket: 'user:settings:get',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_SETTINGS_UPDATE: {
		bucket: 'user:settings:update',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_GUILD_SETTINGS_UPDATE: {
		bucket: 'user:guild_settings:update',
		config: {limit: 30, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_CHANNELS: {
		bucket: 'user:channels',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_RELATIONSHIPS_LIST: {
		bucket: 'user:relationships:list',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_FRIEND_REQUEST_SEND: {
		bucket: 'user:friend_request:send',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_FRIEND_REQUEST_ACCEPT: {
		bucket: 'user:friend_request:accept',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_BLOCK: {
		bucket: 'user:block',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_RELATIONSHIP_DELETE: {
		bucket: 'user:relationship:delete',
		config: {limit: 30, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_RELATIONSHIP_UPDATE: {
		bucket: 'user:relationship:update',
		config: {limit: 30, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_NOTES_READ: {
		bucket: 'user:notes:read',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_NOTES_WRITE: {
		bucket: 'user:notes:write',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_BETA_CODES_READ: {
		bucket: 'user:beta_codes:read',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_BETA_CODES_CREATE: {
		bucket: 'user:beta_codes:create',
		config: {limit: 6, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_BETA_CODES_DELETE: {
		bucket: 'user:beta_codes:delete',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_MENTIONS_READ: {
		bucket: 'user:mentions:read',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_MENTIONS_DELETE: {
		bucket: 'user:mentions:delete',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_SAVED_MESSAGES_READ: {
		bucket: 'user:saved_messages:read',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_SAVED_MESSAGES_WRITE: {
		bucket: 'user:saved_messages:write',
		config: {limit: 30, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_MFA_TOTP_ENABLE: {
		bucket: 'user:mfa:totp:enable',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_MFA_TOTP_DISABLE: {
		bucket: 'user:mfa:totp:disable',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_MFA_BACKUP_CODES: {
		bucket: 'user:mfa:backup_codes',
		config: {limit: 6, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_PUSH_SUBSCRIBE: {
		bucket: 'user:push:subscribe',
		config: {limit: 20, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_PUSH_UNSUBSCRIBE: {
		bucket: 'user:push:unsubscribe',
		config: {limit: 40, windowMs: 60000},
	} as RouteRateLimitConfig,

	USER_PUSH_LIST: {
		bucket: 'user:push:list',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_HARVEST_LATEST: {
		bucket: 'user:harvest:latest',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_HARVEST_STATUS: {
		bucket: 'user:harvest:status',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	USER_HARVEST_DOWNLOAD: {
		bucket: 'user:harvest:download',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,
} as const;

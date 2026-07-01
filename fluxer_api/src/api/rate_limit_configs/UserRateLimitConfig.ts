// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';
import type {RouteRateLimitConfig} from '../middleware/RateLimitMiddleware';

export const UserRateLimitConfigs = {
	USER_GET: {
		bucket: 'user:read::user_id',
		config: {limit: 100, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_GET_PROFILE: {
		bucket: 'user:profile::target_id',
		config: {limit: 100, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_CHECK_TAG: {
		bucket: 'user:check_tag',
		config: {limit: 60, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_UPDATE_SELF: {
		bucket: 'user:update',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_EMAIL_CHANGE_START: {
		bucket: 'user:email_change:start',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_EMAIL_CHANGE_RESEND_ORIGINAL: {
		bucket: 'user:email_change:resend_original',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_EMAIL_CHANGE_VERIFY_ORIGINAL: {
		bucket: 'user:email_change:verify_original',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_EMAIL_CHANGE_REQUEST_NEW: {
		bucket: 'user:email_change:request_new',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_EMAIL_CHANGE_RESEND_NEW: {
		bucket: 'user:email_change:resend_new',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_EMAIL_CHANGE_VERIFY_NEW: {
		bucket: 'user:email_change:verify_new',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_EMAIL_CHANGE_APPLY: {
		bucket: 'user:email_change:apply',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_EMAIL_CHANGE_BOUNCED_REQUEST_NEW: {
		bucket: 'user:email_change:bounced:request_new',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_EMAIL_CHANGE_BOUNCED_RESEND_NEW: {
		bucket: 'user:email_change:bounced:resend_new',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_EMAIL_CHANGE_BOUNCED_VERIFY_NEW: {
		bucket: 'user:email_change:bounced:verify_new',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_PASSWORD_CHANGE_START: {
		bucket: 'user:password_change:start',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_PASSWORD_CHANGE_RESEND: {
		bucket: 'user:password_change:resend',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_PASSWORD_CHANGE_VERIFY: {
		bucket: 'user:password_change:verify',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_PASSWORD_CHANGE_COMPLETE: {
		bucket: 'user:password_change:complete',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_ACCOUNT_DISABLE: {
		bucket: 'user:account:disable',
		config: {limit: 5, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	USER_ACCOUNT_DELETE: {
		bucket: 'user:account:delete',
		config: {limit: 5, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	USER_DATA_HARVEST: {
		bucket: 'user:data:harvest',
		config: {limit: 5, windowMs: ms('30 minutes')},
	} as RouteRateLimitConfig,
	USER_PRELOAD_MESSAGES: {
		bucket: 'user:preload_messages',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_BULK_MESSAGE_DELETE: {
		bucket: 'user:messages:bulk_delete',
		config: {limit: 6, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_BULK_DELETE_MY_MESSAGES_FILTERED: {
		bucket: 'user:messages:bulk_delete_mine_filtered',
		config: {limit: 5, windowMs: ms('30 minutes')},
	} as RouteRateLimitConfig,
	USER_SETTINGS_GET: {
		bucket: 'user:settings:get',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_SETTINGS_UPDATE: {
		bucket: 'user:settings:update',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_GUILD_SETTINGS_UPDATE: {
		bucket: 'user:guild_settings:update',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_CHANNELS: {
		bucket: 'user:channels',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_GROUP_DM_CREATE: {
		bucket: 'user:group_dm:create',
		config: {limit: 10, windowMs: ms('1 hour'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
	USER_GROUP_DM_RECIPIENT_ADD: {
		bucket: 'user:group_dm:recipient:add',
		config: {limit: 10, windowMs: ms('1 hour'), exemptFromGlobal: true},
	} as RouteRateLimitConfig,
	USER_RELATIONSHIPS_LIST: {
		bucket: 'user:relationships:list',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_FRIEND_REQUEST_SEND: {
		bucket: 'user:friend_request:send',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_FRIEND_REQUEST_ACCEPT: {
		bucket: 'user:friend_request:accept',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_BLOCK: {
		bucket: 'user:block',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_BULK_IGNORE_FRIEND_REQUESTS: {
		bucket: 'user:friend_request:bulk_ignore',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_RELATIONSHIP_DELETE: {
		bucket: 'user:relationship:delete',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_RELATIONSHIP_UPDATE: {
		bucket: 'user:relationship:update',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_NOTES_READ: {
		bucket: 'user:notes:read',
		config: {limit: 60, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_NOTES_WRITE: {
		bucket: 'user:notes:write',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_MENTIONS_READ: {
		bucket: 'user:mentions:read',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_MENTIONS_DELETE: {
		bucket: 'user:mentions:delete',
		config: {limit: 60, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_SAVED_MESSAGES_READ: {
		bucket: 'user:saved_messages:read',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_SAVED_MESSAGES_WRITE: {
		bucket: 'user:saved_messages:write',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_MFA_TOTP_ENABLE: {
		bucket: 'user:mfa:totp:enable',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_MFA_TOTP_DISABLE: {
		bucket: 'user:mfa:totp:disable',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_AUTHORIZED_IPS_FORGET: {
		bucket: 'user:authorized_ips:forget',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_MFA_BACKUP_CODES: {
		bucket: 'user:mfa:backup_codes',
		config: {limit: 6, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_PUSH_SUBSCRIBE: {
		bucket: 'user:push:subscribe',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_PUSH_UNSUBSCRIBE: {
		bucket: 'user:push:unsubscribe',
		config: {limit: 40, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_PUSH_LIST: {
		bucket: 'user:push:list',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_HARVEST_LATEST: {
		bucket: 'user:harvest:latest',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_HARVEST_STATUS: {
		bucket: 'user:harvest:status',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	USER_HARVEST_DOWNLOAD: {
		bucket: 'user:harvest:download',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_CANARY_TESTER_JOIN: {
		bucket: 'user:canary_tester:join',
		config: {limit: 3, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	USER_ENTRANCE_SOUND_LIST: {
		bucket: 'user:entrance_sound:list',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	USER_ENTRANCE_SOUND_UPLOAD: {
		bucket: 'user:entrance_sound:upload',
		config: {limit: 5, windowMs: ms('5 minutes')},
	} as RouteRateLimitConfig,
	USER_ENTRANCE_SOUND_MUTATE: {
		bucket: 'user:entrance_sound:mutate',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
} as const;

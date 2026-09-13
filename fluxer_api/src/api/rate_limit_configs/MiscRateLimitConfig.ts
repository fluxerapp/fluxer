// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const MiscRateLimitConfigs = {
	INSTANCE_INFO: {
		bucket: 'instance:info',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	IP_GEO_LOOKUP: {
		bucket: 'ip:geo_lookup',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	DOWNLOAD_DESKTOP_LATEST: {
		bucket: 'download:desktop:latest',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	DOWNLOAD_DESKTOP_METADATA: {
		bucket: 'download:desktop:metadata',
		config: {limit: 120, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	DOWNLOAD_ARTIFACT: {
		bucket: 'download:artifact',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	DOWNLOAD_PACKAGE: {
		bucket: 'download:package',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	DOWNLOAD_STORE_REDIRECT: {
		bucket: 'download:store:redirect',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	DOWNLOAD_MODULE: {
		bucket: 'download:module',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	READ_STATE_ACK_BULK: {
		bucket: 'read_state:ack_bulk',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	REPORT_CREATE: {
		bucket: 'report:create',
		config: {limit: 10, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	DSA_REPORT_EMAIL_SEND: {
		bucket: 'dsa:report:email:send',
		config: {limit: 5, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	DSA_REPORT_EMAIL_VERIFY: {
		bucket: 'dsa:report:email:verify',
		config: {limit: 10, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	DSA_REPORT_CREATE: {
		bucket: 'dsa:report:create',
		config: {limit: 5, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	REPORT_LIST: {
		bucket: 'report:list',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	FAVORITE_MEME_LIST: {
		bucket: 'favorite_meme:list',
		config: {limit: 60, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	FAVORITE_MEME_GET: {
		bucket: 'favorite_meme:get',
		config: {limit: 100, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	FAVORITE_MEME_CREATE_FROM_MESSAGE: {
		bucket: 'favorite_meme:create:message',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	FAVORITE_MEME_CREATE_FROM_URL: {
		bucket: 'favorite_meme:create:url',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	FAVORITE_MEME_UPDATE: {
		bucket: 'favorite_meme:update',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	FAVORITE_MEME_DELETE: {
		bucket: 'favorite_meme:delete',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	FAVORITE_GIF_RESOLVE: {
		bucket: 'favorite_gif:resolve',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	UNFURL_DEBUG: {
		bucket: 'unfurl:debug',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	GATEWAY_BOT_INFO: {
		bucket: 'gateway:bot_info',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	THEME_SHARE_CREATE: {
		bucket: 'theme:share:create',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	SEARCH_MESSAGES: {
		bucket: 'search:messages',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	DEFAULT: {
		bucket: 'default',
		config: {limit: 60, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
} as const;

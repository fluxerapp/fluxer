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

export const MiscRateLimitConfigs = {
	INSTANCE_INFO: {
		bucket: 'instance:info',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	DOWNLOAD_DESKTOP_LATEST: {
		bucket: 'download:desktop:latest',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	DOWNLOAD_DESKTOP_METADATA: {
		bucket: 'download:desktop:metadata',
		config: {limit: 120, windowMs: 60000},
	} as RouteRateLimitConfig,

	DOWNLOAD_ARTIFACT: {
		bucket: 'download:artifact',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	DOWNLOAD_PACKAGE: {
		bucket: 'download:package',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	DOWNLOAD_STORE_REDIRECT: {
		bucket: 'download:store:redirect',
		config: {limit: 30, windowMs: 60000},
	} as RouteRateLimitConfig,

	DOWNLOAD_MODULE: {
		bucket: 'download:module',
		config: {limit: 30, windowMs: 60000},
	} as RouteRateLimitConfig,

	READ_STATE_ACK_BULK: {
		bucket: 'read_state:ack_bulk',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	REPORT_CREATE: {
		bucket: 'report:create',
		config: {limit: 10, windowMs: 3600000},
	} as RouteRateLimitConfig,

	DSA_REPORT_EMAIL_SEND: {
		bucket: 'dsa:report:email:send',
		config: {limit: 5, windowMs: 3600000},
	} as RouteRateLimitConfig,

	DSA_REPORT_EMAIL_VERIFY: {
		bucket: 'dsa:report:email:verify',
		config: {limit: 10, windowMs: 3600000},
	} as RouteRateLimitConfig,

	DSA_REPORT_CREATE: {
		bucket: 'dsa:report:create',
		config: {limit: 5, windowMs: 3600000},
	} as RouteRateLimitConfig,

	REPORT_LIST: {
		bucket: 'report:list',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	FAVORITE_MEME_LIST: {
		bucket: 'favorite_meme:list',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	FAVORITE_MEME_GET: {
		bucket: 'favorite_meme:get',
		config: {limit: 100, windowMs: 10000},
	} as RouteRateLimitConfig,

	FAVORITE_MEME_CREATE_FROM_MESSAGE: {
		bucket: 'favorite_meme:create:message',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	FAVORITE_MEME_CREATE_FROM_URL: {
		bucket: 'favorite_meme:create:url',
		config: {limit: 20, windowMs: 60000},
	} as RouteRateLimitConfig,

	FAVORITE_MEME_UPDATE: {
		bucket: 'favorite_meme:update',
		config: {limit: 30, windowMs: 10000},
	} as RouteRateLimitConfig,

	FAVORITE_MEME_DELETE: {
		bucket: 'favorite_meme:delete',
		config: {limit: 30, windowMs: 10000},
	} as RouteRateLimitConfig,

	GATEWAY_BOT_INFO: {
		bucket: 'gateway:bot_info',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	THEME_SHARE_CREATE: {
		bucket: 'theme:share:create',
		config: {limit: 20, windowMs: 60000},
	} as RouteRateLimitConfig,

	SEARCH_MESSAGES: {
		bucket: 'search:messages',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	DEFAULT: {
		bucket: 'default',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,
} as const;

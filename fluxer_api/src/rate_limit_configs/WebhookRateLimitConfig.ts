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

export const WebhookRateLimitConfigs = {
	WEBHOOK_LIST_GUILD: {
		bucket: 'webhook:list::guild_id',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	WEBHOOK_LIST_CHANNEL: {
		bucket: 'webhook:list::channel_id',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	WEBHOOK_CREATE: {
		bucket: 'webhook:create::channel_id',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	WEBHOOK_GET: {
		bucket: 'webhook:read::webhook_id',
		config: {limit: 100, windowMs: 10000},
	} as RouteRateLimitConfig,

	WEBHOOK_UPDATE: {
		bucket: 'webhook:update::webhook_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	WEBHOOK_DELETE: {
		bucket: 'webhook:delete::webhook_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	WEBHOOK_EXECUTE: {
		bucket: 'webhook:execute::webhook_id',
		config: {limit: 60, windowMs: 60000, exemptFromGlobal: true},
	} as RouteRateLimitConfig,

	WEBHOOK_GITHUB: {
		bucket: 'webhook:github::webhook_id',
		config: {limit: 200, windowMs: 60000, exemptFromGlobal: true},
	} as RouteRateLimitConfig,
} as const;

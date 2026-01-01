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

export const InviteRateLimitConfigs = {
	INVITE_GET: {
		bucket: 'invite:read::invite_code',
		config: {limit: 100, windowMs: 10000},
	} as RouteRateLimitConfig,

	INVITE_ACCEPT: {
		bucket: 'invite:accept',
		config: {limit: 10, windowMs: 10000},
	} as RouteRateLimitConfig,

	INVITE_CREATE: {
		bucket: 'invite:create::channel_id',
		config: {limit: 20, windowMs: 60000},
	} as RouteRateLimitConfig,

	INVITE_DELETE: {
		bucket: 'invite:delete::invite_code',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	INVITE_LIST_CHANNEL: {
		bucket: 'invite:list::channel_id',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	INVITE_LIST_GUILD: {
		bucket: 'invite:list::guild_id',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,
} as const;

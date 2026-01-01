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

export const GuildRateLimitConfigs = {
	GUILD_CREATE: {
		bucket: 'guild:create',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	GUILD_LIST: {
		bucket: 'guild:list',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_GET: {
		bucket: 'guild:read::guild_id',
		config: {limit: 100, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_UPDATE: {
		bucket: 'guild:update::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_DELETE: {
		bucket: 'guild:delete::guild_id',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	GUILD_LEAVE: {
		bucket: 'guild:leave::guild_id',
		config: {limit: 10, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_VANITY_URL_GET: {
		bucket: 'guild:vanity_url:get::guild_id',
		config: {limit: 100, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_VANITY_URL_PATCH: {
		bucket: 'guild:vanity_url:patch::guild_id',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	GUILD_MEMBERS: {
		bucket: 'guild:members::guild_id',
		config: {limit: 40, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_MEMBER_UPDATE: {
		bucket: 'guild:member:update::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_MEMBER_REMOVE: {
		bucket: 'guild:member:remove::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_MEMBER_ROLE_ADD: {
		bucket: 'guild:member:role:add::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_MEMBER_ROLE_REMOVE: {
		bucket: 'guild:member:role:remove::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_CHANNELS_LIST: {
		bucket: 'guild:channels:list::guild_id',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_CHANNEL_CREATE: {
		bucket: 'guild:channel:create::guild_id',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	GUILD_CHANNEL_POSITIONS: {
		bucket: 'guild:channel:positions::guild_id',
		config: {limit: 30, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_SEARCH: {
		bucket: 'guild:search::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_AUDIT_LOGS: {
		bucket: 'guild:audit_logs::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_ROLE_CREATE: {
		bucket: 'guild:role:create::guild_id',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	GUILD_ROLE_UPDATE: {
		bucket: 'guild:role:update::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_ROLE_DELETE: {
		bucket: 'guild:role:delete::guild_id',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	GUILD_ROLE_POSITIONS: {
		bucket: 'guild:role:positions::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_ROLE_LIST: {
		bucket: 'guild:role:list::guild_id',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_ROLE_HOIST_POSITIONS: {
		bucket: 'guild:role:hoist_positions::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_ROLE_HOIST_POSITIONS_RESET: {
		bucket: 'guild:role:hoist_positions_reset::guild_id',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	GUILD_EMOJIS_LIST: {
		bucket: 'guild:emojis:list::guild_id',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_EMOJI_CREATE: {
		bucket: 'guild:emoji:create::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_EMOJI_BULK_CREATE: {
		bucket: 'guild:emoji:bulk_create::guild_id',
		config: {limit: 6, windowMs: 60000},
	} as RouteRateLimitConfig,

	GUILD_EMOJI_UPDATE: {
		bucket: 'guild:emoji:update::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_EMOJI_DELETE: {
		bucket: 'guild:emoji:delete::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_STICKERS_LIST: {
		bucket: 'guild:sticker:list::guild_id',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_STICKER_CREATE: {
		bucket: 'guild:sticker:create::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_STICKER_BULK_CREATE: {
		bucket: 'guild:sticker:bulk_create::guild_id',
		config: {limit: 6, windowMs: 60000},
	} as RouteRateLimitConfig,

	GUILD_STICKER_UPDATE: {
		bucket: 'guild:sticker:update::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	GUILD_STICKER_DELETE: {
		bucket: 'guild:sticker:delete::guild_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,
} as const;

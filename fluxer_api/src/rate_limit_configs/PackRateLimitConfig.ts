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

export const PackRateLimitConfigs = {
	PACKS_LIST: {
		bucket: 'packs:list',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_CREATE: {
		bucket: 'packs:create',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_UPDATE: {
		bucket: 'packs:update::pack_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_DELETE: {
		bucket: 'packs:delete::pack_id',
		config: {limit: 10, windowMs: 60000},
	} as RouteRateLimitConfig,

	PACKS_INSTALL: {
		bucket: 'packs:install::pack_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_INVITES_LIST: {
		bucket: 'packs:invite:list::pack_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_INVITES_CREATE: {
		bucket: 'packs:invite:create::pack_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_EMOJIS_LIST: {
		bucket: 'packs:emoji:list::pack_id',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_EMOJI_CREATE: {
		bucket: 'packs:emoji:create::pack_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_EMOJI_BULK_CREATE: {
		bucket: 'packs:emoji:bulk_create::pack_id',
		config: {limit: 6, windowMs: 60000},
	} as RouteRateLimitConfig,

	PACKS_EMOJI_UPDATE: {
		bucket: 'packs:emoji:update::pack_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_EMOJI_DELETE: {
		bucket: 'packs:emoji:delete::pack_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_STICKERS_LIST: {
		bucket: 'packs:sticker:list::pack_id',
		config: {limit: 60, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_STICKER_CREATE: {
		bucket: 'packs:sticker:create::pack_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_STICKER_BULK_CREATE: {
		bucket: 'packs:sticker:bulk_create::pack_id',
		config: {limit: 6, windowMs: 60000},
	} as RouteRateLimitConfig,

	PACKS_STICKER_UPDATE: {
		bucket: 'packs:sticker:update::pack_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,

	PACKS_STICKER_DELETE: {
		bucket: 'packs:sticker:delete::pack_id',
		config: {limit: 20, windowMs: 10000},
	} as RouteRateLimitConfig,
} as const;

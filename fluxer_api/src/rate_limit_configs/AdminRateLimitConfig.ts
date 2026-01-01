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

export const AdminRateLimitConfigs = {
	ADMIN_LOOKUP: {
		bucket: 'admin:lookup',
		config: {limit: 200, windowMs: 60000},
	} as RouteRateLimitConfig,

	ADMIN_USER_MODIFY: {
		bucket: 'admin:user:modify',
		config: {limit: 100, windowMs: 60000},
	} as RouteRateLimitConfig,

	ADMIN_GUILD_MODIFY: {
		bucket: 'admin:guild:modify',
		config: {limit: 100, windowMs: 60000},
	} as RouteRateLimitConfig,

	ADMIN_BAN_OPERATION: {
		bucket: 'admin:ban:operation',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	ADMIN_BULK_OPERATION: {
		bucket: 'admin:bulk:operation',
		config: {limit: 20, windowMs: 60000},
	} as RouteRateLimitConfig,

	ADMIN_GATEWAY_RELOAD: {
		bucket: 'admin:gateway:reload',
		config: {limit: 5, windowMs: 60000},
	} as RouteRateLimitConfig,

	ADMIN_MESSAGE_OPERATION: {
		bucket: 'admin:message:operation',
		config: {limit: 100, windowMs: 60000},
	} as RouteRateLimitConfig,

	ADMIN_CODE_GENERATION: {
		bucket: 'admin:code:generation',
		config: {limit: 30, windowMs: 60000},
	} as RouteRateLimitConfig,

	ADMIN_AUDIT_LOG: {
		bucket: 'admin:audit_log',
		config: {limit: 100, windowMs: 60000},
	} as RouteRateLimitConfig,
} as const;

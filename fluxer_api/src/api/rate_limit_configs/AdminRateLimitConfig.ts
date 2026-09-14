// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const AdminRateLimitConfigs = {
	ADMIN_LOOKUP: {
		bucket: 'admin:lookup',
		config: {limit: 200, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_USER_MODIFY: {
		bucket: 'admin:user:modify',
		config: {limit: 100, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_GUILD_MODIFY: {
		bucket: 'admin:guild:modify',
		config: {limit: 100, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_BAN_OPERATION: {
		bucket: 'admin:ban:operation',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_BULK_OPERATION: {
		bucket: 'admin:bulk:operation',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_GATEWAY_RELOAD: {
		bucket: 'admin:gateway:reload',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_MESSAGE_OPERATION: {
		bucket: 'admin:message:operation',
		config: {limit: 100, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_CODE_GENERATION: {
		bucket: 'admin:code:generation',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_AUDIT_LOG: {
		bucket: 'admin:audit_log',
		config: {limit: 100, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_JOBS_VIEW: {
		bucket: 'admin:jobs:view',
		config: {limit: 600, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_GENERAL: {
		bucket: 'admin:general',
		config: {limit: 200, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	ADMIN_SYSTEM_HEAP_SNAPSHOT: {
		bucket: 'admin:system:heap_snapshot',
		config: {limit: 2, windowMs: ms('5 minutes')},
	} as RouteRateLimitConfig,
} as const;

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const InviteRateLimitConfigs = {
	INVITE_GET: {
		bucket: 'invite:read::invite_code',
		config: {limit: 100, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	INVITE_ACCEPT: {
		bucket: 'invite:accept',
		config: {limit: 10, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	INVITE_CREATE: {
		bucket: 'invite:create::channel_id',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	INVITE_DELETE: {
		bucket: 'invite:delete::invite_code',
		config: {limit: 20, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	INVITE_LIST_CHANNEL: {
		bucket: 'invite:list::channel_id',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
	INVITE_LIST_GUILD: {
		bucket: 'invite:list::guild_id',
		config: {limit: 40, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
} as const;

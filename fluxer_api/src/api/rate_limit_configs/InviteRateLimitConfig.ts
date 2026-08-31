// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';
import type {RouteRateLimitConfig} from '../middleware/RateLimitMiddleware';

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
	GUILD_INVITE_BUNDLE_CREATE: {
		bucket: 'guild_invite_bundle:create',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	GUILD_INVITE_BUNDLE_ACCEPT: {
		bucket: 'guld_invite_bundle:accept',
		config: {limit: 5, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
} as const;

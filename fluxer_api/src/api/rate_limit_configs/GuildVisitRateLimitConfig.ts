// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const GuildVisitRateLimitConfigs = {
	GUILD_VISIT_START: {
		bucket: 'guild:visit:start',
		config: {limit: 10, windowMs: ms('60 seconds')},
	} as RouteRateLimitConfig,
	USER_GUILD_VISITS: {
		bucket: 'user:guild_visits',
		config: {limit: 30, windowMs: ms('10 seconds')},
	} as RouteRateLimitConfig,
} as const;

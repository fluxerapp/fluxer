// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';
import type {RouteRateLimitConfig} from '../middleware/RateLimitMiddleware';

export const ConnectionRateLimitConfigs = {
	CONNECTION_LIST: {
		bucket: 'connection:list',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	CONNECTION_CREATE: {
		bucket: 'connection:create',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	CONNECTION_UPDATE: {
		bucket: 'connection:update',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	CONNECTION_DELETE: {
		bucket: 'connection:delete',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	CONNECTION_VERIFY: {
		bucket: 'connection:verify',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	CONNECTION_VERIFY_AND_CREATE: {
		bucket: 'connection:verify_and_create',
		config: {limit: 5, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	BLUESKY_CLIENT_DOCUMENT: {
		bucket: 'connection:bluesky:client_document',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	BLUESKY_CALLBACK: {
		bucket: 'connection:bluesky:callback',
		config: {limit: 10, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
} as const;

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const OAuthRateLimitConfigs = {
	OAUTH_AUTHORIZE: {
		bucket: 'oauth:authorize',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_TOKEN: {
		bucket: 'oauth:token',
		config: {limit: 120, windowMs: ms('1 minute')},
		trustForwardedClientIp: true,
	} as RouteRateLimitConfig,
	OAUTH_INTROSPECT: {
		bucket: 'oauth:introspect',
		config: {limit: 120, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_REVOKE: {
		bucket: 'oauth:revoke',
		config: {limit: 120, windowMs: ms('1 minute')},
		trustForwardedClientIp: true,
	} as RouteRateLimitConfig,
	OAUTH_DEV_CLIENTS_LIST: {
		bucket: 'oauth_dev:clients:list',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_CLIENT_CREATE: {
		bucket: 'oauth_dev:clients:create',
		config: {limit: 10, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_CLIENT_UPDATE: {
		bucket: 'oauth_dev:clients:update::client_id',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_CLIENT_ROTATE_SECRET: {
		bucket: 'oauth_dev:clients:rotate_secret::client_id',
		config: {limit: 10, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_CLIENT_DELETE: {
		bucket: 'oauth_dev:clients:delete::client_id',
		config: {limit: 10, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
} as const;

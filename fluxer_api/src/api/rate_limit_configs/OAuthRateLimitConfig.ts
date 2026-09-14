// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {ms} from 'itty-time';

export const OAuthRateLimitConfigs = {
	OAUTH_AUTHORIZE: {
		bucket: 'oauth:authorize',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_CONSENT: {
		bucket: 'oauth:consent',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_TOKEN: {
		bucket: 'oauth:token',
		config: {limit: 120, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_INTROSPECT: {
		bucket: 'oauth:introspect',
		config: {limit: 120, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_REVOKE: {
		bucket: 'oauth:revoke',
		config: {limit: 120, windowMs: ms('1 minute')},
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
	OAUTH_DEV_TEAMS_LIST: {
		bucket: 'oauth_dev:teams:list',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_TEAM_CREATE: {
		bucket: 'oauth_dev:teams:create',
		config: {limit: 15, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_TEAM_DELETE: {
		bucket: 'oauth_dev:teams:delete::team_id',
		config: {limit: 15, windowMs: ms('1 hour')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_TEAM_MEMBERS_LIST: {
		bucket: 'oauth_dev:teams:members:list::team_id',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_TEAM_MEMBER_ADD: {
		bucket: 'oauth_dev:teams:members:add::team_id',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_TEAM_MEMBER_REMOVE: {
		bucket: 'oauth_dev:teams:members:remove::team_id::user_id',
		config: {limit: 30, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_BOT_TOKENS_LIST: {
		bucket: 'oauth_dev:bot_tokens:list::client_id',
		config: {limit: 60, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_BOT_TOKENS_CREATE: {
		bucket: 'oauth_dev:bot_tokens:create::client_id',
		config: {limit: 20, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
	OAUTH_DEV_BOT_TOKEN_REVOKE: {
		bucket: 'oauth_dev:bot_tokens:revoke',
		config: {limit: 40, windowMs: ms('1 minute')},
	} as RouteRateLimitConfig,
} as const;

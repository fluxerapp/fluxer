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

export const OAuthRateLimitConfigs = {
	OAUTH_AUTHORIZE: {
		bucket: 'oauth:authorize',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_CONSENT: {
		bucket: 'oauth:consent',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_TOKEN: {
		bucket: 'oauth:token',
		config: {limit: 120, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_INTROSPECT: {
		bucket: 'oauth:introspect',
		config: {limit: 120, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_REVOKE: {
		bucket: 'oauth:revoke',
		config: {limit: 120, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_CLIENTS_LIST: {
		bucket: 'oauth_dev:clients:list',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_CLIENT_CREATE: {
		bucket: 'oauth_dev:clients:create',
		config: {limit: 10, windowMs: 3600000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_CLIENT_UPDATE: {
		bucket: 'oauth_dev:clients:update::client_id',
		config: {limit: 30, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_CLIENT_ROTATE_SECRET: {
		bucket: 'oauth_dev:clients:rotate_secret::client_id',
		config: {limit: 10, windowMs: 3600000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_CLIENT_DELETE: {
		bucket: 'oauth_dev:clients:delete::client_id',
		config: {limit: 10, windowMs: 3600000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_TEAMS_LIST: {
		bucket: 'oauth_dev:teams:list',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_TEAM_CREATE: {
		bucket: 'oauth_dev:teams:create',
		config: {limit: 15, windowMs: 3600000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_TEAM_DELETE: {
		bucket: 'oauth_dev:teams:delete::team_id',
		config: {limit: 15, windowMs: 3600000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_TEAM_MEMBERS_LIST: {
		bucket: 'oauth_dev:teams:members:list::team_id',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_TEAM_MEMBER_ADD: {
		bucket: 'oauth_dev:teams:members:add::team_id',
		config: {limit: 30, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_TEAM_MEMBER_REMOVE: {
		bucket: 'oauth_dev:teams:members:remove::team_id::user_id',
		config: {limit: 30, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_BOT_TOKENS_LIST: {
		bucket: 'oauth_dev:bot_tokens:list::client_id',
		config: {limit: 60, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_BOT_TOKENS_CREATE: {
		bucket: 'oauth_dev:bot_tokens:create::client_id',
		config: {limit: 20, windowMs: 60000},
	} as RouteRateLimitConfig,

	OAUTH_DEV_BOT_TOKEN_REVOKE: {
		bucket: 'oauth_dev:bot_tokens:revoke',
		config: {limit: 40, windowMs: 60000},
	} as RouteRateLimitConfig,
} as const;

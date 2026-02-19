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

import type {BlueskyAuthorizeResult, BlueskyCallbackResult} from '@fluxer/api/src/bluesky/IBlueskyOAuthService';
import {Config} from '@fluxer/api/src/Config';
import {BlueskyOAuthAuthorizationFailedError} from '@fluxer/api/src/connection/errors/BlueskyOAuthAuthorizationFailedError';
import {BlueskyOAuthCallbackFailedError} from '@fluxer/api/src/connection/errors/BlueskyOAuthCallbackFailedError';
import {BlueskyOAuthNotEnabledError} from '@fluxer/api/src/connection/errors/BlueskyOAuthNotEnabledError';
import {BlueskyOAuthStateInvalidError} from '@fluxer/api/src/connection/errors/BlueskyOAuthStateInvalidError';
import {ConnectionAlreadyExistsError} from '@fluxer/api/src/connection/errors/ConnectionAlreadyExistsError';
import {Logger} from '@fluxer/api/src/Logger';
import {DefaultUserOnly, LoginRequired} from '@fluxer/api/src/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@fluxer/api/src/middleware/RateLimitMiddleware';
import {OpenAPI} from '@fluxer/api/src/middleware/ResponseTypeMiddleware';
import {ConnectionRateLimitConfigs} from '@fluxer/api/src/rate_limit_configs/ConnectionRateLimitConfig';
import type {HonoApp} from '@fluxer/api/src/types/HonoEnv';
import {Validator} from '@fluxer/api/src/Validator';
import {ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';
import {
	BlueskyAuthorizeRequest,
	BlueskyAuthorizeResponse,
} from '@fluxer/schema/src/domains/connection/BlueskyOAuthSchemas';

const BLUESKY_PROFILE_URL_RE = /^https?:\/\/bsky\.app\/profile\//i;

function normalizeBlueskyHandle(input: string): string {
	let handle = input.trim();
	handle = handle.replace(BLUESKY_PROFILE_URL_RE, '');
	handle = handle.replace(/^@/, '');
	return handle;
}

export function BlueskyOAuthController(app: HonoApp) {
	app.get('/connections/bluesky/client-metadata.json', async (ctx) => {
		const service = ctx.get('blueskyOAuthService');
		if (!service) {
			return ctx.json({error: 'Bluesky OAuth is not enabled'}, 404);
		}
		return ctx.json(service.clientMetadata);
	});

	app.get('/connections/bluesky/jwks.json', async (ctx) => {
		const service = ctx.get('blueskyOAuthService');
		if (!service) {
			return ctx.json({error: 'Bluesky OAuth is not enabled'}, 404);
		}
		return ctx.json(service.jwks);
	});

	app.post(
		'/users/@me/connections/bluesky/authorize',
		RateLimitMiddleware(ConnectionRateLimitConfigs.CONNECTION_CREATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', BlueskyAuthorizeRequest),
		OpenAPI({
			operationId: 'authorize_bluesky_connection',
			summary: 'Start Bluesky OAuth flow',
			responseSchema: BlueskyAuthorizeResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Connections'],
			description: 'Initiates the Bluesky OAuth2 authorisation flow and returns a URL to redirect the user to.',
		}),
		async (ctx) => {
			const service = ctx.get('blueskyOAuthService');
			if (!service) {
				throw new BlueskyOAuthNotEnabledError();
			}
			const {handle: rawHandle} = ctx.req.valid('json');
			const userId = ctx.get('user').id;
			const handle = normalizeBlueskyHandle(rawHandle);

			const connectionService = ctx.get('connectionService');
			const connections = await connectionService.getConnectionsForUser(userId);
			const lowerHandle = handle.toLowerCase();
			const existing = connections.find(
				(c) => c.connection_type === ConnectionTypes.BLUESKY && c.name.toLowerCase() === lowerHandle,
			);
			if (existing) {
				throw new ConnectionAlreadyExistsError();
			}

			let result: BlueskyAuthorizeResult;
			try {
				result = await service.authorize(handle, userId);
			} catch (error) {
				Logger.error({error, handle}, 'Bluesky OAuth authorize failed');
				throw new BlueskyOAuthAuthorizationFailedError();
			}
			return ctx.json({authorize_url: result.authorizeUrl});
		},
	);

	app.get('/connections/bluesky/callback', async (ctx) => {
		const appUrl = Config.endpoints.webApp;
		const callbackUrl = `${appUrl}/connection-callback`;

		const service = ctx.get('blueskyOAuthService');
		if (!service) {
			return ctx.redirect(`${callbackUrl}?status=error&reason=not_enabled`);
		}

		try {
			const params = new URLSearchParams(ctx.req.url.split('?')[1] ?? '');

			let result: BlueskyCallbackResult;
			try {
				result = await service.callback(params);
			} catch (callbackError) {
				Logger.error({error: callbackError}, 'Bluesky OAuth callback error from upstream');
				if (
					callbackError instanceof Error &&
					(callbackError.message.toLowerCase().includes('state') ||
						callbackError.message.toLowerCase().includes('expired'))
				) {
					throw new BlueskyOAuthStateInvalidError();
				}
				throw new BlueskyOAuthCallbackFailedError();
			}

			const connectionService = ctx.get('connectionService');
			await connectionService.createOrUpdateBlueskyConnection(result.userId, result.did, result.handle);

			return ctx.redirect(`${callbackUrl}?status=connected`);
		} catch (error) {
			Logger.error({error}, 'Bluesky OAuth callback failed');

			if (error instanceof BlueskyOAuthStateInvalidError) {
				return ctx.redirect(`${callbackUrl}?status=error&reason=state_invalid`);
			}
			if (error instanceof BlueskyOAuthCallbackFailedError) {
				return ctx.redirect(`${callbackUrl}?status=error&reason=callback_failed`);
			}

			return ctx.redirect(`${callbackUrl}?status=error&reason=unknown`);
		}
	});
}

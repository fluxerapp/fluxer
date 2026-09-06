// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';
import {
	BlueskyAuthorizeRequest,
	BlueskyAuthorizeResponse,
} from '@fluxer/schema/src/domains/connection/BlueskyOAuthSchemas';
import {Config} from '../Config';
import {BlueskyOAuthAuthorizationFailedError} from '../connection/errors/BlueskyOAuthAuthorizationFailedError';
import {BlueskyOAuthCallbackFailedError} from '../connection/errors/BlueskyOAuthCallbackFailedError';
import {BlueskyOAuthNotEnabledError} from '../connection/errors/BlueskyOAuthNotEnabledError';
import {BlueskyOAuthStateInvalidError} from '../connection/errors/BlueskyOAuthStateInvalidError';
import {ConnectionAlreadyExistsError} from '../connection/errors/ConnectionAlreadyExistsError';
import {Logger} from '../Logger';
import {DefaultUserOnly, LoginRequired} from '../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {ConnectionRateLimitConfigs} from '../rate_limit_configs/ConnectionRateLimitConfig';
import type {HonoApp} from '../types/HonoEnv';
import {Validator} from '../Validator';
import {DisabledBlueskyOAuthService} from './DisabledBlueskyOAuthService';
import type {BlueskyAuthorizeResult, BlueskyCallbackResult} from './IBlueskyOAuthService';

const BLUESKY_PROFILE_URL_RE = /^https?:\/\/bsky\.app\/profile\//i;

function normalizeBlueskyHandle(input: string): string {
	let handle = input.trim();
	handle = handle.replace(BLUESKY_PROFILE_URL_RE, '');
	handle = handle.replace(/^@/, '');
	return handle;
}

function isBlueskyOAuthEnabled(service: unknown): boolean {
	return service != null && !(service instanceof DisabledBlueskyOAuthService);
}

export function BlueskyOAuthController(app: HonoApp) {
	app.get(
		'/connections/bluesky/client-metadata.json',
		RateLimitMiddleware(ConnectionRateLimitConfigs.BLUESKY_CLIENT_DOCUMENT),
		async (ctx) => {
			const service = ctx.get('blueskyOAuthService');
			if (!isBlueskyOAuthEnabled(service)) {
				return ctx.json({error: 'Bluesky OAuth is not enabled'}, 404);
			}
			return ctx.json(service.clientMetadata);
		},
	);
	app.get(
		'/connections/bluesky/jwks.json',
		RateLimitMiddleware(ConnectionRateLimitConfigs.BLUESKY_CLIENT_DOCUMENT),
		async (ctx) => {
			const service = ctx.get('blueskyOAuthService');
			if (!isBlueskyOAuthEnabled(service)) {
				return ctx.json({error: 'Bluesky OAuth is not enabled'}, 404);
			}
			return ctx.json(service.jwks);
		},
	);
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
			if (!isBlueskyOAuthEnabled(service)) {
				throw new BlueskyOAuthNotEnabledError();
			}
			const resolvedServices = await ctx.get('instanceConfigRepository').getResolvedServicesConfig();
			if (!resolvedServices.bluesky_enabled) {
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
				if (error instanceof BlueskyOAuthNotEnabledError) {
					throw error;
				}
				Logger.error({error, handle}, 'Bluesky OAuth authorize failed');
				throw new BlueskyOAuthAuthorizationFailedError();
			}
			return ctx.json({authorize_url: result.authorizeUrl});
		},
	);
	app.get(
		'/connections/bluesky/callback',
		RateLimitMiddleware(ConnectionRateLimitConfigs.BLUESKY_CALLBACK),
		async (ctx) => {
			const appUrl = Config.endpoints.webApp;
			const callbackUrl = `${appUrl}/connection-callback`;
			const service = ctx.get('blueskyOAuthService');
			if (!isBlueskyOAuthEnabled(service)) {
				return ctx.redirect(`${callbackUrl}?status=error&reason=not_enabled`);
			}
			try {
				const params = new URLSearchParams(ctx.req.url.split('?')[1] ?? '');
				let result: BlueskyCallbackResult;
				try {
					result = await service.callback(params);
				} catch (callbackError) {
					if (callbackError instanceof BlueskyOAuthNotEnabledError) {
						throw callbackError;
					}
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
				if (error instanceof BlueskyOAuthNotEnabledError) {
					return ctx.redirect(`${callbackUrl}?status=error&reason=not_enabled`);
				}
				return ctx.redirect(`${callbackUrl}?status=error&reason=unknown`);
			}
		},
	);
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {BlueskyOAuthStateInvalidError, BlueskyOAuthStoreError} from '@app/api/bluesky/BlueskyOAuthStores';
import {DisabledBlueskyOAuthService} from '@app/api/bluesky/DisabledBlueskyOAuthService';
import type {BlueskyAuthorizeResult, IBlueskyOAuthService} from '@app/api/bluesky/IBlueskyOAuthService';
import {Config} from '@app/api/Config';
import {BlueskyOAuthAuthorizationFailedError} from '@app/api/connection/errors/BlueskyOAuthAuthorizationFailedError';
import {BlueskyOAuthNotEnabledError} from '@app/api/connection/errors/BlueskyOAuthNotEnabledError';
import {Logger} from '@app/api/Logger';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {ConnectionRateLimitConfigs} from '@app/api/rate_limit_configs/ConnectionRateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {
	BlueskyAuthorizeRequest,
	BlueskyAuthorizeResponse,
} from '@fluxer/schema/src/domains/connection/BlueskyOAuthSchemas';

const BLUESKY_PROFILE_URL_RE = /^https?:\/\/bsky\.app\/profile\//i;
type BlueskyCallbackPhase = 'callback' | 'connection';
type BlueskyCallbackFailureReason = 'not_enabled' | 'state_invalid' | 'callback_failed' | 'unknown';

function normalizeBlueskyHandle(input: string): string {
	return input.trim().replace(BLUESKY_PROFILE_URL_RE, '').replace(/^@/, '');
}

function isBlueskyOAuthEnabled(service: IBlueskyOAuthService): boolean {
	return !(service instanceof DisabledBlueskyOAuthService);
}

function getCallbackFailureReason(error: unknown, phase: BlueskyCallbackPhase): BlueskyCallbackFailureReason {
	if (error instanceof BlueskyOAuthNotEnabledError) return 'not_enabled';
	if (
		error instanceof BlueskyOAuthStateInvalidError ||
		(error instanceof BlueskyOAuthStoreError && error.kind === 'state')
	) {
		return 'state_invalid';
	}
	return phase === 'callback' ? 'callback_failed' : 'unknown';
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
			let result: BlueskyAuthorizeResult;
			try {
				result = await service.authorize(handle, userId);
			} catch (error) {
				if (error instanceof BlueskyOAuthNotEnabledError) {
					throw error;
				}
				Logger.error({error}, 'Bluesky OAuth authorize failed');
				throw new BlueskyOAuthAuthorizationFailedError();
			}
			return ctx.json({authorize_url: result.authorizeUrl});
		},
	);
	app.get(
		'/connections/bluesky/callback',
		RateLimitMiddleware(ConnectionRateLimitConfigs.BLUESKY_CALLBACK),
		async (ctx) => {
			const callbackUrl = `${Config.endpoints.webApp}/connection-callback`;
			const service = ctx.get('blueskyOAuthService');
			if (!isBlueskyOAuthEnabled(service)) {
				return ctx.redirect(`${callbackUrl}?status=error&reason=not_enabled`);
			}
			let phase: BlueskyCallbackPhase = 'callback';
			try {
				const params = new URL(ctx.req.url).searchParams;
				const result = await service.callback(params);
				phase = 'connection';
				const connectionService = ctx.get('connectionService');
				await connectionService.createOrUpdateBlueskyConnection(result);
				return ctx.redirect(`${callbackUrl}?status=connected`);
			} catch (error) {
				const reason = getCallbackFailureReason(error, phase);
				Logger.error(
					{phase, reason, storeError: error instanceof BlueskyOAuthStoreError ? error.message : undefined},
					'Bluesky OAuth callback failed',
				);
				return ctx.redirect(`${callbackUrl}?status=error&reason=${reason}`);
			}
		},
	);
}

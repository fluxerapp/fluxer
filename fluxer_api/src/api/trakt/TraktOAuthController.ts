// SPDX-License-Identifier: AGPL-3.0-or-later

import {TraktAuthorizeResponse} from '@fluxer/schema/src/domains/connection/TraktOAuthSchemas';
import {Config} from '../Config';
import {ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';
import {ConnectionAlreadyExistsError} from '../connection/errors/ConnectionAlreadyExistsError';
import {TraktOAuthAuthorizationFailedError} from '../connection/errors/TraktOAuthAuthorizationFailedError';
import {TraktOAuthCallbackFailedError} from '../connection/errors/TraktOAuthCallbackFailedError';
import {TraktOAuthNotEnabledError} from '../connection/errors/TraktOAuthNotEnabledError';
import {TraktOAuthStateInvalidError} from '../connection/errors/TraktOAuthStateInvalidError';
import {Logger} from '../Logger';
import {DefaultUserOnly, LoginRequired} from '../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {ConnectionRateLimitConfigs} from '../rate_limit_configs/ConnectionRateLimitConfig';
import type {HonoApp} from '../types/HonoEnv';
import {DisabledTraktOAuthService} from './DisabledTraktOAuthService';
import type {TraktAuthorizeResult, TraktCallbackResult} from './ITraktOAuthService';

function isTraktOAuthEnabled(service: unknown): boolean {
	return service != null && !(service instanceof DisabledTraktOAuthService);
}

export function TraktOAuthController(app: HonoApp) {
	app.post(
		'/users/@me/connections/trakt/authorize',
		RateLimitMiddleware(ConnectionRateLimitConfigs.CONNECTION_CREATE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'authorize_trakt_connection',
			summary: 'Start Trakt OAuth flow',
			responseSchema: TraktAuthorizeResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Connections'],
			description: 'Initiates the Trakt OAuth2 authorisation flow and returns a URL to redirect the user to.',
		}),
		async (ctx) => {
			const service = ctx.get('traktOAuthService');
			if (!isTraktOAuthEnabled(service)) {
				throw new TraktOAuthNotEnabledError();
			}
			const resolvedServices = await ctx.get('instanceConfigRepository').getResolvedServicesConfig();
			if (!resolvedServices.trakt_enabled) {
				throw new TraktOAuthNotEnabledError();
			}
			const userId = ctx.get('user').id;
			const connectionService = ctx.get('connectionService');
			const connections = await connectionService.getConnectionsForUser(userId);
			const existing = connections.find((c) => c.connection_type === ConnectionTypes.TRAKT);
			if (existing) {
				throw new ConnectionAlreadyExistsError();
			}
			let result: TraktAuthorizeResult;
			try {
				result = await service.authorize(userId);
			} catch (error) {
				if (error instanceof TraktOAuthNotEnabledError) {
					throw error;
				}
				Logger.error({error}, 'Trakt OAuth authorize failed');
				throw new TraktOAuthAuthorizationFailedError();
			}
			return ctx.json({authorize_url: result.authorizeUrl});
		},
	);
	app.get('/connections/trakt/callback', async (ctx) => {
		const appUrl = Config.endpoints.webApp;
		const callbackUrl = `${appUrl}/connection-callback`;
		const service = ctx.get('traktOAuthService');
		if (!isTraktOAuthEnabled(service)) {
			return ctx.redirect(`${callbackUrl}?status=error&reason=not_enabled`);
		}
		try {
			const params = new URLSearchParams(ctx.req.url.split('?')[1] ?? '');
			let result: TraktCallbackResult;
			try {
				result = await service.callback(params);
			} catch (callbackError) {
				if (callbackError instanceof TraktOAuthNotEnabledError) {
					throw callbackError;
				}
				Logger.error({error: callbackError}, 'Trakt OAuth callback error from upstream');
				if (
					callbackError instanceof Error &&
					(callbackError.message.toLowerCase().includes('state') ||
						callbackError.message.toLowerCase().includes('expired'))
				) {
					throw new TraktOAuthStateInvalidError();
				}
				throw new TraktOAuthCallbackFailedError();
			}
			const connectionService = ctx.get('connectionService');
			await connectionService.createOrUpdateTraktConnection(result.userId, result.traktUserId, result.username);
			return ctx.redirect(`${callbackUrl}?status=connected`);
		} catch (error) {
			Logger.error({error}, 'Trakt OAuth callback failed');
			if (error instanceof TraktOAuthStateInvalidError) {
				return ctx.redirect(`${callbackUrl}?status=error&reason=state_invalid`);
			}
			if (error instanceof TraktOAuthCallbackFailedError) {
				return ctx.redirect(`${callbackUrl}?status=error&reason=callback_failed`);
			}
			if (error instanceof TraktOAuthNotEnabledError) {
				return ctx.redirect(`${callbackUrl}?status=error&reason=not_enabled`);
			}
			return ctx.redirect(`${callbackUrl}?status=error&reason=unknown`);
		}
	});
}

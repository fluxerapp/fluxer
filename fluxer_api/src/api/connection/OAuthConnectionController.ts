// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	OAuthConnectionCallbackError,
	type OAuthConnectionCallbackReason,
} from '@app/api/connection/OAuthConnectionService';
import {Logger} from '@app/api/Logger';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {ConnectionRateLimitConfigs} from '@app/api/rate_limit_configs/ConnectionRateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {assertAccountNotLimited} from '@app/api/user/AccountLimit';
import {Validator} from '@app/api/Validator';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import {
	OAuthConnectionAuthorizeResponse,
	OAuthConnectionProviderParam,
} from '@fluxer/schema/src/domains/connection/OAuthConnectionSchemas';

function callbackRedirect(reason: OAuthConnectionCallbackReason | null): string {
	const base = `${Config.endpoints.webApp}/connection-callback`;
	return reason === null ? `${base}?status=connected` : `${base}?status=error&reason=${reason}`;
}

export function OAuthConnectionController(app: HonoApp) {
	app.post(
		'/users/@me/connections/oauth/:provider_id/authorize',
		RateLimitMiddleware(ConnectionRateLimitConfigs.OAUTH_AUTHORIZE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', OAuthConnectionProviderParam),
		OpenAPI({
			operationId: 'authorize_oauth_connection',
			summary: 'Start a provider connection',
			responseSchema: OAuthConnectionAuthorizeResponse,
			statusCode: 200,
			security: ['sessionToken'],
			tags: ['Connections'],
			description:
				'Starts connecting an account at one of the OAuth or OpenID Connect providers listed in the instance discovery document under services.connection_providers. Open authorize_url in a browser. The provider sends the browser back to the API, which adds the connection and redirects to the web app.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const authSession = ctx.get('authSession');
			if (!authSession) {
				throw new UnauthorizedError();
			}
			assertAccountNotLimited(user);
			const authorizeUrl = await ctx
				.get('oauthConnectionService')
				.authorize(user.id, authSession.sessionIdHash, ctx.req.valid('param').provider_id);
			return ctx.json({authorize_url: authorizeUrl});
		},
	);
	app.get(
		'/connections/oauth/:provider_id/callback',
		RateLimitMiddleware(ConnectionRateLimitConfigs.OAUTH_CALLBACK),
		async (ctx) => {
			const providerId = ctx.req.param('provider_id');
			try {
				await ctx.get('oauthConnectionService').callback(providerId, new URL(ctx.req.url).searchParams);
				return ctx.redirect(callbackRedirect(null));
			} catch (error) {
				if (error instanceof OAuthConnectionCallbackError) {
					return ctx.redirect(callbackRedirect(error.reason));
				}
				Logger.error({error, provider_id: providerId}, 'Connection provider callback failed');
				return ctx.redirect(callbackRedirect('unknown'));
			}
		},
	);
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {SudoVerificationSchema} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {
	ApplicationAuthorizationIdParam,
	ApplicationIdParam,
} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	ApplicationPublicResponse,
	ApplicationResponse,
	ApplicationsMeResponse,
	AuthorizeConsentRequest,
	AuthorizeRequest,
	BotTokenResetResponse,
	IntrospectRequestForm,
	OAuth2AuthorizationsBulkRevokeRequest,
	OAuth2AuthorizationsListResponse,
	OAuth2ConsentResponse,
	OAuth2IntrospectResponse,
	OAuth2MeResponse,
	OAuth2TokenResponse,
	OAuth2UserInfoResponse,
	RevokeRequestForm,
	TokenRequest,
} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';
import type {z} from 'zod';
import {Config} from '../Config';
import {DefaultUserOnly, LoginRequiredAllowSuspicious} from '../middleware/AuthMiddleware';
import {requireOAuth2BearerToken, requireOAuth2Scope} from '../middleware/OAuth2ScopeMiddleware';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {SudoModeMiddleware} from '../middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoApp} from '../types/HonoEnv';
import {Validator} from '../Validator';

export function OAuth2Controller(app: HonoApp) {
	app.get(
		'/oauth2/authorize',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_AUTHORIZE),
		Validator('query', AuthorizeRequest),
		async (ctx) => {
			const q = ctx.req.valid('query');
			if (q.prompt === 'none') {
				const user = ctx.get('user');
				const errorRedirectBase = await ctx
					.get('oauth2Service')
					.resolveErrorRedirectBase(q.client_id.toString(), q.redirect_uri ?? undefined);
				const errorUrl = new URL(errorRedirectBase);
				if (q.state) errorUrl.searchParams.set('state', q.state);
				if (!user) {
					errorUrl.searchParams.set('error', 'login_required');
					return ctx.redirect(errorUrl.toString(), 302);
				}
				const requestedScopes = new Set(q.scope.split(/[\s+]+/).filter(Boolean));
				const nonBotScopes = [...requestedScopes].filter((s) => s !== 'bot');
				if (nonBotScopes.length > 0) {
					const refreshTokens = await ctx.get('oauth2TokenRepository').listRefreshTokensForUser(user.id);
					const authorizedScopes = new Set<string>();
					for (const token of refreshTokens) {
						if (token.applicationId.toString() === q.client_id.toString()) {
							for (const scope of token.scope) {
								authorizedScopes.add(scope);
							}
						}
					}
					if (!nonBotScopes.every((s) => authorizedScopes.has(s))) {
						errorUrl.searchParams.set('error', 'consent_required');
						return ctx.redirect(errorUrl.toString(), 302);
					}
				}
				try {
					const {redirectTo} = await ctx.get('oauth2Service').authorizeAndConsent({
						clientId: q.client_id.toString(),
						redirectUri: q.redirect_uri,
						scope: q.scope,
						state: q.state ?? undefined,
						codeChallenge: q.code_challenge,
						codeChallengeMethod: q.code_challenge_method as 'S256' | 'plain' | undefined,
						responseType: (q.response_type ?? 'code') as 'code',
						userId: user.id,
					});
					return ctx.redirect(redirectTo, 302);
				} catch {
					errorUrl.searchParams.set('error', 'consent_required');
					return ctx.redirect(errorUrl.toString(), 302);
				}
			}
			const consentUrl = new URL(`${Config.endpoints.webApp}/oauth2/authorize`);
			consentUrl.searchParams.set('client_id', q.client_id.toString());
			consentUrl.searchParams.set('scope', q.scope);
			if (q.response_type) consentUrl.searchParams.set('response_type', q.response_type);
			if (q.redirect_uri) consentUrl.searchParams.set('redirect_uri', q.redirect_uri);
			if (q.state) consentUrl.searchParams.set('state', q.state);
			if (q.prompt) consentUrl.searchParams.set('prompt', q.prompt);
			if (q.guild_id) consentUrl.searchParams.set('guild_id', q.guild_id.toString());
			if (q.channel_id) consentUrl.searchParams.set('channel_id', q.channel_id.toString());
			if (q.permissions) consentUrl.searchParams.set('permissions', q.permissions);
			if (q.disable_guild_select) consentUrl.searchParams.set('disable_guild_select', q.disable_guild_select);
			if (q.code_challenge) consentUrl.searchParams.set('code_challenge', q.code_challenge);
			if (q.code_challenge_method) consentUrl.searchParams.set('code_challenge_method', q.code_challenge_method);
			return ctx.redirect(consentUrl.toString(), 302);
		},
	);
	app.post(
		'/oauth2/authorize/consent',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_AUTHORIZE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('json', AuthorizeConsentRequest),
		OpenAPI({
			operationId: 'provide_oauth2_consent',
			summary: 'Grant OAuth2 consent',
			responseSchema: OAuth2ConsentResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'User grants permission for an OAuth2 application to access authorized scopes. Used in authorization code flow to complete the authorization process after user review.',
		}),
		async (ctx) => {
			const body: z.infer<typeof AuthorizeConsentRequest> = ctx.req.valid('json');
			const user = ctx.get('user');
			return ctx.json(
				await ctx.get('oauth2RequestService').authorizeConsent({
					body,
					userId: user.id,
					requestCache: ctx.get('requestCache'),
				}),
			);
		},
	);
	app.post(
		'/oauth2/token',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_TOKEN),
		Validator('form', TokenRequest),
		OpenAPI({
			operationId: 'exchange_oauth2_token',
			summary: 'Exchange OAuth2 token',
			responseSchema: OAuth2TokenResponse,
			statusCode: 200,
			security: [],
			tags: ['OAuth2'],
			description:
				'Exchanges authorization code or other grant type for access tokens. Supports authorization code, refresh token, and client credentials flows. Client authentication via authorization header or client credentials.',
		}),
		async (ctx) => {
			const form = ctx.req.valid('form');
			const result = await ctx.get('oauth2RequestService').tokenExchange({
				form,
				authorizationHeader: ctx.req.header('authorization') ?? undefined,
				logPrefix: 'OAuth2',
			});
			return ctx.json(result);
		},
	);
	app.get(
		'/oauth2/userinfo',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_INTROSPECT),
		requireOAuth2Scope('identify'),
		OpenAPI({
			operationId: 'get_oauth2_userinfo',
			summary: 'Get OAuth2 user information',
			responseSchema: OAuth2UserInfoResponse,
			statusCode: 200,
			security: ['bearerToken'],
			tags: ['OAuth2'],
			description:
				'Retrieves authenticated user information using a valid access token. Requires identify scope and supports email scope for email fields.',
		}),
		async (ctx) => {
			return ctx.json(await ctx.get('oauth2RequestService').userInfo(ctx.req.header('authorization') ?? undefined));
		},
	);
	app.post(
		'/oauth2/token/revoke',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_INTROSPECT),
		Validator('form', RevokeRequestForm),
		OpenAPI({
			operationId: 'revoke_oauth2_token',
			summary: 'Revoke OAuth2 token',
			responseSchema: null,
			statusCode: 200,
			security: [],
			tags: ['OAuth2'],
			description:
				'Revokes an access or refresh token, immediately invalidating it. Client authentication required via authorization header or client credentials. Returns 200 on success.',
		}),
		async (ctx) => {
			await ctx.get('oauth2RequestService').revoke({
				form: ctx.req.valid('form'),
				authorizationHeader: ctx.req.header('authorization') ?? undefined,
			});
			return ctx.body(null, 200);
		},
	);
	app.post(
		'/oauth2/introspect',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_INTROSPECT),
		Validator('form', IntrospectRequestForm),
		OpenAPI({
			operationId: 'introspect_oauth2_token',
			summary: 'Introspect OAuth2 token',
			responseSchema: OAuth2IntrospectResponse,
			statusCode: 200,
			security: [],
			tags: ['OAuth2'],
			description:
				'Verifies token validity and retrieves metadata. Returns active status, scope, expiration, and user information. Client authentication via authorization header or client credentials.',
		}),
		async (ctx) => {
			const result = await ctx.get('oauth2RequestService').introspect({
				form: ctx.req.valid('form'),
				authorizationHeader: ctx.req.header('authorization') ?? undefined,
			});
			return ctx.json(result);
		},
	);
	app.get(
		'/oauth2/@me',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_INTROSPECT),
		requireOAuth2BearerToken(),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'get_current_user_oauth2',
			summary: 'Get current OAuth2 user',
			responseSchema: OAuth2MeResponse,
			statusCode: 200,
			security: ['bearerToken'],
			tags: ['OAuth2'],
			description:
				'Retrieves current authorization details for a valid OAuth2 bearer token. Includes OAuth2 metadata and user details when identify is present.',
		}),
		async (ctx) => {
			const response = await ctx.get('oauth2RequestService').getMe(ctx.req.header('authorization') ?? undefined);
			return ctx.json(response);
		},
	);
	app.get(
		'/oauth2/applications/:id/public',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		Validator('param', ApplicationIdParam),
		OpenAPI({
			operationId: 'get_public_application',
			summary: 'Get public application',
			responseSchema: ApplicationPublicResponse,
			statusCode: 200,
			security: [],
			tags: ['OAuth2'],
			description:
				'Retrieves public information about an OAuth2 application without authentication. Allows clients to discover application metadata before initiating authorization.',
		}),
		async (ctx) => {
			const appId = ctx.req.valid('param').id;
			const user = ctx.get('user');
			const response = await ctx.get('oauth2RequestService').getApplicationPublic(appId, user?.id);
			return ctx.json(response);
		},
	);
	app.get(
		'/applications/@me',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		OpenAPI({
			operationId: 'get_current_user_applications',
			summary: 'List current user applications',
			responseSchema: ApplicationsMeResponse,
			statusCode: 200,
			security: [],
			tags: ['OAuth2'],
			description:
				'Lists all OAuth2 applications registered by the authenticated user. Includes application credentials and metadata. Requires valid OAuth2 access token.',
		}),
		async (ctx) => {
			const response = await ctx
				.get('oauth2RequestService')
				.getApplicationsMe(ctx.req.header('authorization') ?? undefined);
			return ctx.json(response);
		},
	);
	app.post(
		'/oauth2/applications/:id/bot/reset-token',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_ROTATE_SECRET),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('param', ApplicationIdParam),
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'reset_bot_token',
			summary: 'Reset bot token',
			responseSchema: BotTokenResetResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Rotates the bot token for an OAuth2 application. Requires sudo mode authentication. Invalidates all previously issued bot tokens. Used for security rotation and compromise mitigation.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			const applicationId = ctx.req.valid('param').id;
			const response = await ctx.get('oauth2RequestService').resetBotToken({
				ctx,
				userId: user.id,
				body,
				applicationId,
			});
			return ctx.json(response);
		},
	);
	app.post(
		'/oauth2/applications/:id/client-secret/reset',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_ROTATE_SECRET),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('param', ApplicationIdParam),
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'reset_client_secret',
			summary: 'Reset client secret',
			responseSchema: ApplicationResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Rotates the client secret for an OAuth2 application. Requires sudo mode authentication. Essential security operation for protecting client credentials. Existing access tokens remain valid.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			const applicationId = ctx.req.valid('param').id;
			const response = await ctx.get('oauth2RequestService').resetClientSecret({
				ctx,
				userId: user.id,
				body,
				applicationId,
			});
			return ctx.json(response);
		},
	);
	app.get(
		'/oauth2/@me/authorizations',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_user_oauth2_authorizations',
			summary: 'List user OAuth2 authorizations',
			responseSchema: OAuth2AuthorizationsListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Lists all third-party applications the user has authorized. Shows granted scopes and authorization metadata. Allows user to review and manage delegated access.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const authorizations = await ctx.get('oauth2RequestService').listAuthorizations(user.id);
			return ctx.json(authorizations);
		},
	);
	app.delete(
		'/oauth2/@me/authorizations/:applicationId',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_INTROSPECT),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('param', ApplicationAuthorizationIdParam),
		OpenAPI({
			operationId: 'delete_user_oauth2_authorization',
			summary: 'Revoke OAuth2 authorization',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Revokes user authorization for a third-party application. Immediately invalidates all tokens issued to that application. User regains control of delegated access.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const applicationId = ctx.req.valid('param').applicationId;
			await ctx.get('oauth2RequestService').deleteAuthorization(user.id, applicationId);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/oauth2/@me/authorizations/revoke',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_INTROSPECT),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('json', OAuth2AuthorizationsBulkRevokeRequest),
		OpenAPI({
			operationId: 'bulk_delete_user_oauth2_authorizations',
			summary: 'Bulk revoke OAuth2 authorizations',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Revokes user authorizations for multiple third-party applications. Immediately invalidates all tokens issued to those applications.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await ctx.get('oauth2RequestService').deleteAuthorizations(user.id, body.application_ids);
			return ctx.body(null, 204);
		},
	);
}

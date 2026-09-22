// SPDX-License-Identifier: AGPL-3.0-or-later

import {DefaultUserOnly, LoginRequiredAllowSuspicious} from '@app/api/middleware/AuthMiddleware';
import {CaptchaMiddleware} from '@app/api/middleware/CaptchaMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {SudoModeMiddleware} from '@app/api/middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {SudoVerificationSchema} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {ApplicationIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	ApplicationCreateRequest,
	ApplicationListResponse,
	ApplicationResponse,
	ApplicationUpdateRequest,
	BotProfileResponse,
	BotProfileUpdateRequest,
	OAuth2ApplicationsMeResponse,
} from '@fluxer/schema/src/domains/oauth/OAuthSchemas';
import type {Context} from 'hono';

export function OAuth2ApplicationsController(app: HonoApp) {
	const listApplicationsHandler = async (ctx: Context<HonoEnv>) => {
		const userId = ctx.get('user').id;
		const response = await ctx.get('oauth2ApplicationsRequestService').listApplications(userId);
		return ctx.json(response);
	};
	const applicationsMeHandler = async (ctx: Context<HonoEnv>) => {
		if (ctx.get('authTokenType') === 'bot') {
			const response = await ctx
				.get('oauth2RequestService')
				.getApplicationsMe(ctx.req.header('authorization') ?? undefined);
			return ctx.json(response);
		}
		return listApplicationsHandler(ctx);
	};
	app.get(
		'/users/@me/applications',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_user_applications',
			summary: 'List user applications',
			responseSchema: ApplicationListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Lists all OAuth2 applications owned by the authenticated user. Includes application credentials, metadata, and configuration.',
		}),
		listApplicationsHandler,
	);
	app.get(
		'/oauth2/applications/@me',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		LoginRequiredAllowSuspicious,
		OpenAPI({
			operationId: 'get_oauth_applications_me',
			summary: 'Get current applications endpoint',
			responseSchema: OAuth2ApplicationsMeResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'For user tokens, lists all OAuth2 applications owned by the authenticated user. For bot tokens, returns the bot application object.',
		}),
		applicationsMeHandler,
	);
	app.post(
		'/oauth2/applications',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_CREATE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		CaptchaMiddleware,
		Validator('json', ApplicationCreateRequest),
		OpenAPI({
			operationId: 'create_oauth_application',
			summary: 'Create OAuth2 application',
			responseSchema: ApplicationResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Creates a new bot-backed OAuth2 application (client). Requires CAPTCHA verification. Returns client credentials including ID and secret. Application can be used for authorization flows and API access.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const body = ctx.req.valid('json');
			const response = await ctx.get('oauth2ApplicationsRequestService').createApplication(userId, body);
			return ctx.json(response);
		},
	);
	app.get(
		'/oauth2/applications/:id',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('param', ApplicationIdParam),
		OpenAPI({
			operationId: 'get_oauth_application',
			summary: 'Get application',
			responseSchema: ApplicationResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Retrieves details of a specific OAuth2 application owned by the user. Returns full application configuration and credentials.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const response = await ctx
				.get('oauth2ApplicationsRequestService')
				.getApplication(userId, ctx.req.valid('param').id);
			return ctx.json(response);
		},
	);
	app.patch(
		'/oauth2/applications/:id',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_UPDATE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('param', ApplicationIdParam),
		Validator('json', ApplicationUpdateRequest),
		OpenAPI({
			operationId: 'update_oauth_application',
			summary: 'Update application',
			responseSchema: ApplicationResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Modifies OAuth2 application configuration such as name, description, and redirect URIs. Does not rotate credentials.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const body = ctx.req.valid('json');
			const response = await ctx
				.get('oauth2ApplicationsRequestService')
				.updateApplication(userId, ctx.req.valid('param').id, body);
			return ctx.json(response);
		},
	);
	app.delete(
		'/oauth2/applications/:id',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_DELETE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('param', ApplicationIdParam),
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'delete_oauth_application',
			summary: 'Delete application',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Permanently deletes an OAuth2 application. Requires sudo mode authentication. Invalidates all issued tokens and revokes all user authorizations.',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await ctx.get('oauth2ApplicationsRequestService').deleteApplication({
				ctx,
				userId: user.id,
				body,
				applicationId: ctx.req.valid('param').id,
			});
			return ctx.body(null, 204);
		},
	);
	app.patch(
		'/oauth2/applications/:id/bot',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_UPDATE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator('param', ApplicationIdParam),
		Validator('json', BotProfileUpdateRequest),
		OpenAPI({
			operationId: 'update_bot_profile',
			summary: 'Update bot profile',
			responseSchema: BotProfileResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['OAuth2'],
			description:
				'Modifies bot profile information such as name, avatar, and status. Changes apply to the bot account associated with this OAuth2 application.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const body = ctx.req.valid('json');
			const response = await ctx
				.get('oauth2ApplicationsRequestService')
				.updateBotProfile(userId, ctx.req.valid('param').id, body);
			return ctx.json(response);
		},
	);
}

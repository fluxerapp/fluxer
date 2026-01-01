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

import type {Context} from 'hono';
import type {HonoApp, HonoEnv} from '~/App';
import {requireSudoMode} from '~/auth/services/SudoVerificationService';
import {createApplicationID} from '~/BrandedTypes';
import {AVATAR_MAX_SIZE} from '~/Constants';
import {AccessDeniedError, BotUserNotFoundError, InvalidClientError, UnknownApplicationError} from '~/Errors';
import {UsernameNotAvailableError} from '~/infrastructure/DiscriminatorService';
import {DefaultUserOnly, LoginRequiredAllowSuspicious} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {SudoModeMiddleware} from '~/middleware/SudoModeMiddleware';
import type {Application} from '~/models/Application';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {
	createBase64StringType,
	createStringType,
	DiscriminatorType,
	SudoVerificationSchema,
	UsernameType,
	z,
} from '~/Schema';
import {Validator} from '~/Validator';
import {ApplicationNotOwnedError} from './ApplicationService';
import {mapApplicationToResponse, mapBotProfileToResponse, mapBotTokenResetResponse} from './OAuth2Mappers';
import {OAuth2RedirectURICreateType, OAuth2RedirectURIUpdateType} from './OAuth2RedirectURI';

export const OAuth2ApplicationsController = (app: HonoApp) => {
	const listApplicationsHandler = async (ctx: Context<HonoEnv>) => {
		const userId = ctx.get('user').id;
		const applications: Array<Application> = await ctx.get('applicationService').listApplicationsByOwner(userId);

		const botUserIds = applications
			.filter((app: Application) => app.hasBotUser())
			.map((app: Application) => app.getBotUserId()!);
		const botUsers = await Promise.all(botUserIds.map((botUserId) => ctx.get('userRepository').findUnique(botUserId)));
		const botUserMap = new Map(botUsers.filter((u) => u !== null).map((u) => [u!.id, u!]));

		return ctx.json(
			applications.map((app: Application) => {
				const botUser = app.hasBotUser() && app.getBotUserId() ? botUserMap.get(app.getBotUserId()!) : null;
				return mapApplicationToResponse(app, {botUser: botUser ?? undefined});
			}),
		);
	};

	app.get(
		'/users/@me/applications',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		listApplicationsHandler,
	);

	app.get(
		'/oauth2/applications/@me',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		listApplicationsHandler,
	);

	app.post(
		'/oauth2/applications',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_CREATE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator(
			'json',
			z.object({
				name: createStringType(1, 100),
				redirect_uris: z
					.array(OAuth2RedirectURICreateType)
					.max(10, 'Maximum of 10 redirect URIs allowed')
					.optional()
					.nullable()
					.transform((value) => value ?? []),
				bot_public: z.boolean().optional(),
			}),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const body = ctx.req.valid('json');

			const result = await ctx.get('applicationService').createApplication({
				ownerUserId: userId,
				name: body.name,
				redirectUris: body.redirect_uris,
				botPublic: body.bot_public,
			});

			return ctx.json(
				mapApplicationToResponse(result.application, {
					botUser: result.botUser,
					botToken: result.botToken,
					clientSecret: result.clientSecret,
				}),
			);
		},
	);

	app.get(
		'/oauth2/applications/:id',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENTS_LIST),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		async (ctx) => {
			const userId = ctx.get('user').id;
			const applicationId = createApplicationID(BigInt(ctx.req.param('id')));

			const application = await ctx.get('applicationRepository').getApplication(applicationId);
			if (!application) {
				throw new UnknownApplicationError();
			}

			if (application.ownerUserId !== userId) {
				throw new AccessDeniedError();
			}

			let botUser = null;
			if (application.hasBotUser()) {
				const botUserId = application.getBotUserId();
				if (botUserId) {
					botUser = await ctx.get('userRepository').findUnique(botUserId);
				}
			}

			return ctx.json(mapApplicationToResponse(application, {botUser}));
		},
	);

	app.patch(
		'/oauth2/applications/:id',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_UPDATE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator(
			'json',
			z.object({
				name: createStringType(1, 100).optional(),
				redirect_uris: z
					.array(OAuth2RedirectURIUpdateType)
					.max(10, 'Maximum of 10 redirect URIs allowed')
					.optional()
					.nullable()
					.transform((value) => (value === undefined ? undefined : (value ?? []))),
				bot_public: z.boolean().optional(),
			}),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const applicationId = createApplicationID(BigInt(ctx.req.param('id')));
			const body = ctx.req.valid('json');

			try {
				const updated = await ctx.get('applicationService').updateApplication({
					userId,
					applicationId,
					name: body.name,
					redirectUris: body.redirect_uris,
					botPublic: body.bot_public,
				});

				let botUser = null;
				if (updated.hasBotUser()) {
					const botUserId = updated.getBotUserId();
					if (botUserId) {
						botUser = await ctx.get('userRepository').findUnique(botUserId);
					}
				}

				return ctx.json(mapApplicationToResponse(updated, {botUser: botUser ?? undefined}));
			} catch (err) {
				if (err instanceof ApplicationNotOwnedError) {
					throw new AccessDeniedError();
				}
				if (err instanceof UnknownApplicationError) {
					throw err;
				}
				throw err;
			}
		},
	);

	app.delete(
		'/oauth2/applications/:id',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_DELETE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));

			const applicationId = createApplicationID(BigInt(ctx.req.param('id')));
			try {
				await ctx.get('applicationService').deleteApplication(user.id, applicationId);
				return ctx.body(null, 204);
			} catch (err) {
				if (err instanceof ApplicationNotOwnedError) {
					throw new AccessDeniedError();
				}
				if (err instanceof UnknownApplicationError) {
					throw err;
				}
				throw err;
			}
		},
	);

	app.post(
		'/oauth2/applications/:id/bot/reset-token',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_ROTATE_SECRET),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));

			const applicationId = createApplicationID(BigInt(ctx.req.param('id')));

			try {
				const {token} = await ctx.get('applicationService').rotateBotToken(user.id, applicationId);

				const application = await ctx.get('applicationRepository').getApplication(applicationId);
				if (!application || !application.botUserId) {
					throw new BotUserNotFoundError();
				}

				const botUser = await ctx.get('userRepository').findUnique(application.botUserId);
				if (!botUser) {
					throw new BotUserNotFoundError();
				}

				return ctx.json(mapBotTokenResetResponse(botUser, token));
			} catch (err) {
				if (err instanceof ApplicationNotOwnedError) {
					throw new AccessDeniedError();
				}
				if (err instanceof InvalidClientError || err instanceof UnknownApplicationError) {
					throw new UnknownApplicationError();
				}
				throw err;
			}
		},
	);

	app.post(
		'/oauth2/applications/:id/client-secret/reset',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_ROTATE_SECRET),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));

			const applicationId = createApplicationID(BigInt(ctx.req.param('id')));

			try {
				const {clientSecret} = await ctx.get('applicationService').rotateClientSecret(user.id, applicationId);
				const application = await ctx.get('applicationRepository').getApplication(applicationId);
				if (!application) {
					throw new UnknownApplicationError();
				}

				return ctx.json(mapApplicationToResponse(application, {clientSecret}));
			} catch (err) {
				if (err instanceof ApplicationNotOwnedError) {
					throw new AccessDeniedError();
				}
				if (err instanceof InvalidClientError || err instanceof UnknownApplicationError) {
					throw new UnknownApplicationError();
				}
				throw err;
			}
		},
	);

	app.patch(
		'/oauth2/applications/:id/bot',
		RateLimitMiddleware(RateLimitConfigs.OAUTH_DEV_CLIENT_UPDATE),
		LoginRequiredAllowSuspicious,
		DefaultUserOnly,
		Validator(
			'json',
			z.object({
				username: UsernameType.optional(),
				discriminator: DiscriminatorType.optional(),
				avatar: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
				banner: createBase64StringType(1, AVATAR_MAX_SIZE * 1.33).nullish(),
				bio: createStringType(0, 1024).nullish(),
			}),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const applicationId = createApplicationID(BigInt(ctx.req.param('id')));
			const body = ctx.req.valid('json');

			try {
				const result = await ctx.get('applicationService').updateBotProfile(userId, applicationId, {
					username: body.username,
					discriminator: body.discriminator,
					avatar: body.avatar,
					banner: body.banner,
					bio: body.bio,
				});

				return ctx.json(mapBotProfileToResponse(result.user));
			} catch (err) {
				if (err instanceof ApplicationNotOwnedError) {
					throw new AccessDeniedError();
				}
				if (err instanceof BotUserNotFoundError) {
					throw err;
				}
				if (err instanceof InvalidClientError || err instanceof UnknownApplicationError) {
					throw new UnknownApplicationError();
				}
				if (err instanceof UsernameNotAvailableError) {
					throw err;
				}
				throw err;
			}
		},
	);
};

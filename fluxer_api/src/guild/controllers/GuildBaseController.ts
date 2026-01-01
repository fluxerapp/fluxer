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

import type {HonoApp} from '~/App';
import {requireSudoMode} from '~/auth/services/SudoVerificationService';
import {createGuildID} from '~/BrandedTypes';
import {AccessDeniedError} from '~/Errors';
import {GuildCreateRequest, GuildUpdateRequest} from '~/guild/GuildModel';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {SudoModeMiddleware} from '~/middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, PasswordType, SudoVerificationSchema, VanityURLCodeType, z} from '~/Schema';
import {Validator} from '~/Validator';

export const GuildBaseController = (app: HonoApp) => {
	app.post(
		'/guilds',
		RateLimitMiddleware(RateLimitConfigs.GUILD_CREATE),
		Validator('json', GuildCreateRequest),
		LoginRequired,
		async (ctx) => {
			const user = ctx.get('user');
			const data = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(await ctx.get('guildService').createGuild({user, data}, auditLogReason));
		},
	);

	app.get('/users/@me/guilds', RateLimitMiddleware(RateLimitConfigs.GUILD_LIST), LoginRequired, async (ctx) => {
		if (ctx.get('authTokenType') === 'bearer') {
			const scopes = ctx.get('oauthBearerScopes');
			if (!scopes || !scopes.has('guilds')) {
				throw new AccessDeniedError();
			}
		}
		const userId = ctx.get('user').id;
		return ctx.json(await ctx.get('guildService').getUserGuilds(userId));
	});

	app.delete(
		'/users/@me/guilds/:guild_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_LEAVE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').leaveGuild({userId, guildId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/guilds/:guild_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_GET),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			return ctx.json(await ctx.get('guildService').getGuild({userId, guildId}));
		},
	);

	app.patch(
		'/guilds/:guild_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', GuildUpdateRequest),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(await ctx.get('guildService').updateGuild({userId, guildId, data, requestCache}, auditLogReason));
		},
	);

	app.post(
		'/guilds/:guild_id/delete',
		RateLimitMiddleware(RateLimitConfigs.GUILD_DELETE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		SudoModeMiddleware,
		Validator('json', z.object({password: PasswordType.optional()}).merge(SudoVerificationSchema)),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').deleteGuild({user, guildId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/guilds/:guild_id/vanity-url',
		RateLimitMiddleware(RateLimitConfigs.GUILD_VANITY_URL_GET),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			return ctx.json(await ctx.get('guildService').getVanityURL({userId, guildId}));
		},
	);

	app.patch(
		'/guilds/:guild_id/vanity-url',
		RateLimitMiddleware(RateLimitConfigs.GUILD_VANITY_URL_PATCH),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', z.object({code: VanityURLCodeType.nullish()})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {code} = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const {code: newCode} = await ctx
				.get('guildService')
				.updateVanityURL({userId, guildId, code: code ?? null, requestCache}, auditLogReason);
			return ctx.json({code: newCode});
		},
	);

	app.patch(
		'/guilds/:guild_id/text-channel-flexible-names',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', z.object({enabled: z.boolean()})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {enabled} = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx
					.get('guildService')
					.updateTextChannelFlexibleNamesFeature({userId, guildId, enabled, requestCache}, auditLogReason),
			);
		},
	);

	app.patch(
		'/guilds/:guild_id/detached-banner',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', z.object({enabled: z.boolean()})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {enabled} = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx
					.get('guildService')
					.updateDetachedBannerFeature({userId, guildId, enabled, requestCache}, auditLogReason),
			);
		},
	);

	app.patch(
		'/guilds/:guild_id/disallow-unclaimed-accounts',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', z.object({enabled: z.boolean()})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {enabled} = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx
					.get('guildService')
					.updateDisallowUnclaimedAccountsFeature({userId, guildId, enabled, requestCache}, auditLogReason),
			);
		},
	);
};

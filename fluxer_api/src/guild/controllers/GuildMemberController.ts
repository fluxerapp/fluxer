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
import {createGuildID, createRoleID, createUserID} from '~/BrandedTypes';
import {
	GuildBanCreateRequest,
	GuildMemberUpdateRequest,
	GuildTransferOwnershipRequest,
	MyGuildMemberUpdateRequest,
} from '~/guild/GuildModel';
import {LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {SudoModeMiddleware} from '~/middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, SudoVerificationSchema, z} from '~/Schema';
import {Validator} from '~/Validator';

export const GuildMemberController = (app: HonoApp) => {
	app.get(
		'/guilds/:guild_id/members',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBERS),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const requestCache = ctx.get('requestCache');
			return ctx.json(await ctx.get('guildService').getMembers({userId, guildId, requestCache}));
		},
	);

	app.get(
		'/guilds/:guild_id/members/@me',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBERS),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const requestCache = ctx.get('requestCache');
			return ctx.json(await ctx.get('guildService').getMember({userId, targetId: userId, guildId, requestCache}));
		},
	);

	app.get(
		'/guilds/:guild_id/members/:user_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBERS),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, user_id: Int64Type})),
		async (ctx) => {
			const {guild_id, user_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const targetId = createUserID(user_id);
			const guildId = createGuildID(guild_id);
			const requestCache = ctx.get('requestCache');
			return ctx.json(await ctx.get('guildService').getMember({userId, targetId, guildId, requestCache}));
		},
	);

	app.patch(
		'/guilds/:guild_id/members/@me',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBER_UPDATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator('json', MyGuildMemberUpdateRequest),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const requestCache = ctx.get('requestCache');
			const data = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const result = await ctx
				.get('guildService')
				.updateMember({userId, targetId: userId, guildId, data, requestCache}, auditLogReason);
			return ctx.json(result);
		},
	);

	app.patch(
		'/guilds/:guild_id/members/:user_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBER_UPDATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, user_id: Int64Type})),
		Validator('json', GuildMemberUpdateRequest),
		async (ctx) => {
			const {guild_id, user_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const targetId = createUserID(user_id);
			const guildId = createGuildID(guild_id);
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const result = await ctx
				.get('guildService')
				.updateMember({userId, targetId, guildId, data, requestCache}, auditLogReason);
			return ctx.json(result);
		},
	);

	app.delete(
		'/guilds/:guild_id/members/:user_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBER_REMOVE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, user_id: Int64Type})),
		async (ctx) => {
			const {guild_id, user_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const targetId = createUserID(user_id);
			const guildId = createGuildID(guild_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').removeMember({userId, targetId, guildId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/guilds/:guild_id/transfer-ownership',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		SudoModeMiddleware,
		Validator('json', GuildTransferOwnershipRequest.merge(SudoVerificationSchema)),
		async (ctx) => {
			const user = ctx.get('user');
			const userId = user.id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body, ctx.get('authService'), ctx.get('authMfaService'));
			const {new_owner_id} = body;
			const newOwnerId = createUserID(new_owner_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(await ctx.get('guildService').transferOwnership({userId, guildId, newOwnerId}, auditLogReason));
		},
	);

	app.get(
		'/guilds/:guild_id/bans',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBERS),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type})),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const requestCache = ctx.get('requestCache');
			return ctx.json(await ctx.get('guildService').listBans({userId, guildId, requestCache}));
		},
	);

	app.put(
		'/guilds/:guild_id/bans/:user_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBER_REMOVE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, user_id: Int64Type})),
		Validator('json', GuildBanCreateRequest),
		async (ctx) => {
			const {guild_id, user_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const targetId = createUserID(user_id);
			const guildId = createGuildID(guild_id);
			const {delete_message_days, reason, ban_duration_seconds} = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').banMember(
				{
					userId,
					guildId,
					targetId,
					deleteMessageDays: delete_message_days,
					reason: reason ?? undefined,
					banDurationSeconds: ban_duration_seconds,
				},
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/guilds/:guild_id/bans/:user_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBER_REMOVE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, user_id: Int64Type})),
		async (ctx) => {
			const {guild_id, user_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const targetId = createUserID(user_id);
			const guildId = createGuildID(guild_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').unbanMember({userId, guildId, targetId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);

	app.put(
		'/guilds/:guild_id/members/:user_id/roles/:role_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBER_ROLE_ADD),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, user_id: Int64Type, role_id: Int64Type})),
		async (ctx) => {
			const {guild_id, user_id, role_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const targetId = createUserID(user_id);
			const guildId = createGuildID(guild_id);
			const roleId = createRoleID(role_id);
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').addMemberRole({userId, targetId, guildId, roleId, requestCache}, auditLogReason);
			return ctx.body(null, 204);
		},
	);

	app.delete(
		'/guilds/:guild_id/members/:user_id/roles/:role_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_MEMBER_ROLE_REMOVE),
		LoginRequired,
		Validator('param', z.object({guild_id: Int64Type, user_id: Int64Type, role_id: Int64Type})),
		async (ctx) => {
			const {guild_id, user_id, role_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const targetId = createUserID(user_id);
			const guildId = createGuildID(guild_id);
			const roleId = createRoleID(role_id);
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').removeMemberRole({userId, targetId, guildId, roleId, requestCache}, auditLogReason);
			return ctx.body(null, 204);
		},
	);
};

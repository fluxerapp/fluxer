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
import type {ChannelID, GuildID} from '~/BrandedTypes';
import {createChannelID, createGuildID, createInviteCode} from '~/BrandedTypes';
import {InviteTypes} from '~/Constants';
import {PackAccessDeniedError} from '~/errors/PackAccessDeniedError';
import {UnknownPackError} from '~/errors/UnknownPackError';
import {
	mapInviteToGroupDmInviteMetadataResponse,
	mapInviteToGroupDmInviteResponse,
	mapInviteToGuildInviteMetadataResponse,
	mapInviteToGuildInviteResponse,
	mapInviteToPackInviteMetadataResponse,
	mapInviteToPackInviteResponse,
} from '~/invite/InviteModel';
import type {Invite} from '~/Models';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createStringType, Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

const inviteCodeParamSchema = z.object({invite_code: createStringType()});
const channelIdParamSchema = z.object({channel_id: Int64Type});
const guildIdParamSchema = z.object({guild_id: Int64Type});
const packIdParamSchema = z.object({pack_id: Int64Type});
const packInviteBodySchema = z.object({
	max_uses: z.number().int().min(0).max(100).nullish().default(0),
	max_age: z.number().int().min(0).max(604800).nullish().default(0),
	unique: z.boolean().nullish().default(false),
});

const createMappingHelpers = (ctx: Context<HonoEnv>) => ({
	userCacheService: ctx.get('userCacheService'),
	requestCache: ctx.get('requestCache'),
	getChannelResponse: async (channelId: ChannelID) => await ctx.get('channelService').getPublicChannelData(channelId),
	getChannelSystem: async (channelId: ChannelID) => await ctx.get('channelService').getChannelSystem(channelId),
	getChannelMemberCount: async (channelId: ChannelID) =>
		await ctx.get('channelService').getChannelMemberCount(channelId),
	getGuildResponse: async (guildId: GuildID) => await ctx.get('guildService').getPublicGuildData(guildId),
	getGuildCounts: async (guildId: GuildID) => await ctx.get('gatewayService').getGuildCounts(guildId),
	packRepository: ctx.get('packRepository'),
	gatewayService: ctx.get('gatewayService'),
});

const mapInviteResponse = async (invite: Invite, ctx: Context<HonoEnv>) => {
	const helpers = createMappingHelpers(ctx);
	if (invite.type === InviteTypes.GROUP_DM) {
		return mapInviteToGroupDmInviteResponse({invite, ...helpers});
	}
	if (invite.type === InviteTypes.EMOJI_PACK || invite.type === InviteTypes.STICKER_PACK) {
		return mapInviteToPackInviteResponse({invite, ...helpers});
	}
	return mapInviteToGuildInviteResponse({invite, ...helpers});
};

const mapInviteMetadataResponse = async (invite: Invite, ctx: Context<HonoEnv>) => {
	const helpers = createMappingHelpers(ctx);
	if (invite.type === InviteTypes.GROUP_DM) {
		return mapInviteToGroupDmInviteMetadataResponse({invite, ...helpers});
	}
	if (invite.type === InviteTypes.EMOJI_PACK || invite.type === InviteTypes.STICKER_PACK) {
		return mapInviteToPackInviteMetadataResponse({invite, ...helpers});
	}
	return mapInviteToGuildInviteMetadataResponse({invite, ...helpers});
};

const mapInviteList = async (invites: Array<Invite>, ctx: Context<HonoEnv>) => {
	return Promise.all(invites.map((invite) => mapInviteMetadataResponse(invite, ctx)));
};

export const InviteController = (app: HonoApp) => {
	app.get(
		'/invites/:invite_code',
		RateLimitMiddleware(RateLimitConfigs.INVITE_GET),
		Validator('param', inviteCodeParamSchema),
		async (ctx) => {
			const inviteCode = createInviteCode(ctx.req.valid('param').invite_code);
			const invite = await ctx.get('inviteService').getInvite(inviteCode);
			return ctx.json(await mapInviteResponse(invite, ctx));
		},
	);

	app.post(
		'/invites/:invite_code',
		RateLimitMiddleware(RateLimitConfigs.INVITE_ACCEPT),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', inviteCodeParamSchema),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const inviteCode = createInviteCode(ctx.req.valid('param').invite_code);
			const helpers = createMappingHelpers(ctx);
			await ctx.get('inviteService').acceptInvite({userId, inviteCode, requestCache: helpers.requestCache});
			const invite = await ctx.get('inviteService').getInvite(inviteCode);
			return ctx.json(await mapInviteResponse(invite, ctx));
		},
	);

	app.delete(
		'/invites/:invite_code',
		RateLimitMiddleware(RateLimitConfigs.INVITE_DELETE),
		LoginRequired,
		Validator('param', inviteCodeParamSchema),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const inviteCode = createInviteCode(ctx.req.valid('param').invite_code);
			const inviteService = ctx.get('inviteService');
			const invite = await inviteService.getInvite(inviteCode);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await inviteService.deleteInvite({userId, inviteCode}, auditLogReason);
			await inviteService.dispatchInviteDelete(invite);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/channels/:channel_id/invites',
		RateLimitMiddleware(RateLimitConfigs.INVITE_CREATE),
		LoginRequired,
		Validator('param', channelIdParamSchema),
		Validator(
			'json',
			z.object({
				max_uses: z.number().int().min(0).max(100).nullish().default(0),
				max_age: z.number().int().min(0).max(604800).nullish().default(0),
				unique: z.boolean().nullish().default(false),
				temporary: z.boolean().nullish().default(false),
			}),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const {max_uses: maxUses, max_age: maxAge, unique, temporary} = ctx.req.valid('json');
			const inviteService = ctx.get('inviteService');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const {invite, isNew} = await inviteService.createInvite(
				{
					inviterId: userId,
					channelId,
					maxUses: maxUses ?? 0,
					maxAge: maxAge ?? 0,
					unique: unique ?? false,
					temporary: temporary ?? false,
				},
				auditLogReason,
			);
			const inviteData = await mapInviteMetadataResponse(invite, ctx);
			if (isNew) {
				await inviteService.dispatchInviteCreate(invite, inviteData);
			}
			return ctx.json(inviteData);
		},
	);

	app.get(
		'/channels/:channel_id/invites',
		RateLimitMiddleware(RateLimitConfigs.INVITE_LIST_CHANNEL),
		LoginRequired,
		Validator('param', channelIdParamSchema),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const channelId = createChannelID(ctx.req.valid('param').channel_id);
			const invites = await ctx.get('inviteService').getChannelInvitesSorted({userId, channelId});
			return ctx.json(await mapInviteList(invites, ctx));
		},
	);

	app.get(
		'/guilds/:guild_id/invites',
		RateLimitMiddleware(RateLimitConfigs.INVITE_LIST_GUILD),
		LoginRequired,
		Validator('param', guildIdParamSchema),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const invites = await ctx.get('inviteService').getGuildInvitesSorted({userId, guildId});
			return ctx.json(await mapInviteList(invites, ctx));
		},
	);

	app.get(
		'/packs/:pack_id/invites',
		RateLimitMiddleware(RateLimitConfigs.PACKS_INVITES_LIST),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', packIdParamSchema),
		async (ctx) => {
			const user = ctx.get('user');
			const packId = createGuildID(ctx.req.valid('param').pack_id);
			const invites = await ctx.get('inviteService').getPackInvitesSorted({
				userId: user.id,
				packId,
			});
			return ctx.json(await mapInviteList(invites, ctx));
		},
	);

	app.post(
		'/packs/:pack_id/invites',
		RateLimitMiddleware(RateLimitConfigs.PACKS_INVITES_CREATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', packIdParamSchema),
		Validator('json', packInviteBodySchema),
		async (ctx) => {
			const user = ctx.get('user');
			const packId = createGuildID(ctx.req.valid('param').pack_id);
			const pack = await ctx.get('packRepository').getPack(packId);
			if (!pack) {
				throw new UnknownPackError();
			}
			if (pack.creatorId !== user.id) {
				throw new PackAccessDeniedError();
			}

			const {max_uses, max_age, unique} = ctx.req.valid('json');
			const {invite, isNew} = await ctx.get('inviteService').createPackInvite({
				inviterId: user.id,
				packId,
				packType: pack.type,
				maxUses: max_uses ?? 0,
				maxAge: max_age ?? 0,
				unique: unique ?? false,
			});
			const inviteData = await mapInviteMetadataResponse(invite, ctx);
			if (isNew) {
				await ctx.get('inviteService').dispatchInviteCreate(invite, inviteData);
			}
			return ctx.json(inviteData);
		},
	);
};

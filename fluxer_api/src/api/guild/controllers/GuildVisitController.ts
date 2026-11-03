// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {chargeGuildVisitorBuckets, GUILD_VISIT_START_IP_BUCKETS} from '@app/api/guild/visits/GuildVisitorAccess';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {getAccountTier, tierContextFrom} from '@app/api/user/AccountTier';
import {getRequestClientIp} from '@app/api/utils/RequestClientIp';
import {Validator} from '@app/api/Validator';
import {DiscoveryDisabledError} from '@fluxer/errors/src/domains/discovery/DiscoveryDisabledError';
import {GuildIdParam, InviteCodeParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {GuildVisitListResponse, GuildVisitStartResponse} from '@fluxer/schema/src/domains/guild/GuildVisitSchemas';
import type {Context} from 'hono';

async function chargeVisitStart(ctx: Context<HonoEnv>): Promise<void> {
	const user = ctx.get('user');
	await chargeGuildVisitorBuckets({
		buckets: GUILD_VISIT_START_IP_BUCKETS,
		userId: user.id,
		guildId: null,
		tier: getAccountTier(user),
	});
}

export function GuildVisitController(app: HonoApp) {
	app.post(
		'/invites/:invite_code/visit',
		RateLimitMiddleware(RateLimitConfigs.GUILD_VISIT_START),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', InviteCodeParam),
		OpenAPI({
			operationId: 'visit_invite_guild',
			summary: 'Visit the guild of an invite',
			description:
				'Opens the guild behind an invite without joining it. The visitor can read the channels that @everyone can see, but cannot post, react or see the member list, and other members see no change. Answers with member set to true and no visit when the user is already a member. Visiting does not use up a limited invite. Joining later from the visit uses one.',
			responseSchema: GuildVisitStartResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Invites'],
		}),
		async (ctx) => {
			await chargeVisitStart(ctx);
			return ctx.json(
				await ctx.get('guildVisitService').startFromInvite({
					user: ctx.get('user'),
					code: ctx.req.valid('param').invite_code,
					ip: getRequestClientIp(ctx),
				}),
			);
		},
	);
	app.post(
		'/discovery/guilds/:guild_id/visit',
		RateLimitMiddleware(RateLimitConfigs.GUILD_VISIT_START),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'visit_discovery_guild',
			summary: 'Visit a discoverable guild',
			description:
				'Opens a guild listed in discovery without joining it. Works like visiting through an invite, and answers with Unknown Guild when the guild is not listed.',
			responseSchema: GuildVisitStartResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Discovery'],
		}),
		async (ctx) => {
			if (!Config.discovery.enabled) throw new DiscoveryDisabledError();
			await chargeVisitStart(ctx);
			return ctx.json(
				await ctx.get('guildVisitService').startFromDiscovery({
					user: ctx.get('user'),
					guildId: createGuildID(ctx.req.valid('param').guild_id),
					ip: getRequestClientIp(ctx),
				}),
			);
		},
	);
	app.get(
		'/users/@me/guild-visits',
		RateLimitMiddleware(RateLimitConfigs.USER_GUILD_VISITS),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'list_my_guild_visits',
			summary: 'List guild visits',
			description:
				'Lists the guilds the current user is visiting, most recently opened first. A visit ends on its own after 14 days without being opened, or when the invite that started it is deleted or expires.',
			responseSchema: GuildVisitListResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
		}),
		async (ctx) => ctx.json(await ctx.get('guildVisitService').listOwn(ctx.get('user').id)),
	);
	app.delete(
		'/users/@me/guild-visits/:guild_id',
		RateLimitMiddleware(RateLimitConfigs.USER_GUILD_VISITS),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'leave_guild_visit',
			summary: 'Stop visiting a guild',
			description:
				'Ends a visit. Every session of the user gets Guild Delete for the guild. Answers 204 even when there was no visit.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
		}),
		async (ctx) => {
			await ctx.get('guildVisitService').leave(ctx.get('user').id, createGuildID(ctx.req.valid('param').guild_id));
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/users/@me/guild-visits/:guild_id/join',
		RateLimitMiddleware(RateLimitConfigs.INVITE_ACCEPT),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'join_visited_guild',
			summary: 'Join a visited guild',
			description:
				'Turns a visit into membership, with the same checks as joining through the invite or discovery listing that started the visit. The sessions of the user switch the guild to membership in place.',
			responseSchema: GuildResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Users'],
		}),
		async (ctx) =>
			ctx.json(
				await ctx.get('guildVisitService').join({
					user: ctx.get('user'),
					guildId: createGuildID(ctx.req.valid('param').guild_id),
					tierContext: tierContextFrom(ctx),
					requestCache: ctx.get('requestCache'),
				}),
			),
	);
}

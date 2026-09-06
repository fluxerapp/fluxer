// SPDX-License-Identifier: AGPL-3.0-or-later

import {SingleCommunityCannotCreateGuildsError} from '@fluxer/errors/src/domains/guild/SingleCommunityCannotCreateGuildsError';
import {SingleCommunityCannotDeleteError} from '@fluxer/errors/src/domains/guild/SingleCommunityCannotDeleteError';
import {SingleCommunityCannotLeaveError} from '@fluxer/errors/src/domains/guild/SingleCommunityCannotLeaveError';
import {SudoVerificationSchema} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	GuildCreateRequest,
	GuildDeleteRequest,
	GuildLeaveQuery,
	GuildListQuery,
	GuildUpdateRequest,
	GuildVanityURLUpdateRequest,
	GuildVanityURLUpdateResponse,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {GuildResponse, GuildVanityURLResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {z} from 'zod';
import {requireEmailVerified} from '../../auth/EmailVerificationUtils';
import {requireSudoMode} from '../../auth/services/SudoVerificationService';
import {createGuildID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {requireOAuth2ScopeForBearer} from '../../middleware/OAuth2ScopeMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {SudoModeMiddleware} from '../../middleware/SudoModeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function GuildBaseController(app: HonoApp) {
	app.post(
		'/guilds',
		RateLimitMiddleware(RateLimitConfigs.GUILD_CREATE),
		LoginRequired,
		Validator('json', GuildCreateRequest),
		OpenAPI({
			operationId: 'create_guild',
			summary: 'Create guild',
			description: 'Only claimed, email-verified non-bot users can create guilds.',
			responseSchema: GuildResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const data = ctx.req.valid('json');
			const policy = await ctx.get('instanceConfigRepository').getInstancePolicyConfig();
			if (policy.single_community_enabled) {
				throw new SingleCommunityCannotCreateGuildsError();
			}
			if (!user.isUnclaimedAccount()) {
				requireEmailVerified(user, 'guild_creation');
			}
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const locale = ctx.get('requestLocale') ?? null;
			return ctx.json(await ctx.get('guildService').data.createGuild({user, data, locale}, auditLogReason));
		},
	);
	app.get(
		'/users/@me/guilds',
		RateLimitMiddleware(RateLimitConfigs.GUILD_LIST),
		requireOAuth2ScopeForBearer('guilds'),
		LoginRequired,
		Validator('query', GuildListQuery),
		OpenAPI({
			operationId: 'list_guilds',
			summary: 'List current user guilds',
			description: 'Requires guilds OAuth scope if using bearer token. Returns all guilds the user is a member of.',
			responseSchema: z.array(GuildResponse),
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const {before, after, limit, with_counts} = ctx.req.valid('query');
			return ctx.json(
				await ctx.get('guildService').data.getUserGuilds(userId, {
					before: before != null ? createGuildID(before) : undefined,
					after: after != null ? createGuildID(after) : undefined,
					limit,
					withCounts: with_counts,
				}),
			);
		},
	);
	app.delete(
		'/users/@me/guilds/:guild_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_LEAVE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('query', GuildLeaveQuery),
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'leave_guild',
			summary: 'Leave guild',
			description:
				"Removes the current user from the specified guild membership. When `delete_messages` is true, the caller's authored messages in the guild are deleted before leaving; that path requires sudo mode verification.",
			requestSchema: SudoVerificationSchema,
			requestBodyRequired: false,
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const userId = user.id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const policy = await ctx.get('instanceConfigRepository').getInstancePolicyConfig();
			if (policy.single_community_enabled && policy.single_community_guild_id === guildId.toString()) {
				throw new SingleCommunityCannotLeaveError();
			}
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const {delete_messages} = ctx.req.valid('query');
			const body = ctx.req.valid('json');
			if (delete_messages) {
				await ctx.get('guildService').data.getGuild({userId, guildId});
				await requireSudoMode(ctx, user, body);
				await ctx.get('channelService').userMessageDeletion.deleteUserMessagesInScope(userId, {guildId});
			}
			await ctx.get('guildService').members.leaveGuild({userId, guildId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/users/@me/guilds/:guild_id/messages/bulk-delete-mine',
		RateLimitMiddleware(RateLimitConfigs.GUILD_LEAVE),
		LoginRequired,
		Validator('param', GuildIdParam),
		SudoModeMiddleware,
		Validator('json', SudoVerificationSchema),
		OpenAPI({
			operationId: 'bulk_delete_my_messages_in_guild',
			summary: 'Bulk delete my messages in guild',
			description:
				'Deletes every message the caller has authored across all channels of the specified guild. Caller must be a member of the guild and pass sudo mode verification.',
			responseSchema: null,
			statusCode: 202,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const userId = user.id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const body = ctx.req.valid('json');
			await ctx.get('guildService').data.getGuild({userId, guildId});
			await requireSudoMode(ctx, user, body);
			await ctx.get('channelService').userMessageDeletion.deleteUserMessagesInScope(userId, {guildId});
			return ctx.body(null, 202);
		},
	);
	app.get(
		'/guilds/:guild_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_GET),
		requireOAuth2ScopeForBearer('guilds'),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'get_guild',
			summary: 'Get guild information',
			description:
				'User must be a member of the guild to access this endpoint. Requires guilds OAuth scope if using bearer token.',
			responseSchema: GuildResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			return ctx.json(await ctx.get('guildService').data.getGuild({userId, guildId}));
		},
	);
	app.patch(
		'/guilds/:guild_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_UPDATE),
		LoginRequired,
		Validator('param', GuildIdParam),
		SudoModeMiddleware,
		Validator('json', GuildUpdateRequest),
		OpenAPI({
			operationId: 'update_guild',
			summary: 'Update guild settings',
			description:
				'Requires manage_guild permission. Updates guild name, description, icon, banner, and other configuration options.',
			responseSchema: GuildResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const userId = user.id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const data = ctx.req.valid('json');
			let shouldRequireSudoMode = false;
			if (data.mfa_level !== undefined) {
				const currentGuild = await ctx.get('guildService').data.getGuild({userId, guildId});
				shouldRequireSudoMode = currentGuild.mfa_level !== data.mfa_level;
			}
			if (shouldRequireSudoMode) {
				await requireSudoMode(ctx, user, data);
			}
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(await ctx.get('guildService').updateGuild({userId, guildId, data, requestCache}, auditLogReason));
		},
	);
	app.post(
		'/guilds/:guild_id/delete',
		RateLimitMiddleware(RateLimitConfigs.GUILD_DELETE),
		LoginRequired,
		Validator('param', GuildIdParam),
		SudoModeMiddleware,
		Validator('json', GuildDeleteRequest),
		OpenAPI({
			operationId: 'delete_guild',
			summary: 'Delete guild',
			description:
				'Only guild owner can delete. Requires sudo mode verification (MFA). Permanently deletes the guild and all associated data.',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const policy = await ctx.get('instanceConfigRepository').getInstancePolicyConfig();
			if (policy.single_community_enabled && policy.single_community_guild_id === guildId.toString()) {
				throw new SingleCommunityCannotDeleteError();
			}
			const body = ctx.req.valid('json');
			await requireSudoMode(ctx, user, body);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').data.deleteGuild({user, guildId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/guilds/:guild_id/vanity-url',
		RateLimitMiddleware(RateLimitConfigs.GUILD_VANITY_URL_GET),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'get_guild_vanity_url',
			summary: 'Get guild vanity URL',
			description: 'Returns the custom invite code for the guild if configured.',
			responseSchema: GuildVanityURLResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			return ctx.json(await ctx.get('guildService').data.getVanityURL({userId, guildId}));
		},
	);
	app.patch(
		'/guilds/:guild_id/vanity-url',
		RateLimitMiddleware(RateLimitConfigs.GUILD_VANITY_URL_PATCH),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildVanityURLUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_vanity_url',
			summary: 'Update guild vanity URL',
			description: 'Requires manage_guild permission. Sets or removes a custom invite code.',
			responseSchema: GuildVanityURLUpdateResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const {code} = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			const {code: newCode} = await ctx
				.get('guildService')
				.data.updateVanityURL({userId, guildId, code: code ?? null, requestCache}, auditLogReason);
			return ctx.json({code: newCode});
		},
	);
}

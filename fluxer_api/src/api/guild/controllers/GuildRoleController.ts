// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildIdParam, GuildIdRoleIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {
	GuildRoleCreateRequest,
	GuildRoleHoistPositionsRequest,
	GuildRolePositionsRequest,
	GuildRoleUpdateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {GuildRoleListResponse, GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';

import {createGuildID, createRoleID} from '../../BrandedTypes';
import {LoginRequired} from '../../middleware/AuthMiddleware';
import {requireOAuth2ScopeForBearer} from '../../middleware/OAuth2ScopeMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {CLIENT_FEATURES_HEADER, parseClientFeaturesHeader} from '../../utils/featureUtils';
import {Validator} from '../../Validator';

export function GuildRoleController(app: HonoApp) {
	app.get(
		'/guilds/:guild_id/roles',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_LIST),
		requireOAuth2ScopeForBearer('guilds'),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'list_guild_roles',
			summary: 'List guild roles',
			responseSchema: GuildRoleListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'List guild roles. Requires guilds OAuth scope if using bearer token. Returns all roles defined in the guild including their permissions and settings.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			return ctx.json(await ctx.get('guildService').roles.listRoles({userId, guildId}));
		},
	);
	app.post(
		'/guilds/:guild_id/roles',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_CREATE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildRoleCreateRequest),
		OpenAPI({
			operationId: 'create_guild_role',
			summary: 'Create guild role',
			responseSchema: GuildRoleResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Create guild role. Requires manage_roles permission. Creates a new role with specified name, permissions, and color.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const data = ctx.req.valid('json');
			const clientFeatures = parseClientFeaturesHeader(ctx.req.header(CLIENT_FEATURES_HEADER));
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx.get('guildService').roles.createRole({userId, guildId, data, clientFeatures}, auditLogReason),
			);
		},
	);
	app.patch(
		'/guilds/:guild_id/roles/hoist-positions',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_HOIST_POSITIONS),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildRoleHoistPositionsRequest),
		OpenAPI({
			operationId: 'update_role_hoist_positions',
			summary: 'Update role hoist positions',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Update role hoist positions. Requires manage_roles permission. Sets the display priority for hoisted (separated) roles in the member list.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const payload = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').roles.updateHoistPositions(
				{
					userId,
					guildId,
					updates: payload.map((item) => ({roleId: createRoleID(item.id), hoistPosition: item.hoist_position})),
				},
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/guilds/:guild_id/roles/hoist-positions',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_HOIST_POSITIONS_RESET),
		LoginRequired,
		Validator('param', GuildIdParam),
		OpenAPI({
			operationId: 'reset_role_hoist_positions',
			summary: 'Reset role hoist positions',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Reset role hoist positions. Requires manage_roles permission. Clears all hoist position assignments for roles in the guild.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').roles.resetHoistPositions({userId, guildId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);
	app.patch(
		'/guilds/:guild_id/roles/:role_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_UPDATE),
		LoginRequired,
		Validator('param', GuildIdRoleIdParam),
		Validator('json', GuildRoleUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_role',
			summary: 'Update guild role',
			responseSchema: GuildRoleResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Update guild role. Requires manage_roles permission. Modifies role name, permissions, color, and other settings.',
		}),
		async (ctx) => {
			const {guild_id, role_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const roleId = createRoleID(role_id);
			const data = ctx.req.valid('json');
			const clientFeatures = parseClientFeaturesHeader(ctx.req.header(CLIENT_FEATURES_HEADER));
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx.get('guildService').roles.updateRole({userId, guildId, roleId, data, clientFeatures}, auditLogReason),
			);
		},
	);
	app.patch(
		'/guilds/:guild_id/roles',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_POSITIONS),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildRolePositionsRequest),
		OpenAPI({
			operationId: 'update_guild_role_positions',
			summary: 'Update role positions',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'Update role positions. Requires manage_roles permission. Reorders roles to change their hierarchy and permission precedence.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const payload = ctx.req.valid('json');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').roles.updateRolePositions(
				{
					userId,
					guildId,
					updates: payload.map((item) => ({roleId: createRoleID(item.id), position: item.position})),
				},
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/guilds/:guild_id/roles/:role_id',
		RateLimitMiddleware(RateLimitConfigs.GUILD_ROLE_DELETE),
		LoginRequired,
		Validator('param', GuildIdRoleIdParam),
		OpenAPI({
			operationId: 'delete_guild_role',
			summary: 'Delete guild role',
			responseSchema: null,
			statusCode: 204,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description: 'Delete guild role. Requires manage_roles permission. Permanently removes the role from the guild.',
		}),
		async (ctx) => {
			const {guild_id, role_id} = ctx.req.valid('param');
			const userId = ctx.get('user').id;
			const guildId = createGuildID(guild_id);
			const roleId = createRoleID(role_id);
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			await ctx.get('guildService').roles.deleteRole({userId, guildId, roleId}, auditLogReason);
			return ctx.body(null, 204);
		},
	);
}

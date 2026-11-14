// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID} from '@app/api/BrandedTypes';
import {LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {GuildSelfAssignableRoleSectionsUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {GuildSelfAssignableRoleSectionListResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

export function GuildSelfAssignableRoleController(app: HonoApp) {
	app.put(
		'/guilds/:guild_id/self-assignable-roles',
		RateLimitMiddleware(RateLimitConfigs.GUILD_SELF_ASSIGNABLE_ROLES_UPDATE),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('json', GuildSelfAssignableRoleSectionsUpdateRequest),
		OpenAPI({
			operationId: 'update_guild_self_assignable_roles',
			summary: 'Update self-assignable role sections',
			responseSchema: GuildSelfAssignableRoleSectionListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				"Replace the self-assignable role sections of a guild. Requires manage_guild and manage_roles permissions. Roles that are not already self-assignable must be below your highest role, and so must a listed role that the change opens to more members, by removing or adding to its section's required roles or by moving it to a less restricted section. Members can only use the sections while the guild has the SELF_ASSIGNABLE_ROLES_ENABLED feature.",
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const data = ctx.req.valid('json');
			const requestCache = ctx.get('requestCache');
			const auditLogReason = ctx.get('auditLogReason') ?? null;
			return ctx.json(
				await ctx
					.get('guildService')
					.selfAssignableRoles.updateSections({userId, guildId, data, requestCache}, auditLogReason),
			);
		},
	);
}

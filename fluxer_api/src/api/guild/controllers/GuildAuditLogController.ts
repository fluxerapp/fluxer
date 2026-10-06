// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {GuildIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {GuildAuditLogListQuery, GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';

export function GuildAuditLogController(app: HonoApp) {
	app.get(
		'/guilds/:guild_id/audit-logs',
		RateLimitMiddleware(RateLimitConfigs.GUILD_AUDIT_LOGS),
		LoginRequired,
		Validator('param', GuildIdParam),
		Validator('query', GuildAuditLogListQuery),
		OpenAPI({
			operationId: 'list_guild_audit_logs',
			summary: 'List guild audit logs',
			responseSchema: GuildAuditLogListResponse,
			statusCode: 200,
			security: ['botToken', 'bearerToken', 'sessionToken'],
			tags: ['Guilds'],
			description:
				'List guild audit logs. Requires view_audit_logs permission. Returns guild activity history with pagination and action filtering.',
		}),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const query = ctx.req.valid('query');
			const requestCache = ctx.get('requestCache');
			const response = await ctx.get('guildService').listGuildAuditLogs({
				userId,
				guildId,
				requestCache,
				limit: query.limit ?? undefined,
				beforeLogId: query.before ?? undefined,
				afterLogId: query.after ?? undefined,
				filterUserId: query.user_id ? createUserID(query.user_id) : undefined,
				actionType: query.action_type,
			});
			return ctx.json(response);
		},
	);
}

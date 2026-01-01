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
import {createGuildID, createUserID} from '~/BrandedTypes';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {InputValidationError} from '~/Errors';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {coerceNumberFromString, Int32Type, Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

const actionTypeSchema = coerceNumberFromString(Int32Type).pipe(z.nativeEnum(AuditLogActionType));

export const GuildAuditLogController = (app: HonoApp) => {
	app.get(
		'/guilds/:guild_id/audit-logs',
		RateLimitMiddleware(RateLimitConfigs.GUILD_AUDIT_LOGS),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', z.object({guild_id: Int64Type})),
		Validator(
			'query',
			z.object({
				limit: coerceNumberFromString(Int32Type.max(100)).optional(),
				before: Int64Type.optional(),
				after: Int64Type.optional(),
				user_id: Int64Type.optional(),
				action_type: actionTypeSchema.optional(),
			}),
		),
		async (ctx) => {
			const userId = ctx.get('user').id;
			const guildId = createGuildID(ctx.req.valid('param').guild_id);
			const query = ctx.req.valid('query');

			if (query.before !== undefined && query.after !== undefined) {
				throw InputValidationError.create('before', 'Cannot specify both before and after');
			}

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
};

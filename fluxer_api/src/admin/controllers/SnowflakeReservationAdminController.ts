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
import type {
	AddSnowflakeReservationRequest,
	DeleteSnowflakeReservationRequest,
	ListSnowflakeReservationsResponse,
} from '~/admin/models/SnowflakeReservationTypes';
import {AdminACLs} from '~/Constants';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {EmailType, Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

export const SnowflakeReservationAdminController = (app: HonoApp) => {
	app.post(
		'/admin/snowflake-reservations/list',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.INSTANCE_SNOWFLAKE_RESERVATION_VIEW),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const reservations = await adminService.listSnowflakeReservations();

			return ctx.json<ListSnowflakeReservationsResponse>({
				reservations,
			});
		},
	);

	app.post(
		'/admin/snowflake-reservations/add',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_SNOWFLAKE_RESERVATION_MANAGE),
		Validator(
			'json',
			z.object({
				email: EmailType,
				snowflake: Int64Type.transform((val) => val.toString()),
			}),
		),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const data = ctx.req.valid('json') as AddSnowflakeReservationRequest;

			await adminService.setSnowflakeReservation(data, adminUserId, auditLogReason);
			return ctx.json({success: true});
		},
	);

	app.post(
		'/admin/snowflake-reservations/delete',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_SNOWFLAKE_RESERVATION_MANAGE),
		Validator('json', z.object({email: EmailType})),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const data = ctx.req.valid('json') as DeleteSnowflakeReservationRequest;

			await adminService.deleteSnowflakeReservation(data, adminUserId, auditLogReason);
			return ctx.json({success: true});
		},
	);
};

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
import {createUserID} from '~/BrandedTypes';
import {AdminACLs} from '~/Constants';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

export const VerificationAdminController = (app: HonoApp) => {
	app.post(
		'/admin/pending-verifications/list',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('json', z.object({limit: z.number().default(100)})),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {limit} = ctx.req.valid('json');
			return ctx.json(await adminService.listPendingVerifications(limit));
		},
	);

	app.post(
		'/admin/pending-verifications/approve',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', z.object({user_id: Int64Type})),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {user_id} = ctx.req.valid('json');
			return ctx.json(await adminService.approveRegistration(createUserID(user_id), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/pending-verifications/reject',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', z.object({user_id: Int64Type})),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {user_id} = ctx.req.valid('json');
			return ctx.json(await adminService.rejectRegistration(createUserID(user_id), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/pending-verifications/bulk-approve',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', z.object({user_ids: z.array(Int64Type).min(1)})),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {user_ids} = ctx.req.valid('json');
			const parsedUserIds = user_ids.map(createUserID);
			return ctx.json(await adminService.bulkApproveRegistrations(parsedUserIds, adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/pending-verifications/bulk-reject',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', z.object({user_ids: z.array(Int64Type).min(1)})),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {user_ids} = ctx.req.valid('json');
			const parsedUserIds = user_ids.map(createUserID);
			return ctx.json(await adminService.bulkRejectRegistrations(parsedUserIds, adminUserId, auditLogReason));
		},
	);
};

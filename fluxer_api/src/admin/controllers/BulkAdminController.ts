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
import {AdminACLs} from '~/Constants';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Validator} from '~/Validator';
import {
	BulkAddGuildMembersRequest,
	BulkScheduleUserDeletionRequest,
	BulkUpdateGuildFeaturesRequest,
	BulkUpdateUserFlagsRequest,
} from '../AdminModel';

export const BulkAdminController = (app: HonoApp) => {
	app.post(
		'/admin/bulk/update-user-flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		requireAdminACL(AdminACLs.BULK_UPDATE_USER_FLAGS),
		Validator('json', BulkUpdateUserFlagsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.bulkUpdateUserFlags(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/bulk/update-guild-features',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		requireAdminACL(AdminACLs.BULK_UPDATE_GUILD_FEATURES),
		Validator('json', BulkUpdateGuildFeaturesRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.bulkUpdateGuildFeatures(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/bulk/add-guild-members',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		requireAdminACL(AdminACLs.BULK_ADD_GUILD_MEMBERS),
		Validator('json', BulkAddGuildMembersRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.bulkAddGuildMembers(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/bulk/schedule-user-deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_BULK_OPERATION),
		requireAdminACL(AdminACLs.BULK_DELETE_USERS),
		Validator('json', BulkScheduleUserDeletionRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.bulkScheduleUserDeletion(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);
};

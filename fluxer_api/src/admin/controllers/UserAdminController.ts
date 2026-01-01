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
import {Validator} from '~/Validator';
import {
	CancelBulkMessageDeletionRequest,
	ChangeDobRequest,
	ChangeEmailRequest,
	ChangeUsernameRequest,
	ClearUserFieldsRequest,
	DisableForSuspiciousActivityRequest,
	DisableMfaRequest,
	ListUserChangeLogRequest,
	ListUserGuildsRequest,
	ListUserSessionsRequest,
	LookupUserRequest,
	ScheduleAccountDeletionRequest,
	SendPasswordResetRequest,
	SetUserAclsRequest,
	SetUserBotStatusRequest,
	SetUserSystemStatusRequest,
	TempBanUserRequest,
	TerminateSessionsRequest,
	UnlinkPhoneRequest,
	UpdateSuspiciousActivityFlagsRequest,
	UpdateUserFlagsRequest,
	VerifyUserEmailRequest,
} from '../AdminModel';

export const UserAdminController = (app: HonoApp) => {
	app.post(
		'/admin/users/lookup',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('json', LookupUserRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.lookupUser(ctx.req.valid('json')));
		},
	);

	app.post(
		'/admin/users/list-guilds',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LIST_GUILDS),
		Validator('json', ListUserGuildsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.listUserGuilds(ctx.req.valid('json')));
		},
	);

	app.post(
		'/admin/users/disable-mfa',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_MFA),
		Validator('json', DisableMfaRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			await adminService.disableMfa(ctx.req.valid('json'), adminUserId, auditLogReason);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/admin/users/clear-fields',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_PROFILE),
		Validator('json', ClearUserFieldsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.clearUserFields(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/set-bot-status',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_BOT_STATUS),
		Validator('json', SetUserBotStatusRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.setUserBotStatus(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/set-system-status',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_BOT_STATUS),
		Validator('json', SetUserSystemStatusRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.setUserSystemStatus(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/verify-email',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('json', VerifyUserEmailRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.verifyUserEmail(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/send-password-reset',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('json', SendPasswordResetRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			await adminService.sendPasswordReset(ctx.req.valid('json'), adminUserId, auditLogReason);
			return ctx.body(null, 204);
		},
	);

	app.post(
		'/admin/users/change-username',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_USERNAME),
		Validator('json', ChangeUsernameRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.changeUsername(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/change-email',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('json', ChangeEmailRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.changeEmail(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/terminate-sessions',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', TerminateSessionsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.terminateSessions(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/temp-ban',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('json', TempBanUserRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.tempBanUser(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/unban',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('json', DisableMfaRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.unbanUser(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/schedule-deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DELETE),
		Validator('json', ScheduleAccountDeletionRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.scheduleAccountDeletion(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/cancel-deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DELETE),
		Validator('json', DisableMfaRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.cancelAccountDeletion(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/cancel-bulk-message-deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_CANCEL_BULK_MESSAGE_DELETION),
		Validator('json', CancelBulkMessageDeletionRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.cancelBulkMessageDeletion(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/set-acls',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.ACL_SET_USER),
		Validator('json', SetUserAclsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.setUserAcls(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/update-flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', UpdateUserFlagsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			const userId = createUserID(body.user_id);
			return ctx.json(
				await adminService.updateUserFlags({
					userId,
					data: {addFlags: body.add_flags, removeFlags: body.remove_flags},
					adminUserId,
					auditLogReason,
				}),
			);
		},
	);

	app.post(
		'/admin/users/unlink-phone',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_PHONE),
		Validator('json', UnlinkPhoneRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.unlinkPhone(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/change-dob',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_DOB),
		Validator('json', ChangeDobRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(await adminService.changeDob(ctx.req.valid('json'), adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/update-suspicious-activity-flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_SUSPICIOUS_ACTIVITY),
		Validator('json', UpdateSuspiciousActivityFlagsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.updateSuspiciousActivityFlags(ctx.req.valid('json'), adminUserId, auditLogReason),
			);
		},
	);

	app.post(
		'/admin/users/disable-suspicious',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DISABLE_SUSPICIOUS),
		Validator('json', DisableForSuspiciousActivityRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.disableForSuspiciousActivity(ctx.req.valid('json'), adminUserId, auditLogReason),
			);
		},
	);

	app.post(
		'/admin/users/list-sessions',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_LIST_SESSIONS),
		Validator('json', ListUserSessionsRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const body = ctx.req.valid('json');
			return ctx.json(await adminService.listUserSessions(body.user_id, adminUserId, auditLogReason));
		},
	);

	app.post(
		'/admin/users/change-log',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('json', ListUserChangeLogRequest),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.listUserChangeLog(ctx.req.valid('json')));
		},
	);
};

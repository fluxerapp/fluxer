
// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ListUserGuildsRequest, ListUserGuildsResponse} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import {
	AdminUsersMeResponse,
	CancelBulkMessageDeletionRequest,
	ChangeDobRequest,
	ChangeEmailRequest,
	ChangeUsernameRequest,
	ClearUserFieldsRequest,
	DeleteWebAuthnCredentialRequest,
	DisableForSuspiciousActivityRequest,
	DisableMfaRequest,
	ListUserChangeLogRequest,
	ListUserChangeLogResponseSchema,
	ListUserDmChannelsRequest,
	ListUserDmChannelsResponse,
	ListUserGroupDmChannelsRequest,
	ListUserGroupDmChannelsResponse,
	ListUserRelationshipsRequest,
	ListUserRelationshipsResponse,
	ListUserSessionsRequest,
	ListUserSessionsResponse,
	ListWebAuthnCredentialsRequest,
	LookupUserRequest,
	LookupUserResponse,
	RemoveUserRelationshipRequest,
	RemoveUserRelationshipsByCategoryRequest,
	RemoveUserRelationshipsResponse,
	ResendVerificationEmailRequest,
	ScheduleAccountDeletionRequest,
	SendPasswordResetRequest,
	SetUserAclsRequest,
	SetUserBotStatusRequest,
	SetUserSystemStatusRequest,
	SetUserTraitsRequest,
	TempBanUserRequest,
	TerminateSessionsRequest,
	TerminateSessionsResponse,
	UpdateHasVerifiedPhoneRequest,
	UpdatePremiumFlagsRequest,
	UpdateSuspiciousActivityFlagsRequest,
	UpdateUserFlagsRequest,
	UserMutationResponse,
	VerifyUserEmailRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {WebAuthnCredentialListResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {createUserID} from '../../BrandedTypes';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';
import {mapUserToAdminResponse} from '../models/UserTypes';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';

export function UserAdminController(app: HonoApp) {
	app.get(
		'/admin/users/me',
		requireAdminACL(AdminACLs.AUTHENTICATE),
		OpenAPI({
			operationId: 'get_authenticated_admin_user',
			summary: 'Get authenticated admin user',
			responseSchema: AdminUsersMeResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Get profile of currently authenticated admin user. Returns admin permissions, roles, and metadata. Requires AUTHENTICATE permission. Will also return list of pending registrations if the user has the USER_APPROVE_ACCOUNT',
		}),
		async (ctx) => {
			const adminUser = ctx.get('user');
			let instanceConfig = undefined;
			if (adminUser.acls.has("user:approve") || adminUser.acls.has("*")) {
				instanceConfig = (await getInstanceConfigRepository().getPendingRegistrations()).length;
			}
			const cacheService = ctx.get('cacheService');
			return ctx.json({
				user: await mapUserToAdminResponse(adminUser, cacheService, undefined, instanceConfig),
			});
		},
	);
	app.post(
		'/admin/users/lookup',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('json', LookupUserRequest),
		OpenAPI({
			operationId: 'lookup_user',
			summary: 'Lookup user',
			responseSchema: LookupUserResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Look up detailed user profile by ID, username, email, or phone. Returns account status, permissions, and metadata. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(await adminService.userService.lookupService.lookupUser(ctx.req.valid('json'), adminUserAcls));
		},
	);
	app.post(
		'/admin/users/list-guilds',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LIST_GUILDS),
		Validator('json', ListUserGuildsRequest),
		OpenAPI({
			operationId: 'list_user_guilds',
			summary: 'List user guilds',
			responseSchema: ListUserGuildsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'List all guilds a user is a member of. Shows roles and join dates. Requires USER_LIST_GUILDS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.guildServiceAggregate.lookupService.listUserGuilds(ctx.req.valid('json')));
		},
	);
	app.post(
		'/admin/users/list-dm-channels',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LIST_DM_CHANNELS),
		Validator('json', ListUserDmChannelsRequest),
		OpenAPI({
			operationId: 'list_user_dm_channels',
			summary: 'List user DM channels',
			responseSchema: ListUserDmChannelsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'List historical one-to-one DM channels for a user with cursor pagination. Requires USER_LIST_DM_CHANNELS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.userService.listUserDmChannels(ctx.req.valid('json')));
		},
	);
	app.post(
		'/admin/users/list-group-dm-channels',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LIST_DM_CHANNELS),
		Validator('json', ListUserGroupDmChannelsRequest),
		OpenAPI({
			operationId: 'list_user_group_dm_channels',
			summary: 'List user group DM channels',
			responseSchema: ListUserGroupDmChannelsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description: 'List group DM channels for a user. Requires USER_LIST_DM_CHANNELS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.userService.listUserGroupDmChannels(ctx.req.valid('json')));
		},
	);
	app.post(
		'/admin/users/disable-mfa',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_MFA),
		Validator('json', DisableMfaRequest),
		OpenAPI({
			operationId: 'disable_user_mfa',
			summary: 'Disable user MFA',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Disable two-factor authentication for user account. Removes all authenticators. Creates audit log entry. Requires USER_UPDATE_MFA permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			await adminService.userService.securityService.disableMfa(ctx.req.valid('json'), adminUserId, auditLogReason);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/users/list-webauthn-credentials',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_MFA),
		Validator('json', ListWebAuthnCredentialsRequest),
		OpenAPI({
			operationId: 'list_user_webauthn_credentials',
			summary: 'List user WebAuthn credentials',
			responseSchema: WebAuthnCredentialListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'List all WebAuthn credentials (passkeys/security keys) registered for a user. Returns credential names, creation dates, and last usage. Creates audit log entry. Requires USER_UPDATE_MFA permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.userService.securityService.listWebAuthnCredentials(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
	app.post(
		'/admin/users/delete-webauthn-credential',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_MFA),
		Validator('json', DeleteWebAuthnCredentialRequest),
		OpenAPI({
			operationId: 'delete_user_webauthn_credential',
			summary: 'Delete user WebAuthn credential',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Delete a specific WebAuthn credential (passkey/security key) from a user account. Creates audit log entry. Requires USER_UPDATE_MFA permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			await adminService.userService.securityService.deleteWebAuthnCredential(
				ctx.req.valid('json'),
				adminUserId,
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/users/clear-fields',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_PROFILE),
		Validator('json', ClearUserFieldsRequest),
		OpenAPI({
			operationId: 'clear_user_fields',
			summary: 'Clear user fields',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Clear or reset user profile fields such as bio, avatar, or status. Creates audit log entry. Requires USER_UPDATE_PROFILE permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.profileService.clearUserFields(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/set-bot-status',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_BOT_STATUS),
		Validator('json', SetUserBotStatusRequest),
		OpenAPI({
			operationId: 'set_user_bot_status',
			summary: 'Set user bot status',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Mark or unmark a user account as a bot. Controls bot badge visibility and API permissions. Creates audit log entry. Requires USER_UPDATE_BOT_STATUS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.profileService.setUserBotStatus(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/set-system-status',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_BOT_STATUS),
		Validator('json', SetUserSystemStatusRequest),
		OpenAPI({
			operationId: 'set_user_system_status',
			summary: 'Set user system status',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Mark or unmark a user as a system account. System accounts have special permissions for automated operations. Creates audit log entry. Requires USER_UPDATE_BOT_STATUS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.profileService.setUserSystemStatus(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/verify-email',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('json', VerifyUserEmailRequest),
		OpenAPI({
			operationId: 'verify_user_email',
			summary: 'Verify user email',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Manually verify user email address without requiring confirmation link. Bypasses email verification requirement. Creates audit log entry. Requires USER_UPDATE_EMAIL permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.profileService.verifyUserEmail(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/resend-verification-email',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('json', ResendVerificationEmailRequest),
		OpenAPI({
			operationId: 'admin_resend_verification_email',
			summary: 'Resend verification email',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Resend the account verification email for a user. Creates audit log entry and honours email verification resend limits. Requires USER_UPDATE_EMAIL permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			await adminService.userService.securityService.resendVerificationEmail(
				ctx.req.valid('json'),
				adminUserId,
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/users/send-password-reset',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('json', SendPasswordResetRequest),
		OpenAPI({
			operationId: 'send_password_reset',
			summary: 'Send password reset',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Send password reset email to user with reset link. User must use link within expiry window. Creates audit log entry. Requires USER_UPDATE_EMAIL permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			await adminService.userService.securityService.sendPasswordReset(
				ctx.req.valid('json'),
				adminUserId,
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/users/change-username',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_USERNAME),
		Validator('json', ChangeUsernameRequest),
		OpenAPI({
			operationId: 'change_user_username',
			summary: 'Change user username',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Change user username. New username must meet requirements and be unique. Creates audit log entry. Requires USER_UPDATE_USERNAME permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.profileService.changeUsername(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/change-email',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('json', ChangeEmailRequest),
		OpenAPI({
			operationId: 'change_user_email',
			summary: 'Change user email',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Change user email address. New email must be valid and unique. Marks email as verified. Creates audit log entry. Requires USER_UPDATE_EMAIL permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.profileService.changeEmail(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/terminate-sessions',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', TerminateSessionsRequest),
		OpenAPI({
			operationId: 'terminate_user_sessions',
			summary: 'Terminate user sessions',
			responseSchema: TerminateSessionsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Terminate all active user sessions across devices. Forces user to re-authenticate on next connection. Creates audit log entry. Requires USER_UPDATE_FLAGS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.userService.securityService.terminateSessions(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
	app.post(
		'/admin/users/temp-ban',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('json', TempBanUserRequest),
		OpenAPI({
			operationId: 'temp_ban_user',
			summary: 'Temp ban user',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Apply temporary ban to user account for specified duration. Prevents login and guild operations. Automatically lifts after expiry. Creates audit log entry. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.banService.tempBanUser(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/unban',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('json', DisableMfaRequest),
		OpenAPI({
			operationId: 'unban_user',
			summary: 'Unban user',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Immediately remove temporary ban from user account. User can log in and access guilds again. Creates audit log entry. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.banService.unbanUser(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/schedule-deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DELETE),
		Validator('json', ScheduleAccountDeletionRequest),
		OpenAPI({
			operationId: 'schedule_account_deletion',
			summary: 'Schedule account deletion',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Schedule user account for deletion after grace period. Account will be fully deleted with all content unless cancellation is executed. Creates audit log entry. Requires USER_DELETE permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.deletionService.scheduleAccountDeletion(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/cancel-deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DELETE),
		Validator('json', DisableMfaRequest),
		OpenAPI({
			operationId: 'cancel_account_deletion',
			summary: 'Cancel account deletion',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Cancel a scheduled account deletion. User account restoration prevents data loss. Creates audit log entry. Requires USER_DELETE permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.deletionService.cancelAccountDeletion(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/cancel-bulk-message-deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_CANCEL_BULK_MESSAGE_DELETION),
		Validator('json', CancelBulkMessageDeletionRequest),
		OpenAPI({
			operationId: 'admin_cancel_bulk_message_deletion',
			summary: 'Cancel bulk message deletion',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Cancel a scheduled bulk message deletion job for a user. Prevents deletion of user messages across guilds. Creates audit log entry. Requires USER_CANCEL_BULK_MESSAGE_DELETION permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.cancelBulkMessageDeletion(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/set-acls',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.ACL_SET_USER),
		Validator('json', SetUserAclsRequest),
		OpenAPI({
			operationId: 'set_user_acls',
			summary: 'Set user ACLs',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Grant or revoke admin ACL permissions to user. Controls admin capabilities and panel access. Creates audit log entry. Requires ACL_SET_USER permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.securityService.setUserAcls(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/set-traits',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_TRAITS),
		Validator('json', SetUserTraitsRequest),
		OpenAPI({
			operationId: 'set_user_traits',
			summary: 'Set user traits',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Set or update user trait attributes and profile metadata. Traits customize user display and features. Creates audit log entry. Requires USER_UPDATE_TRAITS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.securityService.setUserTraits(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/update-flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', UpdateUserFlagsRequest),
		OpenAPI({
			operationId: 'update_user_flags',
			summary: 'Update user flags',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Add or remove user flags to control account features and restrictions. Flags determine verification status and special properties. Creates audit log entry. Requires USER_UPDATE_FLAGS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			const body = ctx.req.valid('json');
			const userId = createUserID(body.user_id);
			const addFlags = body.add_flags.map((flag) => BigInt(flag));
			const removeFlags = body.remove_flags.map((flag) => BigInt(flag));
			return ctx.json(
				await adminService.userService.securityService.updateUserFlags({
					userId,
					data: {addFlags, removeFlags},
					adminUserId,
					auditLogReason,
					acls: adminUserAcls,
				}),
			);
		},
	);
	app.post(
		'/admin/users/update-premium-flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('json', UpdatePremiumFlagsRequest),
		OpenAPI({
			operationId: 'update_user_premium_flags',
			summary: 'Update user premium flags',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Add or remove premium-related flags on a user account (badge visibility, override, purchase block, etc). Creates audit log entry. Requires USER_UPDATE_FLAGS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			const body = ctx.req.valid('json');
			const userId = createUserID(body.user_id);
			const addFlags = body.add_flags;
			const removeFlags = body.remove_flags;
			return ctx.json(
				await adminService.userService.securityService.updatePremiumFlags({
					userId,
					data: {addFlags, removeFlags},
					adminUserId,
					auditLogReason,
					acls: adminUserAcls,
				}),
			);
		},
	);
	app.post(
		'/admin/users/update-has-verified-phone',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_PHONE),
		Validator('json', UpdateHasVerifiedPhoneRequest),
		OpenAPI({
			operationId: 'update_user_has_verified_phone',
			summary: 'Update user phone verification flag',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Set whether a user is treated as having completed phone verification. This is the only supported path for clearing the irreversible user-facing phone verification flag.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.securityService.updateHasVerifiedPhone(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/change-dob',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_DOB),
		Validator('json', ChangeDobRequest),
		OpenAPI({
			operationId: 'change_user_dob',
			summary: 'Change user DOB',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Update user date of birth. May affect age-restricted content access. Creates audit log entry. Requires USER_UPDATE_DOB permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.profileService.changeDob(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/update-suspicious-activity-flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_SUSPICIOUS_ACTIVITY),
		Validator('json', UpdateSuspiciousActivityFlagsRequest),
		OpenAPI({
			operationId: 'update_suspicious_activity_flags',
			summary: 'Update suspicious activity flags',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Flag user as suspicious for account abuse, fraud, or policy violations. Enables enforcement actions and rate limiting. Creates audit log entry. Requires USER_UPDATE_SUSPICIOUS_ACTIVITY permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.securityService.updateSuspiciousActivityFlags(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/disable-suspicious',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DISABLE_SUSPICIOUS),
		Validator('json', DisableForSuspiciousActivityRequest),
		OpenAPI({
			operationId: 'disable_user_suspicious',
			summary: 'Disable user for suspicious activity',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Disable user account due to suspicious activity or abuse. Account is locked pending review. User cannot access services. Creates audit log entry. Requires USER_DISABLE_SUSPICIOUS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(
				await adminService.userService.securityService.disableForSuspiciousActivity(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/list-sessions',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_LIST_SESSIONS),
		Validator('json', ListUserSessionsRequest),
		OpenAPI({
			operationId: 'list_user_sessions',
			summary: 'List user sessions',
			responseSchema: ListUserSessionsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'List all active user sessions across devices. Shows device info, IP, last activity, and creation time. Requires USER_LIST_SESSIONS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			const body = ctx.req.valid('json');
			return ctx.json(
				await adminService.userService.securityService.listUserSessions(
					body.user_id,
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/change-log',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('json', ListUserChangeLogRequest),
		OpenAPI({
			operationId: 'get_user_change_log',
			summary: 'Get user change log',
			responseSchema: ListUserChangeLogResponseSchema,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Retrieve complete change log history for a user. Shows all profile modifications, admin actions, and account changes with timestamps. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserAcls = ctx.get('adminUserAcls');
			return ctx.json(await adminService.userService.listUserChangeLog(ctx.req.valid('json'), adminUserAcls));
		},
	);
	app.post(
		'/admin/users/list-relationships',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LIST_RELATIONSHIPS),
		Validator('json', ListUserRelationshipsRequest),
		OpenAPI({
			operationId: 'admin_list_user_relationships',
			summary: 'List user relationships',
			responseSchema: ListUserRelationshipsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				"List a user's friends, incoming and outgoing friend requests, and blocked users. Requires USER_LIST_RELATIONSHIPS permission.",
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			return ctx.json(await adminService.relationshipService.listRelationships(ctx.req.valid('json')));
		},
	);
	app.post(
		'/admin/users/remove-relationship',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_REMOVE_RELATIONSHIP),
		Validator('json', RemoveUserRelationshipRequest),
		OpenAPI({
			operationId: 'remove_user_relationship',
			summary: 'Remove user relationship',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Remove a single relationship row for a user. For friend and outgoing_request, the mirror entry on the other user is also removed. Dispatches RELATIONSHIP_REMOVE gateway events. Requires USER_REMOVE_RELATIONSHIP permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			await adminService.relationshipService.removeRelationship(ctx.req.valid('json'), adminUserId, auditLogReason);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/users/remove-relationships-by-category',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_REMOVE_RELATIONSHIP),
		Validator('json', RemoveUserRelationshipsByCategoryRequest),
		OpenAPI({
			operationId: 'remove_user_relationships_by_category',
			summary: "Remove all of a user's relationships in a category",
			responseSchema: RemoveUserRelationshipsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Bulk-remove every relationship of the chosen category (friend, incoming_request, outgoing_request, blocked) for a user. Mirror entries on the other party are removed for friend, incoming_request, and outgoing_request. Requires USER_REMOVE_RELATIONSHIP permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			return ctx.json(
				await adminService.relationshipService.removeRelationshipsByCategory(
					ctx.req.valid('json'),
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
}

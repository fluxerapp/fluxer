// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {MissingACLError} from '@fluxer/errors/src/domains/core/MissingACLError';
import {ListUserGuildsResponse} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import {SearchUsersResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {
	AdminAclListResponse,
	AdminUserAclsRequest,
	AdminUserBanRequest,
	AdminUserBotStatusRequest,
	AdminUserChangeLogQuery,
	AdminUserClearFieldsRequest,
	AdminUserDeletionScheduleRequest,
	AdminUserDmChannelListQuery,
	AdminUserDmChannelListResponse,
	AdminUserDobUpdateRequest,
	AdminUserEmailUpdateRequest,
	AdminUserFlagsUpdateRequest,
	AdminUserGuildListQuery,
	AdminUserListQuery,
	AdminUserPhoneVerificationRequest,
	AdminUserPremiumFlagsUpdateRequest,
	AdminUserRelationshipCategoryQuery,
	AdminUserRelationshipParam,
	AdminUserSuspiciousActivityFlagsRequest,
	AdminUserSuspiciousDisableRequest,
	AdminUserSystemStatusRequest,
	AdminUsersMeResponse,
	AdminUserTraitsRequest,
	AdminUserUsernameUpdateRequest,
	AdminUserWebAuthnCredentialParam,
	ListUserChangeLogResponseSchema,
	ListUserRelationshipsResponse,
	ListUserSessionsResponse,
	LookupUserRequest,
	LookupUserResponse,
	RemoveUserRelationshipsResponse,
	TerminateSessionsResponse,
	UserMutationResponse,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {WebAuthnCredentialListResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {createUserID} from '../../BrandedTypes';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {inputValidationErrorFromZodIssues, Validator} from '../../Validator';
import {mapUserToAdminResponse} from '../models/UserTypes';

function requireSelectorACL(granted: ReadonlySet<string>, acl: string): void {
	if (!granted.has(acl) && !granted.has(AdminACLs.WILDCARD)) {
		throw new MissingACLError(acl);
	}
}

export function UserAdminController(app: HonoApp) {
	app.get(
		'/admin/acls',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.AUTHENTICATE),
		OpenAPI({
			operationId: 'list_admin_acls',
			summary: 'List admin permissions',
			responseSchema: AdminAclListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Returns every access control permission the admin API recognises. This is the registry admin accounts and admin API keys draw their permissions from. Requires AUTHENTICATE permission.',
		}),
		async (ctx) => {
			return ctx.json({acls: Object.values(AdminACLs)});
		},
	);
	app.get(
		'/admin/users/@me',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.AUTHENTICATE),
		OpenAPI({
			operationId: 'get_current_admin_user',
			summary: 'Get current admin',
			responseSchema: AdminUsersMeResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Return the admin the request was authenticated as, with the admin permissions, roles, and metadata of the account. Requires AUTHENTICATE permission.',
		}),
		async (ctx) => {
			const adminUser = ctx.get('user');
			const cacheService = ctx.get('cacheService');
			return ctx.json({
				user: await mapUserToAdminResponse(adminUser, cacheService),
			});
		},
	);
	app.get(
		'/admin/users',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('query', AdminUserListQuery),
		OpenAPI({
			operationId: 'list_admin_users',
			summary: 'List users',
			responseSchema: SearchUsersResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Lists and searches users. Exactly one selector is honoured, in this precedence order: user_id, resolve, email, last_active_ip, then the indexed q search. The resolve selector takes one exact identifier, which may be a username#discriminator tag, a user ID, an email address, or a Stripe subscription ID. The email and user_id selectors ignore limit and offset. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserAcls = ctx.get('adminUserAcls');
			const query = ctx.req.valid('query');
			const userIds =
				query.user_id === undefined ? undefined : Array.isArray(query.user_id) ? query.user_id : [query.user_id];
			if (userIds || query.resolve !== undefined) {
				if (!userIds && query.resolve?.includes('@')) {
					requireSelectorACL(adminUserAcls, AdminACLs.USER_VIEW_EMAIL);
				}
				const parsed = LookupUserRequest.safeParse(userIds ? {user_ids: userIds} : {query: query.resolve});
				if (!parsed.success) {
					throw inputValidationErrorFromZodIssues(parsed.error.issues);
				}
				const {users} = await adminService.userService.lookupService.lookupUser(parsed.data, adminUserAcls);
				return ctx.json({users, total: users.length});
			}
			if (query.email?.trim()) {
				requireSelectorACL(adminUserAcls, AdminACLs.USER_VIEW_EMAIL);
			} else if (query.last_active_ip?.trim()) {
				requireSelectorACL(adminUserAcls, AdminACLs.USER_VIEW_IP);
			}
			return ctx.json(
				await adminService.searchService.searchUsers(
					{
						query: query.q,
						email: query.email,
						last_active_ip: query.last_active_ip,
						limit: query.limit,
						offset: query.offset,
					},
					adminUserAcls,
				),
			);
		},
	);
	app.get(
		'/admin/users/:user_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LOOKUP),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'get_admin_user',
			summary: 'Get user',
			responseSchema: LookupUserResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Look up one detailed user profile by ID. Returns account status, permissions, and metadata. The email address, date of birth, and IP address are redacted without the matching view permissions. Requires USER_LOOKUP permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserAcls = ctx.get('adminUserAcls');
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(await adminService.userService.lookupService.lookupUser({user_ids: [userId]}, adminUserAcls));
		},
	);
	app.get(
		'/admin/users/:user_id/guilds',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LIST_GUILDS),
		Validator('param', UserIdParam),
		Validator('query', AdminUserGuildListQuery),
		OpenAPI({
			operationId: 'list_admin_user_guilds',
			summary: 'List user communities',
			responseSchema: ListUserGuildsResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'List all guilds a user is a member of, optionally with approximate member and presence counts. Shows roles and join dates. Requires USER_LIST_GUILDS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {user_id: userId} = ctx.req.valid('param');
			const query = ctx.req.valid('query');
			return ctx.json(
				await adminService.guildServiceAggregate.lookupService.listUserGuilds({user_id: userId, ...query}),
			);
		},
	);
	app.get(
		'/admin/users/:user_id/dm-channels',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LIST_DM_CHANNELS),
		Validator('param', UserIdParam),
		Validator('query', AdminUserDmChannelListQuery),
		OpenAPI({
			operationId: 'list_admin_user_dm_channels',
			summary: 'List user direct message channels',
			responseSchema: AdminUserDmChannelListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'List the historical one-to-one direct message channels of a user with cursor pagination, or the group direct message channels they are a recipient of when type is group_dm. Requires USER_LIST_DM_CHANNELS permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {user_id: userId} = ctx.req.valid('param');
			const {type, ...pagination} = ctx.req.valid('query');
			if (type === 'group_dm') {
				return ctx.json(await adminService.userService.listUserGroupDmChannels({user_id: userId}));
			}
			return ctx.json(await adminService.userService.listUserDmChannels({user_id: userId, ...pagination}));
		},
	);
	app.get(
		'/admin/users/:user_id/change-log',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_VIEW_CONTACT_LOG),
		Validator('param', UserIdParam),
		Validator('query', AdminUserChangeLogQuery),
		OpenAPI({
			operationId: 'list_admin_user_change_log',
			summary: 'List user contact change log',
			responseSchema: ListUserChangeLogResponseSchema,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Retrieve the identity and contact change log history for a user. Shows all profile modifications, admin actions, and account changes with timestamps. Email values are redacted without USER_VIEW_EMAIL. Requires USER_VIEW_CONTACT_LOG permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserAcls = ctx.get('adminUserAcls');
			const {user_id: userId} = ctx.req.valid('param');
			const query = ctx.req.valid('query');
			return ctx.json(await adminService.userService.listUserChangeLog({user_id: userId, ...query}, adminUserAcls));
		},
	);
	app.get(
		'/admin/users/:user_id/relationships',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_LIST_RELATIONSHIPS),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'list_admin_user_relationships',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(await adminService.relationshipService.listRelationships({user_id: userId}));
		},
	);
	app.delete(
		'/admin/users/:user_id/relationships',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_REMOVE_RELATIONSHIP),
		Validator('param', UserIdParam),
		Validator('query', AdminUserRelationshipCategoryQuery),
		OpenAPI({
			operationId: 'clear_admin_user_relationships',
			summary: 'Clear user relationships',
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
			const {user_id: userId} = ctx.req.valid('param');
			const {category} = ctx.req.valid('query');
			return ctx.json(
				await adminService.relationshipService.removeRelationshipsByCategory(
					{user_id: userId, category},
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
	app.delete(
		'/admin/users/:user_id/relationships/:target_user_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_REMOVE_RELATIONSHIP),
		Validator('param', AdminUserRelationshipParam),
		Validator('query', AdminUserRelationshipCategoryQuery),
		OpenAPI({
			operationId: 'remove_admin_user_relationship',
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
			const {user_id: userId, target_user_id: targetUserId} = ctx.req.valid('param');
			const {category} = ctx.req.valid('query');
			await adminService.relationshipService.removeRelationship(
				{user_id: userId, target_user_id: targetUserId, category},
				adminUserId,
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/admin/users/:user_id/sessions',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_LIST_SESSIONS),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'list_admin_user_sessions',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.securityService.listUserSessions(
					userId,
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.delete(
		'/admin/users/:user_id/sessions',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'terminate_admin_user_sessions',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.securityService.terminateSessions(
					{user_id: userId},
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
	app.get(
		'/admin/users/:user_id/webauthn-credentials',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_MFA),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'list_admin_user_webauthn_credentials',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.securityService.listWebAuthnCredentials(
					{user_id: userId},
					adminUserId,
					auditLogReason,
				),
			);
		},
	);
	app.delete(
		'/admin/users/:user_id/webauthn-credentials/:credential_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_MFA),
		Validator('param', AdminUserWebAuthnCredentialParam),
		OpenAPI({
			operationId: 'delete_admin_user_webauthn_credential',
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
			const {user_id: userId, credential_id: credentialId} = ctx.req.valid('param');
			await adminService.userService.securityService.deleteWebAuthnCredential(
				{user_id: userId, credential_id: credentialId},
				adminUserId,
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/admin/users/:user_id/mfa',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_MFA),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'disable_admin_user_mfa',
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
			const {user_id: userId} = ctx.req.valid('param');
			await adminService.userService.securityService.disableMfa({user_id: userId}, adminUserId, auditLogReason);
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/admin/users/:user_id/profile-fields',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_PROFILE),
		Validator('param', UserIdParam),
		Validator('json', AdminUserClearFieldsRequest),
		OpenAPI({
			operationId: 'clear_admin_user_profile_fields',
			summary: 'Clear user profile fields',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.profileService.clearUserFields(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.put(
		'/admin/users/:user_id/bot-status',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_BOT_STATUS),
		Validator('param', UserIdParam),
		Validator('json', AdminUserBotStatusRequest),
		OpenAPI({
			operationId: 'set_admin_user_bot_status',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.profileService.setUserBotStatus(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.put(
		'/admin/users/:user_id/system-status',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_BOT_STATUS),
		Validator('param', UserIdParam),
		Validator('json', AdminUserSystemStatusRequest),
		OpenAPI({
			operationId: 'set_admin_user_system_status',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.profileService.setUserSystemStatus(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.patch(
		'/admin/users/:user_id/username',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_USERNAME),
		Validator('param', UserIdParam),
		Validator('json', AdminUserUsernameUpdateRequest),
		OpenAPI({
			operationId: 'update_admin_user_username',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.profileService.changeUsername(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.patch(
		'/admin/users/:user_id/email',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('param', UserIdParam),
		Validator('json', AdminUserEmailUpdateRequest),
		OpenAPI({
			operationId: 'update_admin_user_email',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.profileService.changeEmail(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.put(
		'/admin/users/:user_id/email-verification',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'verify_admin_user_email',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.profileService.verifyUserEmail(
					{user_id: userId},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.post(
		'/admin/users/:user_id/verification-email',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'resend_admin_user_verification_email',
			summary: 'Resend user verification email',
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
			const {user_id: userId} = ctx.req.valid('param');
			await adminService.userService.securityService.resendVerificationEmail(
				{user_id: userId},
				adminUserId,
				auditLogReason,
			);
			return ctx.body(null, 204);
		},
	);
	app.post(
		'/admin/users/:user_id/password-reset',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_EMAIL),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'send_admin_user_password_reset',
			summary: 'Send user password reset',
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
			const {user_id: userId} = ctx.req.valid('param');
			await adminService.userService.securityService.sendPasswordReset({user_id: userId}, adminUserId, auditLogReason);
			return ctx.body(null, 204);
		},
	);
	app.put(
		'/admin/users/:user_id/ban',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', UserIdParam),
		Validator('json', AdminUserBanRequest),
		OpenAPI({
			operationId: 'ban_admin_user',
			summary: 'Ban user',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Apply temporary ban to user account for specified duration, or permanently with a duration of zero. Prevents login and guild operations. Automatically lifts after expiry. Creates audit log entry. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.banService.tempBanUser(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.delete(
		'/admin/users/:user_id/ban',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_TEMP_BAN),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'unban_admin_user',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.banService.unbanUser(
					{user_id: userId},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.put(
		'/admin/users/:user_id/deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DELETE),
		Validator('param', UserIdParam),
		Validator('json', AdminUserDeletionScheduleRequest),
		OpenAPI({
			operationId: 'schedule_admin_user_deletion',
			summary: 'Schedule user deletion',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.deletionService.scheduleAccountDeletion(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.delete(
		'/admin/users/:user_id/deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DELETE),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'cancel_admin_user_deletion',
			summary: 'Cancel user deletion',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.deletionService.cancelAccountDeletion(
					{user_id: userId},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.delete(
		'/admin/users/:user_id/message-deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_CANCEL_BULK_MESSAGE_DELETION),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'cancel_admin_user_message_deletion',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.cancelBulkMessageDeletion(
					{user_id: userId},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.put(
		'/admin/users/:user_id/acls',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.ACL_SET_USER),
		Validator('param', UserIdParam),
		Validator('json', AdminUserAclsRequest),
		OpenAPI({
			operationId: 'set_admin_user_acls',
			summary: 'Set user admin permissions',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Replace the admin ACL permissions granted to a user. Controls admin capabilities and panel access. The permissions accepted are the ones listed by GET /admin/acls. Creates audit log entry. Requires ACL_SET_USER permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.securityService.setUserAcls(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.put(
		'/admin/users/:user_id/traits',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_TRAITS),
		Validator('param', UserIdParam),
		Validator('json', AdminUserTraitsRequest),
		OpenAPI({
			operationId: 'set_admin_user_traits',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.securityService.setUserTraits(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.patch(
		'/admin/users/:user_id/flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('param', UserIdParam),
		Validator('json', AdminUserFlagsUpdateRequest),
		OpenAPI({
			operationId: 'update_admin_user_flags',
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
			const userId = createUserID(ctx.req.valid('param').user_id);
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
	app.patch(
		'/admin/users/:user_id/premium-flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('param', UserIdParam),
		Validator('json', AdminUserPremiumFlagsUpdateRequest),
		OpenAPI({
			operationId: 'update_admin_user_premium_flags',
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
			const userId = createUserID(ctx.req.valid('param').user_id);
			return ctx.json(
				await adminService.userService.securityService.updatePremiumFlags({
					userId,
					data: {addFlags: body.add_flags, removeFlags: body.remove_flags},
					adminUserId,
					auditLogReason,
					acls: adminUserAcls,
				}),
			);
		},
	);
	app.put(
		'/admin/users/:user_id/phone-verification',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_PHONE),
		Validator('param', UserIdParam),
		Validator('json', AdminUserPhoneVerificationRequest),
		OpenAPI({
			operationId: 'update_admin_user_phone_verification',
			summary: 'Update user phone verification flag',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Set whether a user is treated as having completed phone verification. This is the only supported path for clearing the irreversible user-facing phone verification flag. Requires USER_UPDATE_PHONE permission.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const adminUserAcls = ctx.get('adminUserAcls');
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.securityService.updateHasVerifiedPhone(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.patch(
		'/admin/users/:user_id/date-of-birth',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_DOB),
		Validator('param', UserIdParam),
		Validator('json', AdminUserDobUpdateRequest),
		OpenAPI({
			operationId: 'update_admin_user_date_of_birth',
			summary: 'Change user date of birth',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.profileService.changeDob(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.put(
		'/admin/users/:user_id/suspicious-activity-flags',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_SUSPICIOUS_ACTIVITY),
		Validator('param', UserIdParam),
		Validator('json', AdminUserSuspiciousActivityFlagsRequest),
		OpenAPI({
			operationId: 'update_admin_user_suspicious_activity_flags',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.securityService.updateSuspiciousActivityFlags(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
	app.put(
		'/admin/users/:user_id/suspicious-activity-disablement',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DISABLE_SUSPICIOUS),
		Validator('param', UserIdParam),
		Validator('json', AdminUserSuspiciousDisableRequest),
		OpenAPI({
			operationId: 'disable_admin_user_suspicious',
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
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.securityService.disableForSuspiciousActivity(
					{user_id: userId, ...ctx.req.valid('json')},
					adminUserId,
					auditLogReason,
					adminUserAcls,
				),
			);
		},
	);
}

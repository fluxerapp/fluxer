// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead} from '@app/api/admin/AdminAuditRecorder';
import {mapUserToAdminResponse} from '@app/api/admin/models/UserTypes';
import {createUserID} from '@app/api/BrandedTypes';
import {enforcementActor} from '@app/api/enforcement/EnforcementActor';
import {
	RequireEmailAccountIdentity,
	RequireUsernameAccountIdentity,
} from '@app/api/middleware/AccountIdentityMiddleware';
import {hasAdminACL, requireAdminACL, requireGrantedAdminACLs} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {inputValidationErrorFromZodIssues, Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {ListUserGuildsResponse} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import {SearchUsersResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {
	AdminAclListResponse,
	AdminPasswordResetLinkResponse,
	AdminUserAclsRequest,
	AdminUserBotStatusRequest,
	AdminUserChangeLogQuery,
	AdminUserClearFieldsRequest,
	AdminUserDeletionCancelRequest,
	AdminUserDeletionScheduleRequest,
	AdminUserDmChannelListQuery,
	AdminUserDmChannelListResponse,
	AdminUserDobUpdateRequest,
	AdminUserEmailUpdateRequest,
	AdminUserFlagsUpdateRequest,
	AdminUserGuildListQuery,
	AdminUserListQuery,
	AdminUserPremiumFlagsUpdateRequest,
	AdminUserRelationshipCategoryQuery,
	AdminUserRelationshipParam,
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
	UserRestrictionResponse,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {
	AdminUserBanRequest,
	AdminUserFeatureLimitRequest,
	AdminUserWarningRequest,
	AdminUserWarningResponse,
} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import {WebAuthnCredentialListResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

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
			const acls = Object.values(AdminACLs);
			await recordAdminRead(ctx, {
				targetType: 'admin_acl',
				targetId: 0n,
				action: AdminAuditReadActions.LIST_ADMIN_ACLS,
				metadata: {acl_count: acls.length},
			});
			return ctx.json({acls});
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
				'Return the admin the request was authenticated as, with the admin permissions, roles, and metadata of the account, plus the number of pending registrations when the admin holds REGISTRATION_REVIEW. Requires AUTHENTICATE permission.',
		}),
		async (ctx) => {
			const canReviewRegistrations = hasAdminACL(ctx.get('adminUserAcls'), AdminACLs.REGISTRATION_REVIEW);
			const [user, pendingRegistrations] = await Promise.all([
				mapUserToAdminResponse(ctx.get('user'), ctx.get('cacheService')),
				canReviewRegistrations ? getInstanceConfigRepository().getPendingRegistrations() : null,
			]);
			return ctx.json({user, pending_registration_count: pendingRegistrations?.length ?? null});
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
					requireGrantedAdminACLs(adminUserAcls, [AdminACLs.USER_VIEW_EMAIL]);
				}
				const parsed = LookupUserRequest.safeParse(userIds ? {user_ids: userIds} : {query: query.resolve});
				if (!parsed.success) {
					throw inputValidationErrorFromZodIssues(parsed.error.issues);
				}
				const {users} = await adminService.userService.lookupService.lookupUser(parsed.data, adminUserAcls);
				await recordAdminRead(ctx, {
					targetType: 'user',
					targetId: 0n,
					action: AdminAuditReadActions.SEARCH_USERS,
					metadata: {
						selector: userIds ? 'user_id' : 'resolve',
						user_id: userIds?.length === 1 ? userIds[0] : undefined,
						user_id_count: userIds?.length,
						result_count: users.length,
						total: users.length,
					},
				});
				return ctx.json({users, total: users.length});
			}
			const selector = query.email?.trim() ? 'email' : query.last_active_ip?.trim() ? 'last_active_ip' : 'search';
			if (selector === 'email') {
				requireGrantedAdminACLs(adminUserAcls, [AdminACLs.USER_VIEW_EMAIL]);
			} else if (selector === 'last_active_ip') {
				requireGrantedAdminACLs(adminUserAcls, [AdminACLs.USER_VIEW_IP]);
			}
			const response = await adminService.searchService.searchUsers(
				{
					query: query.q,
					email: query.email,
					last_active_ip: query.last_active_ip,
					limit: query.limit,
					offset: query.offset,
				},
				adminUserAcls,
			);
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: 0n,
				action: AdminAuditReadActions.SEARCH_USERS,
				metadata: {
					selector,
					has_query: selector === 'search' && query.q?.trim() ? true : undefined,
					limit: selector === 'email' ? undefined : query.limit,
					offset: selector === 'email' ? undefined : query.offset,
					result_count: response.users.length,
					total: response.total,
				},
			});
			return ctx.json(response);
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
			const response = await adminService.userService.lookupService.lookupUser({user_ids: [userId]}, adminUserAcls);
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: userId,
				action: AdminAuditReadActions.GET_USER,
				metadata: {found: response.users.length > 0},
			});
			return ctx.json(response);
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
			const response = await adminService.guildServiceAggregate.lookupService.listUserGuilds({
				user_id: userId,
				...query,
			});
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: userId,
				action: AdminAuditReadActions.LIST_USER_GUILDS,
				metadata: {
					before_guild_id: query.before,
					after_guild_id: query.after,
					limit: query.limit,
					with_counts: query.with_counts,
					guild_count: response.guilds.length,
				},
			});
			return ctx.json(response);
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
				const response = await adminService.userService.listUserGroupDmChannels({user_id: userId});
				await recordAdminRead(ctx, {
					targetType: 'user',
					targetId: userId,
					action: AdminAuditReadActions.LIST_USER_DM_CHANNELS,
					metadata: {type, channel_count: response.channels.length},
				});
				return ctx.json(response);
			}
			const response = await adminService.userService.listUserDmChannels({user_id: userId, ...pagination});
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: userId,
				action: AdminAuditReadActions.LIST_USER_DM_CHANNELS,
				metadata: {
					type,
					before_channel_id: pagination.before,
					after_channel_id: pagination.after,
					limit: pagination.limit,
					channel_count: response.channels.length,
				},
			});
			return ctx.json(response);
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
			const response = await adminService.userService.listUserChangeLog({user_id: userId, ...query}, adminUserAcls);
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: userId,
				action: AdminAuditReadActions.LIST_USER_CHANGE_LOG,
				metadata: {
					limit: query.limit,
					has_page_token: query.page_token === undefined ? undefined : true,
					entry_count: response.entries.length,
				},
			});
			return ctx.json(response);
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
			const response = await adminService.relationshipService.listRelationships({user_id: userId});
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: userId,
				action: AdminAuditReadActions.LIST_USER_RELATIONSHIPS,
				metadata: {
					friend_count: response.friends.length,
					incoming_request_count: response.incoming_requests.length,
					outgoing_request_count: response.outgoing_requests.length,
					blocked_count: response.blocked.length,
				},
			});
			return ctx.json(response);
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
				'Clears profile fields such as the bio, avatar or display name. An enforcement basis saves the old values as evidence, records a profile reset decision and sends its notice by system DM and email. Creates audit log entry. Requires USER_UPDATE_PROFILE permission.',
		}),
		async (ctx) => {
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await ctx
					.get('adminService')
					.userService.profileService.clearUserFields(
						createUserID(userId),
						ctx.req.valid('json'),
						enforcementActor(ctx),
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
		RequireEmailAccountIdentity,
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
		RequireEmailAccountIdentity,
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
		RequireEmailAccountIdentity,
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
		RequireEmailAccountIdentity,
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
	app.post(
		'/admin/users/:user_id/password-reset-link',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_CREATE_PASSWORD_RESET_LINK),
		RequireUsernameAccountIdentity,
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'create_admin_user_password_reset_link',
			summary: 'Create user password reset link',
			responseSchema: AdminPasswordResetLinkResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Create a one-time password reset link on an instance where people sign in with a username. Hand the link to the user yourself. It works once and expires after an hour. Deletes the recovery kit of the account. Creates audit log entry. Requires USER_CREATE_PASSWORD_RESET_LINK permission and every ACL the target account holds. Fails with USERNAME_SIGN_IN_ONLY on email instances.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const adminUserId = ctx.get('adminUserId');
			const auditLogReason = ctx.get('auditLogReason');
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await adminService.userService.securityService.createPasswordResetLink(
					{user_id: userId},
					adminUserId,
					auditLogReason,
					ctx.get('adminUserAcls'),
				),
			);
		},
	);
	app.delete(
		'/admin/users/:user_id/recovery-kit',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DELETE_RECOVERY_KIT),
		RequireUsernameAccountIdentity,
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'revoke_admin_user_recovery_kit',
			summary: 'Revoke user recovery kit',
			responseSchema: null,
			statusCode: 204,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Deletes the recovery kit of an account on an instance where people sign in with a username, so its key stops working. Creates audit log entry. Requires USER_DELETE_RECOVERY_KIT permission and every ACL the target account holds. Fails with USERNAME_SIGN_IN_ONLY on email instances.',
		}),
		async (ctx) => {
			const adminService = ctx.get('adminService');
			const {user_id: userId} = ctx.req.valid('param');
			await adminService.userService.securityService.revokeRecoveryKit(
				{user_id: userId},
				ctx.get('adminUserId'),
				ctx.get('auditLogReason'),
				ctx.get('adminUserAcls'),
			);
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
			summary: 'Suspend user',
			responseSchema: UserRestrictionResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Suspend the account for duration_hours, or permanently with a duration of zero, and record the enforcement decision behind it. The decision is written first, then the suspension, then linked reports are resolved and the notice is sent. The X-Audit-Log-Reason header is required and becomes the first note. remove_evidence queues removal of the evidence messages after they are snapshotted. block_networks blocks addresses the account used in the last 30 days, for 90 days unless days says otherwise, and only with a permanent suspension. Exempt and special-purpose addresses, and addresses already blocked permanently or by another decision, are skipped and listed in skipped_ips. An address on which another account that is not suspended or closed was last active is refused with ENFORCEMENT_NETWORK_SHARED. Returns 409 when the account is closed. Requires USER_TEMP_BAN permission, plus MESSAGE_LOOKUP for evidence, REPORT_VIEW and REPORT_RESOLVE for reports, MESSAGE_SHRED for remove_evidence, and BAN_IP_ADD and USER_VIEW_IP for block_networks.',
		}),
		async (ctx) => {
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await ctx
					.get('adminService')
					.banService.tempBanUser(createUserID(userId), ctx.req.valid('json'), enforcementActor(ctx)),
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
			summary: 'Lift a suspension without a decision',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Lift a suspension that no enforcement decision owns, such as one from before decisions existed. Returns 409 with the decision ID when a decision owns the suspension, which is then ended through its end route. Emails the user that the restriction ended when the suspension was still in force. Creates audit log entry. Requires USER_TEMP_BAN permission.',
		}),
		async (ctx) => {
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(await ctx.get('adminService').banService.unbanUser(createUserID(userId), enforcementActor(ctx)));
		},
	);
	app.post(
		'/admin/users/:user_id/warnings',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('param', UserIdParam),
		Validator('json', AdminUserWarningRequest),
		OpenAPI({
			operationId: 'warn_admin_user',
			summary: 'Warn user',
			responseSchema: AdminUserWarningResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Record a warning decision and send its notice by email and system DM. Nothing about the account changes. The X-Audit-Log-Reason header is required and becomes the first note. Refused for reasons that only allow a suspension or content removal. Requires USER_UPDATE_FLAGS permission, plus MESSAGE_LOOKUP for evidence and REPORT_VIEW and REPORT_RESOLVE for reports.',
		}),
		async (ctx) => {
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await ctx
					.get('adminService')
					.banService.warnUser(createUserID(userId), ctx.req.valid('json'), enforcementActor(ctx)),
			);
		},
	);
	app.put(
		'/admin/users/:user_id/feature-limits',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_UPDATE_FLAGS),
		Validator('param', UserIdParam),
		Validator('json', AdminUserFeatureLimitRequest),
		OpenAPI({
			operationId: 'limit_admin_user_features',
			summary: 'Limit user features',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Record an enforcement decision that limits one feature and send its notice by email and system DM. Direct messages set SPAMMER and are allowed for spam, automation, fraud and malware reasons only. Reports set REPORT_BANNED until ends_at, 90 days by default and at most 365, and expire on their own. Purchases set PURCHASE_DISABLED for payment reasons. The X-Audit-Log-Reason header is required and becomes the first note. Requires USER_UPDATE_FLAGS permission, plus MESSAGE_LOOKUP for evidence and REPORT_VIEW and REPORT_RESOLVE for reports.',
		}),
		async (ctx) => {
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await ctx
					.get('adminService')
					.banService.limitFeatures(createUserID(userId), ctx.req.valid('json'), enforcementActor(ctx)),
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
			responseSchema: UserRestrictionResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Close the account and schedule its deletion. The basis says why. A broken rule records an enforcement decision kind closure, keeps the account for at least 74 days so an appeal can be reviewed first (14 days on a self-hosted instance, which has no appeals), sends the notice and, when the rule bars new accounts, bans the account email. The X-Audit-Log-Reason header is then required and becomes the first note. A holder request or housekeeping keeps the account for at least 14 days, sends a neutral email, and is cancelled when the holder signs in. Those two return 409 while an enforcement decision owns the account state. When a deletion is already scheduled, the request must name it in replace_pending_deletion_at or it returns 409. block_networks works as on the suspend route and is refused for the other bases. Executing the deletion removes the blocks. Creates audit log entry. Requires USER_DELETE permission, plus MESSAGE_LOOKUP for evidence, REPORT_VIEW and REPORT_RESOLVE for reports, MESSAGE_SHRED for remove_evidence, and BAN_IP_ADD and USER_VIEW_IP for block_networks.',
		}),
		async (ctx) => {
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await ctx
					.get('adminService')
					.userService.deletionService.scheduleAccountDeletion(
						createUserID(userId),
						ctx.req.valid('json'),
						enforcementActor(ctx),
					),
			);
		},
	);
	app.delete(
		'/admin/users/:user_id/deletion',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.USER_DELETE),
		Validator('param', UserIdParam),
		Validator('json', AdminUserDeletionCancelRequest),
		OpenAPI({
			operationId: 'cancel_admin_user_deletion',
			summary: 'Cancel user deletion',
			responseSchema: UserMutationResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Cancel a scheduled deletion that no enforcement decision owns, named by expected_pending_deletion_at, such as one the holder asked for, an inactivity deletion or a closure from before decisions existed. Returns 409 with the decision ID when a decision owns the deletion, which is then ended through its end route, 409 when a different deletion is pending and 400 when none is. Emails the user that the deletion was cancelled, or that the restriction ended for a closure from before decisions existed. Creates audit log entry recording the cancelled deletion. Requires USER_DELETE permission.',
		}),
		async (ctx) => {
			const {user_id: userId} = ctx.req.valid('param');
			return ctx.json(
				await ctx
					.get('adminService')
					.userService.deletionService.cancelAccountDeletion(
						createUserID(userId),
						ctx.req.valid('json'),
						enforcementActor(ctx),
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
				'Add or remove user flags. Flags that record a restriction go through their own routes: adding DISABLED, DELETED, SPAMMER or REPORT_BANNED and removing DELETED return 400, removing DISABLED while a suspension is set returns 400, and removing SPAMMER or REPORT_BANNED while an enforcement decision holds it returns 409 with the decision ID. A self-disabled account can still have DISABLED removed. Creates audit log entry. Requires USER_UPDATE_FLAGS permission.',
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
				'Add or remove premium-related flags on a user account (badge visibility, override, purchase block, etc). Adding PURCHASE_DISABLED returns 400, because a purchase limit is recorded through the feature limits route, and removing it while an enforcement decision holds it returns 409 with the decision ID. Creates audit log entry. Requires USER_UPDATE_FLAGS permission.',
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
}

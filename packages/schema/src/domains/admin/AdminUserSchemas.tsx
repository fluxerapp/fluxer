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

import {
	SuspiciousActivityFlags,
	SuspiciousActivityFlagsDescriptions,
	UserFlags,
	UserFlagsDescriptions,
} from '@fluxer/constants/src/UserConstants';
import {
	createBitflagInt32Type,
	createBitflagStringType,
	createNamedStringLiteralUnion,
	createStringType,
	Int32Type,
	SnowflakeStringType,
	SnowflakeType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {EmailType, UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {z} from 'zod';

export const UserAdminResponseSchema = z.object({
	id: SnowflakeStringType,
	username: z.string(),
	discriminator: Int32Type,
	global_name: z.string().nullable(),
	bot: z.boolean(),
	system: z.boolean(),
	flags: createBitflagStringType(UserFlags, UserFlagsDescriptions, 'User account flags (64-bit)', 'UserFlags'),
	avatar: z.string().nullable(),
	banner: z.string().nullable(),
	bio: z.string().nullable(),
	pronouns: z.string().nullable(),
	accent_color: Int32Type.nullable(),
	email: z.string().nullable(),
	email_verified: z.boolean(),
	email_bounced: z.boolean(),
	phone: z.string().nullable(),
	date_of_birth: z.string().nullable(),
	locale: z.string().nullable(),
	premium_type: Int32Type.nullable(),
	premium_since: z.string().nullable(),
	premium_until: z.string().nullable(),
	suspicious_activity_flags: createBitflagInt32Type(
		SuspiciousActivityFlags,
		SuspiciousActivityFlagsDescriptions,
		'Suspicious activity indicators',
		'SuspiciousActivityFlags',
	),
	temp_banned_until: z.string().nullable(),
	pending_deletion_at: z.string().nullable(),
	pending_bulk_message_deletion_at: z.string().nullable(),
	deletion_reason_code: Int32Type.nullable(),
	deletion_public_reason: z.string().nullable(),
	acls: z.array(z.string()).max(100),
	traits: z.array(z.string()).max(100),
	has_totp: z.boolean(),
	authenticator_types: z.array(Int32Type).max(10),
	last_active_at: z.string().nullable(),
	last_active_ip: z.string().nullable(),
	last_active_ip_reverse: z.string().nullable(),
	last_active_location: z.string().nullable(),
});

export type UserAdminResponse = z.infer<typeof UserAdminResponseSchema>;

export const LookupUserByQueryRequest = z.object({
	query: createStringType(1, 1024),
});

export type LookupUserByQueryRequest = z.infer<typeof LookupUserByQueryRequest>;

export const LookupUserByIdsRequest = z.object({
	user_ids: z.array(SnowflakeType).max(100),
});

export type LookupUserByIdsRequest = z.infer<typeof LookupUserByIdsRequest>;

export const LookupUserRequest = z.union([LookupUserByQueryRequest, LookupUserByIdsRequest]);

export type LookupUserRequest = z.infer<typeof LookupUserRequest>;

export const SearchUsersRequest = z.object({
	query: createStringType(1, 1024).optional(),
	limit: z.number().int().min(1).max(200).default(50),
	offset: z.number().int().min(0).default(0),
});

export type SearchUsersRequest = z.infer<typeof SearchUsersRequest>;

export const ListUserSessionsRequest = z.object({
	user_id: SnowflakeType,
});

export type ListUserSessionsRequest = z.infer<typeof ListUserSessionsRequest>;

export const UserContactChangeLogEntrySchema = z.object({
	event_id: z.string(),
	field: z.string(),
	old_value: z.string().nullable(),
	new_value: z.string().nullable(),
	reason: z.string().nullable(),
	actor_user_id: z.string().nullable(),
	event_at: z.string(),
});

export type UserContactChangeLogEntry = z.infer<typeof UserContactChangeLogEntrySchema>;

export const ListUserChangeLogResponseSchema = z.object({
	entries: z.array(UserContactChangeLogEntrySchema).max(200),
	next_page_token: z.string().nullable(),
});

export type ListUserChangeLogResponse = z.infer<typeof ListUserChangeLogResponseSchema>;

export const AdminUsersMeResponse = z.object({
	user: UserAdminResponseSchema,
});

export type AdminUsersMeResponse = z.infer<typeof AdminUsersMeResponse>;

export const UserMutationResponse = z.object({
	user: UserAdminResponseSchema,
});

export type UserMutationResponse = z.infer<typeof UserMutationResponse>;

export const VerificationActionResponse = z.object({
	success: z.boolean(),
});

export type VerificationActionResponse = z.infer<typeof VerificationActionResponse>;

export const LookupUserResponse = z.object({
	users: z.array(UserAdminResponseSchema).max(100),
});

export type LookupUserResponse = z.infer<typeof LookupUserResponse>;

export const UserSessionResponse = z.object({
	session_id_hash: createStringType(8, 256).describe('Hashed session identifier (base64url)'),
	created_at: z.string().describe('ISO timestamp when the session was created'),
	approx_last_used_at: z.string().describe('ISO timestamp of the session last usage (approximate)'),
	client_ip: createStringType(1, 64).describe('Client IP address'),
	client_ip_reverse: z.string().nullable().describe('Reverse DNS hostname for the client IP (PTR), if available'),
	client_os: z.string().nullable().describe('Client operating system, if detected'),
	client_platform: z.string().nullable().describe('Client platform, if detected'),
	client_location: z.string().nullable().describe('Approximate geo location label for the client IP, if available'),
});

export type UserSessionResponse = z.infer<typeof UserSessionResponse>;

export const ListUserSessionsResponse = z.object({
	sessions: z.array(UserSessionResponse).max(100),
});

export type ListUserSessionsResponse = z.infer<typeof ListUserSessionsResponse>;

export const ListUserDmChannelsRequest = z
	.object({
		user_id: SnowflakeType.describe('ID of the user to list DM channels for'),
		before: SnowflakeType.optional().describe('Return channels with IDs lower than this channel ID'),
		after: SnowflakeType.optional().describe('Return channels with IDs higher than this channel ID'),
		limit: z.number().int().min(1).max(200).default(50).describe('Maximum number of DM channels to return'),
	})
	.refine((value) => value.before === undefined || value.after === undefined, {
		message: 'before and after cannot both be provided',
	});

export type ListUserDmChannelsRequest = z.infer<typeof ListUserDmChannelsRequest>;

export const AdminUserDmChannelSchema = z.object({
	channel_id: SnowflakeStringType,
	channel_type: Int32Type.nullable(),
	recipient_ids: z.array(SnowflakeStringType).max(100),
	last_message_id: SnowflakeStringType.nullable(),
	is_open: z.boolean(),
});

export type AdminUserDmChannel = z.infer<typeof AdminUserDmChannelSchema>;

export const ListUserDmChannelsResponse = z.object({
	channels: z.array(AdminUserDmChannelSchema).max(200),
});

export type ListUserDmChannelsResponse = z.infer<typeof ListUserDmChannelsResponse>;

export const TerminateSessionsResponse = z.object({
	terminated_count: Int32Type,
});

export type TerminateSessionsResponse = z.infer<typeof TerminateSessionsResponse>;

const UserFlagValueType = createBitflagStringType(
	UserFlags,
	UserFlagsDescriptions,
	'A single user flag value to add or remove',
	'UserFlags',
);

export const UpdateUserFlagsRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to update'),
	add_flags: z.array(UserFlagValueType).max(64).default([]).describe('User flags to add'),
	remove_flags: z.array(UserFlagValueType).max(64).default([]).describe('User flags to remove'),
});

export type UpdateUserFlagsRequest = z.infer<typeof UpdateUserFlagsRequest>;

export const DisableMfaRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to disable MFA for'),
});

export type DisableMfaRequest = z.infer<typeof DisableMfaRequest>;

export const CancelBulkMessageDeletionRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to cancel bulk message deletion for'),
});

export type CancelBulkMessageDeletionRequest = z.infer<typeof CancelBulkMessageDeletionRequest>;

const UserProfileFieldEnum = createNamedStringLiteralUnion(
	[
		['avatar', 'avatar', 'User profile avatar image'],
		['banner', 'banner', 'User profile banner image'],
		['bio', 'bio', 'User biography text'],
		['pronouns', 'pronouns', 'User pronouns'],
		['global_name', 'global_name', 'User display name'],
	],
	'User profile field that can be cleared',
);

export const ClearUserFieldsRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to clear fields for'),
	fields: z.array(UserProfileFieldEnum).max(10).describe('List of profile fields to clear'),
});

export type ClearUserFieldsRequest = z.infer<typeof ClearUserFieldsRequest>;

export const SetUserBotStatusRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to update'),
	bot: z.boolean().describe('Whether the user should be marked as a bot'),
});

export type SetUserBotStatusRequest = z.infer<typeof SetUserBotStatusRequest>;

export const SetUserSystemStatusRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to update'),
	system: z.boolean().describe('Whether the user should be marked as a system user'),
});

export type SetUserSystemStatusRequest = z.infer<typeof SetUserSystemStatusRequest>;

export const VerifyUserEmailRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to verify email for'),
});

export type VerifyUserEmailRequest = z.infer<typeof VerifyUserEmailRequest>;

export const ResendVerificationEmailRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to resend verification email to'),
});

export type ResendVerificationEmailRequest = z.infer<typeof ResendVerificationEmailRequest>;

export const SendPasswordResetRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to send password reset to'),
});

export type SendPasswordResetRequest = z.infer<typeof SendPasswordResetRequest>;

export const ChangeUsernameRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to change username for'),
	username: UsernameType.describe('New username for the user'),
	discriminator: Int32Type.optional().describe('Legacy discriminator value'),
});

export type ChangeUsernameRequest = z.infer<typeof ChangeUsernameRequest>;

export const ChangeEmailRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to change email for'),
	email: EmailType.describe('New email address for the user'),
});

export type ChangeEmailRequest = z.infer<typeof ChangeEmailRequest>;

export const TerminateSessionsRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to terminate sessions for'),
});

export type TerminateSessionsRequest = z.infer<typeof TerminateSessionsRequest>;

export const TempBanUserRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to temporarily ban'),
	duration_hours: z
		.number()
		.int()
		.min(0)
		.max(8760)
		.describe('Duration of the ban in hours. Use 0 for a permanent ban (until manually unbanned).'),
	reason: createStringType(0, 512).optional().describe('Reason for the temporary ban'),
});

export type TempBanUserRequest = z.infer<typeof TempBanUserRequest>;

export const ScheduleAccountDeletionRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to schedule deletion for'),
	reason_code: Int32Type.describe('Code indicating the reason for deletion'),
	public_reason: createStringType(0, 512).optional().describe('Public-facing reason for the deletion'),
	days_until_deletion: z
		.number()
		.int()
		.min(1)
		.max(365)
		.default(60)
		.describe('Number of days until the account is deleted'),
});

export type ScheduleAccountDeletionRequest = z.infer<typeof ScheduleAccountDeletionRequest>;

export const SetUserAclsRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to set ACLs for'),
	acls: z.array(createStringType(1, 64)).max(100).describe('List of access control permissions to assign'),
});

export type SetUserAclsRequest = z.infer<typeof SetUserAclsRequest>;

export const SetUserTraitsRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to set traits for'),
	traits: z.array(createStringType(1, 64)).max(100).describe('List of traits to assign to the user'),
});

export type SetUserTraitsRequest = z.infer<typeof SetUserTraitsRequest>;

export const UnlinkPhoneRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to unlink phone from'),
});

export type UnlinkPhoneRequest = z.infer<typeof UnlinkPhoneRequest>;

export const ChangeDobRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to change date of birth for'),
	date_of_birth: createStringType(10, 10)
		.refine((value) => /^\d{4}-\d{2}-\d{2}$/.test(value), 'Invalid date format')
		.describe('New date of birth in YYYY-MM-DD format'),
});

export type ChangeDobRequest = z.infer<typeof ChangeDobRequest>;

export const UpdateSuspiciousActivityFlagsRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to update suspicious activity flags for'),
	flags: createBitflagInt32Type(
		SuspiciousActivityFlags,
		SuspiciousActivityFlagsDescriptions,
		'Bitmask of suspicious activity flags',
		'SuspiciousActivityFlags',
	),
});

export type UpdateSuspiciousActivityFlagsRequest = z.infer<typeof UpdateSuspiciousActivityFlagsRequest>;

export const DisableForSuspiciousActivityRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to disable for suspicious activity'),
	flags: createBitflagInt32Type(
		SuspiciousActivityFlags,
		SuspiciousActivityFlagsDescriptions,
		'Bitmask of suspicious activity flags that triggered the disable',
		'SuspiciousActivityFlags',
	),
});

export type DisableForSuspiciousActivityRequest = z.infer<typeof DisableForSuspiciousActivityRequest>;

export const BulkUpdateUserFlagsRequest = z.object({
	user_ids: z.array(SnowflakeType).max(1000).describe('List of user IDs to update'),
	add_flags: z.array(UserFlagValueType).max(64).default([]).describe('User flags to add to all specified users'),
	remove_flags: z
		.array(UserFlagValueType)
		.max(64)
		.default([])
		.describe('User flags to remove from all specified users'),
});

export type BulkUpdateUserFlagsRequest = z.infer<typeof BulkUpdateUserFlagsRequest>;

export const BulkScheduleUserDeletionRequest = z.object({
	user_ids: z.array(SnowflakeType).max(1000).describe('List of user IDs to schedule deletion for'),
	reason_code: Int32Type.describe('Code indicating the reason for deletion'),
	public_reason: createStringType(0, 512).optional().describe('Public-facing reason for the deletion'),
	days_until_deletion: z
		.number()
		.int()
		.min(1)
		.max(365)
		.default(60)
		.describe('Number of days until the accounts are deleted'),
});

export type BulkScheduleUserDeletionRequest = z.infer<typeof BulkScheduleUserDeletionRequest>;

export const ListWebAuthnCredentialsRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to list WebAuthn credentials for'),
});

export type ListWebAuthnCredentialsRequest = z.infer<typeof ListWebAuthnCredentialsRequest>;

export const DeleteWebAuthnCredentialRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user who owns the credential'),
	credential_id: createStringType(1, 512).describe('ID of the WebAuthn credential to delete'),
});

export type DeleteWebAuthnCredentialRequest = z.infer<typeof DeleteWebAuthnCredentialRequest>;

export const ListUserChangeLogRequest = z.object({
	user_id: SnowflakeType.describe('ID of the user to list change logs for'),
	limit: z.number().min(1).max(200).default(50).describe('Maximum number of entries to return'),
	page_token: z.string().optional().describe('Pagination token for the next page of results'),
});

export type ListUserChangeLogRequest = z.infer<typeof ListUserChangeLogRequest>;

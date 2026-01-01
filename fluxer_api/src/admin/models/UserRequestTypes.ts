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

import {createStringType, EmailType, Int64Type, UsernameType, z} from '~/Schema';

export const UpdateUserFlagsRequest = z.object({
	user_id: Int64Type,
	add_flags: z.array(Int64Type).default([]),
	remove_flags: z.array(Int64Type).default([]),
});

export type UpdateUserFlagsRequest = z.infer<typeof UpdateUserFlagsRequest>;

export const DisableMfaRequest = z.object({
	user_id: Int64Type,
});

export type DisableMfaRequest = z.infer<typeof DisableMfaRequest>;

export const CancelBulkMessageDeletionRequest = z.object({
	user_id: Int64Type,
});

export type CancelBulkMessageDeletionRequest = z.infer<typeof CancelBulkMessageDeletionRequest>;

export const ClearUserFieldsRequest = z.object({
	user_id: Int64Type,
	fields: z.array(z.enum(['avatar', 'banner', 'bio', 'pronouns', 'global_name'])),
});

export type ClearUserFieldsRequest = z.infer<typeof ClearUserFieldsRequest>;

export const SetUserBotStatusRequest = z.object({
	user_id: Int64Type,
	bot: z.boolean(),
});

export type SetUserBotStatusRequest = z.infer<typeof SetUserBotStatusRequest>;

export const SetUserSystemStatusRequest = z.object({
	user_id: Int64Type,
	system: z.boolean(),
});

export type SetUserSystemStatusRequest = z.infer<typeof SetUserSystemStatusRequest>;

export const VerifyUserEmailRequest = z.object({
	user_id: Int64Type,
});

export type VerifyUserEmailRequest = z.infer<typeof VerifyUserEmailRequest>;

export const SendPasswordResetRequest = z.object({
	user_id: Int64Type,
});

export type SendPasswordResetRequest = z.infer<typeof SendPasswordResetRequest>;

export const ChangeUsernameRequest = z.object({
	user_id: Int64Type,
	username: UsernameType,
	discriminator: z.number().optional(),
});

export type ChangeUsernameRequest = z.infer<typeof ChangeUsernameRequest>;

export const ChangeEmailRequest = z.object({
	user_id: Int64Type,
	email: EmailType,
});

export type ChangeEmailRequest = z.infer<typeof ChangeEmailRequest>;

export const TerminateSessionsRequest = z.object({
	user_id: Int64Type,
});

export type TerminateSessionsRequest = z.infer<typeof TerminateSessionsRequest>;

export const TempBanUserRequest = z.object({
	user_id: Int64Type,
	duration_hours: z.number(),
	reason: createStringType(0, 512).optional(),
});

export type TempBanUserRequest = z.infer<typeof TempBanUserRequest>;

export const ScheduleAccountDeletionRequest = z.object({
	user_id: Int64Type,
	reason_code: z.number(),
	public_reason: createStringType(0, 512).optional(),
	days_until_deletion: z.number().default(60),
});

export type ScheduleAccountDeletionRequest = z.infer<typeof ScheduleAccountDeletionRequest>;

export const SetUserAclsRequest = z.object({
	user_id: Int64Type,
	acls: z.array(createStringType(1, 64)),
});

export type SetUserAclsRequest = z.infer<typeof SetUserAclsRequest>;

export const UnlinkPhoneRequest = z.object({
	user_id: Int64Type,
});

export type UnlinkPhoneRequest = z.infer<typeof UnlinkPhoneRequest>;

export const ChangeDobRequest = z.object({
	user_id: Int64Type,
	date_of_birth: createStringType(10, 10).refine((value) => /^\d{4}-\d{2}-\d{2}$/.test(value), 'Invalid date format'),
});

export type ChangeDobRequest = z.infer<typeof ChangeDobRequest>;

export const UpdateSuspiciousActivityFlagsRequest = z.object({
	user_id: Int64Type,
	flags: z.number(),
});

export type UpdateSuspiciousActivityFlagsRequest = z.infer<typeof UpdateSuspiciousActivityFlagsRequest>;

export const DisableForSuspiciousActivityRequest = z.object({
	user_id: Int64Type,
	flags: z.number(),
});

export type DisableForSuspiciousActivityRequest = z.infer<typeof DisableForSuspiciousActivityRequest>;

export const BulkUpdateUserFlagsRequest = z.object({
	user_ids: z.array(Int64Type),
	add_flags: z.array(Int64Type).default([]),
	remove_flags: z.array(Int64Type).default([]),
});

export type BulkUpdateUserFlagsRequest = z.infer<typeof BulkUpdateUserFlagsRequest>;

export const BulkScheduleUserDeletionRequest = z.object({
	user_ids: z.array(Int64Type),
	reason_code: z.number(),
	public_reason: createStringType(0, 512).optional(),
	days_until_deletion: z.number().default(60),
});

export type BulkScheduleUserDeletionRequest = z.infer<typeof BulkScheduleUserDeletionRequest>;

export const ListUserChangeLogRequest = z.object({
	user_id: Int64Type,
	limit: z.number().min(1).max(200).default(50),
	page_token: z.string().optional(),
});

export type ListUserChangeLogRequest = z.infer<typeof ListUserChangeLogRequest>;

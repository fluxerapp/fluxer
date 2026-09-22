// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BulkAddGuildMembersRequest,
	BulkUpdateGuildFeaturesRequest,
} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import {BulkDeleteUserMessagesRequest} from '@fluxer/schema/src/domains/admin/AdminMessageSchemas';
import {
	BulkScheduleUserDeletionRequest,
	BulkUpdateSuspiciousActivityFlagsRequest,
	BulkUpdateUserFlagsRequest,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {z} from 'zod';

export const AdminBulkTaskType = {
	UPDATE_USER_FLAGS: 'update_user_flags',
	UPDATE_SUSPICIOUS_ACTIVITY_FLAGS: 'update_suspicious_activity_flags',
	UPDATE_GUILD_FEATURES: 'update_guild_features',
	ADD_GUILD_MEMBERS: 'add_guild_members',
	SCHEDULE_USER_DELETION: 'schedule_user_deletion',
	DELETE_USER_MESSAGES: 'delete_user_messages',
} as const;

export type AdminBulkTaskType = (typeof AdminBulkTaskType)[keyof typeof AdminBulkTaskType];

export const AdminBulkJobCreateRequest = z.discriminatedUnion('task', [
	BulkUpdateUserFlagsRequest.extend({
		task: z
			.literal(AdminBulkTaskType.UPDATE_USER_FLAGS)
			.describe('Adds and removes account flags on every targeted user'),
	}),
	BulkUpdateSuspiciousActivityFlagsRequest.extend({
		task: z
			.literal(AdminBulkTaskType.UPDATE_SUSPICIOUS_ACTIVITY_FLAGS)
			.describe('Adds and removes verification requirements on every targeted user'),
	}),
	BulkUpdateGuildFeaturesRequest.extend({
		task: z
			.literal(AdminBulkTaskType.UPDATE_GUILD_FEATURES)
			.describe('Adds and removes features on every targeted guild'),
	}),
	BulkAddGuildMembersRequest.extend({
		task: z.literal(AdminBulkTaskType.ADD_GUILD_MEMBERS).describe('Adds every targeted user to one guild'),
	}),
	BulkScheduleUserDeletionRequest.extend({
		task: z
			.literal(AdminBulkTaskType.SCHEDULE_USER_DELETION)
			.describe('Schedules account deletion for every targeted user'),
	}),
	BulkDeleteUserMessagesRequest.extend({
		task: z
			.literal(AdminBulkTaskType.DELETE_USER_MESSAGES)
			.describe('Deletes every message authored by each targeted user, across all channels'),
	}),
]);

export type AdminBulkJobCreateRequest = z.infer<typeof AdminBulkJobCreateRequest>;

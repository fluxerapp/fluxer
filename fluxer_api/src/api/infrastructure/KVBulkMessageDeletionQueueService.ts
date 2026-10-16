// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {UserScheduledKvQueue} from '@app/api/infrastructure/UserScheduledKvQueue';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {getValidTimestamp, parseStoredTimestamp} from '@app/api/utils/TimestampUtils';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

interface QueuedBulkMessageDeletion {
	userId: bigint;
	scheduledAt: number;
}

export class KVBulkMessageDeletionQueueService extends UserScheduledKvQueue<QueuedBulkMessageDeletion, []> {
	constructor(kvClient: IKVProvider, userRepository: UserRepository) {
		super(kvClient, userRepository, {
			queueKey: 'bulk_message_deletion_queue',
			stateVersionKey: 'bulk_message_deletion_queue:state_version',
			rebuildLockKey: 'bulk_message_deletion_queue:rebuild_lock',
			secondaryKey: (userId) => `bulk_message_deletion_queue:${userId}`,
			label: 'Bulk message deletion queue',
			timestampLabel: 'Pending bulk message deletion',
			encodePayload: (item) => item.scheduledAt.toString(),
			decodePayload: (userId, payload) => {
				const scheduledAt = parseStoredTimestamp(payload, 'Bulk message deletion queue timestamp');
				assert(scheduledAt !== null, 'A non-null bulk message deletion timestamp must not parse as null');
				return {userId, scheduledAt};
			},
			scheduledItem: (userId, score) => ({userId, scheduledAt: score}),
			rebuildEntry: (user) => {
				if (!user.pendingBulkMessageDeletionAt) return null;
				const score = getValidTimestamp(
					user.pendingBulkMessageDeletionAt,
					`Pending bulk message deletion timestamp for user ${user.id}`,
				);
				return {score, item: {userId: user.id, scheduledAt: score}};
			},
		});
	}
}

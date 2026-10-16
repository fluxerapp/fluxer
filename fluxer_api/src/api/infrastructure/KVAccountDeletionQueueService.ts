// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserScheduledKvQueue} from '@app/api/infrastructure/UserScheduledKvQueue';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {
	isPendingDeletionBlocked,
	resolvePendingDeletionReasonCode,
} from '@app/api/user/services/PendingDeletionCoordinator';
import {getValidTimestamp} from '@app/api/utils/TimestampUtils';
import {Int32Type} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

interface QueuedDeletion {
	userId: bigint;
	deletionReasonCode: number;
}

function validateReasonCode(value: number): number {
	const parsed = Int32Type.safeParse(value);
	if (!parsed.success) {
		throw new TypeError('Deletion queue reason code must be a decimal nonnegative 32-bit integer');
	}
	return parsed.data;
}

export class KVAccountDeletionQueueService extends UserScheduledKvQueue<QueuedDeletion, [number]> {
	constructor(kvClient: IKVProvider, userRepository: UserRepository) {
		super(kvClient, userRepository, {
			queueKey: 'deletion_queue',
			stateVersionKey: 'deletion_queue:state_version',
			rebuildLockKey: 'deletion_queue:rebuild_lock',
			secondaryKey: (userId) => `deletion_queue_by_user:${userId}`,
			label: 'Deletion queue',
			timestampLabel: 'Pending deletion',
			encodePayload: (item) => validateReasonCode(item.deletionReasonCode).toString(),
			decodePayload: (userId, payload) => {
				if (payload.length > 10 || /\D/.test(payload))
					throw new TypeError('Deletion queue reason code must be a decimal nonnegative 32-bit integer');
				return {userId, deletionReasonCode: validateReasonCode(Number(payload))};
			},
			scheduledItem: (userId, _score, reasonCode) => ({userId, deletionReasonCode: reasonCode}),
			rebuildEntry: (user) => {
				if (!user.pendingDeletionAt || (!user.deletionStartedAt && isPendingDeletionBlocked(user))) return null;
				return {
					score: getValidTimestamp(user.pendingDeletionAt, `Pending deletion timestamp for user ${user.id}`),
					item: {userId: user.id, deletionReasonCode: resolvePendingDeletionReasonCode(user, 0)},
				};
			},
		});
	}
}

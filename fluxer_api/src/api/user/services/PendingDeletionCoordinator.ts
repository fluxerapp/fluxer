// SPDX-License-Identifier: AGPL-3.0-or-later

import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {UserID} from '../../BrandedTypes';

interface PendingDeletionRepositoryLike {
	addPendingDeletion(userId: UserID, pendingDeletionAt: Date, deletionReasonCode: number): Promise<void>;
	removePendingDeletion(userId: UserID, pendingDeletionAt: Date): Promise<void>;
}

interface PendingDeletionQueueLike {
	scheduleDeletion(userId: UserID, pendingAt: Date, reasonCode: number): Promise<void>;
	removeFromQueue(userId: UserID): Promise<void>;
}

interface ReschedulePendingDeletionParams {
	userId: UserID;
	currentPendingDeletionAt?: Date | null;
	nextPendingDeletionAt: Date;
	deletionReasonCode: number;
	userRepository: PendingDeletionRepositoryLike;
	deletionQueue: PendingDeletionQueueLike;
}

interface ClearPendingDeletionParams {
	userId: UserID;
	pendingDeletionAt?: Date | null;
	userRepository: PendingDeletionRepositoryLike;
	deletionQueue: Pick<PendingDeletionQueueLike, 'removeFromQueue'>;
}

interface PendingDeletionReasonUserLike {
	deletionReasonCode?: number | null;
	flags: bigint;
}

interface PendingDeletionEligibilityUserLike {
	isBot: boolean;
	flags: bigint;
}

export async function reschedulePendingDeletion({
	userId,
	currentPendingDeletionAt,
	nextPendingDeletionAt,
	deletionReasonCode,
	userRepository,
	deletionQueue,
}: ReschedulePendingDeletionParams): Promise<void> {
	if (currentPendingDeletionAt) {
		await deletionQueue.removeFromQueue(userId);
		if (currentPendingDeletionAt.getTime() !== nextPendingDeletionAt.getTime()) {
			await userRepository.removePendingDeletion(userId, currentPendingDeletionAt);
		}
	}
	await userRepository.addPendingDeletion(userId, nextPendingDeletionAt, deletionReasonCode);
	await deletionQueue.scheduleDeletion(userId, nextPendingDeletionAt, deletionReasonCode);
}

export async function clearPendingDeletion({
	userId,
	pendingDeletionAt,
	userRepository,
	deletionQueue,
}: ClearPendingDeletionParams): Promise<void> {
	if (pendingDeletionAt) {
		await userRepository.removePendingDeletion(userId, pendingDeletionAt);
	}
	await deletionQueue.removeFromQueue(userId);
}

export function resolvePendingDeletionReasonCode(
	user: PendingDeletionReasonUserLike,
	queuedReasonCode: number,
): number {
	if (user.deletionReasonCode !== null && user.deletionReasonCode !== undefined) {
		return user.deletionReasonCode;
	}
	if (queuedReasonCode !== 0) {
		return queuedReasonCode;
	}
	if ((user.flags & UserFlags.SELF_DELETED) !== 0n) {
		return DeletionReasons.USER_REQUESTED;
	}
	if ((user.flags & UserFlags.DELETED) !== 0n) {
		return DeletionReasons.OTHER;
	}
	return 0;
}

export function isPendingDeletionBlocked(user: PendingDeletionEligibilityUserLike): boolean {
	if (user.isBot) {
		return true;
	}
	return (user.flags & UserFlags.APP_STORE_REVIEWER) !== 0n;
}

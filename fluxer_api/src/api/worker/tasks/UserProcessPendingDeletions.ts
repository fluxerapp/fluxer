// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {
	isPendingDeletionBlocked,
	resolvePendingDeletionReasonCode,
} from '@app/api/user/services/PendingDeletionCoordinator';
import {getValidTimestamp} from '@app/api/utils/TimestampUtils';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const userProcessPendingDeletions: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.debug('Processing userProcessPendingDeletions task');
	const {userRepository, workerService, deletionQueueService} = getWorkerDependencies();
	try {
		Logger.debug('Processing pending user deletions from KV queue');
		const needsRebuild = await deletionQueueService.needsRebuild();
		if (needsRebuild) {
			Logger.info('Deletion queue needs rebuild, acquiring lock');
			const lockToken = await deletionQueueService.acquireRebuildLock();
			if (!lockToken) {
				Logger.info('Another worker is rebuilding the queue, skipping this run');
				return;
			}
			try {
				await deletionQueueService.rebuildState(lockToken);
			} catch (error) {
				try {
					await deletionQueueService.releaseRebuildLock(lockToken);
				} catch (releaseError) {
					Logger.error({error: releaseError}, 'Failed to release deletion queue lock after rebuild failure');
				}
				throw error;
			}
			await deletionQueueService.releaseRebuildLock(lockToken);
		}
		const nowMs = Date.now();
		const pendingDeletions = await deletionQueueService.getReadyDeletions(nowMs, 1000);
		Logger.debug({count: pendingDeletions.length}, 'Found users pending deletion from KV');
		let scheduled = 0;
		for (const deletion of pendingDeletions) {
			try {
				const userId = createUserID(deletion.userId);
				const user = await userRepository.findUnique(userId);
				if (!user || !user.pendingDeletionAt) {
					Logger.warn({userId}, 'User not found or not pending deletion in Cassandra, removing from KV');
					await deletionQueueService.removeFromQueue(userId);
					continue;
				}
				if (!user.deletionStartedAt && isPendingDeletionBlocked(user)) {
					Logger.info({userId}, 'User is not eligible for automated deletion, removing from KV');
					await deletionQueueService.removeFromQueue(userId);
					continue;
				}
				const scheduledAt = getValidTimestamp(user.pendingDeletionAt, `Pending deletion timestamp for user ${userId}`);
				const deletionReasonCode = resolvePendingDeletionReasonCode(user, deletion.deletionReasonCode);
				if (scheduledAt > nowMs) {
					Logger.debug({userId, scheduledAt}, 'Requeueing pending user deletion that is not due yet');
					await deletionQueueService.scheduleDeletion(userId, user.pendingDeletionAt, deletionReasonCode);
					continue;
				}
				await workerService.addJob('userProcessPendingDeletion', {
					userId: deletion.userId.toString(),
					deletionReasonCode,
					pendingDeletionAt: user.pendingDeletionAt.toISOString(),
				});
				await deletionQueueService.removeFromQueue(userId);
				scheduled++;
			} catch (error) {
				Logger.error({error, userId: deletion.userId.toString()}, 'Failed to schedule user deletion');
			}
		}
		Logger.debug({scheduled, total: pendingDeletions.length}, 'Scheduled user deletion tasks');
	} catch (error) {
		Logger.error({error}, 'Failed to process pending deletions');
		throw error;
	}
};

export default userProcessPendingDeletions;

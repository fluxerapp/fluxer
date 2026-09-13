// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Logger} from '@app/api/Logger';
import {getValidTimestamp} from '@app/api/utils/TimestampUtils';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const processPendingBulkMessageDeletions: WorkerTaskHandler = async (_payload, helpers) => {
	helpers.logger.debug('Processing pending bulk message deletions');
	const {bulkMessageDeletionQueueService, userRepository, workerService} = getWorkerDependencies();
	if (await bulkMessageDeletionQueueService.needsRebuild()) {
		Logger.info('Bulk message deletion queue needs rebuild, acquiring lock');
		const lockToken = await bulkMessageDeletionQueueService.acquireRebuildLock();
		if (!lockToken) {
			Logger.info('Another worker is rebuilding the bulk message deletion queue, skipping this run');
			return;
		}
		try {
			await bulkMessageDeletionQueueService.rebuildState();
		} catch (error) {
			try {
				await bulkMessageDeletionQueueService.releaseRebuildLock(lockToken);
			} catch (releaseError) {
				Logger.error({error: releaseError}, 'Failed to release bulk message deletion queue lock after rebuild failure');
			}
			throw error;
		}
		await bulkMessageDeletionQueueService.releaseRebuildLock(lockToken);
	}
	const nowMs = Date.now();
	const pendingDeletions = await bulkMessageDeletionQueueService.getReadyDeletions(nowMs, 100);
	Logger.debug({count: pendingDeletions.length}, 'Pending bulk message deletions found');
	for (const deletion of pendingDeletions) {
		try {
			const userId = createUserID(deletion.userId);
			const user = await userRepository.findUnique(userId);
			if (!user?.pendingBulkMessageDeletionAt) {
				await bulkMessageDeletionQueueService.removeFromQueue(userId);
				continue;
			}
			const scheduledAt = getValidTimestamp(
				user.pendingBulkMessageDeletionAt,
				`Pending bulk message deletion timestamp for user ${userId}`,
			);
			if (scheduledAt > nowMs) {
				Logger.debug(
					{
						userId: userId.toString(),
						scheduledAt,
					},
					'Requeueing pending bulk message deletion that is not due yet',
				);
				await bulkMessageDeletionQueueService.scheduleDeletion(userId, user.pendingBulkMessageDeletionAt);
				continue;
			}
			await workerService.addJob(
				'bulkDeleteUserMessages',
				{
					userId: userId.toString(),
					scheduledAt,
				},
				{maxAttempts: 5},
			);
			Logger.debug(
				{
					userId: userId.toString(),
					scheduledAt,
				},
				'Queued worker job for pending bulk message deletion',
			);
			await bulkMessageDeletionQueueService.removeFromQueue(userId);
		} catch (error) {
			Logger.error({error, userId: deletion.userId.toString()}, 'Failed to process pending bulk message deletion');
		}
	}
};

export default processPendingBulkMessageDeletions;

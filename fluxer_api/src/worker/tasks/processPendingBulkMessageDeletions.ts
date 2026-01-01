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

import type {Task} from 'graphile-worker';
import {createUserID} from '~/BrandedTypes';
import {Logger} from '~/Logger';
import {getWorkerDependencies} from '../WorkerContext';

const processPendingBulkMessageDeletions: Task = async (_payload, helpers) => {
	helpers.logger.debug('Processing pending bulk message deletions');

	const {bulkMessageDeletionQueueService, userRepository, workerService} = getWorkerDependencies();
	const nowMs = Date.now();
	const pendingDeletions = await bulkMessageDeletionQueueService.getReadyDeletions(nowMs, 100);

	Logger.debug({count: pendingDeletions.length}, 'Pending bulk message deletions found');

	for (const deletion of pendingDeletions) {
		try {
			const userId = createUserID(deletion.userId);
			const user = await userRepository.findUnique(userId);
			if (!user) {
				await bulkMessageDeletionQueueService.removeFromQueue(userId);
				continue;
			}

			if (!user.pendingBulkMessageDeletionAt) {
				await bulkMessageDeletionQueueService.removeFromQueue(userId);
				continue;
			}

			if (user.pendingBulkMessageDeletionAt.getTime() > nowMs) {
				Logger.debug(
					{
						userId: userId.toString(),
						scheduledAt: user.pendingBulkMessageDeletionAt.getTime(),
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
					scheduledAt: user.pendingBulkMessageDeletionAt.getTime(),
				},
				{maxAttempts: 5},
			);

			Logger.debug(
				{
					userId: userId.toString(),
					scheduledAt: user.pendingBulkMessageDeletionAt.getTime(),
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

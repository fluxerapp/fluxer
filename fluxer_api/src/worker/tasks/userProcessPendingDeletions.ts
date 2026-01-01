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
import {UserFlags} from '~/Constants';
import {Logger} from '~/Logger';
import {getWorkerDependencies} from '../WorkerContext';

const userProcessPendingDeletions: Task = async (_payload, helpers) => {
	helpers.logger.debug('Processing userProcessPendingDeletions task');

	const {userRepository, workerService, deletionQueueService} = getWorkerDependencies();

	try {
		Logger.debug('Processing pending user deletions from Redis queue');

		const needsRebuild = await deletionQueueService.needsRebuild();
		if (needsRebuild) {
			Logger.info('Deletion queue needs rebuild, acquiring lock');
			const lockToken = await deletionQueueService.acquireRebuildLock();

			if (lockToken) {
				try {
					await deletionQueueService.rebuildState();
					await deletionQueueService.releaseRebuildLock(lockToken);
				} catch (error) {
					await deletionQueueService.releaseRebuildLock(lockToken);
					throw error;
				}
			} else {
				Logger.info('Another worker is rebuilding the queue, skipping this run');
				return;
			}
		}

		const nowMs = Date.now();
		const pendingDeletions = await deletionQueueService.getReadyDeletions(nowMs, 1000);

		Logger.debug({count: pendingDeletions.length}, 'Found users pending deletion from Redis');

		let scheduled = 0;
		for (const deletion of pendingDeletions) {
			try {
				const userId = createUserID(deletion.userId);

				const user = await userRepository.findUnique(userId);
				if (!user || !user.pendingDeletionAt) {
					Logger.warn({userId}, 'User not found or not pending deletion in Cassandra, removing from Redis');
					await deletionQueueService.removeFromQueue(userId);
					continue;
				}

				if (user.isBot) {
					Logger.info({userId}, 'User is a bot, skipping deletion');
					continue;
				}

				if (user.flags & UserFlags.APP_STORE_REVIEWER) {
					Logger.info({userId}, 'User is an app store reviewer, skipping deletion');
					continue;
				}

				await workerService.addJob('userProcessPendingDeletion', {
					userId: deletion.userId.toString(),
					deletionReasonCode: deletion.deletionReasonCode,
				});

				await deletionQueueService.removeFromQueue(userId);
				await userRepository.removePendingDeletion(userId, user.pendingDeletionAt);

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

// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import {KVBulkMessageDeletionQueueService} from '@app/api/infrastructure/KVBulkMessageDeletionQueueService';
import {createTestWorkerHelpers, SyncTaskWorkerService} from '@app/api/test/SyncTaskWorkerService';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import processPendingBulkMessageDeletions from '@app/api/worker/tasks/ProcessPendingBulkMessageDeletions';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, describe, expect, test} from 'vitest';

async function createHarness(users: Array<{userId: UserID; pendingAt: Date | null}>) {
	const kvClient = new MockKVProvider();
	const userRepository = new UserRepository();
	for (const {userId, pendingAt} of users) {
		await userRepository.create({
			...EMPTY_USER_ROW,
			user_id: userId,
			username: `user${userId}`,
			pending_bulk_message_deletion_at: pendingAt,
		});
	}
	const workerService = new SyncTaskWorkerService({}, {deferred: true});
	const bulkMessageDeletionQueueService = new KVBulkMessageDeletionQueueService(kvClient, userRepository);
	setWorkerDependenciesForTest({bulkMessageDeletionQueueService, userRepository, workerService});
	return {kvClient, workerService, bulkMessageDeletionQueueService};
}

describe('processPendingBulkMessageDeletions', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('replaces the old sorted-set member when a user is rescheduled', async () => {
		const userId = createUserID(1234n);
		const harness = await createHarness([]);
		const oldAt = new Date(1_700_000_000_000);
		const newAt = new Date(oldAt.getTime() + 60_000);
		await harness.bulkMessageDeletionQueueService.scheduleDeletion(userId, oldAt);
		await harness.bulkMessageDeletionQueueService.scheduleDeletion(userId, newAt);

		expect(await harness.bulkMessageDeletionQueueService.getQueueSize()).toBe(1);
		expect(await harness.bulkMessageDeletionQueueService.getReadyDeletions(oldAt.getTime(), 100)).toEqual([]);
		expect(await harness.bulkMessageDeletionQueueService.getReadyDeletions(newAt.getTime(), 100)).toEqual([
			{userId, scheduledAt: newAt.getTime()},
		]);
	});

	test('rebuilds the queue from the users table when the state version is missing', async () => {
		const userId = createUserID(1234n);
		const scheduledAt = new Date(Date.now() - 60_000);
		const harness = await createHarness([{userId, pendingAt: scheduledAt}]);

		await processPendingBulkMessageDeletions({}, createTestWorkerHelpers());

		expect(harness.workerService.pending.map(({taskType, payload}) => ({taskType, payload}))).toEqual([
			{
				taskType: 'bulkDeleteUserMessages',
				payload: {userId: userId.toString(), scheduledAt: scheduledAt.getTime()},
			},
		]);
		expect(await harness.kvClient.exists('bulk_message_deletion_queue:state_version')).toBe(1);
	});

	test('skips the run when another worker holds the rebuild lock', async () => {
		const userId = createUserID(9012n);
		const harness = await createHarness([{userId, pendingAt: new Date(Date.now() - 60_000)}]);
		await harness.kvClient.set('bulk_message_deletion_queue:rebuild_lock', 'other-worker');

		await processPendingBulkMessageDeletions({}, createTestWorkerHelpers());

		expect(harness.workerService.pending).toEqual([]);
		expect(await harness.bulkMessageDeletionQueueService.getQueueSize()).toBe(0);
		expect(await harness.kvClient.get('bulk_message_deletion_queue:rebuild_lock')).toBe('other-worker');
	});
});

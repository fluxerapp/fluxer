// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import {KVBulkMessageDeletionQueueService} from '@app/api/infrastructure/KVBulkMessageDeletionQueueService';
import {User} from '@app/api/models/User';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import processPendingBulkMessageDeletions from '@app/api/worker/tasks/ProcessPendingBulkMessageDeletions';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import type {WorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, describe, expect, test, vi} from 'vitest';

function createUser(userId: UserID, pendingBulkMessageDeletionAt: Date | null): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: userId,
		username: `user${userId}`,
		pending_bulk_message_deletion_at: pendingBulkMessageDeletionAt,
	});
}

function createHarness(users: Array<User>) {
	const kvClient = new MockKVProvider();
	const scanCalls: Array<number> = [];
	const userRepository = {
		async scanAllUsersPage(limit: number): Promise<{users: Array<User>; pageState: null}> {
			scanCalls.push(limit);
			return {users, pageState: null};
		},
		async findUnique(userId: UserID): Promise<User | null> {
			return users.find((user) => user.id === userId) ?? null;
		},
	} as unknown as UserRepository;
	const jobs: Array<{name: string; payload: unknown}> = [];
	const workerService = {
		async addJob(name: string, payload: unknown): Promise<bigint> {
			jobs.push({name, payload});
			return 0n;
		},
	} as unknown as WorkerDependencies['workerService'];
	const bulkMessageDeletionQueueService = new KVBulkMessageDeletionQueueService(kvClient, userRepository);
	setWorkerDependenciesForTest({bulkMessageDeletionQueueService, userRepository, workerService});
	return {kvClient, scanCalls, jobs, bulkMessageDeletionQueueService};
}

describe('processPendingBulkMessageDeletions', () => {
	afterEach(() => {
		clearWorkerDependencies();
		vi.restoreAllMocks();
	});

	test('replaces the old sorted-set member when a user is rescheduled', async () => {
		const userId = createUserID(1234n);
		const harness = createHarness([]);
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

	test('renews the rebuild lock with the acquired token before publishing the state version', async () => {
		const harness = createHarness([createUser(createUserID(1234n), new Date())]);
		const extendLock = vi.spyOn(harness.kvClient, 'extendLock');
		const token = await harness.bulkMessageDeletionQueueService.acquireRebuildLock();
		expect(token).not.toBeNull();
		await harness.bulkMessageDeletionQueueService.rebuildState(token);

		expect(extendLock).toHaveBeenCalledExactlyOnceWith('bulk_message_deletion_queue:rebuild_lock', token, 300);
		expect((await harness.kvClient.exists('bulk_message_deletion_queue:state_version')) === 1).toBe(true);
	});

	test('rebuilds the queue from the users table when the state version is missing', async () => {
		const userId = createUserID(1234n);
		const scheduledAt = new Date(Date.now() - 60_000);
		const harness = createHarness([createUser(userId, scheduledAt)]);

		await processPendingBulkMessageDeletions({}, createTestWorkerHelpers());

		expect(harness.scanCalls).toEqual([1000]);
		expect(harness.jobs).toEqual([
			{
				name: 'bulkDeleteUserMessages',
				payload: {userId: userId.toString(), scheduledAt: scheduledAt.getTime()},
			},
		]);
		expect((await harness.kvClient.exists('bulk_message_deletion_queue:state_version')) === 1).toBe(true);
	});

	test('does not scan the users table when the state version is fresh', async () => {
		const userId = createUserID(5678n);
		const harness = createHarness([createUser(userId, new Date(Date.now() - 60_000))]);
		await harness.kvClient.set('bulk_message_deletion_queue:state_version', Date.now().toString());

		await processPendingBulkMessageDeletions({}, createTestWorkerHelpers());

		expect(harness.scanCalls).toEqual([]);
		expect(harness.jobs).toEqual([]);
	});

	test('skips the run when another worker holds the rebuild lock', async () => {
		const userId = createUserID(9012n);
		const harness = createHarness([createUser(userId, new Date(Date.now() - 60_000))]);
		await harness.kvClient.set('bulk_message_deletion_queue:rebuild_lock', 'other-worker');

		await processPendingBulkMessageDeletions({}, createTestWorkerHelpers());

		expect(harness.scanCalls).toEqual([]);
		expect(harness.jobs).toEqual([]);
		expect(await harness.kvClient.get('bulk_message_deletion_queue:rebuild_lock')).toBe('other-worker');
	});
});

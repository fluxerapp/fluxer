// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, describe, expect, test} from 'vitest';
import {createUserID, type UserID} from '../BrandedTypes';
import {EMPTY_USER_ROW} from '../database/types/UserTypes';
import {User} from '../models/User';
import {NoopLogger} from '../test/mocks/NoopLogger';
import type {UserRepository} from '../user/repositories/UserRepository';
import processPendingBulkMessageDeletions from '../worker/tasks/ProcessPendingBulkMessageDeletions';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '../worker/WorkerContext';
import type {WorkerDependencies} from '../worker/WorkerDependencies';
import {KVBulkMessageDeletionQueueService} from './KVBulkMessageDeletionQueueService';

class FakeKVProvider {
	readonly strings = new Map<string, string>();
	readonly sortedSets = new Map<string, Map<string, number>>();

	async get(key: string): Promise<string | null> {
		return this.strings.get(key) ?? null;
	}

	async set(key: string, value: string): Promise<string | null> {
		this.strings.set(key, value);
		return 'OK';
	}

	async del(...keys: Array<string>): Promise<number> {
		let removed = 0;
		for (const key of keys) {
			if (this.strings.delete(key)) {
				removed++;
			}
			if (this.sortedSets.delete(key)) {
				removed++;
			}
		}
		return removed;
	}

	async exists(key: string): Promise<number> {
		return this.strings.has(key) || this.sortedSets.has(key) ? 1 : 0;
	}

	async zcard(key: string): Promise<number> {
		return this.sortedSets.get(key)?.size ?? 0;
	}

	async zrangebyscore(
		key: string,
		_min: string | number,
		max: string | number,
		_limitToken?: string,
		offset?: number,
		count?: number,
	): Promise<Array<string>> {
		const members = [...(this.sortedSets.get(key) ?? new Map<string, number>()).entries()]
			.filter(([, score]) => score <= Number(max))
			.sort((a, b) => a[1] - b[1])
			.map(([member]) => member);
		return members.slice(offset ?? 0, (offset ?? 0) + (count ?? members.length));
	}

	async scheduleBulkDeletion(queueKey: string, secondaryKey: string, score: number, value: string): Promise<void> {
		let set = this.sortedSets.get(queueKey);
		if (!set) {
			set = new Map<string, number>();
			this.sortedSets.set(queueKey, set);
		}
		set.set(value, score);
		this.strings.set(secondaryKey, value);
	}

	async removeBulkDeletion(queueKey: string, secondaryKey: string): Promise<boolean> {
		const value = this.strings.get(secondaryKey);
		if (!value) {
			return false;
		}
		this.sortedSets.get(queueKey)?.delete(value);
		this.strings.delete(secondaryKey);
		return true;
	}

	async acquireLock(key: string, token: string, _ttlSeconds: number): Promise<boolean> {
		if (this.strings.has(key)) {
			return false;
		}
		this.strings.set(key, token);
		return true;
	}

	async releaseLock(key: string, token: string): Promise<boolean> {
		if (this.strings.get(key) !== token) {
			return false;
		}
		this.strings.delete(key);
		return true;
	}
}

function createUser(userId: UserID, pendingBulkMessageDeletionAt: Date | null): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: userId,
		username: `user${userId}`,
		pending_bulk_message_deletion_at: pendingBulkMessageDeletionAt,
	});
}

function createHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 1n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

function createHarness(users: Array<User>) {
	const kvClient = new FakeKVProvider();
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
	const bulkMessageDeletionQueueService = new KVBulkMessageDeletionQueueService(
		kvClient as unknown as IKVProvider,
		userRepository,
	);
	setWorkerDependenciesForTest({bulkMessageDeletionQueueService, userRepository, workerService});
	return {kvClient, scanCalls, jobs};
}

describe('processPendingBulkMessageDeletions', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('rebuilds the queue from the users table when the state version is missing', async () => {
		const userId = createUserID(1234n);
		const scheduledAt = new Date(Date.now() - 60_000);
		const harness = createHarness([createUser(userId, scheduledAt)]);

		await processPendingBulkMessageDeletions({}, createHelpers());

		expect(harness.scanCalls).toEqual([1000]);
		expect(harness.jobs).toEqual([
			{
				name: 'bulkDeleteUserMessages',
				payload: {userId: userId.toString(), scheduledAt: scheduledAt.getTime()},
			},
		]);
		expect(harness.kvClient.strings.has('bulk_message_deletion_queue:state_version')).toBe(true);
	});

	test('does not scan the users table when the state version is fresh', async () => {
		const userId = createUserID(5678n);
		const harness = createHarness([createUser(userId, new Date(Date.now() - 60_000))]);
		harness.kvClient.strings.set('bulk_message_deletion_queue:state_version', Date.now().toString());

		await processPendingBulkMessageDeletions({}, createHelpers());

		expect(harness.scanCalls).toEqual([]);
		expect(harness.jobs).toEqual([]);
	});

	test('skips the run when another worker holds the rebuild lock', async () => {
		const userId = createUserID(9012n);
		const harness = createHarness([createUser(userId, new Date(Date.now() - 60_000))]);
		harness.kvClient.strings.set('bulk_message_deletion_queue:rebuild_lock', 'other-worker');

		await processPendingBulkMessageDeletions({}, createHelpers());

		expect(harness.scanCalls).toEqual([]);
		expect(harness.jobs).toEqual([]);
		expect(harness.kvClient.strings.get('bulk_message_deletion_queue:rebuild_lock')).toBe('other-worker');
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {EMPTY_USER_ROW, type UserRow} from '@app/api/database/types/UserTypes';
import {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {User} from '@app/api/models/User';
import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import userProcessPendingDeletions from '@app/api/worker/tasks/UserProcessPendingDeletions';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, describe, expect, test} from 'vitest';

function createFakeUser(
	userId: UserID,
	pendingDeletionAt: Date,
	options: Partial<Pick<UserRow, 'bot' | 'flags'>> = {},
): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: userId,
		pending_deletion_at: pendingDeletionAt,
		deletion_reason_code: 1,
		bot: options.bot ?? false,
		flags: options.flags ?? 0n,
	});
}

async function createHarness(users: Array<User>) {
	const kvClient = new MockKVProvider();
	const removedPendingDeletions: Array<string> = [];
	const scheduledJobs: Array<string> = [];
	const usersById = new Map<string, User>();
	for (const user of users) {
		usersById.set(user.id.toString(), user);
	}
	const userRepository = {
		async findUnique(userId: UserID): Promise<User | null> {
			return usersById.get(userId.toString()) ?? null;
		},
		async removePendingDeletion(userId: UserID): Promise<void> {
			removedPendingDeletions.push(userId.toString());
		},
	} as unknown as UserRepository;
	const workerService = {
		async addJob(name: string, payload: {userId: string; pendingDeletionAt: string}): Promise<bigint> {
			scheduledJobs.push(`${name}:${payload.userId}:${payload.pendingDeletionAt}`);
			return 0n;
		},
	} as unknown as IWorkerService;
	const deletionQueueService = new KVAccountDeletionQueueService(kvClient, userRepository);
	await kvClient.set('deletion_queue:state_version', Date.now().toString());
	for (const user of users) {
		if (user.pendingDeletionAt) {
			await deletionQueueService.scheduleDeletion(user.id, user.pendingDeletionAt, 1);
		}
	}
	setWorkerDependenciesForTest({userRepository, workerService, deletionQueueService});
	return {deletionQueueService, scheduledJobs, removedPendingDeletions};
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

describe('userProcessPendingDeletions', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('drains skipped accounts so a genuine deletion behind them is not starved', async () => {
		const skippedAt = new Date(Date.now() - 10_000_000);
		const genuineAt = new Date(Date.now() - 1_000);
		const users: Array<User> = [];
		for (let i = 0; i < 1000; i++) {
			const userId = createUserID(BigInt(100_000 + i));
			users.push(createFakeUser(userId, skippedAt, i % 2 === 0 ? {bot: true} : {flags: UserFlags.APP_STORE_REVIEWER}));
		}
		const genuineUserId = createUserID(999_999n);
		users.push(createFakeUser(genuineUserId, genuineAt));
		const harness = await createHarness(users);

		await userProcessPendingDeletions({}, createHelpers());
		const queueSizeAfterFirstPass = await harness.deletionQueueService.getQueueSize();
		await userProcessPendingDeletions({}, createHelpers());

		expect(harness.scheduledJobs).toEqual([
			`userProcessPendingDeletion:${genuineUserId.toString()}:${genuineAt.toISOString()}`,
		]);
		expect(harness.removedPendingDeletions).toEqual([]);
		expect(queueSizeAfterFirstPass).toBe(1);
		expect(await harness.deletionQueueService.getQueueSize()).toBe(0);
	});

	test('keeps skipped accounts out of the queue while the skip condition holds', async () => {
		const pendingAt = new Date(Date.now() - 10_000);
		const botId = createUserID(1n);
		const reviewerId = createUserID(2n);
		const harness = await createHarness([
			createFakeUser(botId, pendingAt, {bot: true}),
			createFakeUser(reviewerId, pendingAt, {flags: UserFlags.APP_STORE_REVIEWER}),
		]);

		await userProcessPendingDeletions({}, createHelpers());

		expect(await harness.deletionQueueService.getQueueSize()).toBe(0);
		expect(harness.scheduledJobs).toEqual([]);
		expect(harness.removedPendingDeletions).toEqual([]);
	});
});

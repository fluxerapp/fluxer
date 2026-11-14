// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {EMPTY_USER_ROW, type UserRow} from '@app/api/database/types/UserTypes';
import {KVAccountDeletionQueueService} from '@app/api/infrastructure/KVAccountDeletionQueueService';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import userProcessPendingDeletions from '@app/api/worker/tasks/UserProcessPendingDeletions';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface PendingUser {
	userId: UserID;
	pendingDeletionAt: Date;
	bot?: boolean;
	flags?: bigint;
}

async function setupQueue(users: Array<PendingUser>) {
	const userRepository = getUserRepository();
	const workerService = new RecordingWorkerService();
	const deletionQueueService = new KVAccountDeletionQueueService(getKVClient(), userRepository);
	await getKVClient().set('deletion_queue:state_version', Date.now().toString());
	for (const user of users) {
		const row: UserRow = {...EMPTY_USER_ROW, user_id: user.userId, bot: user.bot ?? false, flags: user.flags ?? 0n};
		const created = await userRepository.create(row);
		await userRepository.updateDeletionSchedule(created, {
			pending_deletion_at: user.pendingDeletionAt,
			deletion_reason_code: 1,
		});
		await deletionQueueService.scheduleDeletion(user.userId, user.pendingDeletionAt, 1);
	}
	setWorkerDependenciesForTest({userRepository, workerService, deletionQueueService});
	return {deletionQueueService, workerService};
}

function scheduledDeletions(workerService: RecordingWorkerService): Array<string> {
	return workerService
		.byTask('userProcessPendingDeletion')
		.map((job) => `${job.payload['userId']}:${job.payload['pendingDeletionAt']}`);
}

describe('userProcessPendingDeletions', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterEach(() => {
		clearWorkerDependencies();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	test('drains skipped accounts so a genuine deletion behind them is not starved', async () => {
		const skippedAt = new Date(Date.now() - 10_000_000);
		const genuineAt = new Date(Date.now() - 1_000);
		const users: Array<PendingUser> = [];
		for (let i = 0; i < 1000; i++) {
			const userId = createUserID(BigInt(100_000 + i));
			users.push(
				i % 2 === 0
					? {userId, pendingDeletionAt: skippedAt, bot: true}
					: {userId, pendingDeletionAt: skippedAt, flags: UserFlags.APP_STORE_REVIEWER},
			);
		}
		const genuineUserId = createUserID(999_999n);
		users.push({userId: genuineUserId, pendingDeletionAt: genuineAt});
		const {deletionQueueService, workerService} = await setupQueue(users);

		await userProcessPendingDeletions({}, createTestWorkerHelpers());
		const queueSizeAfterFirstPass = await deletionQueueService.getQueueSize();
		await userProcessPendingDeletions({}, createTestWorkerHelpers());

		expect(scheduledDeletions(workerService)).toEqual([`${genuineUserId.toString()}:${genuineAt.toISOString()}`]);
		expect(queueSizeAfterFirstPass).toBe(1);
		expect(await deletionQueueService.getQueueSize()).toBe(0);
	});

	test('keeps skipped accounts out of the queue while the skip condition holds', async () => {
		const pendingAt = new Date(Date.now() - 10_000);
		const {deletionQueueService, workerService} = await setupQueue([
			{userId: createUserID(500_001n), pendingDeletionAt: pendingAt, bot: true},
			{userId: createUserID(500_002n), pendingDeletionAt: pendingAt, flags: UserFlags.APP_STORE_REVIEWER},
		]);

		await userProcessPendingDeletions({}, createTestWorkerHelpers());

		expect(await deletionQueueService.getQueueSize()).toBe(0);
		expect(scheduledDeletions(workerService)).toEqual([]);
	});
});

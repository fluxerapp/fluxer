// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {createUserID} from '../../BrandedTypes';
import {EMPTY_USER_ROW} from '../../database/types/UserTypes';
import type {ILogger} from '../../ILogger';
import {KVAccountDeletionQueueService} from '../../infrastructure/KVAccountDeletionQueueService';
import {User} from '../../models/User';
import {MockKVProvider} from '../../test/mocks/MockKVProvider';
import {NoopLogger} from '../../test/mocks/NoopLogger';
import type {UserRepository} from '../../user/repositories/UserRepository';
import {ensureDeletionQueueState} from '../DeletionQueueStartup';

const WORKER_PAGE_COUNT = 2;

class RecordingLogger implements ILogger {
	readonly errors: Array<string> = [];

	trace(): void {}
	debug(): void {}
	info(): void {}
	warn(): void {}
	fatal(): void {}

	error(obj: object | string, msg?: string): void {
		this.errors.push(typeof obj === 'string' ? obj : (msg ?? ''));
	}

	child(): ILogger {
		return this;
	}
}

class UnreadableStateKVProvider extends MockKVProvider {
	override async exists(): Promise<number> {
		throw new Error('kv unavailable');
	}
}

class UnlockableKVProvider extends MockKVProvider {
	override async acquireLock(): Promise<boolean> {
		throw new Error('kv lock unavailable');
	}
}

class UnreleasableKVProvider extends MockKVProvider {
	override async releaseLock(): Promise<boolean> {
		throw new Error('kv lock release failed');
	}
}

function createFailingRepository(): UserRepository {
	return {
		async scanAllUsersPage() {
			throw new Error('paged user scan failed');
		},
	} as unknown as UserRepository;
}

function createPendingUser(index: number): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: createUserID(BigInt(9100 + index)),
		pending_deletion_at: new Date('2026-06-01T00:00:00.000Z'),
		deletion_reason_code: 0,
	});
}

function createWorkerRepository(onPageStart: (page: number) => Promise<void>): UserRepository {
	let page = 0;
	return {
		async scanAllUsersPage() {
			page += 1;
			await onPageStart(page);
			return {
				users: [createPendingUser(page)],
				pageState: page < WORKER_PAGE_COUNT ? `page-${page}` : null,
			};
		},
	} as unknown as UserRepository;
}

describe('ensureDeletionQueueState', () => {
	it('leaves a rebuild owned by another instance alone', async () => {
		const kvClient = new MockKVProvider();
		let apiScans = 0;
		const apiRepository = {
			async scanAllUsersPage() {
				apiScans += 1;
				throw new Error('api pod scan failed');
			},
		} as unknown as UserRepository;
		const apiQueue = new KVAccountDeletionQueueService(kvClient, apiRepository);
		const apiFailures: Array<unknown> = [];
		const workerQueue = new KVAccountDeletionQueueService(
			kvClient,
			createWorkerRepository(async (page) => {
				if (page !== WORKER_PAGE_COUNT) {
					return;
				}
				try {
					await ensureDeletionQueueState(apiQueue, new NoopLogger());
				} catch (error) {
					apiFailures.push(error);
				}
			}),
		);

		const workerToken = await workerQueue.acquireRebuildLock();
		expect(workerToken).not.toBeNull();
		await workerQueue.rebuildState(workerToken);
		expect(await workerQueue.releaseRebuildLock(workerToken!)).toBe(true);

		const queued = await workerQueue.getReadyDeletions(Date.parse('2026-06-02T00:00:00.000Z'), 10);
		expect(queued.map((deletion) => deletion.userId).sort()).toEqual([9101n, 9102n]);
		expect(apiScans).toBe(0);
		expect(apiFailures).toEqual([]);
	});

	it('does not abort startup when the paged user scan fails', async () => {
		const kvClient = new MockKVProvider();
		let scans = 0;
		const repository = {
			async scanAllUsersPage() {
				scans += 1;
				throw new Error('paged user scan failed');
			},
		} as unknown as UserRepository;
		const queue = new KVAccountDeletionQueueService(kvClient, repository);
		const logger = new RecordingLogger();

		await expect(ensureDeletionQueueState(queue, logger)).resolves.toBeUndefined();

		expect(scans).toBe(1);
		expect(logger.errors).toEqual(['KV deletion queue rebuild failed, leaving the rebuild to the deletion worker']);
		expect(await queue.acquireRebuildLock()).not.toBeNull();
	});

	it('aborts startup when the queue state cannot be read', async () => {
		const queue = new KVAccountDeletionQueueService(new UnreadableStateKVProvider(), createFailingRepository());
		const logger = new RecordingLogger();

		await expect(ensureDeletionQueueState(queue, logger)).rejects.toThrow('kv unavailable');

		expect(logger.errors).toEqual(['Failed to read KV deletion queue state, aborting startup']);
	});

	it('aborts startup when the rebuild lock cannot be acquired', async () => {
		const queue = new KVAccountDeletionQueueService(new UnlockableKVProvider(), createFailingRepository());
		const logger = new RecordingLogger();

		await expect(ensureDeletionQueueState(queue, logger)).rejects.toThrow('kv lock unavailable');

		expect(logger.errors).toEqual(['Failed to acquire the KV deletion queue rebuild lock, aborting startup']);
	});

	it('does not abort startup when releasing the rebuild lock fails', async () => {
		const queue = new KVAccountDeletionQueueService(
			new UnreleasableKVProvider(),
			createWorkerRepository(async () => {}),
		);
		const logger = new RecordingLogger();

		await expect(ensureDeletionQueueState(queue, logger)).resolves.toBeUndefined();

		const queued = await queue.getReadyDeletions(Date.parse('2026-06-02T00:00:00.000Z'), 10);
		expect(queued.map((deletion) => deletion.userId).sort()).toEqual([9101n, 9102n]);
		expect(logger.errors).toEqual(['Failed to release the KV deletion queue rebuild lock']);
	});

	it('rebuilds under the lock when no other instance holds it', async () => {
		const kvClient = new MockKVProvider();
		const queue = new KVAccountDeletionQueueService(
			kvClient,
			createWorkerRepository(async () => {}),
		);

		await ensureDeletionQueueState(queue, new NoopLogger());

		const queued = await queue.getReadyDeletions(Date.parse('2026-06-02T00:00:00.000Z'), 10);
		expect(queued.map((deletion) => deletion.userId).sort()).toEqual([9101n, 9102n]);
		expect(await queue.acquireRebuildLock()).not.toBeNull();
	});
});

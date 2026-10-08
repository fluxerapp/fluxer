// SPDX-License-Identifier: AGPL-3.0-or-later

import {MockKVProvider} from '@app/api/test/mocks/MockKVProvider';
import {queueBlocklistFeedStartupJobs} from '@app/api/worker/BlocklistFeedStartup';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {describe, expect, it, vi} from 'vitest';

const INITIAL_SYNC_KEY = 'sync:email_domains:initialized';
const FEED_TASKS = ['syncUrlBlocklists', 'syncFileShaBlocklists'];

function createWorkerService() {
	return {addJob: vi.fn(async (_task: WorkerTaskName, _payload: WorkerJobPayload) => 1n)};
}

function queuedTasks(workerService: ReturnType<typeof createWorkerService>): Array<string> {
	return workerService.addJob.mock.calls.map(([task]) => task);
}

describe('queueBlocklistFeedStartupJobs', () => {
	it('with feeds on, a start queues nothing and marks the feeds as enabled for six hours', async () => {
		const kv = new MockKVProvider();
		const workerService = createWorkerService();

		await queueBlocklistFeedStartupJobs(kv, workerService, true);

		expect(workerService.addJob).not.toHaveBeenCalled();
		expect(kv.setnxSpy.mock.calls).toEqual([[INITIAL_SYNC_KEY, '1', 21600]]);
		expect(kv.delSpy).not.toHaveBeenCalled();
	});

	it('with feeds off, the first start queues all three feed tasks and a second start queues none', async () => {
		const kv = new MockKVProvider();
		const workerService = createWorkerService();

		await queueBlocklistFeedStartupJobs(kv, workerService, false);
		expect(workerService.addJob.mock.calls).toEqual(FEED_TASKS.map((task) => [task, {}]));

		await queueBlocklistFeedStartupJobs(kv, workerService, false);
		expect(queuedTasks(workerService)).toEqual(FEED_TASKS);
	});

	it('with feeds off, a start after a feeds-on start cleans up again inside the claim', async () => {
		const kv = new MockKVProvider();
		const workerService = createWorkerService();

		await queueBlocklistFeedStartupJobs(kv, workerService, false);
		await queueBlocklistFeedStartupJobs(kv, workerService, true);
		await queueBlocklistFeedStartupJobs(kv, workerService, false);

		expect(queuedTasks(workerService)).toEqual([...FEED_TASKS, ...FEED_TASKS]);
		expect(await kv.exists(INITIAL_SYNC_KEY)).toBe(0);
	});

	it('a legacy enabled marker without expiry is cleared when feeds are off', async () => {
		const kv = new MockKVProvider();
		await kv.set(INITIAL_SYNC_KEY, '1');
		expect(await kv.ttl(INITIAL_SYNC_KEY)).toBe(-1);

		await queueBlocklistFeedStartupJobs(kv, createWorkerService(), false);
		expect(await kv.exists(INITIAL_SYNC_KEY)).toBe(0);

		await queueBlocklistFeedStartupJobs(kv, createWorkerService(), true);
		const ttl = await kv.ttl(INITIAL_SYNC_KEY);
		expect(ttl).toBeGreaterThanOrEqual(21599);
		expect(ttl).toBeLessThanOrEqual(21600);
	});

	it('a full jobs stream drops the job without failing startup', async () => {
		const overflowing = createWorkerService();
		overflowing.addJob.mockImplementation(async (task) => {
			throw new WorkerQueueOverflowError(task, 'maximum messages exceeded');
		});
		await expect(queueBlocklistFeedStartupJobs(new MockKVProvider(), overflowing, false)).resolves.toBeUndefined();
		expect(queuedTasks(overflowing)).toEqual(FEED_TASKS);

		const failing = createWorkerService();
		failing.addJob.mockRejectedValue(new Error('jetstream unavailable'));
		await expect(queueBlocklistFeedStartupJobs(new MockKVProvider(), failing, false)).rejects.toThrow(
			'jetstream unavailable',
		);
	});
});

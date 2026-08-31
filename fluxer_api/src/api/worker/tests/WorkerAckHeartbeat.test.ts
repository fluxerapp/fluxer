// SPDX-License-Identifier: AGPL-3.0-or-later

import {JobCancelledError} from '@pkgs/worker/src/contracts/WorkerTask';
import type {JsMsg} from 'nats';
import {afterEach, beforeAll, describe, expect, it, vi} from 'vitest';
import type {IJobLedgerRepository} from '../../jobs/IJobLedgerRepository';
import {setInjectedWorkerService} from '../../middleware/ServiceRegistry';
import {NoopWorkerService} from '../../test/NoopWorkerService';
import {WorkerRunner} from '../WorkerRunner';

const LANE_ACK_WAIT_MS = 120000;
const LANE_MAX_DELIVER = 25;
const TASK_TYPE = 'processInactivityDeletions';

const queueStub = {
	getConnectionManager: () => {
		throw new Error('WorkerRunner tests never consume messages');
	},
	getStreamName: () => 'JOBS',
	publishToDlq: vi.fn(),
};

class TestWorkerRunner extends WorkerRunner {
	async runJob(taskType: string, msg: JsMsg): Promise<boolean> {
		return await this.processJob(taskType, msg);
	}
}

function createRunner(task: () => Promise<void>): TestWorkerRunner {
	return new TestWorkerRunner({
		tasks: {[TASK_TYPE]: task},
		queue: queueStub,
		consumerName: 'workers_batch',
		laneName: 'batch',
		ledger: {} as IJobLedgerRepository,
		concurrency: 12,
		maxDeliver: LANE_MAX_DELIVER,
		ackWaitMs: LANE_ACK_WAIT_MS,
	});
}

function createJobMessage() {
	const envelope = {
		payload: {},
		max_attempts: 5,
		priority: 0,
		created_at: new Date().toISOString(),
	};
	return {
		seq: 1,
		subject: `jobs.${TASK_TYPE}`,
		redelivered: false,
		data: new TextEncoder().encode(JSON.stringify(envelope)),
		info: {deliveryCount: 1},
		ack: vi.fn(),
		nak: vi.fn(),
		term: vi.fn(),
		working: vi.fn(),
	};
}

function createDeferredTask(): {task: () => Promise<void>; settle: (error?: Error) => void} {
	let settle: (error?: Error) => void = () => {};
	const pending = new Promise<void>((resolve, reject) => {
		settle = (error?: Error) => {
			if (error) {
				reject(error);
			} else {
				resolve();
			}
		};
	});
	return {task: () => pending, settle: (error?: Error) => settle(error)};
}

describe('Worker ack heartbeat', () => {
	beforeAll(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	afterEach(() => {
		vi.useRealTimers();
		queueStub.publishToDlq.mockClear();
	});

	it('holds the ack for a task running past the lane ack wait', async () => {
		vi.useFakeTimers();
		const {task, settle} = createDeferredTask();
		const runner = createRunner(task);
		const msg = createJobMessage();

		const job = runner.runJob(TASK_TYPE, msg as unknown as JsMsg);
		await vi.advanceTimersByTimeAsync(LANE_ACK_WAIT_MS * 3);

		expect(msg.working.mock.calls.length).toBeGreaterThanOrEqual(1);
		expect(msg.ack).not.toHaveBeenCalled();
		expect(msg.nak).not.toHaveBeenCalled();

		settle();
		await expect(job).resolves.toBe(true);

		expect(msg.ack).toHaveBeenCalledTimes(1);
		const heartbeatsAtCompletion = msg.working.mock.calls.length;
		await vi.advanceTimersByTimeAsync(LANE_ACK_WAIT_MS * 3);
		expect(msg.working.mock.calls.length).toBe(heartbeatsAtCompletion);
	});

	it('stops the heartbeat when the task throws', async () => {
		vi.useFakeTimers();
		const {task, settle} = createDeferredTask();
		const runner = createRunner(task);
		const msg = createJobMessage();

		const job = runner.runJob(TASK_TYPE, msg as unknown as JsMsg);
		await vi.advanceTimersByTimeAsync(LANE_ACK_WAIT_MS);
		expect(msg.working.mock.calls.length).toBeGreaterThanOrEqual(1);

		settle(new Error('scan failed'));
		await expect(job).resolves.toBe(false);

		expect(msg.nak).toHaveBeenCalledTimes(1);
		const heartbeatsAtCompletion = msg.working.mock.calls.length;
		await vi.advanceTimersByTimeAsync(LANE_ACK_WAIT_MS * 3);
		expect(msg.working.mock.calls.length).toBe(heartbeatsAtCompletion);
	});

	it('stops the heartbeat when the task is cancelled', async () => {
		vi.useFakeTimers();
		const {task, settle} = createDeferredTask();
		const runner = createRunner(task);
		const msg = createJobMessage();

		const job = runner.runJob(TASK_TYPE, msg as unknown as JsMsg);
		await vi.advanceTimersByTimeAsync(LANE_ACK_WAIT_MS);
		expect(msg.working.mock.calls.length).toBeGreaterThanOrEqual(1);

		settle(new JobCancelledError('cancelled by admin'));
		await expect(job).resolves.toBe(false);

		expect(msg.ack).toHaveBeenCalledTimes(1);
		const heartbeatsAtCompletion = msg.working.mock.calls.length;
		await vi.advanceTimersByTimeAsync(LANE_ACK_WAIT_MS * 3);
		expect(msg.working.mock.calls.length).toBe(heartbeatsAtCompletion);
	});

	it('does not heartbeat a job deferred to the future', async () => {
		const runner = new TestWorkerRunner({
			tasks: {[TASK_TYPE]: async () => {}},
			queue: queueStub,
			consumerName: 'workers_batch',
			laneName: 'batch',
			ledger: {} as IJobLedgerRepository,
			concurrency: 12,
			maxDeliver: LANE_MAX_DELIVER,
			ackWaitMs: LANE_ACK_WAIT_MS,
		});
		const runAt = new Date(Date.now() + 60 * 60 * 1000);
		const envelope = {
			payload: {},
			run_at: runAt.toISOString(),
			max_attempts: 5,
			priority: 0,
			created_at: new Date().toISOString(),
		};
		const msg = {
			...createJobMessage(),
			data: new TextEncoder().encode(JSON.stringify(envelope)),
		};

		await runner.runJob(TASK_TYPE, msg as unknown as JsMsg);

		expect(msg.ack).not.toHaveBeenCalled();
		expect(msg.nak).toHaveBeenCalledTimes(1);
		expect(msg.working).not.toHaveBeenCalled();
	});
});

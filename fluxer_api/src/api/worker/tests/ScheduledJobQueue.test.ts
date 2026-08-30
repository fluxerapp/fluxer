// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import type {JsMsg} from 'nats';
import {afterEach, beforeAll, describe, expect, it, vi} from 'vitest';
import {KVScheduledJobQueueService} from '../../infrastructure/KVScheduledJobQueueService';
import type {IJobLedgerRepository} from '../../jobs/IJobLedgerRepository';
import {setInjectedWorkerService} from '../../middleware/ServiceRegistry';
import {MockKVProvider} from '../../test/mocks/MockKVProvider';
import {NoopLogger} from '../../test/mocks/NoopLogger';
import {NoopWorkerService} from '../../test/NoopWorkerService';
import processScheduledJobQueue from '../tasks/ProcessScheduledJobQueue';
import {setWorkerDependenciesForTest} from '../WorkerContext';
import {WorkerRunner} from '../WorkerRunner';

const LANE_ACK_WAIT_MS = 60000;
const LANE_MAX_DELIVER = 25;
const TASK_TYPE = 'sendScheduledMessage';

interface RecordedJob {
	taskType: string;
	payload: WorkerJobPayload;
	options: WorkerJobOptions | undefined;
}

class RecordingWorkerService implements IWorkerService {
	readonly jobs: Array<RecordedJob> = [];

	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: string,
		payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<bigint> {
		this.jobs.push({taskType, payload, options});
		return BigInt(this.jobs.length);
	}

	async cancelJob(_jobId: bigint): Promise<boolean> {
		return false;
	}

	async retryDeadLetterJob(_jobId: bigint): Promise<boolean> {
		return false;
	}
}

const queueStub = {
	getConnectionManager: () => {
		throw new Error('WorkerRunner tests never consume messages');
	},
	getStreamName: () => 'JOBS',
	enqueue: vi.fn(),
	publishToDlq: vi.fn(),
};

class ConcurrentlyClaimedKVProvider extends MockKVProvider {
	override async removeBulkDeletion(queueKey: string, secondaryKey: string): Promise<boolean> {
		await super.removeBulkDeletion(queueKey, secondaryKey);
		return false;
	}
}

class TestWorkerRunner extends WorkerRunner {
	async runJob(taskType: string, msg: JsMsg): Promise<boolean> {
		return await this.processJob(taskType, msg);
	}
}

function createRunner(scheduledJobQueue: KVScheduledJobQueueService): TestWorkerRunner {
	return new TestWorkerRunner({
		tasks: {[TASK_TYPE]: async () => {}},
		queue: queueStub,
		scheduledJobQueue,
		consumerName: 'workers_lifecycle',
		laneName: 'lifecycle',
		ledger: {} as IJobLedgerRepository,
		concurrency: 8,
		maxDeliver: LANE_MAX_DELIVER,
		ackWaitMs: LANE_ACK_WAIT_MS,
	});
}

function createJobMessage(options: {
	runAt: Date;
	payload: Record<string, unknown>;
	seq?: number;
	deliveryCount?: number;
}) {
	const envelope = {
		payload: options.payload,
		run_at: options.runAt.toISOString(),
		max_attempts: 5,
		priority: 0,
		created_at: new Date().toISOString(),
	};
	return {
		seq: options.seq ?? 1,
		subject: `jobs.${TASK_TYPE}`,
		redelivered: (options.deliveryCount ?? 1) > 1,
		data: new TextEncoder().encode(JSON.stringify(envelope)),
		info: {deliveryCount: options.deliveryCount ?? 1},
		ack: vi.fn(),
		nak: vi.fn(),
		term: vi.fn(),
	};
}

function createHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 0n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

async function drain(scheduledJobQueue: KVScheduledJobQueueService, workerService: RecordingWorkerService) {
	setWorkerDependenciesForTest({scheduledJobQueueService: scheduledJobQueue, workerService});
	await processScheduledJobQueue({}, createHelpers());
}

describe('Scheduled job due queue', () => {
	beforeAll(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	afterEach(() => {
		vi.useRealTimers();
		queueStub.enqueue.mockClear();
		queueStub.publishToDlq.mockClear();
	});

	it('naks a job due inside one ack-wait instead of parking it', async () => {
		const scheduledJobQueue = new KVScheduledJobQueueService(new MockKVProvider());
		const runner = createRunner(scheduledJobQueue);
		const msg = createJobMessage({
			runAt: new Date(Date.now() + LANE_ACK_WAIT_MS - 30000),
			payload: {scheduledMessageId: '77', __jobId: '4242'},
		});

		await runner.runJob(TASK_TYPE, msg as unknown as JsMsg);

		expect(msg.nak).toHaveBeenCalledTimes(1);
		expect(msg.ack).not.toHaveBeenCalled();
		const delayMs = msg.nak.mock.calls[0]![0] as number;
		expect(delayMs).toBeGreaterThan(0);
		expect(delayMs).toBeLessThanOrEqual(LANE_ACK_WAIT_MS - 30000);
		expect(await scheduledJobQueue.getQueueSize()).toBe(0);
	});

	it('parks a job due beyond one ack-wait without republishing it', async () => {
		const scheduledJobQueue = new KVScheduledJobQueueService(new MockKVProvider());
		const runner = createRunner(scheduledJobQueue);
		const runAt = new Date(Date.now() + 60 * 60 * 1000);
		const msg = createJobMessage({runAt, payload: {scheduledMessageId: '77', __jobId: '4242'}});

		await runner.runJob(TASK_TYPE, msg as unknown as JsMsg);

		expect(msg.ack).toHaveBeenCalledTimes(1);
		expect(msg.nak).not.toHaveBeenCalled();
		expect(queueStub.enqueue).not.toHaveBeenCalled();
		expect(await scheduledJobQueue.getQueueSize()).toBe(1);
		expect(await scheduledJobQueue.getReadyJobs(Date.now(), 10)).toHaveLength(0);
		const due = await scheduledJobQueue.getReadyJobs(runAt.getTime() - LANE_ACK_WAIT_MS, 10);
		expect(due).toHaveLength(1);
		expect(due[0]).toEqual({
			jobIdentity: '4242',
			taskType: TASK_TYPE,
			payload: {scheduledMessageId: '77'},
			runAtMs: runAt.getTime(),
			ledgerJobId: '4242',
		});
	});

	it('releases a parked job exactly once when its due time arrives', async () => {
		vi.useFakeTimers({toFake: ['Date']});
		const start = new Date('2026-08-30T12:00:00.000Z');
		vi.setSystemTime(start);
		const scheduledJobQueue = new KVScheduledJobQueueService(new MockKVProvider());
		const runner = createRunner(scheduledJobQueue);
		const runAt = new Date(start.getTime() + 60 * 60 * 1000);
		const msg = createJobMessage({runAt, payload: {scheduledMessageId: '77', __jobId: '4242'}});
		await runner.runJob(TASK_TYPE, msg as unknown as JsMsg);
		const workerService = new RecordingWorkerService();

		await drain(scheduledJobQueue, workerService);
		expect(workerService.jobs).toHaveLength(0);

		vi.setSystemTime(new Date(runAt.getTime() - LANE_ACK_WAIT_MS + 1000));
		await drain(scheduledJobQueue, workerService);

		expect(workerService.jobs).toHaveLength(1);
		expect(workerService.jobs[0]!.taskType).toBe(TASK_TYPE);
		expect(workerService.jobs[0]!.payload).toEqual({scheduledMessageId: '77', __jobId: '4242'});
		expect(workerService.jobs[0]!.options?.runAt?.getTime()).toBe(runAt.getTime());
		expect(workerService.jobs[0]!.options?.jobKey).toBe('4242');
		expect(workerService.jobs[0]!.options?.skipLedger).toBe(true);
		expect(await scheduledJobQueue.getQueueSize()).toBe(0);

		await drain(scheduledJobQueue, workerService);
		expect(workerService.jobs).toHaveLength(1);
	});

	it('runs a job parked twice only once', async () => {
		vi.useFakeTimers({toFake: ['Date']});
		const start = new Date('2026-08-30T12:00:00.000Z');
		vi.setSystemTime(start);
		const scheduledJobQueue = new KVScheduledJobQueueService(new MockKVProvider());
		const runner = createRunner(scheduledJobQueue);
		const runAt = new Date(start.getTime() + 60 * 60 * 1000);
		const payload = {scheduledMessageId: '77', __jobId: '4242'};

		await runner.runJob(TASK_TYPE, createJobMessage({runAt, payload, seq: 1}) as unknown as JsMsg);
		await runner.runJob(TASK_TYPE, createJobMessage({runAt, payload, seq: 2, deliveryCount: 2}) as unknown as JsMsg);

		expect(await scheduledJobQueue.getQueueSize()).toBe(1);

		vi.setSystemTime(new Date(runAt.getTime() - LANE_ACK_WAIT_MS + 1000));
		const workerService = new RecordingWorkerService();
		await drain(scheduledJobQueue, workerService);

		expect(workerService.jobs).toHaveLength(1);
		expect(await scheduledJobQueue.getQueueSize()).toBe(0);
	});

	it('parks a nearly delivery-exhausted job so it gets a fresh delivery budget', async () => {
		const scheduledJobQueue = new KVScheduledJobQueueService(new MockKVProvider());
		const runner = createRunner(scheduledJobQueue);
		const runAt = new Date(Date.now() + 20000);
		const msg = createJobMessage({
			runAt,
			payload: {scheduledMessageId: '77', __jobId: '4242'},
			deliveryCount: LANE_MAX_DELIVER - 1,
		});

		await runner.runJob(TASK_TYPE, msg as unknown as JsMsg);

		expect(msg.ack).toHaveBeenCalledTimes(1);
		expect(msg.nak).not.toHaveBeenCalled();
		expect(queueStub.enqueue).not.toHaveBeenCalled();
		const workerService = new RecordingWorkerService();
		await drain(scheduledJobQueue, workerService);
		expect(workerService.jobs).toHaveLength(1);
		expect(workerService.jobs[0]!.options?.runAt?.getTime()).toBe(runAt.getTime());
	});

	it('does nothing when the due queue is empty', async () => {
		const scheduledJobQueue = new KVScheduledJobQueueService(new MockKVProvider());
		const workerService = new RecordingWorkerService();

		await expect(drain(scheduledJobQueue, workerService)).resolves.toBeUndefined();

		expect(workerService.jobs).toHaveLength(0);
		expect(await scheduledJobQueue.getQueueSize()).toBe(0);
	});

	it('does not release a job another drain claimed first', async () => {
		const scheduledJobQueue = new KVScheduledJobQueueService(new ConcurrentlyClaimedKVProvider());
		const runner = createRunner(scheduledJobQueue);
		const runAt = new Date(Date.now() + 20000);
		const msg = createJobMessage({
			runAt,
			payload: {scheduledMessageId: '77', __jobId: '4242'},
			deliveryCount: LANE_MAX_DELIVER - 1,
		});
		await runner.runJob(TASK_TYPE, msg as unknown as JsMsg);
		expect(await scheduledJobQueue.getReadyJobs(Date.now(), 10)).toHaveLength(1);

		const workerService = new RecordingWorkerService();
		await drain(scheduledJobQueue, workerService);

		expect(workerService.jobs).toHaveLength(0);
		expect(await scheduledJobQueue.getQueueSize()).toBe(0);
	});
});

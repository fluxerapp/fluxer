// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LoggerInterface} from '@fluxer/logger/src/LoggerInterface';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {ConsumerMessages, JsMsg} from 'nats';
import {afterEach, beforeAll, describe, expect, it, vi} from 'vitest';
import type {IJobLedgerRepository} from '../../jobs/IJobLedgerRepository';
import {setInjectedWorkerService} from '../../middleware/ServiceRegistry';
import {NoopWorkerService} from '../../test/NoopWorkerService';
import {CronScheduler} from '../CronScheduler';
import {
	WORKER_CRON_STALE_AFTER_MS,
	WORKER_HEARTBEAT_WRITE_INTERVAL_MS,
	WORKER_LANE_STALE_AFTER_MS,
	WorkerHeartbeat,
} from '../WorkerHeartbeat';
import {WorkerRunner} from '../WorkerRunner';
import type {WorkerService} from '../WorkerService';

const HEARTBEAT_PATH = '/tmp/fluxer-worker-heartbeat-test';
const TASK_TYPE = 'processInactivityDeletions';

function createHeartbeat(): {
	heartbeat: WorkerHeartbeat;
	write: ReturnType<typeof vi.fn>;
	logger: {info: ReturnType<typeof vi.fn>; error: ReturnType<typeof vi.fn>};
} {
	const logger = {info: vi.fn(), error: vi.fn()};
	const write = vi.fn();
	const heartbeat = new WorkerHeartbeat({logger, path: HEARTBEAT_PATH, write});
	return {heartbeat, write, logger};
}

class FakeConsumerMessages {
	private notify: (() => void) | null = null;
	private closed = false;

	async close(): Promise<void> {
		this.closed = true;
		const notify = this.notify;
		this.notify = null;
		notify?.();
	}

	async *[Symbol.asyncIterator](): AsyncGenerator<JsMsg> {
		while (!this.closed) {
			await new Promise<void>((resolve) => {
				this.notify = resolve;
			});
		}
	}
}

function createRunner(messages: FakeConsumerMessages, heartbeat: WorkerHeartbeat): WorkerRunner {
	return new WorkerRunner({
		tasks: {[TASK_TYPE]: async () => {}},
		queue: {
			getConnectionManager: () => ({
				getJetStreamClient: () => ({
					consumers: {
						get: async () => ({
							consume: async () => messages as unknown as ConsumerMessages,
						}),
					},
				}),
			}),
			getStreamName: () => 'JOBS',
			publishToDlq: vi.fn(),
		},
		consumerName: 'workers_batch',
		laneName: 'batch',
		ledger: {} as IJobLedgerRepository,
		concurrency: 12,
		maxDeliver: 25,
		ackWaitMs: 120000,
		heartbeat,
	});
}

function createCronLogger(): LoggerInterface {
	const logger = {
		trace: vi.fn(),
		debug: vi.fn(),
		info: vi.fn(),
		warn: vi.fn(),
		error: vi.fn(),
		child: () => logger,
	};
	return logger as unknown as LoggerInterface;
}

function createScheduler(heartbeat: WorkerHeartbeat): CronScheduler {
	const workerService = {addJob: vi.fn().mockResolvedValue(1n)} as unknown as WorkerService;
	const kvClient = {setnx: vi.fn().mockResolvedValue(true)} as unknown as IKVProvider;
	return new CronScheduler(workerService, createCronLogger(), kvClient, heartbeat);
}

describe('Worker heartbeat', () => {
	beforeAll(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	afterEach(() => {
		vi.useRealTimers();
	});

	it('keeps rewriting the file while every component reports', async () => {
		vi.useFakeTimers();
		const {heartbeat, write} = createHeartbeat();
		const lane = heartbeat.register('lane:realtime', WORKER_LANE_STALE_AFTER_MS);

		heartbeat.start();
		expect(write).toHaveBeenCalledTimes(1);
		write.mockClear();
		for (let elapsed = 0; elapsed < WORKER_LANE_STALE_AFTER_MS * 3; elapsed += WORKER_HEARTBEAT_WRITE_INTERVAL_MS) {
			lane.report();
			await vi.advanceTimersByTimeAsync(WORKER_HEARTBEAT_WRITE_INTERVAL_MS);
		}
		heartbeat.stop();

		expect(write).toHaveBeenCalledTimes((WORKER_LANE_STALE_AFTER_MS * 3) / WORKER_HEARTBEAT_WRITE_INTERVAL_MS);
		expect(write.mock.calls[0]?.[0]).toBe(HEARTBEAT_PATH);
		expect(heartbeat.stalledComponents()).toEqual([]);
	});

	it('stops rewriting the file once a component goes stale', async () => {
		vi.useFakeTimers();
		const {heartbeat, write, logger} = createHeartbeat();
		heartbeat.register('lane:realtime', WORKER_LANE_STALE_AFTER_MS);

		heartbeat.start();
		await vi.advanceTimersByTimeAsync(WORKER_LANE_STALE_AFTER_MS);
		const writesBeforeStall = write.mock.calls.length;
		await vi.advanceTimersByTimeAsync(WORKER_LANE_STALE_AFTER_MS * 3);
		heartbeat.stop();

		expect(write).toHaveBeenCalledTimes(writesBeforeStall);
		expect(heartbeat.stalledComponents()).toEqual(['lane:realtime']);
		expect(logger.error).toHaveBeenCalledTimes(1);
		expect(logger.error.mock.calls[0]?.[1]).toBe('Worker heartbeat stalled, the container will report unhealthy');
	});

	it('resumes rewriting the file when a stalled component reports again', async () => {
		vi.useFakeTimers();
		const {heartbeat, write, logger} = createHeartbeat();
		const lane = heartbeat.register('lane:realtime', WORKER_LANE_STALE_AFTER_MS);

		heartbeat.start();
		await vi.advanceTimersByTimeAsync(WORKER_LANE_STALE_AFTER_MS * 3);
		const writesBeforeRecovery = write.mock.calls.length;
		lane.report();
		await vi.advanceTimersByTimeAsync(WORKER_HEARTBEAT_WRITE_INTERVAL_MS);
		heartbeat.stop();

		expect(write).toHaveBeenCalledTimes(writesBeforeRecovery + 1);
		expect(heartbeat.stalledComponents()).toEqual([]);
		expect(logger.info).toHaveBeenCalledWith({path: HEARTBEAT_PATH}, 'Worker heartbeat recovered');
	});

	it('ignores a released component', async () => {
		vi.useFakeTimers();
		const {heartbeat, write} = createHeartbeat();
		const lane = heartbeat.register('lane:realtime', WORKER_LANE_STALE_AFTER_MS);

		heartbeat.start();
		lane.release();
		write.mockClear();
		await vi.advanceTimersByTimeAsync(WORKER_LANE_STALE_AFTER_MS * 3);
		heartbeat.stop();

		expect(write).toHaveBeenCalledTimes((WORKER_LANE_STALE_AFTER_MS * 3) / WORKER_HEARTBEAT_WRITE_INTERVAL_MS);
		expect(heartbeat.stalledComponents()).toEqual([]);
	});

	it('reports a lane for as long as the runner is running and releases it on stop', async () => {
		vi.useFakeTimers();
		const {heartbeat, write} = createHeartbeat();
		const messages = new FakeConsumerMessages();
		const runner = createRunner(messages, heartbeat);

		heartbeat.start();
		await runner.start();
		write.mockClear();
		await vi.advanceTimersByTimeAsync(WORKER_LANE_STALE_AFTER_MS * 3);

		expect(heartbeat.stalledComponents()).toEqual([]);
		expect(write).toHaveBeenCalledTimes((WORKER_LANE_STALE_AFTER_MS * 3) / WORKER_HEARTBEAT_WRITE_INTERVAL_MS);

		await runner.stop();
		await vi.advanceTimersByTimeAsync(WORKER_LANE_STALE_AFTER_MS * 3);
		heartbeat.stop();

		expect(heartbeat.stalledComponents()).toEqual([]);
	});

	it('stalls when a running lane stops ticking, which is what a frozen worker looks like', async () => {
		vi.useFakeTimers();
		const {heartbeat, write} = createHeartbeat();
		const messages = new FakeConsumerMessages();
		const runner = createRunner(messages, heartbeat);

		heartbeat.start();
		await runner.start();
		vi.clearAllTimers();
		write.mockClear();
		await vi.advanceTimersByTimeAsync(WORKER_LANE_STALE_AFTER_MS * 3);

		expect(write).not.toHaveBeenCalled();
		expect(heartbeat.stalledComponents()).toEqual(['lane:batch']);
		expect(heartbeat.writeOnce()).toBe(false);

		await runner.stop();
		heartbeat.stop();
	});

	it('reports the cron scheduler on every tick and releases it on stop', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-01-01T00:00:00.000Z'));
		const {heartbeat} = createHeartbeat();
		const scheduler = createScheduler(heartbeat);
		scheduler.upsert('flushUserActivityBuffer', 'flushUserActivityBuffer', {}, '*/10 * * * * *', {ledger: false});

		scheduler.start();
		await vi.advanceTimersByTimeAsync(WORKER_CRON_STALE_AFTER_MS * 2);
		expect(heartbeat.stalledComponents()).toEqual([]);

		scheduler.stop();
		await vi.advanceTimersByTimeAsync(WORKER_CRON_STALE_AFTER_MS * 2);
		expect(heartbeat.stalledComponents()).toEqual([]);
	});

	it('stalls when the cron scheduler stops ticking while it is still registered', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-01-01T00:00:00.000Z'));
		const {heartbeat} = createHeartbeat();
		const scheduler = createScheduler(heartbeat);
		scheduler.upsert('flushUserActivityBuffer', 'flushUserActivityBuffer', {}, '*/10 * * * * *', {ledger: false});

		scheduler.start();
		await vi.advanceTimersByTimeAsync(1000);
		vi.clearAllTimers();
		await vi.advanceTimersByTimeAsync(WORKER_CRON_STALE_AFTER_MS * 2);

		expect(heartbeat.stalledComponents()).toEqual(['cron']);
	});
});

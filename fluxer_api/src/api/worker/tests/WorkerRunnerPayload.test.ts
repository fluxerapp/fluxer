// SPDX-License-Identifier: AGPL-3.0-or-later

import {JobCancelledError, type WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type {JsMsg} from 'nats';
import {afterEach, describe, expect, it, vi} from 'vitest';
import type {IJobLedgerRepository} from '../../jobs/IJobLedgerRepository';

vi.mock('../../middleware/ServiceRegistry', () => ({
	getWorkerService: () => ({addJob: vi.fn()}),
}));

import {WorkerRunner} from '../WorkerRunner';

class TestWorkerRunner extends WorkerRunner {
	processTestJob(taskType: string, msg: JsMsg): Promise<boolean> {
		return this.processJob(taskType, msg);
	}
}

function message(
	payload: Record<string, unknown>,
	deliveryCount: number,
	seq = 100,
): JsMsg & {
	ack: ReturnType<typeof vi.fn>;
	nak: ReturnType<typeof vi.fn>;
	term: ReturnType<typeof vi.fn>;
	working: ReturnType<typeof vi.fn>;
} {
	return {
		data: new TextEncoder().encode(JSON.stringify({payload})),
		subject: 'jobs.testTask',
		info: {deliveryCount},
		seq,
		ack: vi.fn(),
		nak: vi.fn(),
		term: vi.fn(),
		working: vi.fn(),
	} as unknown as JsMsg & {
		ack: ReturnType<typeof vi.fn>;
		nak: ReturnType<typeof vi.fn>;
		term: ReturnType<typeof vi.fn>;
		working: ReturnType<typeof vi.fn>;
	};
}

function runner(
	task: WorkerTaskHandler,
	queue: Record<string, unknown>,
	ledger: Record<string, unknown>,
	maxDeliver = 5,
): TestWorkerRunner {
	const normalizedLedger = {...ledger};
	if (typeof normalizedLedger['claimJob'] !== 'function') {
		const markRunning = normalizedLedger['markRunning'] as
			| ((jobId: bigint, lane: string) => Promise<boolean>)
			| undefined;
		normalizedLedger['claimJob'] = async (jobId: bigint, lane: string, leaseToken: string) => {
			const claimed = (await markRunning?.(jobId, lane)) ?? false;
			return claimed ? {status: 'running', lease_token: leaseToken, error_message: null} : null;
		};
	}
	normalizedLedger['renewLease'] ??= vi.fn().mockResolvedValue(true);
	normalizedLedger['renewDeadletterPublicationLease'] ??= vi.fn().mockResolvedValue(true);
	return new TestWorkerRunner({
		tasks: {testTask: task},
		queue: queue as never,
		consumerName: 'test-consumer',
		laneName: 'batch',
		ledger: normalizedLedger as unknown as IJobLedgerRepository,
		ackWaitMs: 60_000,
		maxDeliver,
	});
}

function queueForConsumer(consume: () => Promise<unknown> | unknown): Record<string, unknown> {
	return {
		enqueue: vi.fn(),
		getStreamName: () => 'jobs',
		getConnectionManager: () => ({
			getJetStreamClient: () => ({
				consumers: {
					get: vi.fn().mockResolvedValue({consume}),
				},
			}),
		}),
	};
}

describe('WorkerRunner ledger state machine', () => {
	afterEach(() => vi.useRealTimers());

	it.each([
		{__jobId: 'not-a-number'},
		{__jobId: 42},
		{__jobId: '0'},
		{__jobId: '-1'},
	])('terminates malformed ledger identities instead of executing them without a ledger: %j', async (payload) => {
		const task = vi.fn() as unknown as WorkerTaskHandler;
		const claimJob = vi.fn();
		const msg = message(payload, 1);
		const worker = runner(task, {}, {claimJob});

		await expect(worker.processTestJob('testTask', msg)).resolves.toBe(false);

		expect(msg.term).toHaveBeenCalledWith('invalid payload');
		expect(task).not.toHaveBeenCalled();
		expect(claimJob).not.toHaveBeenCalled();
	});

	it.each([
		null,
		123,
		'not-a-timestamp',
	])('terminates malformed run_at metadata instead of executing early: %j', async (runAt) => {
		const task = vi.fn() as unknown as WorkerTaskHandler;
		const msg = message({}, 1);
		msg.data = new TextEncoder().encode(JSON.stringify({payload: {}, run_at: runAt}));
		const worker = runner(task, {}, {});

		await expect(worker.processTestJob('testTask', msg)).resolves.toBe(false);

		expect(msg.term).toHaveBeenCalledWith('invalid payload');
		expect(task).not.toHaveBeenCalled();
	});

	it('preserves the ledger identity through the real future-message processing path', async () => {
		vi.useFakeTimers();
		vi.setSystemTime(new Date('2026-08-10T08:00:00.000Z'));
		const enqueue = vi.fn().mockResolvedValue('next-sequence');
		const task = vi.fn() as unknown as WorkerTaskHandler;
		const markRunning = vi.fn().mockResolvedValue(true);
		const msg = {
			...message({channelId: '123', __jobId: '42'}, 4),
			data: new TextEncoder().encode(
				JSON.stringify({
					payload: {channelId: '123', __jobId: '42'},
					run_at: '2026-08-10T08:02:00.000Z',
				}),
			),
		};
		const worker = runner(task, {enqueue}, {markRunning});

		await expect(worker.processTestJob('testTask', msg)).resolves.toBe(false);

		expect(enqueue).not.toHaveBeenCalled();
		expect(msg.ack).not.toHaveBeenCalled();
		expect(msg.nak).toHaveBeenCalledOnce();
		expect(msg.nak).toHaveBeenCalledWith(120_000);
		expect(markRunning).not.toHaveBeenCalled();
		expect(task).not.toHaveBeenCalled();
	});

	it('renews the durable lease and broker ack timer while task execution is active', async () => {
		vi.useFakeTimers();
		let finishTask: (() => void) | undefined;
		const task = vi.fn(
			() =>
				new Promise<void>((resolve) => {
					finishTask = resolve;
				}),
		);
		const renewLease = vi.fn().mockResolvedValue(true);
		const claimJob = vi.fn(async (_jobId: bigint, _lane: string, leaseToken: string) => ({
			status: 'running',
			lease_token: leaseToken,
			error_message: null,
		}));
		const msg = message({__jobId: '42'}, 1);
		const worker = runner(
			task as unknown as WorkerTaskHandler,
			{},
			{claimJob, renewLease, markSucceeded: vi.fn().mockResolvedValue(true)},
		);

		const processing = worker.processTestJob('testTask', msg);
		await vi.advanceTimersByTimeAsync(40_000);

		expect(renewLease).toHaveBeenCalledWith(42n, expect.any(String), expect.any(Date), 120_000);
		expect(msg.working).toHaveBeenCalledOnce();
		finishTask?.();
		await expect(processing).resolves.toBe(true);
	});

	it('keeps the broker message when successful work cannot be terminalized', async () => {
		const task = vi.fn().mockResolvedValue(undefined);
		const releaseForRetry = vi.fn().mockResolvedValue(true);
		const worker = runner(
			task as unknown as WorkerTaskHandler,
			{},
			{
				markRunning: vi.fn().mockResolvedValue(true),
				markSucceeded: vi.fn().mockRejectedValue(new Error('database unavailable')),
				releaseForRetry,
			},
		);
		const msg = message({__jobId: '42'}, 1);

		await expect(worker.processTestJob('testTask', msg)).resolves.toBe(false);

		expect(task).toHaveBeenCalledOnce();
		expect(releaseForRetry).toHaveBeenCalledWith(
			42n,
			'Terminalization failed after successful execution',
			false,
			expect.any(String),
		);
		expect(msg.nak).toHaveBeenCalledOnce();
		expect(msg.ack).not.toHaveBeenCalled();
	});

	it('keeps the broker message when successful terminalization loses its compare-and-set race', async () => {
		const task = vi.fn().mockResolvedValue(undefined);
		const releaseForRetry = vi.fn().mockResolvedValue(true);
		const worker = runner(
			task as unknown as WorkerTaskHandler,
			{},
			{
				markRunning: vi.fn().mockResolvedValue(true),
				markSucceeded: vi.fn().mockResolvedValue(false),
				getJob: vi.fn().mockResolvedValue({status: 'running'}),
				releaseForRetry,
			},
		);
		const msg = message({__jobId: '42'}, 1);

		await expect(worker.processTestJob('testTask', msg)).resolves.toBe(false);

		expect(releaseForRetry).toHaveBeenCalledWith(
			42n,
			'Terminalization failed after successful execution',
			false,
			expect.any(String),
		);
		expect(msg.nak).toHaveBeenCalledOnce();
		expect(msg.ack).not.toHaveBeenCalled();
	});

	it('keeps the broker message when cancellation cannot be terminalized', async () => {
		const task = vi.fn().mockRejectedValue(new JobCancelledError());
		const releaseForRetry = vi.fn().mockResolvedValue(true);
		const worker = runner(
			task as unknown as WorkerTaskHandler,
			{},
			{
				markRunning: vi.fn().mockResolvedValue(true),
				markCancelled: vi.fn().mockRejectedValue(new Error('database unavailable')),
				releaseForRetry,
			},
		);
		const msg = message({__jobId: '42'}, 1);

		await expect(worker.processTestJob('testTask', msg)).resolves.toBe(false);

		expect(releaseForRetry).toHaveBeenCalledWith(
			42n,
			'Terminalization failed after cancellation',
			false,
			expect.any(String),
		);
		expect(msg.nak).toHaveBeenCalledOnce();
		expect(msg.ack).not.toHaveBeenCalled();
	});

	it('NAKs a concurrent duplicate while another worker owns the running job', async () => {
		const task = vi.fn() as unknown as WorkerTaskHandler;
		const worker = runner(
			task,
			{},
			{
				markRunning: vi.fn().mockResolvedValue(false),
				getJob: vi.fn().mockResolvedValue({status: 'running', error_message: null}),
			},
		);
		const duplicate = message({__jobId: '42'}, 2);

		await expect(worker.processTestJob('testTask', duplicate)).resolves.toBe(false);

		expect(duplicate.nak).toHaveBeenCalledWith(5000);
		expect(duplicate.ack).not.toHaveBeenCalled();
		expect(task).not.toHaveBeenCalled();
	});

	it('closes a consumer acquired after stop begins before start can return', async () => {
		let releaseConsume: ((messages: unknown) => void) | undefined;
		const consume = vi.fn(
			() =>
				new Promise<unknown>((resolve) => {
					releaseConsume = resolve;
				}),
		);
		const close = vi.fn().mockResolvedValue(undefined);
		const messages = {
			close,
			async *[Symbol.asyncIterator]() {},
		};
		const worker = runner(vi.fn() as unknown as WorkerTaskHandler, queueForConsumer(consume), {});

		const starting = worker.start();
		await vi.waitFor(() => expect(consume).toHaveBeenCalledOnce());
		const stopping = worker.stop();
		releaseConsume?.(messages);
		await Promise.all([starting, stopping]);

		expect(close).toHaveBeenCalledOnce();
	});

	it('drains active work even when closing the consumer fails', async () => {
		let finishTask: (() => void) | undefined;
		const task = vi.fn(
			() =>
				new Promise<void>((resolve) => {
					finishTask = resolve;
				}),
		);
		const first = message({__jobId: '80'}, 1, 301);
		const closeError = new Error('close failed');
		const messages = {
			close: vi.fn().mockRejectedValue(closeError),
			async *[Symbol.asyncIterator]() {
				yield first;
			},
		};
		const worker = runner(
			task as unknown as WorkerTaskHandler,
			queueForConsumer(() => messages),
			{
				claimJob: vi.fn().mockResolvedValue({status: 'running', lease_token: 'lease-80', error_message: null}),
				markSucceeded: vi.fn().mockResolvedValue(true),
			},
		);
		await worker.start();
		await vi.waitFor(() => expect(task).toHaveBeenCalledOnce());

		let stopSettled = false;
		let observedStopError: unknown;
		const stopping = worker.stop().then(
			() => {
				stopSettled = true;
			},
			(error) => {
				stopSettled = true;
				observedStopError = error;
			},
		);
		await new Promise<void>((resolve) => setImmediate(resolve));
		expect(stopSettled).toBe(false);
		finishTask?.();
		await stopping;
		expect(observedStopError).toBe(closeError);
	});

	it('joins concurrent stop callers when consumer close fails instead of awaiting a stuck iterator', async () => {
		let finishTask: (() => void) | undefined;
		const task = vi.fn(
			() =>
				new Promise<void>((resolve) => {
					finishTask = resolve;
				}),
		);
		const closeError = new Error('close failed');
		const messages = {
			close: vi.fn().mockRejectedValue(closeError),
			async *[Symbol.asyncIterator]() {
				yield message({__jobId: '85'}, 1, 302);
				await new Promise<never>(() => undefined);
			},
		};
		const worker = runner(
			task as unknown as WorkerTaskHandler,
			queueForConsumer(() => messages),
			{
				claimJob: vi.fn().mockResolvedValue({status: 'running', lease_token: 'lease-85', error_message: null}),
				markSucceeded: vi.fn().mockResolvedValue(true),
			},
		);
		await worker.start();
		await vi.waitFor(() => expect(task).toHaveBeenCalledOnce());

		const firstStop = worker.stop();
		let secondStopSettled = false;
		let secondStopError: unknown;
		void worker.stop().then(
			() => {
				secondStopSettled = true;
			},
			(error) => {
				secondStopSettled = true;
				secondStopError = error;
			},
		);
		finishTask?.();

		await expect(firstStop).rejects.toBe(closeError);
		await new Promise<void>((resolve) => setImmediate(resolve));
		expect(secondStopSettled).toBe(true);
		expect(secondStopError).toBe(closeError);
		expect(messages.close).toHaveBeenCalledOnce();
	});

	it('drains active work and does not start prefetched work after stop begins', async () => {
		let finishTask: (() => void) | undefined;
		const task = vi
			.fn()
			.mockImplementationOnce(
				() =>
					new Promise<void>((resolve) => {
						finishTask = resolve;
					}),
			)
			.mockResolvedValue(undefined);
		const first = message({__jobId: '81'}, 1, 201);
		const second = message({__jobId: '82'}, 1, 202);
		let markSecondPrefetched: (() => void) | undefined;
		const secondPrefetched = new Promise<void>((resolve) => {
			markSecondPrefetched = resolve;
		});
		const consumerMessages = {
			close: vi.fn().mockResolvedValue(undefined),
			async *[Symbol.asyncIterator]() {
				yield first;
				markSecondPrefetched?.();
				yield second;
			},
		};
		const consume = vi.fn().mockResolvedValue(consumerMessages);
		const queue = {
			getStreamName: () => 'jobs',
			getConnectionManager: () => ({
				getJetStreamClient: () => ({consumers: {get: vi.fn().mockResolvedValue({consume})}}),
			}),
		};
		const worker = runner(task as unknown as WorkerTaskHandler, queue, {
			claimJob: vi.fn(async (_jobId: bigint, _lane: string, leaseToken: string) => ({
				status: 'running',
				lease_token: leaseToken,
				error_message: null,
			})),
			markSucceeded: vi.fn().mockResolvedValue(true),
		});

		await worker.start();
		await vi.waitFor(() => expect(task).toHaveBeenCalledOnce());
		await secondPrefetched;
		let stopped = false;
		const stopping = worker.stop().then(() => {
			stopped = true;
		});
		await new Promise<void>((resolve) => setImmediate(resolve));
		expect(stopped).toBe(false);

		finishTask?.();
		await stopping;
		expect(task).toHaveBeenCalledOnce();
		expect(second.nak).toHaveBeenCalled();
	});

	it('releases a failed attempt so the broker redelivery can claim and execute the retry', async () => {
		let status = 'queued';
		const markRunning = vi.fn(async () => {
			if (status !== 'queued') return false;
			status = 'running';
			return true;
		});
		const releaseForRetry = vi.fn(async () => {
			if (status !== 'running') return false;
			status = 'queued';
			return true;
		});
		const markSucceeded = vi.fn(async () => {
			status = 'succeeded';
			return true;
		});
		const task = vi.fn().mockRejectedValueOnce(new Error('temporary')).mockResolvedValueOnce(undefined);
		const worker = runner(
			task as unknown as WorkerTaskHandler,
			{},
			{
				markRunning,
				releaseForRetry,
				markSucceeded,
				getJob: vi.fn(async () => ({status})),
				incrementAttempts: vi.fn(),
			},
		);
		const first = message({__jobId: '42'}, 1);
		const second = message({__jobId: '42'}, 2);

		await expect(worker.processTestJob('testTask', first)).resolves.toBe(false);
		await expect(worker.processTestJob('testTask', second)).resolves.toBe(true);

		expect(task).toHaveBeenCalledTimes(2);
		expect(releaseForRetry).toHaveBeenCalledOnce();
		expect(first.nak).toHaveBeenCalledOnce();
		expect(second.ack).toHaveBeenCalledOnce();
	});

	it('does not count lease-contention redeliveries as business task attempts', async () => {
		const task = vi.fn().mockRejectedValue(new Error('attempt failed'));
		const releaseForRetry = vi.fn().mockResolvedValue(true);
		const publishToDlq = vi.fn();
		const claimJob = vi.fn(async (_jobId: bigint, _lane: string, leaseToken: string) => ({
			status: 'running',
			lease_token: leaseToken,
			error_message: null,
			attempts: 0,
			max_attempts: 5,
		}));
		const worker = runner(
			task as unknown as WorkerTaskHandler,
			{publishToDlq},
			{claimJob, renewLease: vi.fn().mockResolvedValue(true), releaseForRetry},
		);
		const msg = message({__jobId: '42'}, 10);

		await expect(worker.processTestJob('testTask', msg)).resolves.toBe(false);

		expect(releaseForRetry).toHaveBeenCalledWith(42n, 'attempt failed', true, expect.any(String));
		expect(publishToDlq).not.toHaveBeenCalled();
		expect(msg.nak).toHaveBeenCalledWith(5000);
	});

	it('renews the exclusive DLQ publication lease while publication is active', async () => {
		vi.useFakeTimers();
		let finishPublish: (() => void) | undefined;
		const publishToDlq = vi.fn(
			() =>
				new Promise<void>((resolve) => {
					finishPublish = resolve;
				}),
		);
		const renewDeadletterPublicationLease = vi.fn().mockResolvedValue(true);
		const worker = runner(
			vi.fn().mockRejectedValue(new Error('permanent')) as unknown as WorkerTaskHandler,
			{publishToDlq},
			{
				markRunning: vi.fn().mockResolvedValue(true),
				markDeadletterPending: vi.fn().mockResolvedValue({leaseToken: 'publication-owner', errorMessage: 'permanent'}),
				renewDeadletterPublicationLease,
				markDeadletter: vi.fn().mockResolvedValue(true),
			},
		);
		const msg = message({__jobId: '42'}, 5, 100);
		const processing = worker.processTestJob('testTask', msg);
		await vi.waitFor(() => expect(publishToDlq).toHaveBeenCalledOnce());

		await vi.advanceTimersByTimeAsync(20_000);
		expect(renewDeadletterPublicationLease).toHaveBeenCalledWith(42n, 'publication-owner', expect.any(Date), 60_000);
		expect(msg.working).toHaveBeenCalled();
		finishPublish?.();
		await processing;
		expect(msg.term).toHaveBeenCalledOnce();
	});

	it('does not publish DLQ work when another publisher owns the pending generation', async () => {
		const publishToDlq = vi.fn();
		const worker = runner(
			vi.fn() as unknown as WorkerTaskHandler,
			{publishToDlq},
			{
				markRunning: vi.fn().mockResolvedValue(false),
				markDeadletterPending: vi.fn().mockResolvedValue(null),
				getJob: vi.fn().mockResolvedValue({status: 'deadletter_pending', error_message: 'permanent'}),
			},
		);
		const redelivery = message({__jobId: '42'}, 6, 100);

		await expect(worker.processTestJob('testTask', redelivery)).resolves.toBe(false);

		expect(publishToDlq).not.toHaveBeenCalled();
		expect(redelivery.nak).toHaveBeenCalledWith(5000);
	});

	it('retries a durable pending DLQ publication on redelivery without executing the task again', async () => {
		let status = 'queued';
		let errorMessage: string | null = null;
		const markRunning = vi.fn(async () => {
			if (status !== 'queued') return false;
			status = 'running';
			return true;
		});
		let publicationGeneration = 0;
		const markDeadletterPending = vi.fn(async (_jobId: bigint, error: string) => {
			if (status !== 'running' && status !== 'deadletter_pending') return null;
			status = 'deadletter_pending';
			errorMessage ??= error;
			publicationGeneration += 1;
			return {leaseToken: `publication-${publicationGeneration}`, errorMessage};
		});
		const markDeadletter = vi.fn(async () => {
			if (status !== 'deadletter_pending') return false;
			status = 'deadletter';
			return true;
		});
		const getJob = vi.fn(async () => ({status, error_message: errorMessage}));
		const publishToDlq = vi.fn().mockRejectedValueOnce(new Error('nats unavailable')).mockResolvedValueOnce(undefined);
		const task = vi.fn().mockRejectedValue(new Error('permanent'));
		const worker = runner(
			task as unknown as WorkerTaskHandler,
			{publishToDlq},
			{
				markRunning,
				markDeadletterPending,
				recordDlqPublishFailure: vi.fn().mockResolvedValue(1),
				markDeadletter,
				getJob,
			},
		);
		const first = message({__jobId: '42'}, 5, 100);
		const redelivery = message({__jobId: '42'}, 6, 100);

		await expect(worker.processTestJob('testTask', first)).resolves.toBe(false);
		await expect(worker.processTestJob('testTask', redelivery)).resolves.toBe(false);

		expect(task).toHaveBeenCalledOnce();
		expect(markDeadletterPending).toHaveBeenNthCalledWith(
			1,
			42n,
			'permanent',
			expect.any(String),
			expect.any(Date),
			60_000,
		);
		expect(markDeadletterPending).toHaveBeenNthCalledWith(2, 42n, 'permanent', null, expect.any(Date), 60_000);
		expect(publishToDlq).toHaveBeenCalledTimes(2);
		expect(markDeadletter).toHaveBeenCalledOnce();
		expect(first.nak).toHaveBeenCalledOnce();
		expect(redelivery.term).toHaveBeenCalledOnce();
		expect(status).toBe('deadletter');
	});

	it('retains ledgered DLQ work after repeated publication failures until stream expiry reconciliation', async () => {
		const publishToDlq = vi.fn().mockRejectedValue(new Error('nats unavailable'));
		const markFailed = vi.fn().mockRejectedValue(new Error('database unavailable'));
		const task = vi.fn() as unknown as WorkerTaskHandler;
		const worker = runner(
			task,
			{publishToDlq},
			{
				markRunning: vi.fn().mockResolvedValue(false),
				markDeadletterPending: vi.fn().mockResolvedValue({leaseToken: 'publication-retry', errorMessage: 'permanent'}),
				getJob: vi.fn().mockResolvedValue({status: 'deadletter_pending', error_message: 'permanent'}),
				recordDlqPublishFailure: vi.fn().mockResolvedValue(3),
				markFailed,
			},
		);
		const recovery = message({__jobId: '42'}, 8, 100);

		await expect(worker.processTestJob('testTask', recovery)).resolves.toBe(false);

		expect(markFailed).not.toHaveBeenCalled();
		expect(recovery.nak).toHaveBeenCalledOnce();
		expect(recovery.term).not.toHaveBeenCalled();
		expect(task).not.toHaveBeenCalled();
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {IJobLedgerRepository} from '../../jobs/IJobLedgerRepository';
import type {JetStreamWorkerQueue} from '../JetStreamWorkerQueue';
import {WorkerService} from '../WorkerService';

function deferred<T = void>() {
	let resolve!: (value: T | PromiseLike<T>) => void;
	let reject!: (reason?: unknown) => void;
	const promise = new Promise<T>((resolvePromise, rejectPromise) => {
		resolve = resolvePromise;
		reject = rejectPromise;
	});
	return {promise, reject, resolve};
}

function createService(input: {
	queue: Pick<JetStreamWorkerQueue, 'enqueue'>;
	ledger: Pick<IJobLedgerRepository, 'createJob'> & Partial<IJobLedgerRepository>;
}) {
	const snowflake = {generate: vi.fn().mockResolvedValue(42n)} as unknown as ISnowflakeService;
	return new WorkerService(input.queue as JetStreamWorkerQueue, snowflake, input.ledger as IJobLedgerRepository);
}

describe('WorkerService', () => {
	it('persists a complete ledger record before publishing the queue message', async () => {
		const ledgerCreated = deferred();
		const createJob = vi.fn().mockReturnValue(ledgerCreated.promise);
		const enqueue = vi.fn().mockResolvedValue('stream-sequence');
		const service = createService({queue: {enqueue}, ledger: {createJob}});

		const addPromise = service.addJob('flushUserActivityBuffer', {});
		await vi.waitFor(() => expect(createJob).toHaveBeenCalledOnce());

		expect(enqueue).not.toHaveBeenCalled();
		expect(createJob).toHaveBeenCalledWith(
			expect.objectContaining({
				jobId: 42n,
				jetStreamSeq: null,
				taskType: 'flushUserActivityBuffer',
			}),
		);

		ledgerCreated.resolve();
		await addPromise;
		expect(enqueue).toHaveBeenCalledOnce();
	});

	it('marks a precreated ledger job failed when queue publication fails', async () => {
		const queueError = new Error('queue unavailable');
		const createJob = vi.fn().mockResolvedValue(undefined);
		const markEnqueueFailed = vi.fn().mockResolvedValue(true);
		const enqueue = vi.fn().mockRejectedValue(queueError);
		const service = createService({queue: {enqueue}, ledger: {createJob, markEnqueueFailed}});

		await expect(service.addJob('flushUserActivityBuffer', {})).rejects.toBe(queueError);

		expect(markEnqueueFailed).toHaveBeenCalledWith(42n, 'Failed to publish job to the worker queue');
	});

	it('does not publish when authoritative ledger creation fails', async () => {
		const ledgerError = new Error('database unavailable');
		const createJob = vi.fn().mockRejectedValue(ledgerError);
		const enqueue = vi.fn();
		const service = createService({queue: {enqueue}, ledger: {createJob}});

		await expect(service.addJob('flushUserActivityBuffer', {})).rejects.toBe(ledgerError);

		expect(enqueue).not.toHaveBeenCalled();
	});

	it('returns false when cancellation loses its terminalization compare-and-set race', async () => {
		const requestCancel = vi.fn().mockResolvedValue(false);
		const getJob = vi.fn().mockResolvedValue({status: 'running'});
		const service = createService({
			queue: {enqueue: vi.fn()},
			ledger: {createJob: vi.fn(), getJob, requestCancel},
		});

		await expect(service.cancelJob(42n)).resolves.toBe(false);
		expect(requestCancel).toHaveBeenCalledWith(42n);
	});

	it('publishes skip-ledger jobs without creating or enriching ledger state', async () => {
		const createJob = vi.fn();
		const enqueue = vi.fn().mockResolvedValue('stream-sequence');
		const service = createService({queue: {enqueue}, ledger: {createJob}});

		await service.addJob('flushUserActivityBuffer', {batch: 1}, {skipLedger: true});

		expect(createJob).not.toHaveBeenCalled();
		expect(enqueue).toHaveBeenCalledWith('flushUserActivityBuffer', {batch: 1}, {});
	});
});

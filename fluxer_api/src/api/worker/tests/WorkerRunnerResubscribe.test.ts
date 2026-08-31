// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ConsumerMessages, JsMsg} from 'nats';
import {afterEach, beforeAll, describe, expect, it, vi} from 'vitest';
import type {IJobLedgerRepository} from '../../jobs/IJobLedgerRepository';
import {setInjectedWorkerService} from '../../middleware/ServiceRegistry';
import {NoopWorkerService} from '../../test/NoopWorkerService';
import {WorkerRunner} from '../WorkerRunner';

const TASK_TYPE = 'processInactivityDeletions';
const RESUBSCRIBE_WINDOW_MS = 30000;

class FakeConsumerMessages {
	private readonly pending: Array<JsMsg> = [];
	private notify: (() => void) | null = null;
	private closed = false;
	private failure: Error | null = null;

	push(msg: JsMsg): void {
		this.pending.push(msg);
		this.wake();
	}

	abort(error: Error): void {
		this.failure = error;
		this.wake();
	}

	end(): void {
		this.closed = true;
		this.wake();
	}

	async close(): Promise<void> {
		this.end();
	}

	async *[Symbol.asyncIterator](): AsyncGenerator<JsMsg> {
		while (true) {
			while (this.pending.length > 0) {
				yield this.pending.shift()!;
			}
			if (this.failure !== null) {
				throw this.failure;
			}
			if (this.closed) {
				return;
			}
			await new Promise<void>((resolve) => {
				this.notify = resolve;
			});
		}
	}

	private wake(): void {
		const notify = this.notify;
		this.notify = null;
		notify?.();
	}
}

function createRunner(streams: Array<FakeConsumerMessages>): {runner: WorkerRunner; consumeCount: () => number} {
	let consumed = 0;
	const queue = {
		getConnectionManager: () => ({
			getJetStreamClient: () => ({
				consumers: {
					get: async () => ({
						consume: async () => {
							const stream = streams[consumed];
							consumed += 1;
							if (!stream) {
								throw new Error('no more streams');
							}
							return stream as unknown as ConsumerMessages;
						},
					}),
				},
			}),
		}),
		getStreamName: () => 'JOBS',
		publishToDlq: vi.fn(),
	};
	const runner = new WorkerRunner({
		tasks: {[TASK_TYPE]: async () => {}},
		queue,
		consumerName: 'workers_batch',
		laneName: 'batch',
		ledger: {} as IJobLedgerRepository,
		concurrency: 12,
		maxDeliver: 25,
		ackWaitMs: 120000,
	});
	return {runner, consumeCount: () => consumed};
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

describe('Worker runner resubscribe', () => {
	beforeAll(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	afterEach(() => {
		vi.useRealTimers();
	});

	it('resubscribes after the message stream fails', async () => {
		vi.useFakeTimers();
		const streams = [new FakeConsumerMessages(), new FakeConsumerMessages()];
		const {runner, consumeCount} = createRunner(streams);

		await runner.start();
		expect(consumeCount()).toBe(1);

		streams[0]!.abort(new Error('consumer deleted'));
		await vi.advanceTimersByTimeAsync(RESUBSCRIBE_WINDOW_MS);
		expect(consumeCount()).toBe(2);

		const msg = createJobMessage();
		streams[1]!.push(msg as unknown as JsMsg);
		await vi.advanceTimersByTimeAsync(0);
		expect(msg.ack).toHaveBeenCalledTimes(1);

		await runner.stop();
	});

	it('resubscribes after the message stream ends without a stop', async () => {
		vi.useFakeTimers();
		const streams = [new FakeConsumerMessages(), new FakeConsumerMessages()];
		const {runner, consumeCount} = createRunner(streams);

		await runner.start();
		streams[0]!.end();
		await vi.advanceTimersByTimeAsync(RESUBSCRIBE_WINDOW_MS);
		expect(consumeCount()).toBe(2);

		const msg = createJobMessage();
		streams[1]!.push(msg as unknown as JsMsg);
		await vi.advanceTimersByTimeAsync(0);
		expect(msg.ack).toHaveBeenCalledTimes(1);

		await runner.stop();
	});

	it('does not resubscribe after the runner is stopped', async () => {
		vi.useFakeTimers();
		const streams = [new FakeConsumerMessages(), new FakeConsumerMessages()];
		const {runner, consumeCount} = createRunner(streams);

		await runner.start();
		await runner.stop();
		await vi.advanceTimersByTimeAsync(RESUBSCRIBE_WINDOW_MS);

		expect(consumeCount()).toBe(1);
	});
});

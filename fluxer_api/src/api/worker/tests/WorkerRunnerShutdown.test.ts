// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ConsumerMessages, JsMsg} from 'nats';
import {beforeAll, describe, expect, it, vi} from 'vitest';
import type {IJobLedgerRepository} from '../../jobs/IJobLedgerRepository';
import {setInjectedWorkerService} from '../../middleware/ServiceRegistry';
import {NoopWorkerService} from '../../test/NoopWorkerService';
import {WorkerRunner} from '../WorkerRunner';

const TASK_TYPE = 'processInactivityDeletions';

class FakeConsumerMessages {
	private readonly pending: Array<JsMsg> = [];
	private notify: (() => void) | null = null;
	private closed = false;

	push(msg: JsMsg): void {
		this.pending.push(msg);
		this.wake();
	}

	async close(): Promise<void> {
		this.closed = true;
		this.wake();
	}

	async *[Symbol.asyncIterator](): AsyncGenerator<JsMsg> {
		while (true) {
			while (this.pending.length > 0) {
				yield this.pending.shift()!;
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

function createQueueStub(messages: FakeConsumerMessages) {
	return {
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
	};
}

function createRunner(task: () => Promise<void>, messages: FakeConsumerMessages): WorkerRunner {
	return new WorkerRunner({
		tasks: {[TASK_TYPE]: task},
		queue: createQueueStub(messages),
		consumerName: 'workers_batch',
		laneName: 'batch',
		ledger: {} as IJobLedgerRepository,
		concurrency: 12,
		maxDeliver: 25,
		ackWaitMs: 120000,
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

describe('Worker runner shutdown', () => {
	beforeAll(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	it('drains in-flight jobs before stop resolves', async () => {
		let started = false;
		let finished = false;
		let release: () => void = () => {};
		const pending = new Promise<void>((resolve) => {
			release = resolve;
		});
		const messages = new FakeConsumerMessages();
		const runner = createRunner(async () => {
			started = true;
			await pending;
			finished = true;
		}, messages);
		const msg = createJobMessage();

		await runner.start();
		messages.push(msg as unknown as JsMsg);
		await vi.waitFor(() => expect(started).toBe(true));

		setTimeout(release, 0);
		await runner.stop();

		expect(finished).toBe(true);
		expect(msg.ack).toHaveBeenCalledTimes(1);
	});
});

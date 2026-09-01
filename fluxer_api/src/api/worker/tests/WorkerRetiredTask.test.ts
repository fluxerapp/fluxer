// SPDX-License-Identifier: AGPL-3.0-or-later

import type {JsMsg} from 'nats';
import {afterEach, beforeAll, describe, expect, it, vi} from 'vitest';
import type {IJobLedgerRepository} from '../../jobs/IJobLedgerRepository';
import {setInjectedWorkerService} from '../../middleware/ServiceRegistry';
import {NoopWorkerService} from '../../test/NoopWorkerService';
import {WorkerRunner} from '../WorkerRunner';

const RETIRED_TASK_TYPE = 'sendScheduledMessage';
const RETIRED_REASON = 'task type retired';
const LEDGER_JOB_ID = '1509197195776110592';

const queueStub = {
	getConnectionManager: () => {
		throw new Error('WorkerRunner tests never consume messages');
	},
	getStreamName: () => 'JOBS',
	publishToDlq: vi.fn(),
};

const ledgerStub = {
	markDeadletter: vi.fn(),
};

class TestWorkerRunner extends WorkerRunner {
	async runJob(taskType: string, msg: JsMsg): Promise<boolean> {
		return await this.processJob(taskType, msg);
	}
}

function createRunner(): TestWorkerRunner {
	return new TestWorkerRunner({
		tasks: {},
		retiredTaskTypes: [RETIRED_TASK_TYPE],
		queue: queueStub,
		consumerName: 'workers_lifecycle',
		laneName: 'lifecycle',
		ledger: ledgerStub as unknown as IJobLedgerRepository,
		concurrency: 8,
		maxDeliver: 25,
		ackWaitMs: 60000,
	});
}

function createJobMessage(taskType: string, payload: Record<string, unknown>) {
	const envelope = {
		payload,
		run_at: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString(),
		max_attempts: 5,
		priority: 0,
		created_at: new Date().toISOString(),
	};
	return {
		seq: 7,
		subject: `jobs.${taskType}`,
		redelivered: false,
		data: new TextEncoder().encode(JSON.stringify(envelope)),
		info: {deliveryCount: 1},
		ack: vi.fn(),
		nak: vi.fn(),
		term: vi.fn(),
		working: vi.fn(),
	};
}

describe('Retired worker task types', () => {
	beforeAll(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	afterEach(() => {
		queueStub.publishToDlq.mockReset();
		ledgerStub.markDeadletter.mockReset();
	});

	it('dead-letters a legacy job and closes its ledger row instead of redelivering it', async () => {
		const runner = createRunner();
		const msg = createJobMessage(RETIRED_TASK_TYPE, {
			userId: '1',
			scheduledMessageId: '2',
			__jobId: LEDGER_JOB_ID,
		});

		await expect(runner.runJob(RETIRED_TASK_TYPE, msg as unknown as JsMsg)).resolves.toBe(false);

		expect(queueStub.publishToDlq).toHaveBeenCalledTimes(1);
		expect(queueStub.publishToDlq).toHaveBeenCalledWith(
			RETIRED_TASK_TYPE,
			{userId: '1', scheduledMessageId: '2'},
			expect.objectContaining({errorMessage: RETIRED_REASON, lane: 'lifecycle', originalSeq: 7}),
		);
		expect(ledgerStub.markDeadletter).toHaveBeenCalledWith(BigInt(LEDGER_JOB_ID), RETIRED_REASON);
		expect(msg.term).toHaveBeenCalledWith(RETIRED_REASON);
		expect(msg.nak).not.toHaveBeenCalled();
		expect(msg.ack).not.toHaveBeenCalled();
	});

	it('dead-letters a legacy job that carries no ledger id', async () => {
		const runner = createRunner();
		const msg = createJobMessage(RETIRED_TASK_TYPE, {userId: '1', scheduledMessageId: '2'});

		await expect(runner.runJob(RETIRED_TASK_TYPE, msg as unknown as JsMsg)).resolves.toBe(false);

		expect(queueStub.publishToDlq).toHaveBeenCalledTimes(1);
		expect(ledgerStub.markDeadletter).not.toHaveBeenCalled();
		expect(msg.term).toHaveBeenCalledWith(RETIRED_REASON);
		expect(msg.nak).not.toHaveBeenCalled();
	});

	it('redelivers a retired job when the dead-letter publish fails', async () => {
		const runner = createRunner();
		queueStub.publishToDlq.mockRejectedValueOnce(new Error('no responders'));
		const msg = createJobMessage(RETIRED_TASK_TYPE, {__jobId: LEDGER_JOB_ID});

		await expect(runner.runJob(RETIRED_TASK_TYPE, msg as unknown as JsMsg)).resolves.toBe(false);

		expect(ledgerStub.markDeadletter).not.toHaveBeenCalled();
		expect(msg.term).not.toHaveBeenCalled();
		expect(msg.nak).toHaveBeenCalledTimes(1);
	});

	it('still terminates a task type that was never registered or retired', async () => {
		const runner = createRunner();
		const msg = createJobMessage('neverShippedTask', {});

		await expect(runner.runJob('neverShippedTask', msg as unknown as JsMsg)).resolves.toBe(false);

		expect(queueStub.publishToDlq).not.toHaveBeenCalled();
		expect(msg.term).toHaveBeenCalledWith(expect.stringMatching(/unknown task type/));
	});
});

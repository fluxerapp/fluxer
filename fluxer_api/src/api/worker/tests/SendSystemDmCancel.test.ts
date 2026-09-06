// SPDX-License-Identifier: AGPL-3.0-or-later

import type {JsMsg} from 'nats';
import {afterEach, beforeAll, describe, expect, it, vi} from 'vitest';
import type {ChannelService} from '../../channel/services/ChannelService';
import type {IJobLedgerRepository} from '../../jobs/IJobLedgerRepository';
import {setInjectedWorkerService} from '../../middleware/ServiceRegistry';
import {NoopWorkerService} from '../../test/NoopWorkerService';
import type {UserRepository} from '../../user/repositories/UserRepository';
import {sendSystemDm} from '../tasks/SendSystemDm';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '../WorkerContext';
import {WorkerRunner} from '../WorkerRunner';

const TASK_TYPE = 'sendSystemDm';
const LEDGER_JOB_ID = 88n;

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

function createLedgerStub(cancelAfter: number) {
	const cancelChecks = {count: 0};
	const markCancelled = vi.fn(async () => {});
	const markSucceeded = vi.fn(async () => {});
	const ledger = {
		markRunning: async () => {},
		markSucceeded,
		markCancelled,
		markDeadletter: async () => {},
		reportProgress: async () => {},
		setContextLink: async () => {},
		isCancelRequested: async () => {
			cancelChecks.count += 1;
			return cancelChecks.count > cancelAfter;
		},
	} as unknown as IJobLedgerRepository;
	return {ledger, markCancelled, markSucceeded};
}

function createWorkerDependencies() {
	const sentChannelIds: Array<bigint> = [];
	const sentUserIds: Array<bigint> = [];
	const systemUser = {id: 0n, username: 'Fluxer', bot: true, system: true};
	const userRepository = {
		findUnique: async () => systemUser,
		findUniqueAssert: async () => systemUser,
		findExistingDmState: async () => ({id: 500n}),
		isDmChannelOpen: async () => true,
	} as unknown as UserRepository;
	const channelService = {
		messages: {
			send: {
				sendMessage: async ({channelId, user}: {channelId: bigint; user: {id: bigint}}) => {
					sentChannelIds.push(channelId);
					sentUserIds.push(user.id);
				},
			},
		},
	} as unknown as ChannelService;
	setWorkerDependenciesForTest({userRepository, channelService});
	return {sentChannelIds, sentUserIds};
}

function createJobMessage() {
	const envelope = {
		payload: {
			content: 'scheduled maintenance tonight',
			user_ids: ['11', '12', '13'],
			__jobId: LEDGER_JOB_ID.toString(),
		},
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

describe('System DM cancellation', () => {
	beforeAll(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	afterEach(() => {
		clearWorkerDependencies();
		queueStub.publishToDlq.mockClear();
	});

	it('settles the ledger row as cancelled after stopping mid-broadcast', async () => {
		const deps = createWorkerDependencies();
		const {ledger, markCancelled, markSucceeded} = createLedgerStub(1);
		const runner = new TestWorkerRunner({
			tasks: {[TASK_TYPE]: sendSystemDm},
			queue: queueStub,
			consumerName: 'workers_batch',
			laneName: 'batch',
			ledger,
			concurrency: 1,
		});
		const msg = createJobMessage();

		await expect(runner.runJob(TASK_TYPE, msg as unknown as JsMsg)).resolves.toBe(false);

		expect(deps.sentChannelIds).toHaveLength(1);
		expect(markCancelled).toHaveBeenCalledWith(LEDGER_JOB_ID);
		expect(markSucceeded).not.toHaveBeenCalled();
		expect(msg.ack).toHaveBeenCalledTimes(1);
		expect(msg.nak).not.toHaveBeenCalled();
	});

	it('settles the ledger row as succeeded when no cancel is requested', async () => {
		const deps = createWorkerDependencies();
		const {ledger, markCancelled, markSucceeded} = createLedgerStub(Number.POSITIVE_INFINITY);
		const runner = new TestWorkerRunner({
			tasks: {[TASK_TYPE]: sendSystemDm},
			queue: queueStub,
			consumerName: 'workers_batch',
			laneName: 'batch',
			ledger,
			concurrency: 1,
		});
		const msg = createJobMessage();

		await expect(runner.runJob(TASK_TYPE, msg as unknown as JsMsg)).resolves.toBe(true);

		expect(deps.sentChannelIds).toHaveLength(3);
		expect(markSucceeded).toHaveBeenCalledTimes(1);
		expect(markCancelled).not.toHaveBeenCalled();
	});

	it('sends every message as the synthetic system account', async () => {
		const deps = createWorkerDependencies();
		const {ledger} = createLedgerStub(Number.POSITIVE_INFINITY);
		const runner = new TestWorkerRunner({
			tasks: {[TASK_TYPE]: sendSystemDm},
			queue: queueStub,
			consumerName: 'workers_batch',
			laneName: 'batch',
			ledger,
			concurrency: 1,
		});
		const msg = createJobMessage();

		await expect(runner.runJob(TASK_TYPE, msg as unknown as JsMsg)).resolves.toBe(true);

		expect(deps.sentUserIds).toEqual([0n, 0n, 0n]);
	});
});

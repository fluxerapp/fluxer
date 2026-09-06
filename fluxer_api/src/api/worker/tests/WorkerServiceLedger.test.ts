// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {CreateJobInput, IJobLedgerRepository} from '../../jobs/IJobLedgerRepository';
import type {JetStreamWorkerQueue} from '../JetStreamWorkerQueue';
import {WorkerService} from '../WorkerService';

const JOB_ID = 4242n;

function createSnowflake(): ISnowflakeService {
	return {
		generate: async () => JOB_ID,
	} as unknown as ISnowflakeService;
}

function createHarness(options?: {createJobError?: Error; enqueueError?: Error}) {
	const calls: Array<string> = [];
	const createdJobs: Array<CreateJobInput> = [];
	const enqueued: Array<{taskType: string; payload: Record<string, unknown>}> = [];
	const seqUpdates: Array<{jobId: bigint; seq: string}> = [];
	const deadletters: Array<{jobId: bigint; errorMessage: string}> = [];
	const ledger = {
		createJob: async (input: CreateJobInput) => {
			calls.push('createJob');
			if (options?.createJobError) throw options.createJobError;
			createdJobs.push(input);
		},
		setJetStreamSeq: async (jobId: bigint, seq: string) => {
			calls.push('setJetStreamSeq');
			seqUpdates.push({jobId, seq});
		},
		markDeadletter: async (jobId: bigint, errorMessage: string) => {
			calls.push('markDeadletter');
			deadletters.push({jobId, errorMessage});
		},
	} as unknown as IJobLedgerRepository;
	const queue = {
		enqueue: async (taskType: string, payload: Record<string, unknown>) => {
			calls.push('enqueue');
			if (options?.enqueueError) throw options.enqueueError;
			enqueued.push({taskType, payload});
			return 'seq-9';
		},
	} as unknown as JetStreamWorkerQueue;
	const service = new WorkerService(queue, createSnowflake(), ledger);
	return {service, calls, createdJobs, enqueued, seqUpdates, deadletters};
}

describe('WorkerService ledger ordering', () => {
	test('writes the ledger row before enqueueing and patches the sequence afterwards', async () => {
		const harness = createHarness();

		const jobId = await harness.service.addJob('bulkUpdateUserFlags', {user_ids: []});

		expect(jobId).toBe(JOB_ID);
		expect(harness.calls).toEqual(['createJob', 'enqueue', 'setJetStreamSeq']);
		expect(harness.createdJobs[0]!.jetStreamSeq).toBeNull();
		expect(harness.seqUpdates).toEqual([{jobId: JOB_ID, seq: 'seq-9'}]);
		expect(harness.enqueued[0]!.payload.__jobId).toBe(JOB_ID.toString());
	});

	test('rejects without enqueueing when the ledger write fails and the caller requires it', async () => {
		const harness = createHarness({createJobError: new Error('cassandra unavailable')});

		await expect(harness.service.addJob('bulkUpdateUserFlags', {user_ids: []}, {requireLedger: true})).rejects.toThrow(
			'cassandra unavailable',
		);
		expect(harness.calls).toEqual(['createJob']);
		expect(harness.enqueued).toEqual([]);
	});

	test('still enqueues a row-less job when the ledger write fails and the caller tolerates it', async () => {
		const harness = createHarness({createJobError: new Error('cassandra unavailable')});

		const jobId = await harness.service.addJob('bulkUpdateUserFlags', {user_ids: []});

		expect(jobId).toBe(JOB_ID);
		expect(harness.calls).toEqual(['createJob', 'enqueue']);
		expect(harness.enqueued[0]!.payload).not.toHaveProperty('__jobId');
	});

	test('marks the ledger row terminal when the enqueue fails', async () => {
		const harness = createHarness({enqueueError: new Error('stream unreachable')});

		await expect(harness.service.addJob('bulkUpdateUserFlags', {user_ids: []})).rejects.toThrow('stream unreachable');
		expect(harness.calls).toEqual(['createJob', 'enqueue', 'markDeadletter']);
		expect(harness.deadletters).toEqual([{jobId: JOB_ID, errorMessage: 'stream unreachable'}]);
	});

	test('never touches the ledger when the caller skips it', async () => {
		const harness = createHarness();

		await harness.service.addJob('handleMentions', {}, {skipLedger: true});

		expect(harness.calls).toEqual(['enqueue']);
		expect(harness.enqueued[0]!.payload).not.toHaveProperty('__jobId');
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {CreateJobInput, IJobLedgerRepository} from '@app/api/jobs/IJobLedgerRepository';
import type {JetStreamWorkerQueue} from '@app/api/worker/JetStreamWorkerQueue';
import {WorkerService} from '@app/api/worker/WorkerService';
import {describe, expect, test} from 'vitest';

const JOB_ID = 4242n;
const CREATED_AT = new Date('2026-09-21T12:00:00.000Z');

function createSnowflake(): ISnowflakeService {
	return {
		generate: async () => JOB_ID,
	} as unknown as ISnowflakeService;
}

function createHarness(options?: {
	createJobError?: Error;
	enqueueError?: Error;
	duplicate?: boolean;
	discardError?: Error;
}) {
	const calls: Array<string> = [];
	const createdJobs: Array<CreateJobInput> = [];
	const enqueued: Array<{taskType: string; payload: Record<string, unknown>}> = [];
	const seqUpdates: Array<{jobId: bigint; seq: string}> = [];
	const deadletters: Array<{jobId: bigint; errorMessage: string}> = [];
	const discarded: Array<{jobId: bigint; createdAt: Date}> = [];
	const ledger = {
		createJob: async (input: CreateJobInput) => {
			calls.push('createJob');
			if (options?.createJobError) throw options.createJobError;
			createdJobs.push(input);
			return CREATED_AT;
		},
		setJetStreamSeq: async (jobId: bigint, seq: string) => {
			calls.push('setJetStreamSeq');
			seqUpdates.push({jobId, seq});
		},
		markDeadletter: async (jobId: bigint, errorMessage: string) => {
			calls.push('markDeadletter');
			deadletters.push({jobId, errorMessage});
		},
		discardJob: async (jobId: bigint, createdAt: Date) => {
			calls.push('discardJob');
			if (options?.discardError) throw options.discardError;
			discarded.push({jobId, createdAt});
		},
	} as unknown as IJobLedgerRepository;
	const queue = {
		enqueue: async (taskType: string, payload: Record<string, unknown>) => {
			calls.push('enqueue');
			if (options?.enqueueError) throw options.enqueueError;
			enqueued.push({taskType, payload});
			return {seq: 'seq-9', duplicate: options?.duplicate === true};
		},
	} as unknown as JetStreamWorkerQueue;
	const service = new WorkerService(queue, createSnowflake(), ledger);
	return {service, calls, createdJobs, enqueued, seqUpdates, deadletters, discarded};
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

	test('discards the ledger row when the stream already holds a job under the same key', async () => {
		const harness = createHarness({duplicate: true});

		const jobId = await harness.service.addJob(
			'batchGuildAuditLogMessageDeletes',
			{guildId: '1'},
			{jobKey: 'batch-audit-log-message-deletes:1'},
		);

		expect(jobId).toBe(JOB_ID);
		expect(harness.calls).toEqual(['createJob', 'enqueue', 'discardJob']);
		expect(harness.discarded).toEqual([{jobId: JOB_ID, createdAt: CREATED_AT}]);
		expect(harness.seqUpdates).toEqual([]);
	});

	test('still returns the job id when discarding the duplicate ledger row fails', async () => {
		const harness = createHarness({duplicate: true, discardError: new Error('write timeout')});

		const jobId = await harness.service.addJob(
			'batchGuildAuditLogMessageDeletes',
			{guildId: '1'},
			{jobKey: 'batch-audit-log-message-deletes:1'},
		);

		expect(jobId).toBe(JOB_ID);
		expect(harness.calls).toEqual(['createJob', 'enqueue', 'discardJob']);
	});

	test('does not touch the ledger for a duplicate the caller never ledgered', async () => {
		const harness = createHarness({duplicate: true});

		await harness.service.addJob('handleMentions', {}, {skipLedger: true, jobKey: 'mentions:1'});

		expect(harness.calls).toEqual(['enqueue']);
	});

	test('never touches the ledger when the caller skips it', async () => {
		const harness = createHarness();

		await harness.service.addJob('handleMentions', {}, {skipLedger: true});

		expect(harness.calls).toEqual(['enqueue']);
		expect(harness.enqueued[0]!.payload).not.toHaveProperty('__jobId');
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import type {ISnowflakeService} from '../infrastructure/ISnowflakeService';
import type {IJobLedgerRepository} from '../jobs/IJobLedgerRepository';
import {Logger} from '../Logger';
import type {JetStreamWorkerQueue} from './JetStreamWorkerQueue';
import {findLaneForTask, type WorkerTaskName} from './WorkerLaneConfig';
import {WorkerQueueOverflowError} from './WorkerQueueOverflowError';

export class WorkerService implements IWorkerService<WorkerTaskName> {
	private readonly queue: JetStreamWorkerQueue;
	private readonly snowflake: ISnowflakeService;
	private readonly ledger: IJobLedgerRepository;

	constructor(queue: JetStreamWorkerQueue, snowflake: ISnowflakeService, ledger: IJobLedgerRepository) {
		this.queue = queue;
		this.snowflake = snowflake;
		this.ledger = ledger;
	}

	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: WorkerTaskName,
		payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<bigint> {
		const jobId = await this.snowflake.generate();
		const skipLedger = options?.skipLedger === true;
		const requireLedger = options?.requireLedger === true;
		const payloadRecord = payload as Record<string, unknown>;
		let ledgerWritten = false;
		if (!skipLedger) {
			try {
				await this.ledger.createJob({
					jobId,
					taskType,
					payload: payloadRecord,
					requestedByUserId: options?.requestedByUserId ?? null,
					auditLogReason: options?.auditLogReason ?? null,
					maxAttempts: options?.maxAttempts ?? 5,
					runAt: options?.runAt ?? null,
					jetStreamLane: findLaneForTask(taskType),
					jetStreamSeq: null,
				});
				ledgerWritten = true;
			} catch (ledgerErr) {
				Logger.error({err: ledgerErr, jobId: jobId.toString(), taskType}, 'Failed to write ledger row for job');
				if (requireLedger) throw ledgerErr;
			}
		}
		const enrichedPayload = ledgerWritten ? {...payloadRecord, __jobId: jobId.toString()} : payloadRecord;
		try {
			const seq = await this.queue.enqueue(taskType, enrichedPayload, {
				...(options?.runAt !== undefined && {runAt: options.runAt}),
				...(options?.maxAttempts !== undefined && {maxAttempts: options.maxAttempts}),
				...(options?.priority !== undefined && {priority: options.priority}),
				...(options?.jobKey !== undefined && {jobKey: options.jobKey}),
			});
			if (ledgerWritten) {
				await this.ledger
					.setJetStreamSeq(jobId, seq)
					.catch((err) => Logger.warn({err, jobId: jobId.toString()}, 'Ledger setJetStreamSeq failed'));
			}
			Logger.debug({taskType, jobId: jobId.toString(), seq}, 'Job queued successfully');
			return jobId;
		} catch (error) {
			if (ledgerWritten) {
				await this.ledger
					.markDeadletter(jobId, error instanceof Error ? error.message : String(error))
					.catch((err) => Logger.warn({err, jobId: jobId.toString()}, 'Ledger markDeadletter failed'));
			}
			if (error instanceof WorkerQueueOverflowError) {
				Logger.warn({taskType, jobId: jobId.toString()}, 'Jobs stream is at its limit, shedding job');
				throw error;
			}
			Logger.error({error, taskType, payload}, 'Failed to queue job');
			throw error;
		}
	}

	async cancelJob(jobId: bigint): Promise<boolean> {
		const job = await this.ledger.getJob(jobId);
		if (!job) return false;
		if (job.status !== 'queued' && job.status !== 'running') return false;
		await this.ledger.requestCancel(jobId);
		return true;
	}

	async retryDeadLetterJob(_jobId: bigint): Promise<boolean> {
		return false;
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IJobLedgerRepository} from '@app/api/jobs/IJobLedgerRepository';
import {Logger} from '@app/api/Logger';
import type {JetStreamWorkerQueue} from '@app/api/worker/JetStreamWorkerQueue';
import {findLaneForTask, type WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';

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
		let ledgerCreatedAt: Date | null = null;
		if (!skipLedger) {
			try {
				ledgerCreatedAt = await this.ledger.createJob({
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
			} catch (ledgerErr) {
				Logger.error({err: ledgerErr, jobId: jobId.toString(), taskType}, 'Failed to write ledger row for job');
				if (requireLedger) throw ledgerErr;
			}
		}
		const enrichedPayload = ledgerCreatedAt !== null ? {...payloadRecord, __jobId: jobId.toString()} : payloadRecord;
		try {
			const {seq, duplicate} = await this.queue.enqueue(taskType, enrichedPayload, {
				...(options?.runAt !== undefined && {runAt: options.runAt}),
				...(options?.maxAttempts !== undefined && {maxAttempts: options.maxAttempts}),
				...(options?.priority !== undefined && {priority: options.priority}),
				...(options?.jobKey !== undefined && {jobKey: options.jobKey}),
			});
			if (ledgerCreatedAt !== null && duplicate) {
				await this.ledger
					.discardJob(jobId, ledgerCreatedAt)
					.catch((err) => Logger.warn({err, jobId: jobId.toString()}, 'Ledger discardJob failed'));
			} else if (ledgerCreatedAt !== null) {
				await this.ledger
					.setJetStreamSeq(jobId, seq)
					.catch((err) => Logger.warn({err, jobId: jobId.toString()}, 'Ledger setJetStreamSeq failed'));
			}
			Logger.debug({taskType, jobId: jobId.toString(), seq, duplicate}, 'Job queued successfully');
			return jobId;
		} catch (error) {
			if (ledgerCreatedAt !== null) {
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

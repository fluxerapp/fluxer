// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import type {ISnowflakeService} from '../infrastructure/ISnowflakeService';
import type {IJobLedgerRepository} from '../jobs/IJobLedgerRepository';
import {Logger} from '../Logger';
import type {JetStreamWorkerQueue} from './JetStreamWorkerQueue';
import {findLaneForTask, type WorkerTaskName} from './WorkerLaneConfig';

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
		const payloadRecord = payload as Record<string, unknown>;
		const enqueueOptions = {
			...(options?.runAt !== undefined && {runAt: options.runAt}),
			...(options?.maxAttempts !== undefined && {maxAttempts: options.maxAttempts}),
			...(options?.priority !== undefined && {priority: options.priority}),
			...(options?.jobKey !== undefined && {jobKey: options.jobKey}),
		};
		if (skipLedger) {
			const seq = await this.queue.enqueue(taskType, payloadRecord, enqueueOptions);
			Logger.debug({taskType, jobId: jobId.toString(), seq}, 'Job queued successfully');
			return jobId;
		}
		const lane = findLaneForTask(taskType);
		await this.ledger.createJob({
			jobId,
			taskType,
			payload: payloadRecord,
			requestedByUserId: options?.requestedByUserId ?? null,
			auditLogReason: options?.auditLogReason ?? null,
			maxAttempts: options?.maxAttempts ?? 5,
			runAt: options?.runAt ?? null,
			jetStreamLane: lane,
			jetStreamSeq: null,
		});
		const enrichedPayload = {...payloadRecord, __jobId: jobId.toString()};
		try {
			const seq = await this.queue.enqueue(taskType, enrichedPayload, enqueueOptions);
			Logger.debug({taskType, jobId: jobId.toString(), seq}, 'Job queued successfully');
			return jobId;
		} catch (error) {
			try {
				await this.ledger.markEnqueueFailed(jobId, 'Failed to publish job to the worker queue');
			} catch (ledgerError) {
				Logger.error({err: ledgerError, jobId: jobId.toString(), taskType}, 'Failed to mark unpublished job failed');
			}
			Logger.error({error, taskType}, 'Failed to queue job');
			throw error;
		}
	}

	async cancelJob(jobId: bigint): Promise<boolean> {
		const job = await this.ledger.getJob(jobId);
		if (!job) return false;
		if (job.status !== 'queued' && job.status !== 'running') return false;
		return this.ledger.requestCancel(jobId);
	}

	async retryDeadLetterJob(_jobId: bigint): Promise<boolean> {
		return false;
	}
}

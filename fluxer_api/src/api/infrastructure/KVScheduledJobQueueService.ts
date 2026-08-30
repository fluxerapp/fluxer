// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {Logger} from '../Logger';

export interface ParkedScheduledJob {
	jobIdentity: string;
	taskType: string;
	payload: WorkerJobPayload;
	runAtMs: number;
	ledgerJobId: string | null;
}

const QUEUE_KEY = 'scheduled_job_queue';
const SECONDARY_KEY_PREFIX = 'scheduled_job_queue:';

export function buildScheduledJobIdentity(
	taskType: string,
	payload: WorkerJobPayload,
	runAtMs: number,
	ledgerJobId: string | null,
): string {
	if (ledgerJobId !== null) {
		return ledgerJobId;
	}
	return createHash('sha256')
		.update(`${taskType}|${runAtMs}|${JSON.stringify(payload)}`)
		.digest('hex');
}

export class KVScheduledJobQueueService {
	constructor(private readonly kvClient: IKVProvider) {}

	private getSecondaryKey(jobIdentity: string): string {
		return `${SECONDARY_KEY_PREFIX}${jobIdentity}`;
	}

	private serializeQueueItem(job: ParkedScheduledJob): string {
		return JSON.stringify(job);
	}

	private deserializeQueueItem(value: string): ParkedScheduledJob {
		const parsed: unknown = JSON.parse(value);
		if (typeof parsed !== 'object' || parsed === null) {
			throw new Error('parked scheduled job must be a JSON object');
		}
		const {jobIdentity, taskType, payload, runAtMs, ledgerJobId} = parsed as Record<string, unknown>;
		if (typeof jobIdentity !== 'string' || typeof taskType !== 'string' || typeof runAtMs !== 'number') {
			throw new Error('parked scheduled job is missing required fields');
		}
		if (typeof payload !== 'object' || payload === null || Array.isArray(payload)) {
			throw new Error('parked scheduled job payload must be a JSON object');
		}
		return {
			jobIdentity,
			taskType,
			payload: payload as WorkerJobPayload,
			runAtMs,
			ledgerJobId: typeof ledgerJobId === 'string' ? ledgerJobId : null,
		};
	}

	async parkJob(job: ParkedScheduledJob, releaseAt: Date): Promise<void> {
		try {
			const secondaryKey = this.getSecondaryKey(job.jobIdentity);
			const value = this.serializeQueueItem(job);
			await this.kvClient.removeBulkDeletion(QUEUE_KEY, secondaryKey);
			await this.kvClient.scheduleBulkDeletion(QUEUE_KEY, secondaryKey, releaseAt.getTime(), value);
			Logger.debug({jobIdentity: job.jobIdentity, taskType: job.taskType, releaseAt}, 'Parked scheduled job');
		} catch (error) {
			Logger.error({error, jobIdentity: job.jobIdentity, taskType: job.taskType}, 'Failed to park scheduled job');
			throw error;
		}
	}

	async claimJob(jobIdentity: string): Promise<boolean> {
		try {
			return await this.kvClient.removeBulkDeletion(QUEUE_KEY, this.getSecondaryKey(jobIdentity));
		} catch (error) {
			Logger.error({error, jobIdentity}, 'Failed to claim parked scheduled job');
			throw error;
		}
	}

	async getReadyJobs(nowMs: number, limit: number): Promise<Array<ParkedScheduledJob>> {
		try {
			const results = await this.kvClient.zrangebyscore(QUEUE_KEY, '-inf', nowMs, 'LIMIT', 0, limit);
			const jobs: Array<ParkedScheduledJob> = [];
			for (const result of results) {
				try {
					jobs.push(this.deserializeQueueItem(result));
				} catch (error) {
					Logger.error({error, result}, 'Failed to parse parked scheduled job entry');
				}
			}
			return jobs;
		} catch (error) {
			Logger.error({error, nowMs, limit}, 'Failed to fetch ready parked scheduled jobs');
			throw error;
		}
	}

	async getQueueSize(): Promise<number> {
		try {
			return await this.kvClient.zcard(QUEUE_KEY);
		} catch (error) {
			Logger.error({error}, 'Failed to get parked scheduled job queue size');
			throw error;
		}
	}
}

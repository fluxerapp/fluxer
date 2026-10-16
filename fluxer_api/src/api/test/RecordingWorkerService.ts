// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';

export interface RecordedJob {
	taskType: string;
	payload: WorkerJobPayload;
	options: WorkerJobOptions | undefined;
}

export class RecordingWorkerService implements IWorkerService {
	readonly jobs: Array<RecordedJob> = [];
	failure: ((taskType: string) => Error | null) | null = null;
	private nextJobId = 1n;

	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: string,
		payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<bigint> {
		const error = this.failure?.(taskType) ?? null;
		if (error) {
			throw error;
		}
		this.jobs.push({taskType, payload, options});
		return this.nextJobId++;
	}

	async cancelJob(_jobId: bigint): Promise<boolean> {
		return false;
	}

	byTask(taskType: string): Array<RecordedJob> {
		return this.jobs.filter((job) => job.taskType === taskType);
	}

	syncJobsFor(messageId: string): Array<RecordedJob> {
		return this.byTask('syncCrosspostedMessage').filter((job) => job.payload.messageId === messageId);
	}
}

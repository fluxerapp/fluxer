// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';

export interface IWorkerService<TTaskName extends string = string> {
	addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: TTaskName,
		payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<bigint>;
	cancelJob(jobId: bigint): Promise<boolean>;
}

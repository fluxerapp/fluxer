// SPDX-License-Identifier: AGPL-3.0-or-later

import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';

type TaskHandlerMap = Record<string, WorkerTaskHandler>;

let nextSyntheticJobId = 1n;

export class SyncTaskWorkerService implements IWorkerService {
	private handlers: TaskHandlerMap;

	constructor(handlers: TaskHandlerMap) {
		this.handlers = handlers;
	}

	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: string,
		payload: TPayload,
		_options?: WorkerJobOptions,
	): Promise<bigint> {
		const jobId = nextSyntheticJobId++;
		const handler = this.handlers[taskType];
		if (handler) {
			await handler(payload, {
				logger: new NoopLogger(),
				jobId,
				addJob: async () => 0n,
				reportProgress: async () => {},
				shouldCancel: async () => false,
				setContextLink: async () => {},
			});
		}
		return jobId;
	}

	async cancelJob(_jobId: bigint): Promise<boolean> {
		return false;
	}

	async retryDeadLetterJob(_jobId: bigint): Promise<boolean> {
		return false;
	}
}

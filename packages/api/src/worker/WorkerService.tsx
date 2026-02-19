/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Logger} from '@fluxer/api/src/Logger';
import type {JetStreamWorkerQueue} from '@fluxer/api/src/worker/JetStreamWorkerQueue';
import type {IWorkerService} from '@fluxer/worker/src/contracts/IWorkerService';
import type {WorkerJobOptions, WorkerJobPayload} from '@fluxer/worker/src/contracts/WorkerTypes';

export class WorkerService implements IWorkerService {
	private readonly queue: JetStreamWorkerQueue;

	constructor(queue: JetStreamWorkerQueue) {
		this.queue = queue;
	}

	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: string,
		payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<void> {
		try {
			await this.queue.enqueue(taskType, payload, {
				...(options?.runAt !== undefined && {runAt: options.runAt}),
				...(options?.maxAttempts !== undefined && {maxAttempts: options.maxAttempts}),
				...(options?.priority !== undefined && {priority: options.priority}),
			});
			Logger.debug({taskType, payload}, 'Job queued successfully');
		} catch (error) {
			Logger.error({error, taskType, payload}, 'Failed to queue job');
			throw error;
		}
	}

	async cancelJob(_jobId: string): Promise<boolean> {
		return false;
	}

	async retryDeadLetterJob(_jobId: string): Promise<boolean> {
		return false;
	}
}

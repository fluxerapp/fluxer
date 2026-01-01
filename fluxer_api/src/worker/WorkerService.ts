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

import {quickAddJob} from 'graphile-worker';
import {Config} from '~/Config';
import {Logger} from '~/Logger';
import type {IWorkerService, WorkerJobOptions, WorkerJobPayload} from './IWorkerService';

export class WorkerService implements IWorkerService {
	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: string,
		payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<void> {
		try {
			await quickAddJob({connectionString: Config.postgres.url}, taskType, payload, options);
			Logger.debug({taskType, payload}, 'Job queued successfully');
		} catch (error) {
			Logger.error({error, taskType, payload}, 'Failed to queue job');
			throw error;
		}
	}
}

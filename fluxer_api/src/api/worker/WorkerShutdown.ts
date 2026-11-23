// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {settleWithin} from '@app/api/utils/ConcurrencyUtils';

export const WORKER_SHUTDOWN_TIMEOUT_MS = 90_000;
export const WORKER_JOB_DRAIN_MS = 60_000;

interface DrainableRuntime {
	stop(): Promise<void>;
	inFlightTaskTypes(): Array<string>;
}

export async function drainWorkerRuntime(runtime: DrainableRuntime, budgetMs: number): Promise<boolean> {
	const drained = await settleWithin(runtime.stop(), budgetMs);
	if (!drained) {
		Logger.warn(
			{budgetMs, taskTypes: runtime.inFlightTaskTypes()},
			'Worker jobs still running at shutdown, leaving them for redelivery',
		);
	}
	return drained;
}

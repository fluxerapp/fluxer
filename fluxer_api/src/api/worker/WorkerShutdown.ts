// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';

export const WORKER_SHUTDOWN_TIMEOUT_MS = 90_000;
export const WORKER_JOB_DRAIN_MS = 60_000;

interface DrainableRuntime {
	stop(): Promise<void>;
	inFlightTaskTypes(): Array<string>;
}

export async function drainWorkerRuntime(runtime: DrainableRuntime, budgetMs: number): Promise<boolean> {
	let timer: NodeJS.Timeout | undefined;
	const expired = new Promise<false>((resolve) => {
		timer = setTimeout(() => resolve(false), budgetMs);
	});
	try {
		const drained = await Promise.race([runtime.stop().then(() => true as const), expired]);
		if (!drained) {
			Logger.warn(
				{budgetMs, taskTypes: runtime.inFlightTaskTypes()},
				'Worker jobs still running at shutdown, leaving them for redelivery',
			);
		}
		return drained;
	} finally {
		clearTimeout(timer);
	}
}

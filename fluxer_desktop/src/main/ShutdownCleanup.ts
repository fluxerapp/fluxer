// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';

const logger = createChildLogger('Shutdown');

interface ShutdownCleanupStep {
	readonly name: string;
	readonly run: () => Promise<unknown>;
}

interface ShutdownCleanupResult {
	readonly unfinished: Array<string>;
}

export function settleShutdownCleanups(
	steps: ReadonlyArray<ShutdownCleanupStep>,
	deadlineMs: number,
): Promise<ShutdownCleanupResult> {
	const unfinished = new Set(steps.map((step) => step.name));
	const settled = steps.map((step) =>
		Promise.resolve()
			.then(step.run)
			.catch((error: unknown) => {
				logger.error('A shutdown cleanup step failed', {step: step.name}, error);
			})
			.finally(() => {
				unfinished.delete(step.name);
			}),
	);
	return new Promise((resolve) => {
		const deadline = setTimeout(() => {
			const names = Array.from(unfinished);
			logger.warn('Shutdown cleanup steps missed the deadline, exiting without them', {
				steps: names,
				deadlineMs,
			});
			resolve({unfinished: names});
		}, deadlineMs);
		void Promise.all(settled).then(() => {
			clearTimeout(deadline);
			resolve({unfinished: []});
		});
	});
}

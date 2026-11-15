// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerDependencies} from '@app/api/worker/WorkerDependencies';

let workerDependencies: WorkerDependencies | null = null;

export function setWorkerDependencies(dependencies: WorkerDependencies): void {
	workerDependencies = dependencies;
}

export function setWorkerDependenciesForTest(dependencies: Partial<WorkerDependencies>): void {
	workerDependencies = dependencies as WorkerDependencies;
}

export function getWorkerDependencies(): WorkerDependencies {
	if (!workerDependencies) {
		throw new Error('Worker dependencies have not been initialized. Call setWorkerDependencies() first.');
	}
	return workerDependencies;
}

export function clearWorkerDependencies(): void {
	workerDependencies = null;
}

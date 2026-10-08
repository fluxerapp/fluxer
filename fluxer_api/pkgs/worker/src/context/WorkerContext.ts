// SPDX-License-Identifier: AGPL-3.0-or-later

let workerDependencies: unknown | null = null;

export function setWorkerDependencies<T>(dependencies: T): void {
	workerDependencies = dependencies;
}

export function getWorkerDependencies<T>(): T {
	if (!workerDependencies) {
		throw new Error('Worker dependencies have not been initialized. Call setWorkerDependencies() first.');
	}
	return workerDependencies as T;
}

export function clearWorkerDependencies(): void {
	workerDependencies = null;
}

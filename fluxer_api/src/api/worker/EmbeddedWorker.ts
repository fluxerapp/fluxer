// SPDX-License-Identifier: AGPL-3.0-or-later

import {getApiJobQueue, whenSearchReady} from '@app/api/app/APILifecycle';
import {Logger} from '@app/api/Logger';
import {getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {clearWorkerDependencies, setWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerRuntime} from '@app/api/worker/WorkerMain';

let starting: Promise<WorkerRuntime | null> | null = null;
let stopping: Promise<void> | null = null;
let runtime: WorkerRuntime | null = null;
let cancelStart: () => void = () => {};
const startCancelled = new Promise<void>((resolve) => {
	cancelStart = resolve;
});

async function start(cores: number): Promise<WorkerRuntime | null> {
	const jobs = getApiJobQueue();
	if (!jobs) {
		throw new Error('The embedded worker needs the API JetStream connection');
	}
	const [{initializeWorkerDependencies}, {startWorkerRuntime}] = await Promise.all([
		import('@app/api/worker/WorkerDependencies'),
		import('@app/api/worker/WorkerMain'),
	]);
	if (stopping) return null;
	await Promise.race([whenSearchReady(), startCancelled]);
	if (stopping) return null;
	const dependencies = await initializeWorkerDependencies(getSnowflakeService());
	if (stopping) return null;
	setWorkerDependencies(dependencies);
	const started = await startWorkerRuntime({...jobs, dependencies, cores});
	runtime = started;
	Logger.info('Embedded worker started');
	return started;
}

export function startEmbeddedWorker(cores: number): Promise<void> {
	starting ??= start(cores);
	return starting.then(() => undefined);
}

export function stopEmbeddedWorker(): Promise<void> {
	cancelStart();
	stopping ??= (async () => {
		if (!starting) return;
		const [result] = await Promise.allSettled([starting]);
		runtime = null;
		if (result.status === 'fulfilled' && result.value) {
			await result.value.stop();
		}
		clearWorkerDependencies();
		Logger.info('Embedded worker stopped');
	})();
	return stopping;
}

export function embeddedWorkerStalledComponents(): Array<string> {
	return runtime?.stalledComponents() ?? [];
}

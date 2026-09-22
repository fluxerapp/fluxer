// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {type CachePurgeOutcome, createCachePurgeAdapter} from '@app/api/infrastructure/CachePurgeAdapter';
import {CachePurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import {Logger} from '@app/api/Logger';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

type CachePurgeFailure = Extract<CachePurgeOutcome, {kind: 'failed'}>;

function logFailure(context: Record<string, unknown>, failure: CachePurgeFailure): void {
	const {status, error} = failure;
	if (status === null || status === 408 || status === 429 || status >= 500) {
		Logger.warn({...context, status, error}, 'Cache purge request failed, requeued its prefixes');
		return;
	}
	Logger.error({...context, status, error}, 'Cache purge endpoint refused the request, requeued its prefixes');
}

const processCachePurgeQueue: WorkerTaskHandler = async (_payload, _helpers) => {
	const adapter = Config.cachePurge.adapter;
	if (adapter === 'none') {
		Logger.warn('Skipped a cache purge run because the adapter is none');
		return;
	}
	const queue = new CachePurgeQueue(getWorkerDependencies().kvClient);
	const prefixes = await queue.dequeueBatch();
	if (prefixes.length === 0) {
		return;
	}
	const context = {adapter, prefixes: prefixes.length};
	let outcome: CachePurgeOutcome;
	try {
		outcome = await createCachePurgeAdapter(Config.cachePurge).purge(prefixes);
	} catch (error) {
		await queue.requeue(prefixes);
		throw error;
	}
	if (outcome.kind === 'purged') {
		Logger.debug(context, 'Purged a batch of cache prefixes');
		return;
	}
	if (outcome.kind === 'rejected') {
		await queue.reject(prefixes);
		Logger.error({...context, status: outcome.status}, 'Cache purge endpoint rejected the batch, set it aside');
		return;
	}
	await queue.requeue(prefixes);
	logFailure(context, outcome);
};

export default processCachePurgeQueue;

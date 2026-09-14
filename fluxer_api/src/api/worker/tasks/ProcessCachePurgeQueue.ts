// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {
	type CachePurgeBatch,
	type CachePurgeOutcome,
	createCachePurgeAdapter,
} from '@app/api/infrastructure/CachePurgeAdapter';
import {CachePurgeQueue} from '@app/api/infrastructure/CachePurgeQueue';
import {Logger} from '@app/api/Logger';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {CachePurgeAdapterName} from '@fluxer/config/src/MasterConfig';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const RUN_DEADLINE_MS = 10_000;

type CachePurgeFailure = Extract<CachePurgeOutcome, {kind: 'failed'}>;

function describeBatch(adapter: CachePurgeAdapterName, batch: CachePurgeBatch) {
	return {adapter, exact: batch.exact.length, prefix: batch.prefix.length};
}

function logFailure(adapter: CachePurgeAdapterName, requeued: CachePurgeBatch, failure: CachePurgeFailure): void {
	const {status, error} = failure;
	const context = {...describeBatch(adapter, requeued), status, error};
	if (status === null || status === 408 || status === 429 || status >= 500) {
		Logger.warn(context, 'Cache purge request failed, requeued its entries');
		return;
	}
	Logger.error(context, 'Cache purge endpoint refused the request, requeued its entries');
}

function untriedFrom(singles: ReadonlyArray<CachePurgeBatch>, index: number): CachePurgeBatch {
	const untried = singles.slice(index);
	return {exact: untried.flatMap((single) => single.exact), prefix: untried.flatMap((single) => single.prefix)};
}

const processCachePurgeQueue: WorkerTaskHandler = async (_payload, _helpers) => {
	const adapterName = Config.cachePurge.adapter;
	if (adapterName === 'none') {
		Logger.warn('Skipped a cache purge run because the adapter is none');
		return;
	}
	const adapter = createCachePurgeAdapter(Config.cachePurge);
	const queue = new CachePurgeQueue(getWorkerDependencies().kvClient);
	const startedAt = Date.now();
	const batch = await queue.dequeueBatch();
	if (batch.exact.length === 0 && batch.prefix.length === 0) {
		return;
	}
	let outcome: CachePurgeOutcome;
	try {
		outcome = await adapter.purge(batch);
	} catch (error) {
		await queue.requeue(batch);
		throw error;
	}
	if (outcome.kind === 'purged') {
		Logger.debug(describeBatch(adapterName, batch), 'Purged a batch of cache entries');
		return;
	}
	if (outcome.kind === 'failed') {
		await queue.requeue(batch);
		logFailure(adapterName, batch, outcome);
		return;
	}
	Logger.warn(
		{...describeBatch(adapterName, batch), status: outcome.status},
		'Cache purge endpoint rejected the batch, retrying each entry on its own',
	);
	const singles: Array<CachePurgeBatch> = [
		...batch.exact.map((entry) => ({exact: [entry], prefix: []})),
		...batch.prefix.map((entry) => ({exact: [], prefix: [entry]})),
	];
	for (const [index, single] of singles.entries()) {
		if (Date.now() - startedAt >= RUN_DEADLINE_MS) {
			const untried = untriedFrom(singles, index);
			await queue.requeue(untried);
			Logger.warn(
				describeBatch(adapterName, untried),
				'Cache purge run reached its deadline, requeued untried entries',
			);
			return;
		}
		let singleOutcome: CachePurgeOutcome;
		try {
			singleOutcome = await adapter.purge(single);
			if (singleOutcome.kind === 'invalid_entries') {
				await queue.reject(single);
			}
		} catch (error) {
			await queue.requeue(untriedFrom(singles, index));
			throw error;
		}
		if (singleOutcome.kind === 'failed') {
			const untried = untriedFrom(singles, index);
			await queue.requeue(untried);
			logFailure(adapterName, untried, singleOutcome);
			return;
		}
	}
};

export default processCachePurgeQueue;

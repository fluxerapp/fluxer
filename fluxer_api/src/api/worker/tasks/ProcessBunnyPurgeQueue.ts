// SPDX-License-Identifier: AGPL-3.0-or-later

import {formatUrlForDiagnostics} from '@pkgs/http_client/src/HttpClientDiagnostics';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {ms} from 'itty-time';
import {Config} from '../../Config';
import type {BunnyPurgeQueue} from '../../infrastructure/BunnyPurgeQueue';
import {Logger} from '../../Logger';
import {EXTERNAL_RESPONSE_LIMITS} from '../../utils/ExternalResponseLimits';
import * as FetchUtils from '../../utils/FetchUtils';
import {getWorkerDependencies} from '../WorkerContext';

const EXACT_BATCH_SIZE = 120;
const PREFIX_BATCH_SIZE = 20;

type PurgeLabel = 'exact' | 'prefix';

interface PurgeStats {
	attempted: number;
	purged: number;
	requeued: number;
	failures: number;
	rateLimited: number;
}

function createEmptyPurgeStats(): PurgeStats {
	return {
		attempted: 0,
		purged: 0,
		requeued: 0,
		failures: 0,
		rateLimited: 0,
	};
}

function mergePurgeStats(target: PurgeStats, source: PurgeStats): void {
	target.attempted += source.attempted;
	target.purged += source.purged;
	target.requeued += source.requeued;
	target.failures += source.failures;
	target.rateLimited += source.rateLimited;
}

async function purgeUrls(
	urls: Array<string>,
	apiKey: string,
	queue: BunnyPurgeQueue,
	label: PurgeLabel,
): Promise<PurgeStats> {
	const stats = createEmptyPurgeStats();
	for (let index = 0; index < urls.length; index++) {
		const url = urls[index]!;
		stats.attempted++;
		try {
			const response = await fetch(`https://api.bunny.net/purge?url=${encodeURIComponent(url)}&async=true`, {
				method: 'POST',
				headers: {
					AccessKey: apiKey,
				},
				signal: AbortSignal.timeout(ms('30 seconds')),
			});
			if (response.status === 429) {
				FetchUtils.discardResponseBody(response.body, response.status);
				const remaining = urls.slice(index);
				await queue.addUrls(remaining);
				stats.requeued += remaining.length;
				stats.rateLimited += remaining.length;
				Logger.warn({label, rateLimitedUrls: remaining.length}, 'Rate limited by Bunny CDN, re-queued remaining URLs');
				break;
			}
			if (!response.ok) {
				stats.failures++;
				const errorBody = await FetchUtils.streamToBufferWithLimit(response.body, {
					maxBytes: EXTERNAL_RESPONSE_LIMITS.bunnyErrorBytes,
					headers: response.headers,
					url: response.url,
					description: 'Bunny CDN purge response',
				});
				Logger.error(
					{
						status: response.status,
						responseBodyBytes: errorBody.byteLength,
						url: formatUrlForDiagnostics(url),
						label,
					},
					'Failed to purge URL via Bunny CDN',
				);
				await queue.addUrls([url]);
				stats.requeued++;
				continue;
			}
			FetchUtils.discardResponseBody(response.body, response.status);
			stats.purged++;
		} catch (error) {
			stats.failures++;
			Logger.error({error, url: formatUrlForDiagnostics(url), label}, 'Error purging URL via Bunny CDN');
			await queue.addUrls([url]);
			stats.requeued++;
		}
	}
	return stats;
}

const processBunnyPurgeQueue: WorkerTaskHandler = async (_payload, _helpers) => {
	if (!Config.bunny.purgeEnabled) {
		Logger.debug('Bunny CDN cache purge is disabled, skipping queue processing');
		return;
	}
	if (!Config.bunny.apiKey || !Config.bunny.pullZoneId) {
		Logger.error('Bunny CDN cache purge is enabled but credentials are missing');
		return;
	}
	const deps = getWorkerDependencies();
	const queue = deps.purgeQueue as BunnyPurgeQueue;
	const apiKey = Config.bunny.apiKey;
	const totalStats = createEmptyPurgeStats();
	try {
		const queueSizeBefore = await queue.getQueueSize();
		if (queueSizeBefore === 0) {
			Logger.debug('CDN purge queue is empty');
			return;
		}
		Logger.debug({queueSize: queueSizeBefore}, 'Processing CDN purge queue');
		const exactBatch = await queue.dequeueExactBatch(EXACT_BATCH_SIZE);
		if (exactBatch.urls.length > 0) {
			const exactStats = await purgeUrls(exactBatch.urls, apiKey, queue, 'exact');
			mergePurgeStats(totalStats, exactStats);
		}
		const prefixBatch = await queue.dequeuePrefixBatch(PREFIX_BATCH_SIZE);
		if (prefixBatch.urls.length > 0) {
			const prefixStats = await purgeUrls(prefixBatch.urls, apiKey, queue, 'prefix');
			mergePurgeStats(totalStats, prefixStats);
		}
		const remainingQueueSize = await queue.getQueueSize();
		Logger.debug(
			{
				totalAttempted: totalStats.attempted,
				totalPurged: totalStats.purged,
				totalRequeued: totalStats.requeued,
				totalFailures: totalStats.failures,
				totalRateLimited: totalStats.rateLimited,
				remainingQueueSize,
			},
			'Finished processing CDN purge queue',
		);
	} catch (error) {
		Logger.error({error}, 'Error processing CDN purge queue');
		throw error;
	}
};

export default processBunnyPurgeQueue;

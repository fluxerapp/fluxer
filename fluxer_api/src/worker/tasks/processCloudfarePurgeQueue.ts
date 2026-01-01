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

import type {Task} from 'graphile-worker';
import {Config} from '~/Config';
import {Logger} from '~/Logger';
import {getWorkerDependencies} from '../WorkerContext';

const processCloudfarePurgeQueue: Task = async (_payload, _helpers) => {
	if (!Config.cloudflare.purgeEnabled) {
		Logger.debug('Cloudflare cache purge is disabled, skipping queue processing');
		return;
	}

	if (!Config.cloudflare.zoneId || !Config.cloudflare.apiToken) {
		Logger.error('Cloudflare cache purge is enabled but credentials are missing');
		return;
	}

	const queue = getWorkerDependencies().cloudflarePurgeQueue;

	try {
		const queueSize = await queue.getQueueSize();
		if (queueSize === 0) {
			Logger.debug('Cloudflare purge queue is empty');
			return;
		}

		Logger.debug({queueSize}, 'Processing Cloudflare purge queue');

		const TOKEN_BUCKET_CAPACITY = 25;
		const TOKEN_REFILL_RATE = 5;
		const TOKEN_REFILL_INTERVAL_MS = 60_000;
		const MAX_PREFIXES_PER_REQUEST = 100;
		const URLS_PER_SECOND_LIMIT = 800;

		let totalPrefixesPurged = 0;
		let totalRequestsMade = 0;

		while (totalPrefixesPurged < queueSize) {
			const tokensConsumed = await queue.tryConsumeTokens(
				1,
				TOKEN_BUCKET_CAPACITY,
				TOKEN_REFILL_RATE,
				TOKEN_REFILL_INTERVAL_MS,
			);

			if (tokensConsumed < 1) {
				Logger.debug({totalRequestsMade, totalPrefixesPurged}, 'No tokens available, stopping for now');
				break;
			}

			const batch = await queue.getBatch(MAX_PREFIXES_PER_REQUEST);

			if (batch.length === 0) {
				Logger.debug('Queue is empty, no more URLs to process');
				break;
			}

			try {
				const response = await fetch(
					`https://api.cloudflare.com/client/v4/zones/${Config.cloudflare.zoneId}/purge_cache`,
					{
						method: 'POST',
						headers: {
							'Content-Type': 'application/json',
							Authorization: `Bearer ${Config.cloudflare.apiToken}`,
						},
						body: JSON.stringify({
							prefixes: batch,
						}),
					},
				);

				if (!response.ok) {
					const errorText = await response.text();
					Logger.error(
						{status: response.status, error: errorText, prefixCount: batch.length},
						'Failed to purge Cloudflare cache',
					);

					await queue.addUrls(batch);

					if (response.status === 429) {
						Logger.warn('Rate limited by Cloudflare, will retry later');
						break;
					}

					totalRequestsMade++;
					continue;
				}

				const result = await response.json();

				if (!result.success) {
					Logger.error({result, prefixCount: batch.length}, 'Cloudflare cache purge request failed');

					await queue.addUrls(batch);
					totalRequestsMade++;
					continue;
				}

				Logger.debug(
					{count: batch.length, totalPurged: totalPrefixesPurged + batch.length},
					'Successfully purged Cloudflare cache prefix batch',
				);

				totalPrefixesPurged += batch.length;
				totalRequestsMade++;

				const delayMs = Math.max(0, (batch.length / URLS_PER_SECOND_LIMIT) * 1000);
				if (delayMs > 0) {
					await new Promise((resolve) => setTimeout(resolve, delayMs));
				}
			} catch (error) {
				Logger.error({error, prefixCount: batch.length}, 'Error processing Cloudflare purge batch');

				await queue.addUrls(batch);
				totalRequestsMade++;
			}
		}

		const remainingQueueSize = await queue.getQueueSize();
		Logger.debug(
			{
				totalPrefixesPurged,
				totalRequestsMade,
				remainingQueueSize,
			},
			'Finished processing Cloudflare purge queue',
		);
	} catch (error) {
		Logger.error({error}, 'Error processing Cloudflare purge queue');
		throw error;
	}
};

export default processCloudfarePurgeQueue;

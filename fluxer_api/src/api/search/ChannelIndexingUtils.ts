// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {CHANNEL_REINDEX_AFTER_TIMESTAMP} from '@fluxer/constants/src/ChannelConstants';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';

const CHANNEL_INDEX_JOB_ENQUEUE_CONCURRENCY = 16;

export function channelNeedsReindexing(
	indexedAt: Date | null | undefined,
	cutoffSeconds = CHANNEL_REINDEX_AFTER_TIMESTAMP,
): boolean {
	if (!indexedAt) {
		return true;
	}
	const indexedAtSeconds = Math.floor(indexedAt.getTime() / 1000);
	return indexedAtSeconds < cutoffSeconds;
}

export async function queueIndexingChannels(
	workerService: IWorkerService<WorkerTaskName>,
	channelIds: Iterable<string>,
): Promise<void> {
	await mapWithConcurrency(Array.from(channelIds), CHANNEL_INDEX_JOB_ENQUEUE_CONCURRENCY, (channelId) =>
		workerService.addJob(
			'indexChannelMessages',
			{channelId},
			{
				jobKey: `indexChannelMessages-${channelId}`,
				maxAttempts: 3,
			},
		),
	);
}

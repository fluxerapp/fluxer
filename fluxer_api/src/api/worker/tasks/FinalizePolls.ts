// SPDX-License-Identifier: AGPL-3.0-or-later

import {getExpiryBucket} from '@app/api/channel/repositories/PollMessageExpiryRepository';
import {SYSTEM_THREAD_VIEWER} from '@app/api/experiment/ChannelThreadsGate';
import {Logger} from '@app/api/Logger';
import {createRequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const BUCKET_LOOKBACK_DAYS = 3;
const FETCH_LIMIT = 200;

async function processFinalizedPolls(now = new Date()): Promise<void> {
	const {channelService, channelRepository} = getWorkerDependencies();
	const pollService = channelService.messages.poll;
	const requestCache = createRequestCache();

	const repo = pollService.expiry;
	let totalQueued = 0;
	let totalDeletedRows = 0;
	for (let offset = 0; offset <= BUCKET_LOOKBACK_DAYS; offset++) {
		const bucketDate = new Date(Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate() - offset));
		const bucket = getExpiryBucket(bucketDate);
		while (true) {
			const expired = await repo.fetchExpiredByBucket(bucket, now, FETCH_LIMIT);
			if (expired.length === 0) break;
			for (const row of expired) {
				const metadata = await repo.fetchById(row.message_id);
				if (!metadata) {
					await repo.deleteRecords(row);
					totalDeletedRows++;
					continue;
				}
				if (metadata.expires_at > row.expires_at) {
					await repo.deleteRecords(row);
					totalDeletedRows++;
					continue;
				}

				const channel = await channelRepository.channelData.findUnique(row.channel_id);
				if (!channel) {
					await repo.deleteRecords(row);
					totalDeletedRows++;
					continue;
				}

				const message = await channelRepository.messages.getMessage(channel.id, row.message_id);
				if (!message) {
					await repo.deleteRecords(row);
					totalDeletedRows++;
					continue;
				}

				try {
					await pollService.endPollBypassAuth({
						channel,
						viewer: SYSTEM_THREAD_VIEWER,
						message,
						requestCache,
						expiryRow: row,
					});
				} catch (error) {
					Logger.error(
						{error, channelId: row.channel_id.toString(), messageId: row.message_id.toString()},
						'Failed to finalize expired poll',
					);
					await repo.deleteRecords(row);
				}
				totalQueued++;
				totalDeletedRows++;
			}
		}
	}
	Logger.info(
		{
			queuedForFinalization: totalQueued,
			expiryRowsRemoved: totalDeletedRows,
			lookbackDays: BUCKET_LOOKBACK_DAYS,
		},
		'Processed poll message expiry buckets',
	);
}

const finalizePolls: WorkerTaskHandler = async () => {
	await processFinalizedPolls();
};

export default finalizePolls;

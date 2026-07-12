// SPDX-License-Identifier: AGPL-3.0-or-later

import {THREAD_AUTO_ARCHIVE_DURATION_DEFAULT} from '@fluxer/constants/src/ChannelConstants';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {mapChannelToResponse} from '../../channel/ChannelMappers';
import {getThreadArchiveDueBucket} from '../../channel/repositories/ThreadRepository';
import {dispatchChannelEvent} from '../../channel/services/ChannelGatewayDispatch';
import {Logger} from '../../Logger';
import {createRequestCache} from '../../middleware/RequestCacheMiddleware';
import {getWorkerDependencies} from '../WorkerContext';

const MINUTE_MS = 60 * 1000;
// Auto-archive durations cap at seven days, so overdue hints older than eight
// days can only be leftovers whose TTL has not elapsed yet.
const LOOKBACK_DAYS = 8;

interface SweepResult {
	scanned: number;
	archived: number;
	stale: number;
}

async function threadAutoArchiveSweepCore(): Promise<SweepResult> {
	const {channelRepository, gatewayService, userCacheService} = getWorkerDependencies();
	const now = new Date();
	const result: SweepResult = {scanned: 0, archived: 0, stale: 0};
	const currentBucket = getThreadArchiveDueBucket(now);
	for (let offset = 0; offset < LOOKBACK_DAYS; offset++) {
		const dueRows = await channelRepository.threads.fetchArchiveDueByBucket(currentBucket - offset, now);
		for (const row of dueRows) {
			result.scanned++;
			const thread = await channelRepository.channelData.findUnique(row.thread_id);
			const metadata = thread?.threadMetadata ?? null;
			if (!thread || !metadata || metadata.archived) {
				await channelRepository.threads.deleteArchiveDueRow(row);
				result.stale++;
				continue;
			}
			// Due rows are hints; re-derive the real deadline from the latest activity
			// so hints superseded by newer messages archive nothing prematurely. The
			// archive timestamp records the last unarchive, which also resets the timer.
			const lastMessageAt = thread.lastMessageId
				? snowflakeToDate(BigInt(thread.lastMessageId))
				: snowflakeToDate(BigInt(thread.id));
			const lastUnarchivedAt = metadata.archive_timestamp?.getTime() ?? 0;
			const lastActivityAt = new Date(Math.max(lastMessageAt.getTime(), lastUnarchivedAt));
			const autoArchiveDuration = metadata.auto_archive_duration ?? THREAD_AUTO_ARCHIVE_DURATION_DEFAULT;
			const dueAt = new Date(lastActivityAt.getTime() + autoArchiveDuration * MINUTE_MS);
			if (dueAt.getTime() > now.getTime()) {
				await channelRepository.threads.deleteArchiveDueRow(row);
				result.stale++;
				continue;
			}
			const updatedRow = thread.toRow();
			updatedRow.thread_metadata = {...metadata, archived: true, archive_timestamp: now};
			const updated = await channelRepository.channelData.upsert(updatedRow);
			await channelRepository.threads.setThreadArchivedRefs(updated, true);
			const response = await mapChannelToResponse({
				channel: updated,
				currentUserId: null,
				userCacheService,
				requestCache: createRequestCache(),
			});
			await dispatchChannelEvent({gatewayService, channel: updated, event: 'THREAD_UPDATE', data: response});
			await channelRepository.threads.deleteArchiveDueRow(row);
			result.archived++;
		}
	}
	return result;
}

const threadAutoArchiveSweep: WorkerTaskHandler = async (_payload, _helpers) => {
	const result = await threadAutoArchiveSweepCore();
	Logger.info(
		{scanned: result.scanned, archived: result.archived, stale: result.stale},
		'Thread auto-archive sweep complete',
	);
};

export default threadAutoArchiveSweep;

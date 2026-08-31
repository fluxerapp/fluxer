// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {AttachmentDecayRepository} from '../../attachment/AttachmentDecayRepository';
import {makeAttachmentCdnKey, makeAttachmentCdnUrl} from '../../channel/services/message/MessageHelpers';
import {Logger} from '../../Logger';
import {getExpiryBucket} from '../../utils/AttachmentDecay';
import {getWorkerDependencies} from '../WorkerContext';

const BUCKET_LOOKBACK_DAYS = 3;
const FETCH_LIMIT = 200;

export async function processExpiredAttachments(now = new Date()): Promise<void> {
	const {assetDeletionQueue, instanceConfigRepository} = getWorkerDependencies();
	const attachmentDecay = await instanceConfigRepository.getEffectiveAttachmentDecayConfig();
	if (!attachmentDecay.enabled) {
		Logger.info('Attachment decay disabled; skipping expireAttachments task');
		return;
	}
	const repo = new AttachmentDecayRepository();
	let totalQueued = 0;
	let totalDeletedRows = 0;
	for (let offset = 0; offset <= BUCKET_LOOKBACK_DAYS; offset++) {
		const bucketDate = new Date(Date.UTC(now.getUTCFullYear(), now.getUTCMonth(), now.getUTCDate() - offset));
		const bucket = getExpiryBucket(bucketDate);
		while (true) {
			const expired = await repo.fetchExpiredByBucket(bucket, now, FETCH_LIMIT);
			if (expired.length === 0) break;
			for (const row of expired) {
				const metadata = await repo.fetchById(row.attachment_id);
				if (!metadata) {
					await repo.deleteExpiryRecord({
						expiry_bucket: row.expiry_bucket,
						expires_at: row.expires_at,
						attachment_id: row.attachment_id,
					});
					totalDeletedRows++;
					continue;
				}
				if (metadata.expires_at > row.expires_at) {
					await repo.deleteExpiryRecord({
						expiry_bucket: row.expiry_bucket,
						expires_at: row.expires_at,
						attachment_id: row.attachment_id,
					});
					totalDeletedRows++;
					continue;
				}
				const s3Key = makeAttachmentCdnKey(metadata.channel_id, metadata.attachment_id, metadata.filename);
				const cdnUrl = makeAttachmentCdnUrl(metadata.channel_id, metadata.attachment_id, metadata.filename);
				await assetDeletionQueue.queueDeletion({
					s3Key,
					cdnUrl,
					reason: 'attachment-decay-expired',
				});
				await repo.deleteRecords({
					expiry_bucket: row.expiry_bucket,
					expires_at: row.expires_at,
					attachment_id: row.attachment_id,
				});
				totalQueued++;
				totalDeletedRows++;
			}
		}
	}
	Logger.info(
		{
			queuedForDeletion: totalQueued,
			expiryRowsRemoved: totalDeletedRows,
			lookbackDays: BUCKET_LOOKBACK_DAYS,
		},
		'Processed attachment decay expiry buckets',
	);
}

const expireAttachments: WorkerTaskHandler = async () => {
	await processExpiredAttachments();
};

export default expireAttachments;

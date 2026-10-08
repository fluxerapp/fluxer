// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createMessageID, type WebhookID} from '@app/api/BrandedTypes';
import {
	CrosspostTaskNames,
	crosspostSyncBucket,
	crosspostSyncChunkJobKey,
	SyncCrosspostedMessagePayloadSchema,
} from '@app/api/channel/services/message/CrosspostPropagation';
import {chunkArray, createCrosspostDeliveryService} from '@app/api/worker/tasks/utils/CrosspostWorker';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {CROSSPOST_FANOUT_CHUNK_SIZE, CROSSPOST_FANOUT_PAGE_SIZE} from '@fluxer/constants/src/AnnouncementConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const syncCrosspostedMessage: WorkerTaskHandler = async (payload, helpers) => {
	const validated = SyncCrosspostedMessagePayloadSchema.parse(payload);
	const {channelRepository} = getWorkerDependencies();
	const channelId = createChannelID(BigInt(validated.channelId));
	const messageId = createMessageID(BigInt(validated.messageId));
	if (validated.deleteSource) {
		await createCrosspostDeliveryService().deleteSourceMessage(channelId, messageId);
	}
	const bucketOrMode = validated.mode === 'update' ? String(crosspostSyncBucket(Date.now())) : validated.mode;
	const pageSize = CROSSPOST_FANOUT_PAGE_SIZE;
	let afterWebhookId: WebhookID | undefined;
	for (;;) {
		const rows = await channelRepository.crossposts.listBySourceMessage(messageId, {afterWebhookId, limit: pageSize});
		for (const chunk of chunkArray(rows, CROSSPOST_FANOUT_CHUNK_SIZE)) {
			const webhookIds = chunk.map((row) => row.webhook_id.toString());
			await helpers.addJob(
				CrosspostTaskNames.SYNC_CROSSPOST_COPIES,
				{channelId: validated.channelId, messageId: validated.messageId, mode: validated.mode, webhookIds},
				{
					jobKey: crosspostSyncChunkJobKey({
						messageId: validated.messageId,
						mode: validated.mode,
						bucketOrMode,
						firstWebhookId: webhookIds[0]!,
					}),
					skipLedger: true,
				},
			);
		}
		if (rows.length < pageSize || rows.length === 0) break;
		afterWebhookId = rows[rows.length - 1]!.webhook_id;
	}
	if (validated.mode !== 'update') {
		await channelRepository.crossposts.deleteSource({sourceChannelId: channelId, sourceMessageId: messageId});
	}
};

export default syncCrosspostedMessage;

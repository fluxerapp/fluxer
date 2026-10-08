// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createMessageID, createWebhookID} from '@app/api/BrandedTypes';
import {
	CrosspostMessagePayloadSchema,
	CrosspostTaskNames,
	crosspostChunkJobKey,
	crosspostFanoutJobKey,
	isCrosspostedMessage,
} from '@app/api/channel/services/message/CrosspostPropagation';
import {chunkArray} from '@app/api/worker/tasks/utils/CrosspostWorker';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {CROSSPOST_FANOUT_CHUNK_SIZE, CROSSPOST_FANOUT_PAGE_SIZE} from '@fluxer/constants/src/AnnouncementConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const CHUNK_ENQUEUE_CONCURRENCY = 16;

const crosspostMessage: WorkerTaskHandler = async (payload, helpers) => {
	const validated = CrosspostMessagePayloadSchema.parse(payload);
	const {channelRepository, webhookRepository} = getWorkerDependencies();
	const channelId = createChannelID(BigInt(validated.channelId));
	const messageId = createMessageID(BigInt(validated.messageId));
	const source = await channelRepository.messages.getMessage(channelId, messageId);
	if (!source || !isCrosspostedMessage(source)) {
		helpers.logger.info({messageId: validated.messageId}, 'Skipping crosspost fan-out: source is not published');
		return;
	}
	const pageSize = CROSSPOST_FANOUT_PAGE_SIZE;
	const page = await webhookRepository.listIdsBySourceChannel(channelId, {
		afterWebhookId: validated.afterWebhookId ? createWebhookID(BigInt(validated.afterWebhookId)) : undefined,
		limit: pageSize,
	});
	const chunks = chunkArray(
		page.map((entry) => entry.webhookId.toString()),
		CROSSPOST_FANOUT_CHUNK_SIZE,
	);
	for (let offset = 0; offset < chunks.length; offset += CHUNK_ENQUEUE_CONCURRENCY) {
		await Promise.all(
			chunks
				.slice(offset, offset + CHUNK_ENQUEUE_CONCURRENCY)
				.map((webhookIds) =>
					helpers.addJob(
						CrosspostTaskNames.CROSSPOST_MESSAGE_CHUNK,
						{channelId: validated.channelId, messageId: validated.messageId, webhookIds, attempt: 0},
						{jobKey: crosspostChunkJobKey(validated.messageId, webhookIds[0]!, 0), skipLedger: true},
					),
				),
		);
	}
	if (page.length === pageSize && page.length > 0) {
		const afterWebhookId = page[page.length - 1]!.webhookId.toString();
		await helpers.addJob(
			CrosspostTaskNames.CROSSPOST_MESSAGE,
			{channelId: validated.channelId, messageId: validated.messageId, afterWebhookId},
			{jobKey: crosspostFanoutJobKey(validated.messageId, afterWebhookId), skipLedger: true},
		);
	}
};

export default crosspostMessage;

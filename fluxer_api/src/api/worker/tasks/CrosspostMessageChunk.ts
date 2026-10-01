// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createMessageID, createWebhookID} from '@app/api/BrandedTypes';
import {
	CrosspostMessageChunkPayloadSchema,
	CrosspostTaskNames,
	crosspostChunkJobKey,
} from '@app/api/channel/services/message/CrosspostPropagation';
import {createCrosspostDeliveryService, mapWithConcurrency} from '@app/api/worker/tasks/utils/CrosspostWorker';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {
	CROSSPOST_CHUNK_RETRY_DELAYS_MS,
	CROSSPOST_FANOUT_CONCURRENCY,
} from '@fluxer/constants/src/AnnouncementConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const crosspostMessageChunk: WorkerTaskHandler = async (payload, helpers) => {
	const validated = CrosspostMessageChunkPayloadSchema.parse(payload);
	const channelId = createChannelID(BigInt(validated.channelId));
	const messageId = createMessageID(BigInt(validated.messageId));
	const delivery = createCrosspostDeliveryService();
	const loaded = await delivery.loadSourceForDelivery(channelId, messageId);
	if (loaded.kind === 'missing') {
		const {channelRepository} = getWorkerDependencies();
		const mode = await delivery.removalModeFor(messageId);
		for (const webhookId of validated.webhookIds) {
			const row = await channelRepository.crossposts.get(messageId, createWebhookID(BigInt(webhookId)));
			if (row) {
				await delivery.syncCopy(row, mode, null);
			}
		}
		return;
	}
	if (loaded.kind === 'inactive') {
		helpers.logger.info({messageId: validated.messageId}, 'Skipping crosspost chunk: source is not deliverable');
		return;
	}
	const failed: Array<string> = [];
	await mapWithConcurrency(validated.webhookIds, CROSSPOST_FANOUT_CONCURRENCY, async (webhookId) => {
		try {
			await delivery.deliverToWebhook(loaded.context, createWebhookID(BigInt(webhookId)));
		} catch (error) {
			failed.push(webhookId);
			helpers.logger.warn({error, messageId: validated.messageId, webhookId}, 'Crosspost delivery failed');
		}
	});
	if (failed.length === 0) {
		return;
	}
	const orderedFailed = validated.webhookIds.filter((webhookId) => failed.includes(webhookId));
	if (validated.attempt >= CROSSPOST_CHUNK_RETRY_DELAYS_MS.length) {
		helpers.logger.error(
			{messageId: validated.messageId, webhookIds: orderedFailed, attempt: validated.attempt},
			'Dropping crosspost deliveries after the last retry',
		);
		return;
	}
	const nextAttempt = validated.attempt + 1;
	await helpers.addJob(
		CrosspostTaskNames.CROSSPOST_MESSAGE_CHUNK,
		{
			channelId: validated.channelId,
			messageId: validated.messageId,
			webhookIds: orderedFailed,
			attempt: nextAttempt,
		},
		{
			jobKey: crosspostChunkJobKey(validated.messageId, orderedFailed[0]!, nextAttempt),
			runAt: new Date(Date.now() + CROSSPOST_CHUNK_RETRY_DELAYS_MS[validated.attempt]!),
			skipLedger: true,
		},
	);
};

export default crosspostMessageChunk;

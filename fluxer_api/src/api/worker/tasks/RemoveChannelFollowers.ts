// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, type MessageID, type WebhookID} from '@app/api/BrandedTypes';
import {
	CrosspostTaskNames,
	crosspostSyncJobKey,
	RemoveChannelFollowersPayloadSchema,
} from '@app/api/channel/services/message/CrosspostPropagation';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {CROSSPOST_FANOUT_PAGE_SIZE} from '@fluxer/constants/src/AnnouncementConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const removeChannelFollowers: WorkerTaskHandler = async (payload, helpers) => {
	const validated = RemoveChannelFollowersPayloadSchema.parse(payload);
	const {channelRepository, webhookRepository, gatewayService} = getWorkerDependencies();
	const sourceChannelId = createChannelID(BigInt(validated.sourceChannelId));
	if (validated.reason === 'converted') {
		const channel = await channelRepository.findUnique(sourceChannelId);
		if (channel?.type === ChannelTypes.GUILD_ANNOUNCEMENT) {
			helpers.logger.info(
				{sourceChannelId: validated.sourceChannelId},
				'Keeping channel followers: the channel is an announcement channel again',
			);
			return;
		}
	}
	const pageSize = CROSSPOST_FANOUT_PAGE_SIZE;
	let afterWebhookId: WebhookID | undefined;
	let removed = 0;
	for (;;) {
		const page = await webhookRepository.listIdsBySourceChannel(sourceChannelId, {afterWebhookId, limit: pageSize});
		for (const entry of page) {
			const webhook = await webhookRepository.findUnique(entry.webhookId);
			await webhookRepository.delete(entry.webhookId);
			removed++;
			const channelId = webhook?.channelId;
			if (channelId) {
				await gatewayService.dispatchGuild({
					guildId: webhook.guildId ?? entry.guildId,
					event: 'WEBHOOKS_UPDATE',
					data: {channel_id: channelId.toString()},
				});
			}
		}
		if (page.length < pageSize || page.length === 0) break;
		afterWebhookId = page[page.length - 1]!.webhookId;
	}
	let requeued = 0;
	if (validated.copyMode) {
		let afterMessageId: MessageID | undefined;
		for (;;) {
			const sources = await channelRepository.crossposts.listSourcesByChannel(sourceChannelId, {
				afterMessageId,
				limit: pageSize,
			});
			for (const messageId of sources) {
				await helpers.addJob(
					CrosspostTaskNames.SYNC_CROSSPOSTED_MESSAGE,
					{channelId: validated.sourceChannelId, messageId: messageId.toString(), mode: validated.copyMode},
					{jobKey: crosspostSyncJobKey(messageId.toString(), validated.copyMode), skipLedger: true},
				);
				requeued++;
			}
			if (sources.length < pageSize || sources.length === 0) break;
			afterMessageId = sources[sources.length - 1];
		}
	}
	helpers.logger.info(
		{sourceChannelId: validated.sourceChannelId, reason: validated.reason, removed, requeued},
		'Removed channel followers',
	);
};

export default removeChannelFollowers;

// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createMessageID, createWebhookID} from '@app/api/BrandedTypes';
import {SyncCrosspostCopiesPayloadSchema} from '@app/api/channel/services/message/CrosspostPropagation';
import {createCrosspostDeliveryService} from '@app/api/worker/tasks/utils/CrosspostWorker';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';

const syncCrosspostCopies: WorkerTaskHandler = async (payload, helpers) => {
	const validated = SyncCrosspostCopiesPayloadSchema.parse(payload);
	const {channelRepository} = getWorkerDependencies();
	const channelId = createChannelID(BigInt(validated.channelId));
	const messageId = createMessageID(BigInt(validated.messageId));
	const delivery = createCrosspostDeliveryService();
	const source = validated.mode === 'update' ? await delivery.loadSource(channelId, messageId) : null;
	if (validated.mode === 'update' && !source) {
		return;
	}
	const errors: Array<unknown> = [];
	for (const webhookId of validated.webhookIds) {
		const row = await channelRepository.crossposts.get(messageId, createWebhookID(BigInt(webhookId)));
		if (!row) continue;
		try {
			await delivery.syncCopy(row, validated.mode, source);
		} catch (error) {
			errors.push(error);
			helpers.logger.warn(
				{error, messageId: validated.messageId, webhookId, mode: validated.mode},
				'Crosspost copy sync failed',
			);
		}
	}
	if (errors.length > 0) {
		throw errors[0];
	}
};

export default syncCrosspostCopies;

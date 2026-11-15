// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type MessageID} from '@app/api/BrandedTypes';
import {dispatchUserUpdate} from '@app/api/user/UserMappers';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	userId: z.string(),
	scheduledAt: z.number(),
});

const CHUNK_SIZE = 200;

const countBulkMessageDeletionTargets: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing countBulkMessageDeletionTargets task');
	const userId = createUserID(BigInt(validated.userId));
	const {channelRepository, userRepository, gatewayService} = getWorkerDependencies();
	const isStillScheduled = async () => {
		const user = await userRepository.findUnique(userId);
		return user?.pendingBulkMessageDeletionAt?.getTime() === validated.scheduledAt ? user : null;
	};
	if (!(await isStillScheduled())) {
		return;
	}
	let lastMessageId: MessageID | undefined;
	const channels = new Set<string>();
	let messageCount = 0;
	while (true) {
		const messageRefs = await channelRepository.messages.listMessagesByAuthor(userId, CHUNK_SIZE, lastMessageId);
		if (messageRefs.length === 0) {
			break;
		}
		for (const {channelId, messageId} of messageRefs) {
			if (snowflakeToDate(messageId).getTime() > validated.scheduledAt) {
				continue;
			}
			channels.add(channelId.toString());
			messageCount++;
		}
		lastMessageId = messageRefs[messageRefs.length - 1].messageId;
		if (messageRefs.length < CHUNK_SIZE) {
			break;
		}
	}
	const user = await isStillScheduled();
	if (!user) {
		return;
	}
	const updatedUser = await userRepository.patchUpsert(
		userId,
		{
			pending_bulk_message_deletion_channel_count: channels.size,
			pending_bulk_message_deletion_message_count: messageCount,
		},
		user.toRow(),
	);
	await dispatchUserUpdate(gatewayService, updatedUser);
};

export default countBulkMessageDeletionTargets;

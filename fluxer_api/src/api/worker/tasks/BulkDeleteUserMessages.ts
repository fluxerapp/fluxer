// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {UserMessageDeletionService} from '@app/api/channel/services/message/UserMessageDeletionService';
import {Logger} from '@app/api/Logger';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	userId: z.string(),
	scheduledAt: z.number().optional(),
});
const bulkDeleteUserMessages: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing bulkDeleteUserMessages task');
	const userId = createUserID(BigInt(validated.userId));
	const scheduledAtMs = validated.scheduledAt ?? Number.POSITIVE_INFINITY;
	const {channelRepository, gatewayService, userRepository, storageService, purgeQueue} = getWorkerDependencies();
	const user = await userRepository.findUniqueAssert(userId);
	if (!user.pendingBulkMessageDeletionAt) {
		Logger.debug({userId}, 'User has no pending bulk message deletion, skipping (already completed)');
		return;
	}
	const deletionService = new UserMessageDeletionService({
		channelRepository,
		gatewayService,
		storageService,
		purgeQueue,
	});
	const totalDeleted = await deletionService.deleteUserMessagesBulk(userId, {
		beforeTimestamp: scheduledAtMs,
		onProgress: (deleted) => helpers.logger.debug(`Deleted ${deleted} messages so far`),
	});
	await userRepository.patchUpsert(
		userId,
		{
			pending_bulk_message_deletion_at: null,
			pending_bulk_message_deletion_channel_count: null,
			pending_bulk_message_deletion_message_count: null,
		},
		user.toRow(),
	);
	Logger.debug({userId, totalDeleted}, 'Bulk message deletion completed');
};

export default bulkDeleteUserMessages;

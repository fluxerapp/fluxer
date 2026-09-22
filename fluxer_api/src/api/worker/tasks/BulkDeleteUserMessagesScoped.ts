// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID, createUserID} from '@app/api/BrandedTypes';
import {UserMessageDeletionService} from '@app/api/channel/services/message/UserMessageDeletionService';
import {Logger} from '@app/api/Logger';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const PayloadSchema = z.object({
	userId: z.string(),
	channelIds: z.array(z.string()).optional(),
	guildId: z.string().optional(),
});
const bulkDeleteUserMessagesScoped: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing bulkDeleteUserMessagesScoped task');
	const userId = createUserID(BigInt(validated.userId));
	const {channelRepository, gatewayService, storageService, purgeQueue} = getWorkerDependencies();
	const deletionService = new UserMessageDeletionService({
		channelRepository,
		gatewayService,
		storageService,
		purgeQueue,
	});
	const totalDeleted = await deletionService.deleteUserMessagesInScope(
		userId,
		{
			channelIds: validated.channelIds?.map((id) => createChannelID(BigInt(id))),
			guildId: validated.guildId ? createGuildID(BigInt(validated.guildId)) : undefined,
		},
		{
			onProgress: (deleted) => helpers.logger.debug(`Deleted ${deleted} messages so far`),
		},
	);
	Logger.debug({userId: userId.toString(), totalDeleted}, 'Scoped bulk message deletion completed');
};

export default bulkDeleteUserMessagesScoped;

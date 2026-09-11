// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';
import {createUserID} from '../../BrandedTypes';
import {UserMessageDeletionService} from '../../channel/services/message/UserMessageDeletionService';
import {getContentMessage} from '../../content_i18n/ContentI18n';
import {Logger} from '../../Logger';
import {deserializeSelfMessageFilter, SelfMessageFilterPayload} from '../utils/SelfMessageFilterPayload';
import {getWorkerDependencies} from '../WorkerContext';

const PayloadSchema = z.object({
	userId: z.string(),
	filter: SelfMessageFilterPayload,
});
const bulkDeleteSelfMessagesImmediate: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing bulkDeleteSelfMessagesImmediate task');
	const userId = createUserID(BigInt(validated.userId));
	const {
		channelRepository,
		gatewayService,
		userRepository,
		guildRepository,
		storageService,
		purgeQueue,
		workerService,
	} = getWorkerDependencies();
	const user = await userRepository.findUnique(userId);
	if (!user) {
		helpers.logger.info({userId: validated.userId}, 'User no longer exists, skipping deletion');
		return;
	}
	const [privateChannels, userGuilds] = await Promise.all([
		userRepository.listPrivateChannels(userId),
		guildRepository.listUserGuilds(userId),
	]);
	const openDmChannelIds = new Set<string>();
	for (const channel of privateChannels) {
		if (channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) {
			openDmChannelIds.add(channel.id.toString());
		}
	}
	const currentGuildIds = new Set(userGuilds.map((guild) => guild.id.toString()));
	const deletionService = new UserMessageDeletionService({
		channelRepository,
		gatewayService,
		storageService,
		purgeQueue,
	});
	const result = await deletionService.deleteUserMessagesFiltered(
		userId,
		deserializeSelfMessageFilter(validated.filter),
		{currentGuildIds, openDmChannelIds},
		(deleted) => helpers.logger.debug(`Deleted ${deleted} messages so far`),
	);
	Logger.debug(
		{userId: userId.toString(), totalDeleted: result.totalDeleted, channelCount: result.channelCount},
		'Filtered bulk self message deletion completed',
	);
	const content = getContentMessage('bulk_message_deletion.complete', user.locale, {
		message_count: result.totalDeleted,
		channel_count: result.channelCount,
	});
	if (content && content.length > 0) {
		await workerService.addJob(
			'sendSystemDm',
			{
				content,
				user_ids: [userId.toString()],
			},
			{maxAttempts: 5},
		);
	}
};

export default bulkDeleteSelfMessagesImmediate;

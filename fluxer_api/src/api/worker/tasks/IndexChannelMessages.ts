// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';
import type {MessageID, UserID} from '../../BrandedTypes';
import {createChannelID} from '../../BrandedTypes';
import {Logger} from '../../Logger';
import {getMessageSearchService} from '../../SearchFactory';
import type {IMessageSearchService} from '../../search/IMessageSearchService';
import {getWorkerDependencies} from '../WorkerContext';

const PayloadSchema = z.object({
	channelId: z.string(),
	completionKey: z.string().optional(),
	channelCount: z.number().optional(),
});
const BULK_BATCH_SIZE = 5000;
function getMessageIndexServices(): Array<IMessageSearchService> {
	const services: Array<IMessageSearchService> = [];
	const defaultService = getMessageSearchService();
	if (defaultService) {
		services.push(defaultService);
	}
	return services;
}

const indexChannelMessages: WorkerTaskHandler = async (payload) => {
	const validated = PayloadSchema.parse(payload);
	const searchServices = getMessageIndexServices();
	if (searchServices.length === 0) {
		throw new Error('Search service is not available in this worker process');
	}
	const channelId = createChannelID(BigInt(validated.channelId));
	const {channelRepository, userRepository, kvClient} = getWorkerDependencies();
	const authorBotCache = new Map<UserID, boolean>();
	let cursor: MessageID | undefined;
	let totalIndexed = 0;
	try {
		while (true) {
			const messages = await channelRepository.listMessages(channelId, cursor, BULK_BATCH_SIZE);
			if (messages.length === 0) {
				break;
			}
			const newAuthorIds = [
				...new Set(
					messages.map((m) => m.authorId).filter((id): id is UserID => id !== null && !authorBotCache.has(id)),
				),
			];
			if (newAuthorIds.length > 0) {
				const users = await userRepository.listUsers(newAuthorIds);
				for (const user of users) {
					authorBotCache.set(user.id, user.isBot);
				}
			}
			await Promise.all(searchServices.map((s) => s.bulkIndexMessages(messages, authorBotCache)));
			totalIndexed += messages.length;
			cursor = messages[messages.length - 1]!.id;
			Logger.debug(
				{channelId: channelId.toString(), indexed: totalIndexed, batch: messages.length},
				'Bulk indexed message batch',
			);
			if (messages.length < BULK_BATCH_SIZE) {
				break;
			}
		}
		if (validated.completionKey && validated.channelCount) {
			const completed = await kvClient.incr(validated.completionKey);
			if (completed >= validated.channelCount) {
				try {
					await Promise.all(searchServices.map((s) => s.refreshIndex()));
				} catch (error) {
					Logger.warn({error}, 'Search refresh after bulk indexing failed');
				}
				await kvClient.del(validated.completionKey);
				Logger.info({completionKey: validated.completionKey}, 'All channels indexed');
			}
		}
		const channel = await channelRepository.findUnique(channelId);
		if (channel) {
			await channelRepository.upsert({...channel.toRow(), indexed_at: new Date()});
		}
		Logger.info({channelId: channelId.toString(), totalIndexed}, 'Bulk channel indexing complete');
	} catch (error) {
		Logger.error({error, channelId: channelId.toString()}, 'Failed to bulk index channel messages');
		throw error;
	}
};

export default indexChannelMessages;

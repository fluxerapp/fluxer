// SPDX-License-Identifier: AGPL-3.0-or-later

import {CrosspostDeliveryService} from '@app/api/channel/services/message/CrosspostDeliveryService';
import {getWorkerDependencies} from '@app/api/worker/WorkerContext';

export function createCrosspostDeliveryService(): CrosspostDeliveryService {
	const deps = getWorkerDependencies();
	return new CrosspostDeliveryService({
		channelRepository: deps.channelRepository,
		webhookRepository: deps.webhookRepository,
		userRepository: deps.userRepository,
		guildRepository: deps.guildRepository,
		gatewayService: deps.gatewayService,
		storageService: deps.storageService,
		avatarService: deps.avatarService,
		purgeQueue: deps.purgeQueue,
		snowflakeService: deps.snowflakeService,
		cacheService: deps.cacheService,
		limitConfigService: deps.limitConfigService,
		persistenceService: deps.channelService.messages.persistence,
		searchService: deps.channelService.messages.search,
	});
}

export async function mapWithConcurrency<T, R>(
	items: ReadonlyArray<T>,
	concurrency: number,
	fn: (item: T) => Promise<R>,
): Promise<Array<R>> {
	const results: Array<R> = new Array(items.length);
	let nextIndex = 0;
	const workers = Array.from({length: Math.min(concurrency, items.length)}, async () => {
		while (nextIndex < items.length) {
			const index = nextIndex++;
			results[index] = await fn(items[index]!);
		}
	});
	await Promise.all(workers);
	return results;
}

export function chunkArray<T>(items: ReadonlyArray<T>, size: number): Array<Array<T>> {
	const chunks: Array<Array<T>> = [];
	for (let index = 0; index < items.length; index += size) {
		chunks.push(items.slice(index, index + size));
	}
	return chunks;
}

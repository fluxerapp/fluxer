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

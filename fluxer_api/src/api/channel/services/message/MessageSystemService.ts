// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {createMessageID} from '@app/api/BrandedTypes';
import type {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import type {MessageDispatchService} from '@app/api/channel/services/message/MessageDispatchService';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';

export class MessageSystemService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private guildRepository: IGuildRepositoryAggregate,
		private snowflakeService: ISnowflakeService,
		private persistenceService: MessagePersistenceService,
		private dispatchService: MessageDispatchService,
	) {}

	async sendJoinSystemMessage({
		guildId,
		userId,
		requestCache,
	}: {
		guildId: GuildID;
		userId: UserID;
		requestCache: RequestCache;
	}): Promise<void> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild?.systemChannelId) return;
		const systemChannel = await this.channelRepository.channelData.findUnique(guild.systemChannelId);
		if (!systemChannel) return;
		const messageId = createMessageID(await this.snowflakeService.generateForChannel(systemChannel.id));
		const {message} = await this.persistenceService.createMessage({
			messageId,
			channelId: systemChannel.id,
			userId,
			type: MessageTypes.USER_JOIN,
			content: null,
			flags: 0,
			guildId,
			channel: systemChannel,
		});
		await this.dispatchService.dispatchMessageCreate({channel: systemChannel, message, requestCache});
	}
}

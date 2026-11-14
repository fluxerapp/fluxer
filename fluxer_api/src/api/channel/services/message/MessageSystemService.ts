// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {createMessageID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {dispatchMessageCreateBroadcast} from '@app/api/channel/services/message/MessageGatewayDispatch';
import type {MessagePersistenceService} from '@app/api/channel/services/message/MessagePersistenceService';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {MessageTypes, type MessageTypeValue} from '@fluxer/constants/src/ChannelConstants';

export class MessageSystemService {
	constructor(
		private channelRepository: ChannelRepository,
		private guildRepository: GuildRepository,
		private snowflakeService: ISnowflakeService,
		private persistenceService: MessagePersistenceService,
		private gatewayService: IGatewayService,
	) {}

	async sendJoinSystemMessage({guildId, userId}: {guildId: GuildID; userId: UserID}): Promise<void> {
		await this.sendSystemMessage({guildId, userId, type: MessageTypes.USER_JOIN});
	}

	async sendOwnershipTransferSystemMessage({
		guildId,
		newOwnerId,
	}: {
		guildId: GuildID;
		newOwnerId: UserID;
	}): Promise<void> {
		await this.sendSystemMessage({guildId, userId: newOwnerId, type: MessageTypes.OWNERSHIP_TRANSFER});
	}

	private async sendSystemMessage({
		guildId,
		userId,
		type,
	}: {
		guildId: GuildID;
		userId: UserID;
		type: MessageTypeValue;
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
			type,
			content: null,
			flags: 0,
			guildId,
			channel: systemChannel,
		});
		await dispatchMessageCreateBroadcast({gatewayService: this.gatewayService, channel: systemChannel, message});
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PersonaID, UserID} from '@app/api/BrandedTypes';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import {ensureTextChannel, isOperationDisabled} from '@app/api/channel/services/message/MessageHelpers';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {Channel} from '@app/api/models/Channel';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';

export class MessageReadStateService {
	constructor(private gatewayService: IGatewayService) {}
	async startTyping({authChannel, userId, personaId}: {authChannel: AuthenticatedChannel; userId: UserID; personaId?: PersonaID}): Promise<void> {
		const {channel, guild} = authChannel;
		ensureTextChannel(channel);
		if (isOperationDisabled(guild, GuildOperations.TYPING_EVENTS)) {
			return;
		}
		await this.dispatchTypingStart({channel, userId, personaId});
	}

	private async dispatchTypingStart({channel, userId, personaId}: {channel: Channel; userId: UserID; personaId?: PersonaID}): Promise<void> {
		await dispatchChannelEvent({
			gatewayService: this.gatewayService,
			channel,
			event: 'TYPING_START',
			data: {
				channel_id: channel.id.toString(),
				persona_id: personaId?.toString(),
				user_id: userId.toString(),
				timestamp: Math.floor(Date.now() / 1000),
			},
		});
	}
}

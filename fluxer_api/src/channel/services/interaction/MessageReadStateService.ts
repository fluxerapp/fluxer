/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ChannelID, MessageID, UserID} from '~/BrandedTypes';
import {GuildOperations} from '~/Constants';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {Channel} from '~/Models';
import type {ReadStateService} from '~/read_state/ReadStateService';
import type {AuthenticatedChannel} from '../AuthenticatedChannel';
import {MessageInteractionBase} from './MessageInteractionBase';

export class MessageReadStateService extends MessageInteractionBase {
	constructor(
		gatewayService: IGatewayService,
		private readStateService: ReadStateService,
	) {
		super(gatewayService);
	}

	async startTyping({authChannel, userId}: {authChannel: AuthenticatedChannel; userId: UserID}): Promise<void> {
		const {channel, guild} = authChannel;
		this.ensureTextChannel(channel);

		if (this.isOperationDisabled(guild, GuildOperations.TYPING_EVENTS)) {
			return;
		}

		await this.dispatchTypingStart({channel, userId});
	}

	async ackMessage({
		userId,
		channelId,
		messageId,
		mentionCount,
		manual,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		mentionCount: number;
		manual?: boolean;
	}): Promise<void> {
		await this.readStateService.ackMessage({userId, channelId, messageId, mentionCount, manual});
	}

	async deleteReadState({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		await this.readStateService.deleteReadState({userId, channelId});
	}

	async ackPins({
		userId,
		channelId,
		timestamp,
	}: {
		userId: UserID;
		channelId: ChannelID;
		timestamp: Date;
	}): Promise<void> {
		await this.readStateService.ackPins({userId, channelId, timestamp});
	}

	private async dispatchTypingStart({channel, userId}: {channel: Channel; userId: UserID}): Promise<void> {
		await this.dispatchEvent({
			channel,
			event: 'TYPING_START',
			data: {
				channel_id: channel.id.toString(),
				user_id: userId.toString(),
				timestamp: Date.now(),
			},
		});
	}
}

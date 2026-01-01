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

import {channelIdToUserId} from '~/BrandedTypes';
import {ChannelTypes, type GatewayDispatchEvent, TEXT_BASED_CHANNEL_TYPES} from '~/Constants';
import {CannotSendMessageToNonTextChannelError} from '~/Errors';
import type {GuildResponse} from '~/guild/GuildModel';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {Channel} from '~/Models';

export interface ParsedEmoji {
	id?: string;
	name: string;
	animated?: boolean;
}

export abstract class MessageInteractionBase {
	constructor(protected gatewayService: IGatewayService) {}

	protected isOperationDisabled(guild: GuildResponse | null, operation: number): boolean {
		if (!guild) return false;
		return (guild.disabled_operations & operation) !== 0;
	}

	protected ensureTextChannel(channel: Channel): void {
		if (!TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
			throw new CannotSendMessageToNonTextChannelError();
		}
	}

	protected async dispatchEvent(params: {channel: Channel; event: GatewayDispatchEvent; data: unknown}): Promise<void> {
		const {channel, event, data} = params;

		if (channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
			return this.gatewayService.dispatchPresence({
				userId: channelIdToUserId(channel.id),
				event,
				data,
			});
		}

		if (channel.guildId) {
			return this.gatewayService.dispatchGuild({guildId: channel.guildId, event, data});
		} else {
			for (const recipientId of channel.recipientIds) {
				await this.gatewayService.dispatchPresence({userId: recipientId, event, data});
			}
		}
	}
}

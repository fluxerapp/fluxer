// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {Channel} from '@app/api/models/Channel';
import {TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {CannotSendMessageToNonTextChannelError} from '@fluxer/errors/src/domains/channel/CannotSendMessageToNonTextChannelError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';

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
}

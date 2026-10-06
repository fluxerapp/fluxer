// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageID} from '@app/api/BrandedTypes';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import type {GatewayDispatchEvent} from '@app/api/constants/Gateway';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {Channel} from '@app/api/models/Channel';

interface ChannelEventDispatcherDeps {
	gatewayService: IGatewayService;
}

export class ChannelEventDispatcher {
	constructor(private readonly deps: ChannelEventDispatcherDeps) {}

	async dispatchToChannel(channel: Channel, event: GatewayDispatchEvent, data: unknown): Promise<void> {
		await dispatchChannelEvent({gatewayService: this.deps.gatewayService, channel, event, data});
	}

	async dispatchBulkDelete(channel: Channel, messageIds: Array<MessageID>): Promise<void> {
		if (messageIds.length === 0) {
			return;
		}
		await this.dispatchToChannel(channel, 'MESSAGE_DELETE_BULK', {
			channel_id: channel.id.toString(),
			ids: messageIds.map((id) => id.toString()),
		});
	}

	async dispatchMessageUpdate(channel: Channel, messageData: unknown): Promise<void> {
		await this.dispatchToChannel(channel, 'MESSAGE_UPDATE', messageData);
	}

	async dispatchMessageDelete(
		channel: Channel,
		messageId: MessageID,
		content?: string,
		authorId?: string,
	): Promise<void> {
		await this.dispatchToChannel(channel, 'MESSAGE_DELETE', {
			channel_id: channel.id.toString(),
			id: messageId.toString(),
			content,
			author_id: authorId,
		});
	}
}

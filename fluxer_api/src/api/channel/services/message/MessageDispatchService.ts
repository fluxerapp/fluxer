// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageID, UserID} from '@app/api/BrandedTypes';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import {
	dispatchMessageCreateBroadcast,
	dispatchMessageCreateToUser,
	dispatchMessageUpdateBroadcast,
} from '@app/api/channel/services/message/MessageGatewayDispatch';
import type {GatewayDispatchEvent} from '@app/api/constants/Gateway';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';

export class MessageDispatchService {
	constructor(private gatewayService: IGatewayService) {}

	async dispatchEvent(params: {channel: Channel; event: GatewayDispatchEvent; data: unknown}): Promise<void> {
		await dispatchChannelEvent({gatewayService: this.gatewayService, ...params});
	}

	async dispatchMessageCreate({
		channel,
		message,
		currentUserId,
		nonce,
		tts,
		mentionHere = false,
	}: {
		channel: Channel;
		message: Message;
		requestCache: RequestCache;
		currentUserId?: UserID;
		nonce?: string;
		tts?: boolean;
		mentionHere?: boolean;
	}): Promise<void> {
		await dispatchMessageCreateBroadcast({
			gatewayService: this.gatewayService,
			currentUserId,
			channel,
			message,
			nonce,
			tts,
			mentionHere,
		});
	}

	async dispatchMessageCreateToUser({
		channel,
		message,
		userId,
		currentUserId,
		nonce,
		tts,
		mentionHere = false,
	}: {
		channel: Channel;
		message: Message;
		userId: UserID;
		requestCache: RequestCache;
		currentUserId?: UserID;
		nonce?: string;
		tts?: boolean;
		mentionHere?: boolean;
	}): Promise<void> {
		await dispatchMessageCreateToUser({
			gatewayService: this.gatewayService,
			userId,
			currentUserId,
			channel,
			message,
			nonce,
			tts,
			mentionHere,
		});
	}

	async dispatchMessageUpdate({
		channel,
		message,
		currentUserId,
	}: {
		channel: Channel;
		message: Message;
		requestCache: RequestCache;
		currentUserId?: UserID;
	}): Promise<void> {
		await dispatchMessageUpdateBroadcast({
			gatewayService: this.gatewayService,
			currentUserId,
			channel,
			message,
		});
	}

	async dispatchMessageDelete({
		channel,
		messageId,
		message,
	}: {
		channel: Channel;
		messageId: MessageID;
		message: Message;
	}): Promise<void> {
		await this.dispatchEvent({
			channel,
			event: 'MESSAGE_DELETE',
			data: {
				channel_id: channel.id.toString(),
				id: messageId.toString(),
				content: message.content,
				author_id: message.authorId?.toString(),
			},
		});
	}

	async dispatchMessageDeleteBulk({
		channel,
		messageIds,
	}: {
		channel: Channel;
		messageIds: Array<MessageID>;
	}): Promise<void> {
		await this.dispatchEvent({
			channel,
			event: 'MESSAGE_DELETE_BULK',
			data: {
				channel_id: channel.id.toString(),
				ids: messageIds.map((id) => id.toString()),
			},
		});
	}
}

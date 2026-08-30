// SPDX-License-Identifier: AGPL-3.0-or-later

import {UnclaimedAccountCannotSendMessagesError} from '@fluxer/errors/src/domains/channel/UnclaimedAccountCannotSendMessagesError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import type {
	BulkMessageFetchResponse,
	MessageResponse,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {ChannelID, MessageID, UserID} from '../../../BrandedTypes';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {User} from '../../../models/User';
import type {MessageRequest, MessageUpdateRequest} from '../../MessageTypes';
import type {ChannelService} from '../ChannelService';
import {isPersonalNotesChannel} from './MessageHelpers';
import type {MessageResponseDataService} from './MessageResponseDataService';

export class MessageRequestService {
	constructor(
		private readonly channelService: ChannelService,
		private readonly responseDataService: MessageResponseDataService,
	) {}

	async listMessages(params: {
		userId: UserID;
		channelId: ChannelID;
		query: {
			limit: number;
			before?: MessageID;
			after?: MessageID;
			around?: MessageID;
		};
		requestCache: RequestCache;
	}): Promise<Array<MessageResponse>> {
		const access = await this.channelService.messages.retrieval.getResponseAccessContext({
			userId: params.userId,
			channelId: params.channelId,
		});
		return this.responseDataService.listMessages({
			userId: params.userId,
			channelId: params.channelId,
			limit: params.query.limit,
			before: params.query.before,
			after: params.query.after,
			around: params.query.around,
			access,
		});
	}

	async listMessagesBulk(params: {
		userId: UserID;
		requests: Array<{
			channelId: ChannelID;
			query: {
				limit: number;
				before?: MessageID;
				after?: MessageID;
				around?: MessageID;
			};
		}>;
		requestCache: RequestCache;
	}): Promise<BulkMessageFetchResponse> {
		const channels = await mapWithConcurrency(params.requests, 4, async (request) => ({
			channel_id: request.channelId.toString(),
			messages: await this.listMessages({
				userId: params.userId,
				channelId: request.channelId,
				query: request.query,
				requestCache: params.requestCache,
			}),
		}));
		return {channels};
	}

	async getMessage(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<MessageResponse> {
		const access = await this.channelService.messages.retrieval.getResponseAccessContext({
			userId: params.userId,
			channelId: params.channelId,
			messageId: params.messageId,
		});
		const response = await this.responseDataService.getMessage({
			userId: params.userId,
			channelId: params.channelId,
			messageId: params.messageId,
			access,
		});
		if (response === null) {
			throw new UnknownMessageError();
		}
		return response;
	}

	async sendMessage(params: {
		user: User;
		channelId: ChannelID;
		data: MessageRequest;
		requestCache: RequestCache;
	}): Promise<MessageResponse> {
		if (
			params.user.isUnclaimedAccount() &&
			!isPersonalNotesChannel({userId: params.user.id, channelId: params.channelId})
		) {
			throw new UnclaimedAccountCannotSendMessagesError();
		}
		const {message, authChannel} = await this.channelService.messages.send.sendMessage({
			user: params.user,
			channelId: params.channelId,
			data: params.data,
			requestCache: params.requestCache,
		});
		const access = await this.channelService.messages.retrieval.getResponseAccessContext({
			userId: params.user.id,
			channelId: params.channelId,
			authChannel,
		});
		return this.responseDataService.buildMessage({
			userId: params.user.id,
			message,
			access: {...access, messageHistoryCutoff: null, canReadMessageHistory: true},
			nonce: params.data.nonce,
			tts: params.data.tts ?? false,
		});
	}

	async editMessage(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		data: MessageUpdateRequest;
		requestCache: RequestCache;
	}): Promise<MessageResponse> {
		const {message, authChannel} = await this.channelService.messages.edit.editMessage({
			userId: params.userId,
			channelId: params.channelId,
			messageId: params.messageId,
			data: params.data,
			requestCache: params.requestCache,
		});
		const access = await this.channelService.messages.retrieval.getResponseAccessContext({
			userId: params.userId,
			channelId: params.channelId,
			messageId: message.id,
			authChannel,
		});
		return this.responseDataService.buildMessage({
			userId: params.userId,
			message,
			access,
		});
	}
}

async function mapWithConcurrency<T, TResult>(
	items: ReadonlyArray<T>,
	concurrency: number,
	mapper: (item: T, index: number) => Promise<TResult>,
): Promise<Array<TResult>> {
	const results = new Array<TResult>(items.length);
	let nextIndex = 0;
	async function worker(): Promise<void> {
		for (;;) {
			const index = nextIndex++;
			if (index >= items.length) return;
			results[index] = await mapper(items[index], index);
		}
	}
	await Promise.all(Array.from({length: Math.min(concurrency, items.length)}, () => worker()));
	return results;
}

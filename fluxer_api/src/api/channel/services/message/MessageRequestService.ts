// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMessageID, type ChannelID, type MessageID, type UserID} from '@app/api/BrandedTypes';
import type {MessageRequest, MessageUpdateRequest} from '@app/api/channel/MessageTypes';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import {isPersonalNotesChannel} from '@app/api/channel/services/message/MessageHelpers';
import type {MessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {UnclaimedAccountCannotSendMessagesError} from '@fluxer/errors/src/domains/channel/UnclaimedAccountCannotSendMessagesError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import type {
	BulkMessageFetchResponse,
	MessageResponse,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';

export class MessageRequestService {
	constructor(
		private readonly channelService: ChannelService,
		private readonly channelRepository: ChannelRepository,
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
		const messages = await this.responseDataService.listMessages({
			userId: params.userId,
			channelId: params.channelId,
			limit: params.query.limit,
			before: params.query.before,
			after: params.query.after,
			around: params.query.around,
			access,
		});
		await Promise.all(
			messages.map((message) =>
				this.fillMessagePollAnswerAuthorInfo(
					params.channelId,
					createMessageID(BigInt(message.id)),
					params.userId,
					message,
				),
			),
		);
		return messages;
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
		const channels = await mapWithConcurrency(params.requests, 4, async (request) => {
			const messages = await this.listMessages({
				userId: params.userId,
				channelId: request.channelId,
				query: request.query,
				requestCache: params.requestCache,
			});
			await Promise.all(
				messages.map((message) =>
					this.fillMessagePollAnswerAuthorInfo(
						request.channelId,
						createMessageID(BigInt(message.id)),
						params.userId,
						message,
					),
				),
			);
			return {
				channel_id: request.channelId.toString(),
				messages,
			};
		});
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
		} else {
			await this.fillMessagePollAnswerAuthorInfo(params.channelId, params.messageId, params.userId, response);
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
		const messageResponse = await this.responseDataService.buildMessage({
			userId: params.user.id,
			message,
			access: {...access, messageHistoryCutoff: null, canReadMessageHistory: true},
			nonce: params.data.nonce,
			tts: params.data.tts ?? false,
		});
		await this.fillMessagePollAnswerAuthorInfo(params.channelId, message.id, params.user.id, messageResponse);
		return messageResponse;
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

	private async fillMessagePollAnswerAuthorInfo(
		channelId: ChannelID,
		messageId: MessageID,
		userId: UserID,
		message: MessageResponse,
	) {
		if (!message.poll?.results?.answer_counts) return;
		const answers = await this.channelRepository.messageInteractions.getVoteAnswers(channelId, messageId, userId);
		for (const answerCount of message.poll.results.answer_counts) {
			if (answers.find((answer) => answer.id === answerCount.id)) answerCount.me_voted = true;
		}
	}
}

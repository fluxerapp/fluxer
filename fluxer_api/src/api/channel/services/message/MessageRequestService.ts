// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {MessageRequest, MessageUpdateRequest} from '@app/api/channel/MessageTypes';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {CrosspostSourceService} from '@app/api/channel/services/message/CrosspostSourceService';
import {isPersonalNotesChannel} from '@app/api/channel/services/message/MessageHelpers';
import type {MessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import {carriesThreadArtifact, maskThreadArtifactsFor} from '@app/api/channel/services/message/ThreadMessageResponses';
import {everEnabled, isTainted, type ThreadViewer, viewerActive} from '@app/api/experiment/ChannelThreadsGate';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {THREAD_FEATURE_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {UnclaimedAccountCannotSendMessagesError} from '@fluxer/errors/src/domains/channel/UnclaimedAccountCannotSendMessagesError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import type {CrosspostSourceResponse} from '@fluxer/schema/src/domains/message/CrosspostSourceSchemas';
import type {
	BulkMessageFetchResponse,
	MessageChannelMentionResponse,
	MessageResponse,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

function mentionsThreadChannel(mentions: ReadonlyArray<MessageChannelMentionResponse> | null | undefined): boolean {
	return mentions?.some((mention) => THREAD_FEATURE_CHANNEL_TYPES.has(mention.type)) ?? false;
}

function carriesMaskableThreadData(response: MessageResponse): boolean {
	if (carriesThreadArtifact(response) || mentionsThreadChannel(response.mention_channels)) return true;
	if (response.message_snapshots?.some((snapshot) => mentionsThreadChannel(snapshot.mention_channels))) return true;
	const referenced = response.referenced_message;
	return referenced != null && carriesMaskableThreadData(referenced);
}

export class MessageRequestService {
	constructor(
		private readonly channelService: ChannelService,
		private readonly responseDataService: MessageResponseDataService,
		private readonly crosspostSourceService: CrosspostSourceService,
	) {}

	async listMessages(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		query: {
			limit: number;
			before?: MessageID;
			after?: MessageID;
			around?: MessageID;
		};
		requestCache: RequestCache;
	}): Promise<Array<MessageResponse>> {
		const retrieval = this.channelService.messages.retrieval;
		const {access, authChannel} = await retrieval.getResponseAccess({
			viewer: params.viewer,
			userId: params.userId,
			channelId: params.channelId,
		});
		if (authChannel.channel.isThreadOnly()) throw new InvalidChannelTypeError();
		const request = {
			userId: params.userId,
			channelId: params.channelId,
			limit: params.query.limit,
			before: params.query.before,
			after: params.query.after,
			around: params.query.around,
			access,
		};
		let responses = await this.responseDataService.listMessages(request);
		if (await this.needsThreadsMask(params.viewer, authChannel.channel.guildId, responses)) {
			responses = await this.responseDataService.listMessages({...request, threadsMask: true});
		}
		return retrieval.threadResponses.shape({
			viewer: params.viewer,
			userId: params.userId,
			authChannel,
			responses,
			requestCache: params.requestCache,
			query: params.query,
		});
	}

	private async needsThreadsMask(
		viewer: ThreadViewer,
		guildId: GuildID | null,
		responses: Array<MessageResponse>,
	): Promise<boolean> {
		if (guildId === null || !everEnabled() || viewerActive(viewer, guildId)) return false;
		if (!responses.some(carriesMaskableThreadData)) return false;
		return isTainted(guildId);
	}

	async listMessagesBulk(params: {
		userId: UserID;
		viewer: ThreadViewer;
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
				viewer: params.viewer,
				channelId: request.channelId,
				query: request.query,
				requestCache: params.requestCache,
			}),
		}));
		return {channels};
	}

	async getMessage(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<MessageResponse> {
		const retrieval = this.channelService.messages.retrieval;
		const {access, authChannel} = await retrieval.getResponseAccess({
			viewer: params.viewer,
			userId: params.userId,
			channelId: params.channelId,
			messageId: params.messageId,
		});
		const starter = await retrieval.threadResponses.getStarter({
			viewer: params.viewer,
			userId: params.userId,
			authChannel,
			messageId: params.messageId,
			requestCache: params.requestCache,
		});
		if (starter) return starter;
		const request = {
			userId: params.userId,
			channelId: params.channelId,
			messageId: params.messageId,
			access,
		};
		let response = await this.responseDataService.getMessage(request);
		if (response !== null && (await this.needsThreadsMask(params.viewer, authChannel.channel.guildId, [response]))) {
			response = await this.responseDataService.getMessage({...request, threadsMask: true});
		}
		if (response === null) {
			throw new UnknownMessageError();
		}
		const [shaped] = await retrieval.threadResponses.shape({
			viewer: params.viewer,
			userId: params.userId,
			authChannel,
			responses: [response],
			requestCache: params.requestCache,
		});
		if (!shaped) {
			throw new UnknownMessageError();
		}
		return shaped;
	}

	async getCrosspostSource(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<CrosspostSourceResponse> {
		const message = await this.getMessage(params);
		return this.crosspostSourceService.getSource(message);
	}

	async sendMessage(params: {
		user: User;
		viewer: ThreadViewer;
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
			viewer: params.viewer,
			user: params.user,
			channelId: params.channelId,
			data: params.data,
			requestCache: params.requestCache,
		});
		const access = await this.channelService.messages.retrieval.getResponseAccessContext({
			viewer: params.viewer,
			userId: params.user.id,
			channelId: params.channelId,
			authChannel,
		});
		const response = await this.responseDataService.buildMessage({
			userId: params.user.id,
			message,
			access: {...access, messageHistoryCutoff: null, canReadMessageHistory: true},
			nonce: params.data.nonce,
			tts: params.data.tts ?? false,
		});
		const [shaped] = maskThreadArtifactsFor(params.viewer, authChannel.channel.guildId, [response]);
		return shaped ?? response;
	}

	async crosspostMessage(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<MessageResponse> {
		const {message, authChannel} = await this.channelService.messages.crosspost.crosspostMessage(params);
		const retrieval = this.channelService.messages.retrieval;
		const access = await retrieval.getResponseAccessContext({
			viewer: params.viewer,
			userId: params.userId,
			channelId: params.channelId,
			messageId: message.id,
			authChannel,
		});
		const response = await this.responseDataService.buildMessage({
			userId: params.userId,
			message,
			access,
		});
		const [shaped] = await retrieval.threadResponses.shape({
			viewer: params.viewer,
			userId: params.userId,
			authChannel,
			responses: [response],
			requestCache: params.requestCache,
		});
		return shaped ?? response;
	}

	async validateForumStarter(params: {
		user: User;
		parentAuth: AuthenticatedChannel;
		data: MessageRequest;
	}): Promise<void> {
		if (params.user.isUnclaimedAccount()) throw new UnclaimedAccountCannotSendMessagesError();
		await this.channelService.messages.send.validateForumStarter(params);
	}

	async sendForumStarter(params: {
		user: User;
		viewer: ThreadViewer;
		parentAuth: AuthenticatedChannel;
		threadId: ChannelID;
		data: MessageRequest;
		requestCache: RequestCache;
	}): Promise<MessageResponse> {
		if (params.user.isUnclaimedAccount()) throw new UnclaimedAccountCannotSendMessagesError();
		const {message, authChannel} = await this.channelService.messages.send.sendMessage({
			viewer: params.viewer,
			user: params.user,
			channelId: params.threadId,
			data: params.data,
			requestCache: params.requestCache,
			forumStarter: {parentAuth: params.parentAuth},
		});
		const access = await this.channelService.messages.retrieval.getResponseAccessContext({
			viewer: params.viewer,
			userId: params.user.id,
			channelId: params.threadId,
			authChannel,
		});
		return this.responseDataService.buildMessage({
			userId: params.user.id,
			message,
			access: {...access, messageHistoryCutoff: null, canReadMessageHistory: true},
		});
	}

	async editMessage(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		data: MessageUpdateRequest;
		requestCache: RequestCache;
	}): Promise<MessageResponse> {
		const {message, authChannel} = await this.channelService.messages.edit.editMessage({
			viewer: params.viewer,
			userId: params.userId,
			channelId: params.channelId,
			messageId: params.messageId,
			data: params.data,
			requestCache: params.requestCache,
		});
		const access = await this.channelService.messages.retrieval.getResponseAccessContext({
			viewer: params.viewer,
			userId: params.userId,
			channelId: params.channelId,
			messageId: message.id,
			authChannel,
		});
		const response = await this.responseDataService.buildMessage({
			userId: params.userId,
			message,
			access,
		});
		const [shaped] = maskThreadArtifactsFor(params.viewer, authChannel.channel.guildId, [response]);
		return shaped ?? response;
	}
}

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

import {AttachmentDecayService} from '~/attachment/AttachmentDecayService';
import {
	type AttachmentID,
	type ChannelID,
	createChannelID,
	createMessageID,
	type MessageID,
	type UserID,
} from '~/BrandedTypes';
import {ChannelTypes, MessageFlags, Permissions} from '~/Constants';
import type {MessageSearchRequest, MessageSearchResponse} from '~/channel/ChannelModel';
import {mapMessageToResponse} from '~/channel/ChannelModel';
import {
	ChannelIndexingError,
	FeatureTemporarilyDisabledError,
	MissingPermissionsError,
	UnknownMessageError,
} from '~/Errors';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import {getMessageSearchService} from '~/Meilisearch';
import type {Channel, Message} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {buildMessageSearchFilters} from '~/search/buildMessageSearchFilters';
import type {IUserRepository} from '~/user/IUserRepository';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import {getDmChannelIdsForScope} from './dmScopeUtils';
import type {MessageChannelAuthService} from './MessageChannelAuthService';
import {collectMessageAttachments} from './MessageHelpers';
import type {MessageProcessingService} from './MessageProcessingService';
import type {MessageSearchService} from './MessageSearchService';

export class MessageRetrievalService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userCacheService: UserCacheService,
		private mediaService: IMediaService,
		private channelAuthService: MessageChannelAuthService,
		private processingService: MessageProcessingService,
		private searchService: MessageSearchService,
		private userRepository: IUserRepository,
		private attachmentDecayService: AttachmentDecayService = new AttachmentDecayService(),
	) {}

	async getMessage({
		userId,
		channelId,
		messageId,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<Message> {
		const authChannel = await this.channelAuthService.getChannelAuthenticated({userId, channelId});

		if (authChannel.guild && !(await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY))) {
			throw new MissingPermissionsError();
		}

		const message = await this.channelRepository.messages.getMessage(channelId, messageId);
		if (!message) {
			throw new UnknownMessageError();
		}

		await this.extendAttachments([message]);
		return message;
	}

	async getMessages({
		userId,
		channelId,
		limit,
		before,
		after,
		around,
	}: {
		userId: UserID;
		channelId: ChannelID;
		limit: number;
		before?: MessageID;
		after?: MessageID;
		around?: MessageID;
	}): Promise<Array<Message>> {
		const authChannel = await this.channelAuthService.getChannelAuthenticated({userId, channelId});

		if (authChannel.guild && !(await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY))) {
			return [];
		}

		const messages = around
			? await this.processingService.fetchMessagesAround({channelId, limit, around})
			: await this.channelRepository.messages.listMessages(channelId, before, limit, after);

		await this.extendAttachments(messages);
		return messages;
	}

	async searchMessages({
		userId,
		channelId,
		searchParams,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
	}): Promise<MessageSearchResponse> {
		const authChannel = await this.channelAuthService.getChannelAuthenticated({userId, channelId});
		const {channel} = authChannel;

		if (authChannel.guild && !(await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY))) {
			throw new MissingPermissionsError();
		}

		const searchService = getMessageSearchService();
		if (!searchService) {
			throw new FeatureTemporarilyDisabledError();
		}

		let channelIndexedAt: Date | null = channel.indexedAt;
		if (channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
			const persistedChannel = await this.channelRepository.channelData.findUnique(channelId);
			if (persistedChannel?.indexedAt) {
				channelIndexedAt = persistedChannel.indexedAt;
			}
		}

		if (!channelIndexedAt) {
			await this.searchService.triggerChannelIndexing(channelId);
			throw new ChannelIndexingError();
		}

		const resolvedSearchParams = await this.applyDmScopeToSearchParams({
			userId,
			channel,
			channelId,
			searchParams,
		});

		const channelIdStrings = resolvedSearchParams.channel_id
			? resolvedSearchParams.channel_id.map((id) => id.toString())
			: [channelId.toString()];
		const filters = buildMessageSearchFilters(resolvedSearchParams, channelIdStrings);

		const hitsPerPage = resolvedSearchParams.hits_per_page ?? 25;
		const page = resolvedSearchParams.page ?? 1;
		const result = await searchService.searchMessages('', filters, {
			hitsPerPage,
			page,
		});

		const messageEntries = result.hits.map((hit) => ({
			channelId: createChannelID(BigInt(hit.channelId)),
			messageId: createMessageID(BigInt(hit.id)),
		}));
		const messages = await Promise.all(
			messageEntries.map(({channelId, messageId}) => this.channelRepository.messages.getMessage(channelId, messageId)),
		);

		const validMessages = messages.filter((msg: Message | null): msg is Message => msg !== null);

		if (validMessages.length > 0) {
			await this.extendAttachments(validMessages);
		}

		const attachmentPayloads = validMessages.flatMap((message) => this.buildAttachmentDecayEntriesForMessage(message));
		const attachmentDecayMap =
			attachmentPayloads.length > 0
				? await this.attachmentDecayService.fetchMetadata(
						attachmentPayloads.map((payload) => ({attachmentId: payload.attachmentId})),
					)
				: undefined;

		const messageResponses = await Promise.all(
			validMessages.map((message: Message) =>
				mapMessageToResponse({
					message,
					currentUserId: userId,
					userCacheService: this.userCacheService,
					requestCache,
					mediaService: this.mediaService,
					attachmentDecayMap,
					getReactions: (channelId, messageId) =>
						this.channelRepository.messageInteractions.listMessageReactions(channelId, messageId),
					setHasReaction: (channelId, messageId, hasReaction) =>
						this.channelRepository.messageInteractions.setHasReaction(channelId, messageId, hasReaction),
					getReferencedMessage: (channelId, messageId) =>
						this.channelRepository.messages.getMessage(channelId, messageId),
				}),
			),
		);

		return {
			messages: messageResponses,
			total: result.total,
			hits_per_page: hitsPerPage,
			page,
		};
	}

	private async extendAttachments(messages: Array<Message>): Promise<void> {
		const payloads = messages.flatMap((message) => {
			if (message.flags & MessageFlags.COMPACT_ATTACHMENTS) {
				return [];
			}
			return this.buildAttachmentDecayEntriesForMessage(message);
		});

		if (payloads.length === 0) return;

		await this.attachmentDecayService.extendForAttachments(payloads);
	}

	private buildAttachmentDecayEntriesForMessage(message: Message): Array<{
		attachmentId: AttachmentID;
		channelId: ChannelID;
		messageId: MessageID;
		filename: string;
		sizeBytes: bigint;
		uploadedAt: Date;
	}> {
		const attachments = collectMessageAttachments(message);

		if (attachments.length === 0) return [];

		const uploadedAt = new Date(SnowflakeUtils.extractTimestamp(message.id));
		return attachments.map((attachment) => ({
			attachmentId: attachment.id,
			channelId: message.channelId,
			messageId: message.id,
			filename: attachment.filename,
			sizeBytes: attachment.size,
			uploadedAt,
		}));
	}

	private async applyDmScopeToSearchParams({
		userId,
		channel,
		channelId,
		searchParams,
	}: {
		userId: UserID;
		channel: Channel;
		channelId: ChannelID;
		searchParams: MessageSearchRequest;
	}): Promise<MessageSearchRequest> {
		const scope = searchParams.scope;
		const isDmSearch = channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM;
		if (!isDmSearch || !scope || scope === 'current') {
			return searchParams;
		}

		if (scope !== 'all_dms' && scope !== 'open_dms') {
			return searchParams;
		}

		const targetChannelIds = await getDmChannelIdsForScope({
			scope,
			userId,
			userRepository: this.userRepository,
			includeChannelId: channelId,
		});
		if (targetChannelIds.length === 0) {
			return searchParams;
		}

		return {...searchParams, channel_id: targetChannelIds.map((id) => BigInt(id))};
	}
}

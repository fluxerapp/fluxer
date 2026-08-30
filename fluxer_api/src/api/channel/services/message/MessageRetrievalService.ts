// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import type {MessageSearchRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {MessageResponse, MessageSearchResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {AttachmentDecayService} from '../../../attachment/AttachmentDecayService';
import type {AttachmentID, ChannelID, MessageID, UserID} from '../../../BrandedTypes';
import {createChannelID, createMessageID} from '../../../BrandedTypes';
import type {UserCacheService} from '../../../infrastructure/UserCacheService';
import type {RequestCache} from '../../../middleware/RequestCacheMiddleware';
import type {Channel} from '../../../models/Channel';
import type {Message} from '../../../models/Message';
import {getMessageSearchService} from '../../../SearchFactory';
import {buildMessageSearchFilters} from '../../../search/BuildMessageSearchFilters';
import {channelNeedsReindexing} from '../../../search/ChannelIndexingUtils';
import {searchExistingMessages} from '../../../search/MessageSearchResultReconciler';
import type {IUserRepository} from '../../../user/IUserRepository';
import {mapChannelToResponse} from '../../ChannelMappers';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from '../AuthenticatedChannel';
import {getDmChannelIdsForScope} from './DmScopeUtils';
import type {MessageChannelAuthService} from './MessageChannelAuthService';
import {collectMessageAttachments} from './MessageHelpers';
import type {MessageProcessingService} from './MessageProcessingService';
import {createMessageResponseDataService, type MessageResponseAccessContext} from './MessageResponseDataService';
import type {MessageSearchService} from './MessageSearchService';

export class MessageRetrievalService {
	constructor(
		private channelRepository: IChannelRepositoryAggregate,
		private userCacheService: UserCacheService,
		private channelAuthService: MessageChannelAuthService,
		private processingService: MessageProcessingService,
		private searchService: MessageSearchService,
		private userRepository: IUserRepository,
		private attachmentDecayService: AttachmentDecayService = new AttachmentDecayService(),
	) {}

	private isMessageAfterCutoff(messageId: MessageID, cutoffIso: string): boolean {
		const messageTimestamp = snowflakeToDate(messageId).getTime();
		const cutoffTimestamp = new Date(cutoffIso).getTime();
		return messageTimestamp >= cutoffTimestamp;
	}

	private async canAccessMessage(authChannel: AuthenticatedChannel, messageId: MessageID): Promise<boolean> {
		if (!authChannel.guild) {
			return true;
		}
		if (await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY)) {
			return true;
		}
		const cutoff = authChannel.guild.message_history_cutoff;
		if (!cutoff) {
			return false;
		}
		return this.isMessageAfterCutoff(messageId, cutoff);
	}

	async getResponseAccessContext(params: {
		userId: UserID;
		channelId: ChannelID;
		messageId?: MessageID;
	}): Promise<MessageResponseAccessContext> {
		const authChannel = await this.channelAuthService.getChannelAuthenticated({
			userId: params.userId,
			channelId: params.channelId,
		});
		if (params.messageId && !(await this.canAccessMessage(authChannel, params.messageId))) {
			throw new UnknownMessageError();
		}
		const canReadMessageHistory =
			!authChannel.guild || (await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY));
		return {
			sourceGuildId: authChannel.channel.guildId,
			messageHistoryCutoff: canReadMessageHistory ? null : (authChannel.guild?.message_history_cutoff ?? null),
			canReadMessageHistory,
		};
	}

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
		if (!(await this.canAccessMessage(authChannel, messageId))) {
			throw new UnknownMessageError();
		}
		const message = await this.channelRepository.messages.getMessage(channelId, messageId);
		if (!message) {
			throw new UnknownMessageError();
		}
		const repairedMessage = await this.processingService.repairMentionsOnRead(message, authChannel.channel);
		await this.extendAttachments([repairedMessage]);
		return repairedMessage;
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
		const hasReadHistory = !authChannel.guild || (await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY));
		if (!hasReadHistory) {
			const cutoff = authChannel.guild?.message_history_cutoff;
			if (!cutoff) {
				return {
					channels: [],
					messages: [],
					total: 0,
					hits_per_page: searchParams.hits_per_page ?? 25,
					page: searchParams.page ?? 1,
				};
			}
		}
		const searchService = getMessageSearchService();
		if (!searchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		if (await this.channelNeedsIndexing(channel, channelId)) {
			await this.searchService.triggerChannelIndexing(channelId);
			return {indexing: true as const};
		}
		const resolvedSearchParams = await this.applyDmScopeToSearchParams({
			userId,
			channel,
			searchParams,
		});
		const scope = resolvedSearchParams.scope ?? 'current';
		const channelIdStrings =
			scope === 'current'
				? [channelId.toString()]
				: resolvedSearchParams.channel_id
					? resolvedSearchParams.channel_id.map((id) => id.toString())
					: [channelId.toString()];
		const normalizedSearchParams =
			scope === 'current' ? {...resolvedSearchParams, channel_id: undefined} : resolvedSearchParams;
		const filters = buildMessageSearchFilters(normalizedSearchParams, channelIdStrings);
		const hitsPerPage = resolvedSearchParams.hits_per_page ?? 25;
		const page = resolvedSearchParams.page ?? 1;
		const result = await searchExistingMessages({
			searchService,
			messageRepository: this.channelRepository,
			query: resolvedSearchParams.content ?? '',
			filters,
			hitsPerPage,
			page,
		});
		const messageEntries = result.hits.map((hit) => ({
			channelId: createChannelID(BigInt(hit.channelId)),
			messageId: createMessageID(BigInt(hit.id)),
		}));
		const access = {
			sourceGuildId: channel.guildId,
			messageHistoryCutoff: !hasReadHistory ? (authChannel.guild?.message_history_cutoff ?? null) : null,
			canReadMessageHistory: hasReadHistory,
		};
		const responseDataService = createMessageResponseDataService();
		const foundMessages = await Promise.all(
			messageEntries.map(({channelId, messageId}) =>
				responseDataService.getMessage({
					userId,
					channelId,
					messageId,
					access,
				}),
			),
		);
		const messageResponses = foundMessages
			.filter((message): message is MessageResponse => message !== null)
			.map(({referenced_message: _referencedMessage, ...searchMessage}) => searchMessage);
		return {
			channels: messageResponses.length > 0 ? [await this.mapSearchChannelResponse(channel, userId, requestCache)] : [],
			messages: messageResponses,
			total: hasReadHistory ? result.total : messageResponses.length,
			hits_per_page: hitsPerPage,
			page,
		};
	}

	private async mapSearchChannelResponse(channel: Channel, userId: UserID, requestCache: RequestCache) {
		return mapChannelToResponse({
			channel,
			currentUserId: userId,
			userCacheService: this.userCacheService,
			requestCache,
		});
	}

	private async channelNeedsIndexing(channel: Channel, channelId: ChannelID): Promise<boolean> {
		let channelIndexedAt: Date | null = channel.indexedAt;
		if (channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
			const persistedChannel = await this.channelRepository.channelData.findUnique(channelId);
			if (persistedChannel?.indexedAt) {
				channelIndexedAt = persistedChannel.indexedAt;
			}
		}
		return channelNeedsReindexing(channelIndexedAt);
	}

	private async extendAttachments(messages: Array<Message>): Promise<void> {
		const payloads = messages.flatMap((message) => {
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
		const uploadedAt = snowflakeToDate(message.id);
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
		searchParams,
	}: {
		userId: UserID;
		channel: Channel;
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
			includeChannel: channel,
		});
		if (targetChannelIds.length === 0) {
			return searchParams;
		}
		return {...searchParams, channel_id: targetChannelIds.map((id) => BigInt(id))};
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentDecayService} from '@app/api/attachment/AttachmentDecayService';
import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {AuthenticatedChannel, ChannelAccess} from '@app/api/channel/services/AuthenticatedChannel';
import {getDmChannelIdsForScope} from '@app/api/channel/services/message/DmScopeUtils';
import type {MessageChannelAuthService} from '@app/api/channel/services/message/MessageChannelAuthService';
import {attachmentDecayPayloadsForMessage} from '@app/api/channel/services/message/MessageHelpers';
import type {MessageProcessingService} from '@app/api/channel/services/message/MessageProcessingService';
import {
	createMessageResponseDataService,
	type MessageResponseAccessContext,
	messageResponseAccessForAuth,
} from '@app/api/channel/services/message/MessageResponseDataService';
import type {MessageSearchService} from '@app/api/channel/services/message/MessageSearchService';
import {maskThreadArtifactsFor, ThreadMessageResponses} from '@app/api/channel/services/message/ThreadMessageResponses';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import {maskChannelResponseThreadBits} from '@app/api/guild/services/ThreadPermissionBits';
import {chargeGuildVisitorBuckets, GUILD_VISITOR_SEARCH_BUCKETS} from '@app/api/guild/visits/GuildVisitorAccess';
import {assertVisitorSearchFilters} from '@app/api/guild/visits/GuildVisitorSearch';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import {getMessageSearchService} from '@app/api/SearchFactory';
import {buildMessageSearchFilters} from '@app/api/search/BuildMessageSearchFilters';
import {channelNeedsReindexing} from '@app/api/search/ChannelIndexingUtils';
import {searchExistingMessages} from '@app/api/search/MessageSearchResultReconciler';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import type {MessageSearchRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {MessageSearchResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

export class MessageRetrievalService {
	constructor(
		private channelRepository: ChannelRepository,
		private userCacheService: UserCacheService,
		private channelAuthService: MessageChannelAuthService,
		private processingService: MessageProcessingService,
		private searchService: MessageSearchService,
		private userRepository: UserRepository,
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

	private threadResponsesInstance: ThreadMessageResponses | undefined;

	get threadResponses(): ThreadMessageResponses {
		this.threadResponsesInstance ??= new ThreadMessageResponses(
			this.channelRepository,
			createMessageResponseDataService(),
			this.userCacheService,
		);
		return this.threadResponsesInstance;
	}

	async getResponseAccessContext(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId?: MessageID;
		authChannel?: AuthenticatedChannel;
	}): Promise<MessageResponseAccessContext> {
		return (await this.getResponseAccess(params)).access;
	}

	async getResponseAccess(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId?: MessageID;
		authChannel?: AuthenticatedChannel;
		access?: ChannelAccess;
	}): Promise<{access: MessageResponseAccessContext; authChannel: AuthenticatedChannel}> {
		const authChannel =
			params.authChannel ??
			(await this.channelAuthService.getChannelAuthenticated({
				userId: params.userId,
				channelId: params.channelId,
				viewer: params.viewer,
				access: params.access,
			}));
		if (params.messageId && !(await this.canAccessMessage(authChannel, params.messageId))) {
			throw new UnknownMessageError();
		}
		return {authChannel, access: await messageResponseAccessForAuth(authChannel)};
	}

	async getMessage({
		userId,
		viewer,
		channelId,
		messageId,
		access,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		access?: ChannelAccess;
	}): Promise<Message> {
		const authChannel = await this.channelAuthService.getChannelAuthenticated({userId, channelId, viewer, access});
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

	async getMessagesByIds({
		userId,
		viewer,
		channelId,
		messageIds,
		access,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageIds: Array<MessageID>;
		access?: ChannelAccess;
	}): Promise<Map<string, Message>> {
		const authChannel = await this.channelAuthService.getChannelAuthenticated({userId, channelId, viewer, access});
		const canReadMessageHistory =
			!authChannel.guild || (await authChannel.hasPermission(Permissions.READ_MESSAGE_HISTORY));
		const cutoff = authChannel.guild?.message_history_cutoff ?? null;
		const readableIds = messageIds.filter(
			(messageId) => canReadMessageHistory || (cutoff != null && this.isMessageAfterCutoff(messageId, cutoff)),
		);
		const found = (
			await Promise.all(
				readableIds.map((messageId) => this.channelRepository.messages.getMessage(channelId, messageId)),
			)
		).filter((message): message is Message => message != null);
		const repairedMessages = await Promise.all(
			found.map((message) => this.processingService.repairMentionsOnRead(message, authChannel.channel)),
		);
		await this.extendAttachments(repairedMessages);
		return new Map(repairedMessages.map((message) => [message.id.toString(), message] as const));
	}

	async searchMessages({
		userId,
		viewer,
		channelId,
		searchParams,
		requestCache,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
	}): Promise<MessageSearchResponse> {
		const authChannel = await this.channelAuthService.getChannelAuthenticated({
			userId,
			channelId,
			viewer,
			access: 'visitor_read',
		});
		const {channel} = authChannel;
		if (authChannel.visitor && channel.guildId !== null) {
			assertVisitorSearchFilters(searchParams);
			searchParams = {...searchParams, scope: 'current', channel_id: undefined, exclude_channel_id: undefined};
			await chargeGuildVisitorBuckets({
				buckets: GUILD_VISITOR_SEARCH_BUCKETS,
				userId,
				guildId: channel.guildId,
				tier: authChannel.visitor.tier,
			});
		}
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
		let channelIdStrings: Array<string>;
		if (scope === 'current') {
			channelIdStrings = [channelId.toString()];
		} else if (resolvedSearchParams.channel_id) {
			channelIdStrings = resolvedSearchParams.channel_id.map((id) => id.toString());
		} else {
			channelIdStrings = [channelId.toString()];
		}
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
		const access = {
			sourceGuildId: channel.guildId,
			messageHistoryCutoff: !hasReadHistory ? (authChannel.guild?.message_history_cutoff ?? null) : null,
			canReadMessageHistory: hasReadHistory,
		};
		const visibleMessages = hasReadHistory
			? result.messages
			: result.messages.filter((message) => this.isWithinCutoff(message.id, access.messageHistoryCutoff));
		const builtMessages = await createMessageResponseDataService().buildMessages({
			userId,
			messages: visibleMessages,
			access,
		});
		const messageResponses = maskThreadArtifactsFor(viewer, channel.guildId, builtMessages).map(
			({referenced_message: _referencedMessage, ...searchMessage}) => searchMessage,
		);
		return {
			channels:
				messageResponses.length > 0 ? await this.mapSearchChannelResponse(channel, userId, viewer, requestCache) : [],
			messages: messageResponses,
			total: hasReadHistory ? result.total : messageResponses.length,
			hits_per_page: hitsPerPage,
			page,
		};
	}

	private isWithinCutoff(messageId: MessageID, cutoffIso: string | null): boolean {
		return cutoffIso !== null && this.isMessageAfterCutoff(messageId, cutoffIso);
	}

	private async mapSearchChannelResponse(
		channel: Channel,
		userId: UserID,
		viewer: ThreadViewer,
		requestCache: RequestCache,
	) {
		const response = await mapChannelToResponse({
			channel,
			currentUserId: userId,
			userCacheService: this.userCacheService,
			requestCache,
		});
		return maskChannelResponseThreadBits(channel.guildId, viewer, [response]);
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
		const payloads = messages.flatMap(attachmentDecayPayloadsForMessage);
		if (payloads.length === 0) return;
		await this.attachmentDecayService.extendForAttachments(payloads);
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

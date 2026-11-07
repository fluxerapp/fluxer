// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, UserID} from '@app/api/BrandedTypes';
import {createChannelID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {
	type DmSearchScope,
	getDmChannelIdsForScope,
	isDmScopeChannelForUser,
} from '@app/api/channel/services/message/DmScopeUtils';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import {getMessageSearchService} from '@app/api/SearchFactory';
import {channelNeedsReindexing, queueIndexingChannels} from '@app/api/search/ChannelIndexingUtils';
import type {IMessageSearchService} from '@app/api/search/IMessageSearchService';
import {MessageSearchResponseMapper} from '@app/api/search/MessageSearchResponseMapper';
import {emptyMessageSearchResponse, runMessageSearch} from '@app/api/search/MessageSearchResultReconciler';
import {channelIsAgeRestricted, getSearchNsfwAccess} from '@app/api/search/SearchNsfwUtils';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import type {MessageSearchRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {MessageSearchResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';

const CHANNEL_INDEX_CHECK_CONCURRENCY = 32;

export class GlobalSearchService {
	private readonly responseMapper: MessageSearchResponseMapper;

	constructor(
		private readonly channelRepository: ChannelRepository,
		private readonly guildService: GuildService,
		private readonly userRepository: UserRepository,
		private readonly userCacheService: UserCacheService,
		private readonly workerService: IWorkerService<WorkerTaskName>,
	) {
		this.responseMapper = new MessageSearchResponseMapper(this.channelRepository, this.userCacheService);
	}

	private getMessageSearchService(): IMessageSearchService {
		const searchService = getMessageSearchService();
		if (!searchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		return searchService;
	}

	async searchAcrossDms(params: {
		userId: UserID;
		viewer: ThreadViewer;
		scope: DmSearchScope;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
		includeChannelId?: ChannelID | null;
		requestedChannelIds?: Array<ChannelID>;
	}): Promise<MessageSearchResponse> {
		const includeChannel = await this.findDmScopeContextChannel(params.userId, params.includeChannelId);
		const dmChannelIds = await this.memberDmChannelIds(
			params.userId,
			await getDmChannelIdsForScope({
				scope: params.scope,
				userId: params.userId,
				userRepository: this.userRepository,
				includeChannel,
			}),
		);
		const finalChannelIds = this.filterRequestedChannelIds(dmChannelIds, params.requestedChannelIds);
		if (finalChannelIds.length === 0) {
			return emptyMessageSearchResponse(params.searchParams);
		}
		const needsIndexing = await this.ensureChannelsIndexed(finalChannelIds);
		if (needsIndexing) {
			return {indexing: true};
		}
		return this.runSearch(finalChannelIds, params.userId, params.viewer, params.searchParams, params.requestCache);
	}

	async searchAcrossGuildsAndDms(params: {
		userId: UserID;
		viewer: ThreadViewer;
		dmScope: DmSearchScope;
		iosApp: boolean;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
		includeChannelId?: ChannelID | null;
		requestedChannelIds?: Array<ChannelID>;
	}): Promise<MessageSearchResponse> {
		const [guildAccess, includeChannel] = await Promise.all([
			this.guildService.search.collectAccessibleGuildChannels(params.userId, params.viewer, params.requestedChannelIds),
			this.findDmScopeContextChannel(params.userId, params.includeChannelId),
		]);
		const {accessibleChannels, unindexedChannelIds, nsfwGuildIds, parentCategories, threadParents} = guildAccess;
		if (unindexedChannelIds.size > 0) {
			await queueIndexingChannels(this.workerService, unindexedChannelIds);
			return {indexing: true};
		}
		const guildChannelIds = [...accessibleChannels.keys(), ...threadParents.keys()];
		const dmChannelIds = await this.memberDmChannelIds(
			params.userId,
			await getDmChannelIdsForScope({
				scope: params.dmScope,
				userId: params.userId,
				userRepository: this.userRepository,
				includeChannel,
			}),
		);
		const combinedChannelSet = new Set<string>([...guildChannelIds, ...dmChannelIds]);
		const validatedChannelIds = this.filterRequestedChannelIds(
			Array.from(combinedChannelSet),
			params.requestedChannelIds,
		);
		const includeNsfwRequested = params.searchParams.include_nsfw ?? false;
		let canIncludeNsfw = false;
		if (includeNsfwRequested) {
			canIncludeNsfw = (await getSearchNsfwAccess(this.userRepository, params.userId, params.iosApp)).allowed;
		}
		const finalChannelIds = validatedChannelIds.filter((channelIdStr) => {
			const channel = accessibleChannels.get(channelIdStr) ?? threadParents.get(channelIdStr);
			if (!channel) {
				return true;
			}
			if (channel.guildId != null && nsfwGuildIds.has(channel.guildId.toString())) {
				return canIncludeNsfw;
			}
			if (channelIsAgeRestricted(channel, parentCategories, false)) {
				return canIncludeNsfw;
			}
			return true;
		});
		if (finalChannelIds.length === 0) {
			return emptyMessageSearchResponse(params.searchParams);
		}
		const needsIndexing = await this.ensureChannelsIndexed(
			threadParents.size > 0 ? finalChannelIds.filter((id) => !threadParents.has(id)) : finalChannelIds,
		);
		if (needsIndexing) {
			return {indexing: true};
		}
		return this.runSearch(finalChannelIds, params.userId, params.viewer, params.searchParams, params.requestCache);
	}

	private filterRequestedChannelIds(available: Array<string>, requested?: Array<ChannelID>): Array<string> {
		if (!requested || requested.length === 0) {
			return available;
		}
		const availableSet = new Set(available);
		const requestedStrings = requested.map((id) => id.toString());
		for (const channelId of requestedStrings) {
			if (!availableSet.has(channelId)) {
				throw new MissingPermissionsError();
			}
		}
		return requestedStrings;
	}

	private async memberDmChannelIds(userId: UserID, channelIds: Array<string>): Promise<Array<string>> {
		const channels = await this.channelRepository.channelData.listChannels(
			channelIds.map((id) => createChannelID(BigInt(id))),
		);
		const departed = channels.filter(
			(channel) =>
				(channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) &&
				!channel.recipientIds.has(userId),
		);
		if (departed.length === 0) {
			return channelIds;
		}
		void Promise.allSettled(departed.map((channel) => this.closeDepartedDmRow(userId, channel.id)));
		const departedIds = new Set(departed.map((channel) => channel.id.toString()));
		return channelIds.filter((id) => !departedIds.has(id));
	}

	private async closeDepartedDmRow(userId: UserID, channelId: ChannelID): Promise<void> {
		if (await this.userRepository.isDmChannelOpen(userId, channelId)) {
			await this.userRepository.closeDmForUser(userId, channelId);
		}
	}

	private async findDmScopeContextChannel(userId: UserID, channelId?: ChannelID | null): Promise<Channel | null> {
		if (!channelId) {
			return null;
		}
		const channel = await this.channelRepository.channelData.findUnique(channelId);
		if (!channel || !isDmScopeChannelForUser(channel, userId)) {
			return null;
		}
		return channel;
	}

	private async ensureChannelsIndexed(channelIds: Array<string>): Promise<boolean> {
		const channelBrandedIds = channelIds.map((id) => createChannelID(BigInt(id)));
		const channels = await mapWithConcurrency(channelBrandedIds, CHANNEL_INDEX_CHECK_CONCURRENCY, (id) =>
			this.channelRepository.channelData.findUnique(id),
		);
		const personalNotesIds = channelBrandedIds.filter((_id, i) => channels[i]?.type === ChannelTypes.DM_PERSONAL_NOTES);
		const personalNotesData =
			personalNotesIds.length > 0
				? await mapWithConcurrency(personalNotesIds, CHANNEL_INDEX_CHECK_CONCURRENCY, (id) =>
						this.channelRepository.channelData.findUnique(id),
					)
				: [];
		const personalNotesMap = new Map(personalNotesIds.map((id, i) => [id.toString(), personalNotesData[i]]));
		const unindexed = new Set<string>();
		for (let i = 0; i < channelIds.length; i++) {
			const channel = channels[i];
			if (!channel) {
				continue;
			}
			let indexedAt = channel.indexedAt;
			if (channel.type === ChannelTypes.DM_PERSONAL_NOTES) {
				const persisted = personalNotesMap.get(channelIds[i]!);
				if (persisted?.indexedAt) {
					indexedAt = persisted.indexedAt;
				}
			}
			if (channelNeedsReindexing(indexedAt)) {
				unindexed.add(channelIds[i]!);
			}
		}
		if (unindexed.size === 0) {
			return false;
		}
		await queueIndexingChannels(this.workerService, unindexed);
		return true;
	}

	private async runSearch(
		channelIds: Array<string>,
		userId: UserID,
		viewer: ThreadViewer,
		searchParams: MessageSearchRequest,
		requestCache: RequestCache,
	): Promise<MessageSearchResponse> {
		return runMessageSearch({
			searchService: this.getMessageSearchService(),
			messageRepository: this.channelRepository,
			responseMapper: this.responseMapper,
			filterParams: {...searchParams, channel_id: undefined},
			searchParams,
			channelIds,
			userId,
			viewer,
			requestCache,
		});
	}
}

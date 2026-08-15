// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {GuildNSFWLevel} from '@fluxer/constants/src/GuildConstants';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {MessageSearchRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {MessageSearchResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {ChannelID, UserID} from '../BrandedTypes';
import {createChannelID} from '../BrandedTypes';
import type {IChannelRepository} from '../channel/IChannelRepository';
import {
	type DmSearchScope,
	getDmChannelIdsForScope,
	isDmScopeChannelForUser,
} from '../channel/services/message/DmScopeUtils';
import type {GuildService} from '../guild/services/GuildService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Channel} from '../models/Channel';
import {getMessageSearchService} from '../SearchFactory';
import type {IUserRepository} from '../user/IUserRepository';
import {canUserAccessNsfwContent} from '../utils/AgeUtils';
import {mapWithConcurrency} from '../utils/ConcurrencyUtils';
import type {WorkerTaskName} from '../worker/WorkerLaneConfig';
import {buildMessageSearchFilters} from './BuildMessageSearchFilters';
import {channelNeedsReindexing} from './ChannelIndexingUtils';
import type {IMessageSearchService} from './IMessageSearchService';
import {MessageSearchResponseMapper} from './MessageSearchResponseMapper';
import {searchExistingMessages} from './MessageSearchResultReconciler';

const CHANNEL_INDEX_CHECK_CONCURRENCY = 32;
const CHANNEL_INDEX_JOB_ENQUEUE_CONCURRENCY = 16;

export class GlobalSearchService {
	private readonly responseMapper: MessageSearchResponseMapper;

	constructor(
		private readonly channelRepository: IChannelRepository,
		private readonly guildService: GuildService,
		private readonly userRepository: IUserRepository,
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
		scope: DmSearchScope;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
		includeChannelId?: ChannelID | null;
		requestedChannelIds?: Array<ChannelID>;
	}): Promise<MessageSearchResponse> {
		const includeChannel = await this.findDmScopeContextChannel(params.userId, params.includeChannelId);
		const dmChannelIds = await getDmChannelIdsForScope({
			scope: params.scope,
			userId: params.userId,
			userRepository: this.userRepository,
			includeChannel,
		});
		const finalChannelIds = this.filterRequestedChannelIds(dmChannelIds, params.requestedChannelIds);
		if (finalChannelIds.length === 0) {
			const hitsPerPage = params.searchParams.hits_per_page ?? 25;
			const page = params.searchParams.page ?? 1;
			return {
				channels: [],
				messages: [],
				total: 0,
				hits_per_page: hitsPerPage,
				page,
			};
		}
		const needsIndexing = await this.ensureChannelsIndexed(finalChannelIds);
		if (needsIndexing) {
			return {indexing: true};
		}
		return this.runSearch(finalChannelIds, params.userId, params.searchParams, params.requestCache);
	}

	async searchAcrossGuildsAndDms(params: {
		userId: UserID;
		dmScope: DmSearchScope;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
		includeChannelId?: ChannelID | null;
		requestedChannelIds?: Array<ChannelID>;
	}): Promise<MessageSearchResponse> {
		const [guildAccess, includeChannel] = await Promise.all([
			this.guildService.search.collectAccessibleGuildChannels(params.userId),
			this.findDmScopeContextChannel(params.userId, params.includeChannelId),
		]);
		const {accessibleChannels, unindexedChannelIds, guildNsfwLevels} = guildAccess;
		if (unindexedChannelIds.size > 0) {
			await this.queueIndexingChannels(unindexedChannelIds);
			return {indexing: true};
		}
		const guildChannelIds = Array.from(accessibleChannels.keys());
		const dmChannelIds = await getDmChannelIdsForScope({
			scope: params.dmScope,
			userId: params.userId,
			userRepository: this.userRepository,
			includeChannel,
		});
		const combinedChannelSet = new Set<string>([...guildChannelIds, ...dmChannelIds]);
		const validatedChannelIds = this.filterRequestedChannelIds(
			Array.from(combinedChannelSet),
			params.requestedChannelIds,
		);
		const includeNsfwRequested = params.searchParams.include_nsfw ?? false;
		let canIncludeNsfw = false;
		if (includeNsfwRequested) {
			canIncludeNsfw = await this.getCanUserAccessNsfw(params.userId);
		}
		const finalChannelIds = validatedChannelIds.filter((channelIdStr) => {
			const channel = accessibleChannels.get(channelIdStr);
			if (!channel) {
				return true;
			}
			const guildId = channel.guildId?.toString();
			const guildIsAgeRestricted = guildId != null && guildNsfwLevels.get(guildId) === GuildNSFWLevel.AGE_RESTRICTED;
			if (guildIsAgeRestricted) {
				return canIncludeNsfw;
			}
			if (channel.isNsfw) {
				return canIncludeNsfw;
			}
			return true;
		});
		if (finalChannelIds.length === 0) {
			const hitsPerPage = params.searchParams.hits_per_page ?? 25;
			const page = params.searchParams.page ?? 1;
			return {
				channels: [],
				messages: [],
				total: 0,
				hits_per_page: hitsPerPage,
				page,
			};
		}
		const needsIndexing = await this.ensureChannelsIndexed(finalChannelIds);
		if (needsIndexing) {
			return {indexing: true};
		}
		return this.runSearch(finalChannelIds, params.userId, params.searchParams, params.requestCache);
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

	private async findDmScopeContextChannel(userId: UserID, channelId?: ChannelID | null): Promise<Channel | null> {
		if (!channelId) {
			return null;
		}
		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel || !isDmScopeChannelForUser(channel, userId)) {
			return null;
		}
		return channel;
	}

	private async ensureChannelsIndexed(channelIds: Array<string>): Promise<boolean> {
		const channelBrandedIds = channelIds.map((id) => createChannelID(BigInt(id)));
		const channels = await mapWithConcurrency(channelBrandedIds, CHANNEL_INDEX_CHECK_CONCURRENCY, (id) =>
			this.channelRepository.findUnique(id),
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
		await this.queueIndexingChannels(unindexed);
		return true;
	}

	private async queueIndexingChannels(channelIds: Iterable<string>): Promise<void> {
		await mapWithConcurrency(Array.from(channelIds), CHANNEL_INDEX_JOB_ENQUEUE_CONCURRENCY, async (channelId) => {
			await this.workerService.addJob(
				'indexChannelMessages',
				{channelId},
				{
					jobKey: `indexChannelMessages-${channelId}`,
					maxAttempts: 3,
				},
			);
		});
	}

	private async runSearch(
		channelIds: Array<string>,
		userId: UserID,
		searchParams: MessageSearchRequest,
		requestCache: RequestCache,
	): Promise<MessageSearchResponse> {
		const searchService = this.getMessageSearchService();
		const normalizedSearchParams = {...searchParams, channel_id: undefined};
		const filters = buildMessageSearchFilters(normalizedSearchParams, channelIds);
		const hitsPerPage = searchParams.hits_per_page ?? 25;
		const page = searchParams.page ?? 1;
		const cursor = searchParams.cursor;
		const result = await searchExistingMessages({
			searchService,
			messageRepository: this.channelRepository,
			query: searchParams.content ?? '',
			filters,
			hitsPerPage,
			page,
			cursor,
		});
		const mappedResponses = await this.responseMapper.mapSearchResultToResponses(result, userId, requestCache);
		return {
			messages: mappedResponses.messages,
			channels: mappedResponses.channels,
			total: result.total,
			hits_per_page: hitsPerPage,
			page,
			cursor: result.cursor,
		};
	}

	private async getCanUserAccessNsfw(userId: UserID): Promise<boolean> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		return canUserAccessNsfwContent(user);
	}
}

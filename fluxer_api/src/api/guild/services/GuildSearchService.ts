// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildNSFWLevel} from '@fluxer/constants/src/GuildConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {NsfwContentRequiresAgeVerificationError} from '@fluxer/errors/src/domains/moderation/NsfwContentRequiresAgeVerificationError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {MessageSearchRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {MessageSearchResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {ChannelID, GuildID, UserID} from '../../BrandedTypes';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {Channel} from '../../models/Channel';
import {getMessageSearchService} from '../../SearchFactory';
import {buildMessageSearchFilters} from '../../search/BuildMessageSearchFilters';
import {channelNeedsReindexing} from '../../search/ChannelIndexingUtils';
import {MessageSearchResponseMapper} from '../../search/MessageSearchResponseMapper';
import {searchExistingMessages} from '../../search/MessageSearchResultReconciler';
import type {IUserRepository} from '../../user/IUserRepository';
import {canUserAccessNsfwContent} from '../../utils/AgeUtils';
import {mapWithConcurrency} from '../../utils/ConcurrencyUtils';
import type {WorkerTaskName} from '../../worker/WorkerLaneConfig';

const GUILD_FANOUT_CONCURRENCY = 16;
const PERMISSION_CHECK_CONCURRENCY = 64;
const CHANNEL_INDEX_JOB_ENQUEUE_CONCURRENCY = 16;

export class GuildSearchService {
	private readonly responseMapper: MessageSearchResponseMapper;

	constructor(
		private readonly channelRepository: IChannelRepository,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		private readonly userRepository: IUserRepository,
		private readonly workerService: IWorkerService<WorkerTaskName>,
	) {
		this.responseMapper = new MessageSearchResponseMapper(this.channelRepository, this.userCacheService);
	}

	async searchMessages(params: {
		userId: UserID;
		guildId: GuildID;
		channelIds: Array<ChannelID>;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
	}): Promise<MessageSearchResponse> {
		const {userId, guildId, searchParams, requestCache} = params;
		let {channelIds} = params;
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		const guildIsAgeRestricted = guildData?.nsfw_level === GuildNSFWLevel.AGE_RESTRICTED;
		const searchService = getMessageSearchService();
		if (!searchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const explicitChannelIds = channelIds.length > 0;
		if (!explicitChannelIds) {
			const channels = await this.channelRepository.listGuildChannels(guildId);
			channelIds = channels.map((c) => c.id);
		}
		const includeNsfwRequested = searchParams.include_nsfw ?? false;
		const canUserAccessNsfw =
			guildIsAgeRestricted || includeNsfwRequested ? await this.getCanUserAccessNsfw(userId) : false;
		if (guildIsAgeRestricted) {
			if (!canUserAccessNsfw) {
				throw new NsfwContentRequiresAgeVerificationError();
			}
			if (!includeNsfwRequested) {
				const hitsPerPage = searchParams.hits_per_page ?? 25;
				const page = searchParams.page ?? 1;
				return {
					channels: [],
					messages: [],
					total: 0,
					hits_per_page: hitsPerPage,
					page,
				};
			}
		}
		const canIncludeNsfw = includeNsfwRequested && canUserAccessNsfw;
		const channels = await this.channelRepository.listChannels(channelIds);
		const channelMap = new Map<string, Channel>();
		for (const channel of channels) {
			if (channel.guildId === guildId) {
				channelMap.set(channel.id.toString(), channel);
			}
		}
		for (const id of channelIds) {
			if (!channelMap.has(id.toString())) {
				throw InputValidationError.fromCode('channel_ids', ValidationErrorCodes.ALL_CHANNELS_MUST_BELONG_TO_GUILD);
			}
		}
		const nsfwFilteredIds = channelIds.filter((id) => {
			const channel = channelMap.get(id.toString())!;
			return !(channel.isNsfw && !canIncludeNsfw);
		});
		const permissionResults = await mapWithConcurrency(nsfwFilteredIds, PERMISSION_CHECK_CONCURRENCY, (channelId) =>
			this.gatewayService.checkPermission({
				guildId,
				userId,
				channelId,
				permission: Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY,
			}),
		);
		const validChannelIds: Array<ChannelID> = [];
		for (let i = 0; i < nsfwFilteredIds.length; i++) {
			if (!permissionResults[i]) {
				if (explicitChannelIds) {
					throw new MissingPermissionsError();
				}
				continue;
			}
			validChannelIds.push(nsfwFilteredIds[i]!);
		}
		if (validChannelIds.length === 0) {
			const hitsPerPage = searchParams.hits_per_page ?? 25;
			const page = searchParams.page ?? 1;
			return {
				channels: [],
				messages: [],
				total: 0,
				hits_per_page: hitsPerPage,
				page,
			};
		}
		const channelsToIndex = validChannelIds
			.filter((channelId) => {
				const channel = channelMap.get(channelId.toString());
				return channel && channelNeedsReindexing(channel.indexedAt);
			})
			.map((id) => id.toString());
		if (channelsToIndex.length > 0) {
			await mapWithConcurrency(channelsToIndex, CHANNEL_INDEX_JOB_ENQUEUE_CONCURRENCY, (channelId) =>
				this.workerService.addJob(
					'indexChannelMessages',
					{
						channelId,
					},
					{
						jobKey: `indexChannelMessages-${channelId}`,
						maxAttempts: 3,
					},
				),
			);
			return {indexing: true};
		}
		const filters = buildMessageSearchFilters(
			searchParams,
			validChannelIds.map((id) => id.toString()),
		);
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

	async searchAllGuilds(params: {
		userId: UserID;
		channelIds: Array<ChannelID>;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
	}): Promise<MessageSearchResponse> {
		const {userId, channelIds, searchParams, requestCache} = params;
		const searchService = getMessageSearchService();
		if (!searchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const {accessibleChannels, unindexedChannelIds, guildNsfwLevels} =
			await this.collectAccessibleGuildChannels(userId);
		if (unindexedChannelIds.size > 0) {
			await this.queueIndexingChannels(unindexedChannelIds);
			return {indexing: true};
		}
		let searchChannelIds = Array.from(accessibleChannels.keys());
		if (channelIds.length > 0) {
			const requestedChannelStrings = channelIds.map((id) => id.toString());
			for (const requested of requestedChannelStrings) {
				if (!accessibleChannels.has(requested)) {
					throw new MissingPermissionsError();
				}
			}
			searchChannelIds = requestedChannelStrings;
		}
		const includeNsfwRequested = searchParams.include_nsfw ?? false;
		let canIncludeNsfw = false;
		if (includeNsfwRequested) {
			canIncludeNsfw = await this.getCanUserAccessNsfw(userId);
		}
		searchChannelIds = searchChannelIds.filter((channelIdStr) => {
			const channel = accessibleChannels.get(channelIdStr);
			if (!channel) {
				return false;
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
		if (searchChannelIds.length === 0) {
			const hitsPerPage = searchParams.hits_per_page ?? 25;
			const page = searchParams.page ?? 1;
			return {
				channels: [],
				messages: [],
				total: 0,
				hits_per_page: hitsPerPage,
				page,
			};
		}
		const filters = buildMessageSearchFilters(searchParams, searchChannelIds);
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

	private async queueIndexingChannels(channelIds: Iterable<string>): Promise<void> {
		await mapWithConcurrency(Array.from(channelIds), CHANNEL_INDEX_JOB_ENQUEUE_CONCURRENCY, (channelId) =>
			this.workerService.addJob(
				'indexChannelMessages',
				{channelId},
				{
					jobKey: `indexChannelMessages-${channelId}`,
					maxAttempts: 3,
				},
			),
		);
	}

	async collectAccessibleGuildChannels(userId: UserID): Promise<{
		accessibleChannels: Map<string, Channel>;
		unindexedChannelIds: Set<string>;
		guildNsfwLevels: Map<string, number>;
	}> {
		const guildIds = await this.userRepository.getUserGuildIds(userId);
		const accessibleChannels = new Map<string, Channel>();
		const unindexedChannelIds = new Set<string>();
		const guildNsfwLevels = new Map<string, number>();
		const permissionChecks: Array<{
			channel: Channel;
			guildId: GuildID;
		}> = [];
		await mapWithConcurrency(guildIds, GUILD_FANOUT_CONCURRENCY, async (guildId) => {
			const [guildData, guildChannels, viewableChannels] = await Promise.all([
				this.gatewayService.getGuildData({guildId, userId}),
				this.channelRepository.listGuildChannels(guildId),
				this.gatewayService.getViewableChannels({guildId, userId}),
			]);
			if (guildData) {
				guildNsfwLevels.set(guildId.toString(), guildData.nsfw_level);
			}
			const viewableChannelIds = new Set(viewableChannels.map((channelId) => channelId.toString()));
			for (const channel of guildChannels) {
				if (viewableChannelIds.has(channel.id.toString())) {
					permissionChecks.push({channel, guildId});
				}
			}
		});
		const permissionResults = await mapWithConcurrency(
			permissionChecks,
			PERMISSION_CHECK_CONCURRENCY,
			({channel, guildId}) =>
				this.gatewayService.checkPermission({
					guildId,
					userId,
					channelId: channel.id,
					permission: Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY,
				}),
		);
		for (let i = 0; i < permissionChecks.length; i++) {
			if (!permissionResults[i]) {
				continue;
			}
			const {channel} = permissionChecks[i]!;
			const channelIdStr = channel.id.toString();
			accessibleChannels.set(channelIdStr, channel);
			if (channelNeedsReindexing(channel.indexedAt)) {
				unindexedChannelIds.add(channelIdStr);
			}
		}
		return {accessibleChannels, unindexedChannelIds, guildNsfwLevels};
	}
}

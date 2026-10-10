// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {threadSearchIndexEnqueueCappedTotal} from '@app/api/channel/threads/ThreadMetrics';
import {
	guildActive,
	THREAD_FEATURE_CHANNEL_TYPES,
	THREAD_PARENT_CHANNEL_TYPES,
	type ThreadViewer,
	viewerActive,
} from '@app/api/experiment/ChannelThreadsGate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import {getMessageSearchService} from '@app/api/SearchFactory';
import {buildMessageSearchFilters} from '@app/api/search/BuildMessageSearchFilters';
import {channelNeedsReindexing} from '@app/api/search/ChannelIndexingUtils';
import {MessageSearchResponseMapper} from '@app/api/search/MessageSearchResponseMapper';
import {searchExistingMessages} from '@app/api/search/MessageSearchResultReconciler';
import {channelRequiresAgeVerification} from '@app/api/search/SearchNsfwUtils';
import {accessibleRequestedThreadIds, accessibleThreadIds} from '@app/api/search/ThreadSearchScope';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {canUserAccessNsfwContent} from '@app/api/utils/AgeUtils';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildNSFWLevel} from '@fluxer/constants/src/GuildConstants';
import {SEARCH_INDEX_ENQUEUE_MAX} from '@fluxer/constants/src/ThreadConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {NsfwContentRequiresAgeVerificationError} from '@fluxer/errors/src/domains/moderation/NsfwContentRequiresAgeVerificationError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {MessageSearchRequest} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {MessageSearchResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';

const GUILD_FANOUT_CONCURRENCY = 16;
const PERMISSION_CHECK_CONCURRENCY = 64;
const CHANNEL_INDEX_JOB_ENQUEUE_CONCURRENCY = 16;

export class GuildSearchService {
	private readonly responseMapper: MessageSearchResponseMapper;

	constructor(
		private readonly channelRepository: ChannelRepository,
		private readonly userCacheService: UserCacheService,
		private readonly gatewayService: IGatewayService,
		private readonly userRepository: UserRepository,
		private readonly workerService: IWorkerService<WorkerTaskName>,
	) {
		this.responseMapper = new MessageSearchResponseMapper(this.channelRepository, this.userCacheService);
	}

	async searchMessages(params: {
		userId: UserID;
		viewer: ThreadViewer;
		guildId: GuildID;
		channelIds: Array<ChannelID>;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
	}): Promise<MessageSearchResponse> {
		const {userId, viewer, guildId, searchParams, requestCache} = params;
		let {channelIds} = params;
		const guildData = await this.gatewayService.getGuildData({guildId, userId});
		const guildIsAgeRestricted = guildData?.nsfw_level === GuildNSFWLevel.AGE_RESTRICTED;
		const searchService = getMessageSearchService();
		if (!searchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const threadsVisible = viewerActive(viewer, guildId);
		const explicitChannelIds = channelIds.length > 0;
		if (!explicitChannelIds) {
			const channels = await this.channelRepository.channelData.listGuildChannels(guildId, 'enrolled');
			channelIds = channels.filter((c) => threadsVisible || !c.isThreadOnly()).map((c) => c.id);
		}
		const includeNsfwRequested = searchParams.include_nsfw ?? false;
		const canUserAccessNsfw =
			guildIsAgeRestricted || includeNsfwRequested ? await this.getCanUserAccessNsfw(userId) : false;
		if (guildIsAgeRestricted && !canUserAccessNsfw) {
			throw new NsfwContentRequiresAgeVerificationError();
		}
		const canIncludeNsfw = canUserAccessNsfw && (includeNsfwRequested || guildIsAgeRestricted);
		const guildNsfw = guildData?.nsfw ?? false;
		const channels = await this.channelRepository.channelData.listChannels(channelIds);
		const channelMap = new Map<string, Channel>();
		for (const channel of channels) {
			if (channel.guildId === guildId && (threadsVisible || !THREAD_FEATURE_CHANNEL_TYPES.has(channel.type))) {
				channelMap.set(channel.id.toString(), channel);
			}
		}
		for (const id of channelIds) {
			if (!channelMap.has(id.toString())) {
				throw InputValidationError.fromCode('channel_ids', ValidationErrorCodes.ALL_CHANNELS_MUST_BELONG_TO_GUILD);
			}
		}
		const requestedThreads = explicitChannelIds
			? channels.filter((channel) => channelMap.get(channel.id.toString())?.isThread())
			: [];
		if (requestedThreads.length > 0) {
			const threadIds = new Set(requestedThreads.map((thread) => thread.id.toString()));
			const parentIds = requestedThreads.flatMap((thread) => (thread.parentId ? [thread.parentId] : []));
			for (const parent of await this.channelRepository.channelData.listChannels(
				parentIds.filter((id) => !channelMap.has(id.toString())),
			)) {
				if (parent.guildId === guildId) channelMap.set(parent.id.toString(), parent);
			}
			channelIds = [
				...new Map(
					[...channelIds.filter((id) => !threadIds.has(id.toString())), ...parentIds]
						.filter((id) => channelMap.has(id.toString()))
						.map((id) => [id.toString(), id] as const),
				).values(),
			];
		}
		const categoryLookup = await this.buildParentCategoryLookup(channelMap);
		const nsfwFilteredIds = channelIds.filter((id) => {
			const channel = channelMap.get(id.toString())!;
			return !(channelRequiresAgeVerification(channel, categoryLookup, guildNsfw) && !canIncludeNsfw);
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
		let channelsToIndex = validChannelIds
			.filter((channelId) => {
				const channel = channelMap.get(channelId.toString());
				return channel && !channel.isThread() && !channel.isThreadOnly() && channelNeedsReindexing(channel.indexedAt);
			})
			.map((id) => id.toString());
		if (channelsToIndex.length > SEARCH_INDEX_ENQUEUE_MAX && guildActive(guildId)) {
			channelsToIndex = channelsToIndex.slice(0, SEARCH_INDEX_ENQUEUE_MAX);
			threadSearchIndexEnqueueCappedTotal.inc();
		}
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
		const searchChannelIds = threadsVisible
			? await this.threadSearchChannelIds({
					userId,
					guildId,
					validChannelIds,
					channelMap,
					requestedIds: explicitChannelIds ? new Set(params.channelIds.map((id) => id.toString())) : null,
				})
			: validChannelIds.map((id) => id.toString());
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
		const mappedResponses = await this.responseMapper.mapSearchResultToResponses(
			result.messages,
			userId,
			viewer,
			requestCache,
		);
		return {
			messages: mappedResponses.messages,
			channels: mappedResponses.channels,
			total: result.total,
			hits_per_page: hitsPerPage,
			page,
			cursor: result.cursor,
			...(mappedResponses.threads ? {threads: mappedResponses.threads, members: mappedResponses.members} : {}),
		};
	}

	async searchAllGuilds(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelIds: Array<ChannelID>;
		searchParams: MessageSearchRequest;
		requestCache: RequestCache;
	}): Promise<MessageSearchResponse> {
		const {userId, viewer, channelIds, searchParams, requestCache} = params;
		const searchService = getMessageSearchService();
		if (!searchService) {
			throw new FeatureTemporarilyDisabledError();
		}
		const {accessibleChannels, unindexedChannelIds, guildNsfwLevels, parentCategories, threadParents} =
			await this.collectAccessibleGuildChannels(userId, viewer, channelIds);
		if (unindexedChannelIds.size > 0) {
			await this.queueIndexingChannels(unindexedChannelIds);
			return {indexing: true};
		}
		let searchChannelIds = [...accessibleChannels.keys(), ...threadParents.keys()];
		if (channelIds.length > 0) {
			const requestedChannelStrings = channelIds.map((id) => id.toString());
			for (const requested of requestedChannelStrings) {
				if (!accessibleChannels.has(requested) && !threadParents.has(requested)) {
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
			const channel = accessibleChannels.get(channelIdStr) ?? threadParents.get(channelIdStr);
			if (!channel) {
				return false;
			}
			const guildId = channel.guildId?.toString();
			const guildIsAgeRestricted = guildId != null && guildNsfwLevels.get(guildId) === GuildNSFWLevel.AGE_RESTRICTED;
			if (guildIsAgeRestricted) {
				return canIncludeNsfw;
			}
			if (channelRequiresAgeVerification(channel, parentCategories, false)) {
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
		const mappedResponses = await this.responseMapper.mapSearchResultToResponses(
			result.messages,
			userId,
			viewer,
			requestCache,
		);
		return {
			messages: mappedResponses.messages,
			channels: mappedResponses.channels,
			total: result.total,
			hits_per_page: hitsPerPage,
			page,
			cursor: result.cursor,
			...(mappedResponses.threads ? {threads: mappedResponses.threads, members: mappedResponses.members} : {}),
		};
	}

	private async threadSearchChannelIds(params: {
		userId: UserID;
		guildId: GuildID;
		validChannelIds: Array<ChannelID>;
		channelMap: Map<string, Channel>;
		requestedIds: Set<string> | null;
	}): Promise<Array<string>> {
		const {channelMap, requestedIds} = params;
		const parents = params.validChannelIds.filter((id) => {
			const channel = channelMap.get(id.toString());
			return channel !== undefined && THREAD_PARENT_CHANNEL_TYPES.has(channel.type);
		});
		const ids = params.validChannelIds
			.map((id) => id.toString())
			.filter((id) => !channelMap.get(id)?.isThreadOnly() && (requestedIds === null || requestedIds.has(id)));
		const scopeParents =
			requestedIds === null
				? parents
				: parents.filter((id) => requestedIds.has(id.toString()) && channelMap.get(id.toString())?.isThreadOnly());
		const scope = await accessibleThreadIds({
			gatewayService: this.gatewayService,
			userId: params.userId,
			groups: [{guildId: params.guildId, parentIds: scopeParents}],
		});
		if (requestedIds === null) return [...ids, ...scope.keys()];
		const visibleParents = new Set(parents.map((id) => id.toString()));
		const requestedThreads = [...requestedIds].flatMap((id) => {
			const channel = channelMap.get(id);
			if (!channel?.isThread()) return [];
			const parentVisible = channel.parentId !== null && visibleParents.has(channel.parentId.toString());
			return [{id: channel.id, type: channel.type, parentId: parentVisible ? channel.parentId : null}];
		});
		const accessible = await accessibleRequestedThreadIds({
			gatewayService: this.gatewayService,
			userId: params.userId,
			guildId: params.guildId,
			threads: requestedThreads,
		});
		for (const thread of requestedThreads) {
			if (!accessible.has(thread.id.toString())) throw new MissingPermissionsError();
		}
		return [...new Set([...ids, ...scope.keys(), ...accessible])];
	}

	private async buildParentCategoryLookup(channelMap: Map<string, Channel>): Promise<Map<string, Channel>> {
		const lookup = new Map<string, Channel>(channelMap);
		const missingParentIds: Array<ChannelID> = [];
		for (const channel of channelMap.values()) {
			const parentId = channel.parentId;
			if (parentId != null && !lookup.has(parentId.toString())) {
				missingParentIds.push(parentId);
			}
		}
		if (missingParentIds.length > 0) {
			const parents = await this.channelRepository.channelData.listChannels(missingParentIds);
			for (const parent of parents) {
				lookup.set(parent.id.toString(), parent);
			}
		}
		return lookup;
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

	private async addRequestedThreads(
		userId: UserID,
		threadIds: ReadonlyArray<ChannelID>,
		parentById: Map<string, Channel>,
		threadParents: Map<string, Channel>,
	): Promise<void> {
		const threadsByGuild = new Map<string, Array<Channel>>();
		for (const thread of await this.channelRepository.channelData.listChannels([...threadIds])) {
			const parent = thread.parentId === null ? undefined : parentById.get(thread.parentId.toString());
			if (!thread.isThread() || !parent || thread.guildId === null || parent.guildId !== thread.guildId) continue;
			const threads = threadsByGuild.get(thread.guildId.toString());
			if (threads) threads.push(thread);
			else threadsByGuild.set(thread.guildId.toString(), [thread]);
		}
		await mapWithConcurrency([...threadsByGuild.values()], GUILD_FANOUT_CONCURRENCY, async (threads) => {
			const accessible = await accessibleRequestedThreadIds({
				gatewayService: this.gatewayService,
				userId,
				guildId: threads[0]!.guildId!,
				threads: threads.map((thread) => ({id: thread.id, parentId: thread.parentId, type: thread.type})),
			});
			for (const thread of threads) {
				if (accessible.has(thread.id.toString())) {
					threadParents.set(thread.id.toString(), parentById.get(thread.parentId!.toString())!);
				}
			}
		});
	}

	async collectAccessibleGuildChannels(
		userId: UserID,
		viewer?: ThreadViewer,
		requestedChannelIds?: ReadonlyArray<ChannelID>,
	): Promise<{
		accessibleChannels: Map<string, Channel>;
		unindexedChannelIds: Set<string>;
		guildNsfwLevels: Map<string, number>;
		parentCategories: Map<string, Channel>;
		threadParents: Map<string, Channel>;
	}> {
		const guildIds = await this.userRepository.getUserGuildIds(userId);
		const accessibleChannels = new Map<string, Channel>();
		const unindexedChannelIds = new Set<string>();
		const guildNsfwLevels = new Map<string, number>();
		const parentCategories = new Map<string, Channel>();
		const permissionChecks: Array<{
			channel: Channel;
			guildId: GuildID;
		}> = [];
		await mapWithConcurrency(guildIds, GUILD_FANOUT_CONCURRENCY, async (guildId) => {
			const [guildData, guildChannels, viewableChannels] = await Promise.all([
				this.gatewayService.getGuildData({guildId, userId}),
				this.channelRepository.channelData
					.listGuildChannels(guildId, 'enrolled')
					.then((channels) =>
						channels.filter(
							(channel) => !channel.isThreadOnly() || (viewer !== undefined && viewerActive(viewer, guildId)),
						),
					),
				this.gatewayService.getViewableChannels({guildId, userId}),
			]);
			if (guildData) {
				guildNsfwLevels.set(guildId.toString(), guildData.nsfw_level);
			}
			const viewableChannelIds = new Set(viewableChannels.map((channelId) => channelId.toString()));
			for (const channel of guildChannels) {
				if (channel.type === ChannelTypes.GUILD_CATEGORY) {
					parentCategories.set(channel.id.toString(), channel);
				}
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
		const threadParentsByGuild = new Map<GuildID, Array<ChannelID>>();
		const parentById = new Map<string, Channel>();
		for (let i = 0; i < permissionChecks.length; i++) {
			if (!permissionResults[i]) {
				continue;
			}
			const {channel, guildId} = permissionChecks[i]!;
			if (viewer !== undefined && THREAD_PARENT_CHANNEL_TYPES.has(channel.type) && viewerActive(viewer, guildId)) {
				const parentIds = threadParentsByGuild.get(guildId);
				if (parentIds) parentIds.push(channel.id);
				else threadParentsByGuild.set(guildId, [channel.id]);
				parentById.set(channel.id.toString(), channel);
			}
			if (channel.isThreadOnly()) {
				continue;
			}
			const channelIdStr = channel.id.toString();
			accessibleChannels.set(channelIdStr, channel);
			if (channelNeedsReindexing(channel.indexedAt)) {
				unindexedChannelIds.add(channelIdStr);
			}
		}
		const scope = await accessibleThreadIds({
			gatewayService: this.gatewayService,
			userId,
			groups: Array.from(threadParentsByGuild, ([guildId, parentIds]) => ({guildId, parentIds})),
		});
		const threadParents = new Map(
			Array.from(scope, ([threadId, parentId]) => [threadId, parentById.get(parentId.toString())!] as const),
		);
		const missingThreadIds =
			parentById.size > 0
				? (requestedChannelIds ?? []).filter(
						(id) => !accessibleChannels.has(id.toString()) && !threadParents.has(id.toString()),
					)
				: [];
		if (missingThreadIds.length > 0) {
			await this.addRequestedThreads(userId, missingThreadIds, parentById, threadParents);
		}
		return {accessibleChannels, unindexedChannelIds, guildNsfwLevels, parentCategories, threadParents};
	}
}

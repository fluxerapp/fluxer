// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import {channelIdToMessageId, createChannelID, createMessageID} from '@app/api/BrandedTypes';
import type {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import {
	type MessageResponseDataService,
	messageResponseAccessForGuild,
} from '@app/api/channel/services/message/MessageResponseDataService';
import {mapThreadToResponse} from '@app/api/channel/services/thread/ThreadMappers';
import {type ThreadViewer, viewerActive} from '@app/api/experiment/ChannelThreadsGate';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import {getCachedUserPartialResponse} from '@app/api/user/UserCacheHelpers';
import {ChannelTypes, MessageReferenceTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {
	ServerMessageFlags,
	TEXT_THREAD_PARENT_CHANNEL_TYPES,
	THREAD_MESSAGE_FLAG_MASK,
} from '@fluxer/constants/src/ThreadConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

interface ThreadListQuery {
	limit: number;
	before?: MessageID;
	after?: MessageID;
	around?: MessageID;
}

export function carriesThreadArtifact(response: MessageResponse): boolean {
	if (response.type === MessageTypes.THREAD_CREATED || response.type === MessageTypes.THREAD_STARTER_MESSAGE) {
		return true;
	}
	if ((response.flags & THREAD_MESSAGE_FLAG_MASK) !== 0 || response.thread !== undefined) return true;
	if (response.message_snapshots?.some((snapshot) => (snapshot.flags & THREAD_MESSAGE_FLAG_MASK) !== 0)) return true;
	const referenced = response.referenced_message;
	return referenced != null && carriesThreadArtifact(referenced);
}

function maskResponse(response: MessageResponse): MessageResponse {
	const {thread: _thread, ...rest} = response;
	const masked: MessageResponse = {...rest, flags: response.flags & ~THREAD_MESSAGE_FLAG_MASK};
	if (response.message_snapshots) {
		masked.message_snapshots = response.message_snapshots.map((snapshot) => ({
			...snapshot,
			flags: snapshot.flags & ~THREAD_MESSAGE_FLAG_MASK,
		}));
	}
	if (response.referenced_message) {
		masked.referenced_message =
			response.referenced_message.type === MessageTypes.THREAD_CREATED
				? null
				: maskResponse(response.referenced_message);
	}
	return masked;
}

export function maskThreadArtifacts(responses: Array<MessageResponse>): Array<MessageResponse> {
	if (!responses.some(carriesThreadArtifact)) return responses;
	return responses.filter((response) => response.type !== MessageTypes.THREAD_CREATED).map(maskResponse);
}

export function maskThreadArtifactsFor(
	viewer: ThreadViewer,
	guildId: string | bigint | null,
	responses: Array<MessageResponse>,
): Array<MessageResponse> {
	if (!responses.some(carriesThreadArtifact)) return responses;
	if (guildId !== null && viewerActive(viewer, guildId)) return responses;
	return maskThreadArtifacts(responses);
}

export function maskThreadArtifactsByChannel(
	viewer: ThreadViewer,
	channelById: Map<string, Channel>,
	responses: Array<MessageResponse>,
): Array<MessageResponse> {
	if (!responses.some(carriesThreadArtifact)) return responses;
	const byGuild = new Map<string, Array<MessageResponse>>();
	for (const response of responses) {
		const guildKey = channelById.get(response.channel_id)?.guildId?.toString() ?? '';
		const group = byGuild.get(guildKey) ?? [];
		group.push(response);
		byGuild.set(guildKey, group);
	}
	const masked = new Map<MessageResponse, MessageResponse | null>();
	for (const [guildKey, group] of byGuild) {
		const shaped = maskThreadArtifactsFor(viewer, guildKey === '' ? null : guildKey, group);
		if (shaped === group) continue;
		const byId = new Map(shaped.map((response) => [response.id, response]));
		for (const response of group) masked.set(response, byId.get(response.id) ?? null);
	}
	if (masked.size === 0) return responses;
	return responses.flatMap((response) => {
		const shaped = masked.get(response);
		if (shaped === undefined) return [response];
		return shaped === null ? [] : [shaped];
	});
}

export class ThreadMessageResponses {
	constructor(
		private readonly channelRepository: IChannelRepositoryAggregate,
		private readonly responseDataService: MessageResponseDataService,
		private readonly userCacheService: UserCacheService,
	) {}

	async shape(params: {
		viewer: ThreadViewer;
		userId: UserID;
		authChannel: AuthenticatedChannel;
		responses: Array<MessageResponse>;
		requestCache: RequestCache;
		query?: ThreadListQuery;
	}): Promise<Array<MessageResponse>> {
		const {authChannel, viewer} = params;
		const guildId = authChannel.channel.guildId;
		let responses = params.responses;
		if (guildId === null || !viewerActive(viewer, guildId)) {
			return maskThreadArtifacts(responses);
		}
		if (authChannel.thread && params.query && (await this.starterBelongsToPage(authChannel, responses, params.query))) {
			const starter = await this.buildStarter(params);
			if (starter) {
				responses = [...responses, starter];
				if (responses.length > params.query.limit) responses = responses.slice(responses.length - params.query.limit);
			}
		}
		return this.attachThreads(responses, params.userId);
	}

	async getStarter(params: {
		viewer: ThreadViewer;
		userId: UserID;
		authChannel: AuthenticatedChannel;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<MessageResponse | null> {
		const {authChannel} = params;
		if (!authChannel.thread || params.messageId.toString() !== authChannel.channel.id.toString()) return null;
		return this.buildStarter(params);
	}

	async getFirstMessages(params: {
		userId: UserID;
		guildId: GuildID;
		canReadMessageHistory: boolean;
		threadIds: Array<ChannelID>;
	}): Promise<Map<string, MessageResponse>> {
		if (params.threadIds.length === 0) return new Map();
		const messages = await Promise.all(
			params.threadIds.map((threadId) =>
				this.channelRepository.messages.getMessage(threadId, channelIdToMessageId(threadId)),
			),
		);
		const found = messages.filter((message): message is Message => message !== null);
		const responses = await this.responseDataService.buildMessages({
			userId: params.userId,
			messages: found,
			access: {
				sourceGuildId: params.guildId,
				messageHistoryCutoff: null,
				canReadMessageHistory: params.canReadMessageHistory,
			},
		});
		return new Map(responses.map((response) => [response.channel_id, response]));
	}

	private async starterBelongsToPage(
		authChannel: AuthenticatedChannel,
		responses: Array<MessageResponse>,
		query: ThreadListQuery,
	): Promise<boolean> {
		const threadId = BigInt(authChannel.channel.id);
		if (query.after !== undefined) return BigInt(query.after) < threadId;
		if (query.around !== undefined && BigInt(query.around) === threadId) return true;
		if (query.before !== undefined && BigInt(query.before) <= threadId) return false;
		if (responses.length >= query.limit) return false;
		const oldest = responses.reduce<bigint | undefined>((min, response) => {
			const id = BigInt(response.id);
			return min === undefined || id < min ? id : min;
		}, undefined);
		const anchor =
			oldest ??
			(query.before !== undefined ? BigInt(query.before) : undefined) ??
			(query.around !== undefined ? BigInt(query.around) + 1n : undefined);
		const older = await this.channelRepository.messages.listMessages(
			createChannelID(threadId),
			anchor !== undefined ? createMessageID(anchor) : undefined,
			1,
		);
		return older.length === 0;
	}

	private async buildStarter(params: {
		userId: UserID;
		authChannel: AuthenticatedChannel;
		requestCache: RequestCache;
	}): Promise<MessageResponse | null> {
		const thread = params.authChannel.thread;
		const channel = params.authChannel.channel;
		if (!thread?.state.hasStarter || !TEXT_THREAD_PARENT_CHANNEL_TYPES.has(thread.parent.type)) return null;
		const source = await this.responseDataService.getMessage({
			userId: params.userId,
			channelId: thread.parent.id,
			messageId: createMessageID(BigInt(channel.id)),
			access: messageResponseAccessForGuild(channel.guildId),
		});
		const author =
			source?.author ??
			(channel.ownerId
				? await getCachedUserPartialResponse({
						userId: channel.ownerId,
						userCacheService: this.userCacheService,
						requestCache: params.requestCache,
					})
				: null);
		if (!author) return null;
		const [referenced] = source ? await this.attachThreads([source], params.userId) : [null];
		return {
			id: channel.id.toString(),
			channel_id: channel.id.toString(),
			author,
			webhook_id: null,
			type: MessageTypes.THREAD_STARTER_MESSAGE,
			flags: 0,
			content: '',
			timestamp: snowflakeToDate(channel.id).toISOString(),
			edited_timestamp: null,
			pinned: false,
			mention_everyone: false,
			tts: false,
			mentions: [],
			mention_roles: [],
			embeds: [],
			attachments: [],
			message_reference: {
				channel_id: thread.parent.id.toString(),
				message_id: channel.id.toString(),
				guild_id: channel.guildId?.toString() ?? null,
				type: MessageReferenceTypes.DEFAULT,
			},
			referenced_message: referenced ?? null,
		};
	}

	private async attachThreads(responses: Array<MessageResponse>, userId: UserID): Promise<Array<MessageResponse>> {
		const threadIds = responses
			.filter((response) => (response.flags & ServerMessageFlags.HAS_THREAD) !== 0)
			.map((response) => createChannelID(BigInt(response.id)));
		if (threadIds.length === 0) return responses;
		const views = await this.loadThreadViews(threadIds, userId);
		return responses.map((response) => {
			const view = views.get(response.id);
			return view ? {...response, thread: view} : response;
		});
	}

	private async loadThreadViews(
		threadIds: Array<ChannelID>,
		userId: UserID,
	): Promise<Map<string, NonNullable<MessageResponse['thread']>>> {
		const {channelData, threads} = this.channelRepository;
		const [channels, states, stats, members] = await Promise.all([
			channelData.listChannels(threadIds),
			threads.getStates(threadIds),
			threads.getStatsMany(threadIds),
			Promise.all(threadIds.map((threadId) => threads.getMember(threadId, userId))),
		]);
		const channelById = new Map<string, Channel>(channels.map((channel) => [channel.id.toString(), channel]));
		const memberById = new Map(members.flatMap((member) => (member ? [[member.threadId.toString(), member]] : [])));
		const views = new Map<string, NonNullable<MessageResponse['thread']>>();
		for (const state of states) {
			const key = state.threadId.toString();
			const channel = channelById.get(key);
			const threadStats = stats.get(state.threadId);
			if (!channel || !threadStats) continue;
			views.set(
				key,
				mapThreadToResponse(
					{channel, state, stats: threadStats, parentType: ChannelTypes.GUILD_TEXT},
					memberById.get(key) ?? null,
				),
			);
		}
		return views;
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {mapThreadMemberToRpcResponse, mapThreadToRpcResponse} from '@app/api/channel/services/thread/ThreadMappers';
import {mapThreadParentFields} from '@app/api/channel/services/thread/ThreadParentSettings';
import {loadThreadViews} from '@app/api/channel/services/thread/ThreadViews';
import {getCompiledChannelThreadsConfig, guildActive, isTainted} from '@app/api/experiment/ChannelThreadsGate';
import type {Channel} from '@app/api/models/Channel';
import type {ThreadMember} from '@app/api/models/ThreadMember';
import type {ThreadParentConfig} from '@app/api/models/ThreadParentConfig';
import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {
	FORUM_UNREAD_COUNT_CAP,
	MAX_THREAD_MEMBERS,
	THREAD_ONLY_CHANNEL_TYPES,
} from '@fluxer/constants/src/ThreadConstants';
import type {
	ThreadMemberRpcResponse,
	ThreadParentSettingsRpcResponse,
} from '@fluxer/schema/src/domains/channel/ThreadSchemas';

const MEMBER_PAGE_SIZE = 1000;
const MEMBER_LOAD_CONCURRENCY = 16;

export type ThreadGateRpcData =
	| {thread_gate: {active: boolean; config_version: number}; thread_tainted: boolean}
	| {thread_tainted: true};

export interface ThreadCollectionRpcData {
	threads: Array<ReturnType<typeof mapThreadToRpcResponse>>;
	thread_members?: Array<ThreadMemberRpcResponse>;
	thread_parent_settings: Array<ThreadParentSettingsRpcResponse>;
}

export async function resolveThreadGateRpcData(guildId: GuildID): Promise<ThreadGateRpcData | null> {
	if (!guildActive(guildId)) return (await isTainted(guildId)) ? {thread_tainted: true} : null;
	return {
		thread_gate: {active: true, config_version: getCompiledChannelThreadsConfig().config.config_version},
		thread_tainted: await isTainted(guildId),
	};
}

function mapParentSettingsToRpc(
	parent: Channel,
	config: ThreadParentConfig | null,
): ThreadParentSettingsRpcResponse | null {
	if (config === null && !THREAD_ONLY_CHANNEL_TYPES.has(parent.type)) return null;
	return {channel_id: parent.id.toString(), ...mapThreadParentFields(parent.type, config)};
}

async function listAllMembers(repository: ChannelRepository, threadId: ChannelID): Promise<Array<ThreadMember>> {
	const members: Array<ThreadMember> = [];
	let after: UserID | undefined;
	while (members.length < MAX_THREAD_MEMBERS * 2) {
		const page = await repository.threads.listMembers(threadId, {after, limit: MEMBER_PAGE_SIZE});
		members.push(...page);
		if (page.length < MEMBER_PAGE_SIZE) break;
		after = page[page.length - 1]!.userId;
	}
	return members;
}

export async function loadThreadCollectionRpcData(
	repository: ChannelRepository,
	guildId: GuildID,
	parents: ReadonlyArray<Channel>,
	{members: includeMembers}: {members: boolean},
): Promise<ThreadCollectionRpcData> {
	const [states, parentConfigs] = await Promise.all([
		repository.threads.listActiveThreads(guildId),
		repository.threads.listParentConfigs(guildId),
	]);
	const views = await loadThreadViews(repository, states, parents);
	const members = includeMembers
		? await mapWithConcurrency(views, MEMBER_LOAD_CONCURRENCY, (view) =>
				listAllMembers(repository, view.state.threadId),
			)
		: null;
	const configs = new Map(parentConfigs.map((config) => [config.channelId, config]));
	return {
		threads: views.map(mapThreadToRpcResponse),
		...(members ? {thread_members: members.flat().map(mapThreadMemberToRpcResponse)} : {}),
		thread_parent_settings: parents.flatMap((parent) => {
			const settings = mapParentSettingsToRpc(parent, configs.get(parent.id) ?? null);
			return settings ? [settings] : [];
		}),
	};
}

export async function loadActiveThreadMemberships(
	repository: ChannelRepository,
	guildId: GuildID,
	userId: UserID,
): Promise<Array<ThreadMemberRpcResponse>> {
	const joined = await repository.threads.listJoinedThreadIds(userId, guildId);
	if (joined.length === 0) return [];
	const states = await repository.threads.getStates(joined);
	const active = states.filter((state) => !state.archived && state.guildId === guildId);
	const members = await Promise.all(active.map((state) => repository.threads.getMember(state.threadId, userId)));
	return members.flatMap((member) => (member ? [mapThreadMemberToRpcResponse(member)] : []));
}

export async function listThreadMembersPage(
	repository: ChannelRepository,
	params: {guildId: GuildID; threadIds: Array<ChannelID>; limit: number; after?: UserID},
): Promise<{
	members: Array<ThreadMemberRpcResponse>;
	has_more: boolean;
	next_thread_id: string | null;
	next_after_user_id: string | null;
}> {
	const states = await repository.threads.getStates(params.threadIds);
	const inGuild = new Set(states.filter((state) => state.guildId === params.guildId).map((state) => state.threadId));
	const members: Array<ThreadMember> = [];
	let after = params.after;
	for (const threadId of params.threadIds) {
		const cursor = after;
		after = undefined;
		if (!inGuild.has(threadId)) continue;
		const remaining = params.limit - members.length;
		const page = await repository.threads.listMembers(threadId, {after: cursor, limit: remaining + 1});
		if (page.length > remaining) {
			members.push(...page.slice(0, remaining));
			return {
				members: members.map(mapThreadMemberToRpcResponse),
				has_more: true,
				next_thread_id: threadId.toString(),
				next_after_user_id: remaining > 0 ? page[remaining - 1]!.userId.toString() : null,
			};
		}
		members.push(...page);
	}
	return {
		members: members.map(mapThreadMemberToRpcResponse),
		has_more: false,
		next_thread_id: null,
		next_after_user_id: null,
	};
}

export interface ForumUnreadRpcEntry {
	thread_id: string;
	count?: number;
	missing?: true;
}

export async function loadForumUnreads(
	repository: ChannelRepository,
	params: {
		guildId: GuildID;
		channelId: ChannelID;
		threads: Array<{threadId: ChannelID; ackMessageId?: MessageID}>;
	},
): Promise<Array<ForumUnreadRpcEntry>> {
	if (!guildActive(params.guildId)) return [];
	const states = await repository.threads.getStates(params.threads.map((thread) => thread.threadId));
	const inForum = new Set(
		states
			.filter((state) => state.guildId === params.guildId && state.parentId === params.channelId)
			.map((state) => state.threadId),
	);
	return Promise.all(
		params.threads
			.filter((thread) => inForum.has(thread.threadId))
			.map(async ({threadId, ackMessageId}): Promise<ForumUnreadRpcEntry> => {
				if (ackMessageId === undefined) return {thread_id: threadId.toString(), missing: true};
				const unread = await repository.messages.listMessages(
					threadId,
					undefined,
					FORUM_UNREAD_COUNT_CAP,
					ackMessageId,
				);
				return {thread_id: threadId.toString(), count: unread.length};
			}),
	);
}

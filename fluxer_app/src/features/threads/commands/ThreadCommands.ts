// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {Channel, ChannelWire} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_CHANNEL_TYPES, THREAD_SEARCH_MAX_LIMIT} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';

const logger = new Logger('Threads');

export type ArchivedThreadListKind = 'public' | 'private' | 'joined';

export interface ArchivedThreadPage {
	threadIds: ReadonlyArray<string>;
	hasMore: boolean;
}

interface ThreadListResponse {
	threads: Array<ChannelWire>;
	members: Array<ThreadMemberResponse>;
	has_more?: boolean;
}

export interface ThreadPatchBody {
	name?: string;
	archived?: boolean;
	locked?: boolean;
	auto_archive_duration?: number;
	rate_limit_per_user?: number;
	invitable?: boolean;
}

export interface ThreadMemberSettingsBody {
	flags?: number;
	muted?: boolean;
	mute_config?: {end_time: string | null; selected_time_window: number} | null;
}

function assertActive(guildId: string | null | undefined): asserts guildId is string {
	if (!ThreadGuilds.isActive(guildId)) {
		throw new Error('Threads are not available in this community');
	}
}

function ingestThread(wire: ChannelWire, guildId: string): Channel | undefined {
	return ChannelThreads.upsert(wire, guildId);
}

export async function createThreadFromMessage(
	parent: Channel,
	messageId: string,
	body: {name: string; auto_archive_duration?: number},
): Promise<Channel | undefined> {
	assertActive(parent.guildId);
	const response = await http.post<ChannelWire>(Endpoints.CHANNEL_MESSAGE_THREADS(parent.id, messageId), {
		body: {
			...body,
			auto_archive_duration: body.auto_archive_duration ?? parent.defaultAutoArchiveDuration ?? undefined,
		},
	});
	return ingestThread(response.body, parent.guildId);
}

export async function createThread(
	parent: Channel,
	body: {name: string; type: number; auto_archive_duration?: number; invitable?: boolean},
): Promise<Channel | undefined> {
	assertActive(parent.guildId);
	const withDuration = {
		...body,
		auto_archive_duration: body.auto_archive_duration ?? parent.defaultAutoArchiveDuration ?? undefined,
	};
	const payload = body.type === ChannelTypes.PRIVATE_THREAD ? withDuration : {...withDuration, invitable: undefined};
	const response = await http.post<ChannelWire>(Endpoints.CHANNEL_THREADS(parent.id), {body: payload});
	return ingestThread(response.body, parent.guildId);
}

export async function fetchThread(guildId: string, threadId: string): Promise<Channel | undefined> {
	assertActive(guildId);
	const response = await http.get<ChannelWire>(Endpoints.CHANNEL(threadId));
	const wire = response.body;
	if (!wire || !THREAD_CHANNEL_TYPES.has(wire.type)) {
		return undefined;
	}
	return ingestThread(wire, guildId);
}

export async function updateThread(thread: Channel, body: ThreadPatchBody): Promise<void> {
	assertActive(thread.guildId);
	const response = await http.patch<ChannelWire>(Endpoints.CHANNEL(thread.id), {body});
	ingestThread(response.body, thread.guildId);
}

export async function deleteThread(thread: Channel): Promise<void> {
	assertActive(thread.guildId);
	await http.delete(Endpoints.CHANNEL(thread.id));
}

export async function joinThread(thread: Channel): Promise<void> {
	assertActive(thread.guildId);
	await http.put(Endpoints.CHANNEL_THREAD_MEMBER(thread.id));
}

export async function leaveThread(thread: Channel): Promise<void> {
	assertActive(thread.guildId);
	await http.delete(Endpoints.CHANNEL_THREAD_MEMBER(thread.id));
	ThreadMemberships.remove(thread.id);
}

export async function removeThreadMember(thread: Channel, userId: string): Promise<void> {
	assertActive(thread.guildId);
	await http.delete(Endpoints.CHANNEL_THREAD_MEMBER(thread.id, userId));
}

export async function updateThreadMemberSettings(thread: Channel, body: ThreadMemberSettingsBody): Promise<void> {
	assertActive(thread.guildId);
	const response = await http.patch<ThreadMemberResponse | undefined>(
		Endpoints.CHANNEL_THREAD_MEMBER_SETTINGS(thread.id),
		{body},
	);
	if (response.body) {
		ThreadMemberships.set(thread.id, response.body);
	}
}

export async function fetchArchivedThreads(
	parent: Channel,
	kind: ArchivedThreadListKind,
	before?: string,
): Promise<ArchivedThreadPage> {
	assertActive(parent.guildId);
	const endpoint =
		kind === 'public'
			? Endpoints.CHANNEL_THREADS_ARCHIVED_PUBLIC(parent.id)
			: kind === 'private'
				? Endpoints.CHANNEL_THREADS_ARCHIVED_PRIVATE(parent.id)
				: Endpoints.CHANNEL_THREADS_JOINED_ARCHIVED_PRIVATE(parent.id);
	const response = await http.get<ThreadListResponse>(endpoint, {query: before ? {before} : {}});
	const body = response.body;
	const membersByThread = new Map<string, ThreadMemberResponse>();
	for (const member of body?.members ?? []) {
		if (member.id) membersByThread.set(member.id, member);
	}
	const threadIds: Array<string> = [];
	for (const thread of body?.threads ?? []) {
		const member = membersByThread.get(thread.id);
		if (ingestThread(member ? {...thread, member} : thread, parent.guildId)) {
			threadIds.push(thread.id);
		}
	}
	return {threadIds, hasMore: body?.has_more ?? false};
}

export async function ensureThreadLoaded(guildId: string, threadId: string): Promise<boolean> {
	if (Channels.getChannel(threadId)) return true;
	if (!ThreadGuilds.isActive(guildId)) return false;
	try {
		return (await fetchThread(guildId, threadId)) != null;
	} catch (error) {
		logger.warn(`Failed to load thread ${threadId}`, error);
		return false;
	}
}

const MAX_THREAD_LOOKUPS_PER_BATCH = 25;
const nonThreadChannelIds = new Set<string>();

export async function loadThreadsForMessages(
	messages: ReadonlyArray<{channel_id: string; guild_id?: string}>,
): Promise<void> {
	if (!ThreadGuilds.anyActive) return;
	const pending = new Set<string>();
	for (const message of messages) {
		if (pending.size >= MAX_THREAD_LOOKUPS_PER_BATCH) break;
		if (message.guild_id && !ThreadGuilds.isActive(message.guild_id)) continue;
		if (Channels.getChannel(message.channel_id)) continue;
		if (nonThreadChannelIds.has(message.channel_id)) continue;
		pending.add(message.channel_id);
	}
	await Promise.all(Array.from(pending, loadUnknownThread));
}

async function loadUnknownThread(channelId: string): Promise<void> {
	try {
		const response = await http.get<ChannelWire>(Endpoints.CHANNEL(channelId));
		const wire = response.body;
		if (!wire || !THREAD_CHANNEL_TYPES.has(wire.type) || !wire.guild_id) {
			nonThreadChannelIds.add(channelId);
			return;
		}
		if (!ThreadGuilds.isActive(wire.guild_id)) return;
		ingestThread(wire, wire.guild_id);
	} catch (error) {
		if (error instanceof HttpError && (error.status === 403 || error.status === 404)) {
			nonThreadChannelIds.add(channelId);
			return;
		}
		logger.warn(`Failed to load channel ${channelId}`, error);
	}
}

export function ingestSearchThreads(
	threads: ReadonlyArray<ChannelWire>,
	members: ReadonlyArray<ThreadMemberResponse>,
): void {
	const membersByThread = new Map<string, ThreadMemberResponse>();
	for (const member of members) {
		if (member.id) membersByThread.set(member.id, member);
	}
	for (const thread of threads) {
		if (!thread.guild_id || !ThreadGuilds.isActive(thread.guild_id)) continue;
		const member = membersByThread.get(thread.id);
		ingestThread(member ? {...thread, member} : thread, thread.guild_id);
	}
}

export type ThreadSearchResult =
	| {status: 'ok'; threadIds: ReadonlyArray<string>}
	| {status: 'indexing'; retryAfterSeconds: number | undefined};

export async function searchThreads(parent: Channel, name: string, archived: boolean): Promise<ThreadSearchResult> {
	const search = new URLSearchParams({
		name,
		archived: String(archived),
		sort_by: 'relevance',
		sort_order: 'desc',
		limit: String(THREAD_SEARCH_MAX_LIMIT),
	});
	const response = await http.get<
		{threads: Array<ChannelWire>; members: Array<ThreadMemberResponse>} | {retry_after?: number} | null
	>(`${Endpoints.CHANNEL_THREADS_SEARCH(parent.id)}?${search.toString()}`);
	if (response.status === 202) {
		return {status: 'indexing', retryAfterSeconds: (response.body as {retry_after?: number} | null)?.retry_after};
	}
	const body = response.body as {threads?: Array<ChannelWire>; members?: Array<ThreadMemberResponse>} | null;
	const threads = body?.threads ?? [];
	ingestSearchThreads(threads, body?.members ?? []);
	return {
		status: 'ok',
		threadIds: threads.filter((thread) => thread.parent_id === parent.id).map((thread) => thread.id),
	};
}

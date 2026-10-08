// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {Channel, ChannelWire} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {getDefaultLayout, getDefaultSortOrder, getDefaultTagSetting} from '@app/features/forum/utils/ForumChannelUtils';
import Permission from '@app/features/permissions/state/Permission';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	type ForumLayoutType,
	type ForumSortOrderType,
	ForumSortOrderTypes,
	type ForumTagSetting,
	POST_DATA_MAX_IDS,
	THREAD_SEARCH_MAX_LIMIT,
} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import type {Message as WireMessage} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {makeAutoObservable, observable, runInAction} from 'mobx';

const logger = new Logger('ForumPosts');

export const FORUM_SEARCH_DEBOUNCE_MS = 300;
export const POST_DATA_DEBOUNCE_MS = 50;
export const SEARCH_RETRY_MAX_ATTEMPTS = 6;
export const SEARCH_RETRY_MAX_DELAY_MS = 60_000;

export type ForumListKind = 'archived' | 'search';

export interface ForumListState {
	readonly ids: ReadonlyArray<string>;
	readonly hasMore: boolean;
	readonly loading: boolean;
	readonly loaded: boolean;
	readonly indexing: boolean;
	readonly failed: boolean;
}

interface ThreadSearchResponseBody {
	threads: Array<ChannelWire>;
	members: Array<ThreadMemberResponse>;
	has_more: boolean;
	total_results: number;
	first_messages?: Array<WireMessage>;
}

interface SearchNotReadyBody {
	code?: string;
	retry_after?: number;
}

interface PostDataResponseBody {
	threads?: Record<string, {first_message: WireMessage | null} | undefined>;
}

const EMPTY_LIST: ForumListState = Object.freeze({
	ids: [],
	hasMore: true,
	loading: false,
	loaded: false,
	indexing: false,
	failed: false,
});

const EMPTY_TAGS: ReadonlyArray<string> = Object.freeze([]);

interface ReactionEmojiKey {
	id?: string | null;
	name?: string | null;
}

type WireReaction = NonNullable<WireMessage['reactions']>[number];

function sameEmoji(a: ReactionEmojiKey, b: ReactionEmojiKey): boolean {
	return a.id != null ? a.id === b.id : b.id == null && a.name === b.name;
}

function applyReaction(
	reactions: ReadonlyArray<WireReaction>,
	emoji: ReactionEmojiKey,
	add: boolean,
	me: boolean,
): Array<WireReaction> | null {
	const existing = reactions.find((reaction) => sameEmoji(reaction.emoji, emoji));
	if (add) {
		if (!existing)
			return [
				...reactions,
				{emoji: {id: emoji.id ?? null, name: emoji.name ?? ''}, count: 1, me: me ? true : undefined},
			];
		if (me && existing.me) return null;
		return reactions.map((reaction) =>
			reaction === existing
				? {...reaction, count: reaction.count + 1, me: me || reaction.me ? true : undefined}
				: reaction,
		);
	}
	if (!existing || (me && !existing.me)) return null;
	if (existing.count <= 1) return reactions.filter((reaction) => reaction !== existing);
	return reactions.map((reaction) =>
		reaction === existing ? {...reaction, count: reaction.count - 1, me: me ? undefined : reaction.me} : reaction,
	);
}

function listKey(forumId: string, kind: ForumListKind): string {
	return `${forumId}:${kind}`;
}

export function canRequestForumPosts(forum: Channel): boolean {
	return (
		forum.isThreadOnly() &&
		ThreadGuilds.isActive(forum.guildId) &&
		Permission.can(Permissions.READ_MESSAGE_HISTORY, forum)
	);
}

export function searchRetryDelayMs(retryAfterSeconds: number | undefined, attempt: number): number {
	const base = Math.max(1, retryAfterSeconds ?? 1) * 1000;
	return Math.min(SEARCH_RETRY_MAX_DELAY_MS, base * 2 ** attempt);
}

function isSearchDisabledError(error: unknown): boolean {
	return (
		error instanceof HttpError &&
		error.status === 403 &&
		(error.body as {code?: string} | null)?.code === APIErrorCodes.FEATURE_TEMPORARILY_DISABLED
	);
}

function buildSearchPath(
	forumId: string,
	params: {
		name: string | null;
		tagIds: ReadonlyArray<string>;
		tagSetting: ForumTagSetting;
		archived: boolean | null;
		sortBy: 'last_message_time' | 'creation_time' | 'relevance';
		offset: number;
	},
): string {
	const search = new URLSearchParams();
	if (params.name) search.set('name', params.name);
	for (const tagId of params.tagIds) search.append('tag', tagId);
	if (params.tagIds.length > 1) search.set('tag_setting', params.tagSetting);
	if (params.archived != null) search.set('archived', String(params.archived));
	search.set('sort_by', params.sortBy);
	search.set('sort_order', 'desc');
	search.set('limit', String(THREAD_SEARCH_MAX_LIMIT));
	search.set('offset', String(params.offset));
	return `${Endpoints.CHANNEL_THREADS_SEARCH(forumId)}?${search.toString()}`;
}

class ForumPosts {
	private readonly lists = observable.map<string, ForumListState>();
	private readonly queries = observable.map<string, string>();
	private readonly tagFilters = observable.map<string, ReadonlyArray<string>>();
	private readonly sortOverrides = observable.map<string, ForumSortOrderType>();
	private readonly layoutOverrides = observable.map<string, ForumLayoutType>();
	private readonly firstMessages = observable.map<string, WireMessage | null>({}, {deep: false});
	private readonly generations = new Map<string, number>();
	private readonly retryTimers = new Map<string, ReturnType<typeof setTimeout>>();
	private readonly retryAttempts = new Map<string, number>();
	private readonly searchTimers = new Map<string, ReturnType<typeof setTimeout>>();
	private readonly requestedPostData = new Set<string>();
	private readonly pendingPostData = new Map<string, Set<string>>();
	private postDataTimer: ReturnType<typeof setTimeout> | null = null;
	searchUnavailable = false;

	constructor() {
		makeAutoObservable<
			this,
			| 'lists'
			| 'queries'
			| 'tagFilters'
			| 'sortOverrides'
			| 'layoutOverrides'
			| 'firstMessages'
			| 'generations'
			| 'retryTimers'
			| 'retryAttempts'
			| 'searchTimers'
			| 'requestedPostData'
			| 'pendingPostData'
			| 'postDataTimer'
		>(
			this,
			{
				lists: false,
				queries: false,
				tagFilters: false,
				sortOverrides: false,
				layoutOverrides: false,
				firstMessages: false,
				generations: false,
				retryTimers: false,
				retryAttempts: false,
				searchTimers: false,
				requestedPostData: false,
				pendingPostData: false,
				postDataTimer: false,
			},
			{autoBind: true},
		);
	}

	getList(forumId: string, kind: ForumListKind): ForumListState {
		return this.lists.get(listKey(forumId, kind)) ?? EMPTY_LIST;
	}

	getQuery(forumId: string): string {
		return this.queries.get(forumId) ?? '';
	}

	isSearching(forumId: string): boolean {
		return this.getQuery(forumId).trim().length > 0;
	}

	getTagFilter(forumId: string): ReadonlyArray<string> {
		return this.tagFilters.get(forumId) ?? EMPTY_TAGS;
	}

	getSortOrder(forum: Channel): ForumSortOrderType {
		return this.sortOverrides.get(forum.id) ?? getDefaultSortOrder(forum);
	}

	getLayout(forum: Channel): ForumLayoutType {
		return this.layoutOverrides.get(forum.id) ?? getDefaultLayout(forum);
	}

	getFirstMessage(threadId: string): WireMessage | null | undefined {
		return this.firstMessages.get(threadId);
	}

	setLayout(forum: Channel, layout: ForumLayoutType): void {
		this.layoutOverrides.set(forum.id, layout);
	}

	setSortOrder(forum: Channel, sortOrder: ForumSortOrderType): void {
		if (this.getSortOrder(forum) === sortOrder) return;
		this.sortOverrides.set(forum.id, sortOrder);
		this.resetList(forum.id, 'archived');
	}

	setTagFilter(forum: Channel, tagIds: ReadonlyArray<string>): void {
		this.tagFilters.set(forum.id, [...tagIds]);
		this.resetList(forum.id, 'archived');
		if (this.isSearching(forum.id)) {
			this.cancelSearchTimer(forum.id);
			void this.fetchList(forum, 'search', false);
		}
	}

	toggleTag(forum: Channel, tagId: string): void {
		const current = this.getTagFilter(forum.id);
		this.setTagFilter(forum, current.includes(tagId) ? current.filter((id) => id !== tagId) : [...current, tagId]);
	}

	setQuery(forum: Channel, query: string): void {
		this.queries.set(forum.id, query);
		this.cancelSearchTimer(forum.id);
		if (!query.trim()) {
			this.resetList(forum.id, 'search');
			return;
		}
		const forumId = forum.id;
		this.searchTimers.set(
			forumId,
			setTimeout(() => {
				this.searchTimers.delete(forumId);
				const current = Channels.getChannel(forumId);
				if (current) void this.fetchList(current, 'search', false);
			}, FORUM_SEARCH_DEBOUNCE_MS),
		);
	}

	loadMore(forum: Channel, kind: ForumListKind): void {
		const list = this.getList(forum.id, kind);
		void this.fetchList(forum, kind, list.loaded);
	}

	retry(forum: Channel, kind: ForumListKind): void {
		this.retryAttempts.delete(listKey(forum.id, kind));
		const list = this.getList(forum.id, kind);
		void this.fetchList(forum, kind, list.loaded && list.ids.length > 0);
	}

	requestPostData(forum: Channel, threadIds: ReadonlyArray<string>): void {
		if (!canRequestForumPosts(forum)) return;
		let pending = this.pendingPostData.get(forum.id);
		for (const threadId of threadIds) {
			if (this.requestedPostData.has(threadId) || this.firstMessages.has(threadId)) continue;
			this.requestedPostData.add(threadId);
			pending ??= new Set();
			pending.add(threadId);
		}
		if (pending == null || pending.size === 0) return;
		this.pendingPostData.set(forum.id, pending);
		if (this.postDataTimer == null) {
			this.postDataTimer = setTimeout(this.flushPostData, POST_DATA_DEBOUNCE_MS);
		}
	}

	setFirstMessage(message: WireMessage): void {
		this.firstMessages.set(message.channel_id, message);
	}

	handleMessageUpdate(message: Partial<WireMessage> & {id: string}): void {
		if (message.id !== message.channel_id) return;
		const existing = this.firstMessages.get(message.id);
		if (existing) this.firstMessages.set(message.id, {...existing, ...message});
	}

	handleMessageDelete(channelId: string, messageId: string): void {
		if (messageId !== channelId || !this.firstMessages.has(messageId)) return;
		this.firstMessages.set(messageId, null);
	}

	handleReaction(channelId: string, messageId: string, emoji: ReactionEmojiKey, add: boolean, me: boolean): void {
		if (messageId !== channelId) return;
		const existing = this.firstMessages.get(messageId);
		if (!existing) return;
		const reactions = applyReaction(existing.reactions ?? [], emoji, add, me);
		if (reactions) this.firstMessages.set(messageId, {...existing, reactions});
	}

	handleReactionClear(channelId: string, messageId: string, emoji?: ReactionEmojiKey): void {
		if (messageId !== channelId) return;
		const existing = this.firstMessages.get(messageId);
		if (!existing?.reactions?.length) return;
		const reactions = emoji ? existing.reactions.filter((reaction) => !sameEmoji(reaction.emoji, emoji)) : [];
		this.firstMessages.set(messageId, {...existing, reactions});
	}

	handleGatewayReady(): void {
		const sortOverrides = new Map(this.sortOverrides);
		const layoutOverrides = new Map(this.layoutOverrides);
		this.reset();
		this.sortOverrides.replace(sortOverrides);
		this.layoutOverrides.replace(layoutOverrides);
	}

	purgeForum(forumId: string, threadIds: ReadonlyArray<string>): void {
		const postIds = new Set([...threadIds, ...(this.pendingPostData.get(forumId) ?? [])]);
		for (const kind of ['archived', 'search'] as const) {
			for (const id of this.getList(forumId, kind).ids) postIds.add(id);
			this.resetList(forumId, kind);
		}
		this.cancelSearchTimer(forumId);
		this.queries.delete(forumId);
		this.tagFilters.delete(forumId);
		this.pendingPostData.delete(forumId);
		for (const id of postIds) {
			this.requestedPostData.delete(id);
			this.firstMessages.delete(id);
		}
	}

	reset(): void {
		for (const timer of this.retryTimers.values()) clearTimeout(timer);
		for (const timer of this.searchTimers.values()) clearTimeout(timer);
		if (this.postDataTimer != null) clearTimeout(this.postDataTimer);
		this.postDataTimer = null;
		this.retryTimers.clear();
		this.searchTimers.clear();
		this.retryAttempts.clear();
		this.generations.clear();
		this.requestedPostData.clear();
		this.pendingPostData.clear();
		this.lists.clear();
		this.queries.clear();
		this.tagFilters.clear();
		this.sortOverrides.clear();
		this.layoutOverrides.clear();
		this.firstMessages.clear();
		this.searchUnavailable = false;
	}

	private cancelSearchTimer(forumId: string): void {
		const timer = this.searchTimers.get(forumId);
		if (timer != null) clearTimeout(timer);
		this.searchTimers.delete(forumId);
	}

	private cancelRetry(key: string): void {
		const timer = this.retryTimers.get(key);
		if (timer != null) clearTimeout(timer);
		this.retryTimers.delete(key);
	}

	private resetList(forumId: string, kind: ForumListKind): void {
		const key = listKey(forumId, kind);
		this.generations.set(key, (this.generations.get(key) ?? 0) + 1);
		this.cancelRetry(key);
		this.retryAttempts.delete(key);
		this.lists.delete(key);
	}

	private async fetchList(forum: Channel, kind: ForumListKind, append: boolean): Promise<void> {
		if (!canRequestForumPosts(forum)) return;
		const key = listKey(forum.id, kind);
		const current = this.getList(forum.id, kind);
		if (append && (current.loading || !current.hasMore)) return;
		const query = this.getQuery(forum.id).trim();
		if (kind === 'search' && !query) return;
		const generation = (this.generations.get(key) ?? 0) + 1;
		this.generations.set(key, generation);
		this.cancelRetry(key);
		const base = append ? current : EMPTY_LIST;
		this.lists.set(key, {...base, loading: true, failed: false});
		if (kind === 'archived' && this.searchUnavailable) {
			await this.fetchArchivedFallback(forum, append, base, generation);
			return;
		}
		const sortOrder = this.getSortOrder(forum);
		const path = buildSearchPath(forum.id, {
			name: kind === 'search' ? query : null,
			tagIds: this.getTagFilter(forum.id),
			tagSetting: getDefaultTagSetting(forum),
			archived: kind === 'archived' ? true : null,
			sortBy:
				kind === 'search'
					? 'relevance'
					: sortOrder === ForumSortOrderTypes.CREATION_TIME
						? 'creation_time'
						: 'last_message_time',
			offset: append ? current.ids.length : 0,
		});
		try {
			const response = await http.get<ThreadSearchResponseBody | SearchNotReadyBody>(path);
			if (this.generations.get(key) !== generation) return;
			if (response.status === 202) {
				this.scheduleRetry(forum.id, kind, append, base, (response.body as SearchNotReadyBody | null)?.retry_after);
				return;
			}
			const body = response.body as ThreadSearchResponseBody;
			runInAction(() => {
				this.retryAttempts.delete(key);
				const ids = this.ingestSearch(forum, body);
				const merged = append ? [...current.ids, ...ids.filter((id) => !current.ids.includes(id))] : ids;
				this.lists.set(key, {
					ids: merged,
					hasMore: body.has_more,
					loading: false,
					loaded: true,
					indexing: false,
					failed: false,
				});
			});
		} catch (error) {
			if (this.generations.get(key) !== generation) return;
			if (isSearchDisabledError(error)) {
				this.disableSearch();
				if (kind === 'archived') await this.fetchArchivedFallback(forum, append, base, generation);
				return;
			}
			logger.warn(`Failed to load ${kind} posts for forum ${forum.id}`, error);
			runInAction(() => {
				this.lists.set(key, {...base, loading: false, indexing: false, failed: true});
			});
		}
	}

	private disableSearch(): void {
		this.searchUnavailable = true;
		for (const timer of this.searchTimers.values()) clearTimeout(timer);
		this.searchTimers.clear();
		for (const id of Array.from(this.queries.keys())) this.resetList(id, 'search');
		this.queries.clear();
	}

	private async fetchArchivedFallback(
		forum: Channel,
		append: boolean,
		base: ForumListState,
		generation: number,
	): Promise<void> {
		const key = listKey(forum.id, 'archived');
		const lastId = append ? base.ids[base.ids.length - 1] : undefined;
		const before = lastId ? Channels.getChannel(lastId)?.threadMetadata?.archive_timestamp : undefined;
		const query: Record<string, string> = {limit: String(THREAD_SEARCH_MAX_LIMIT)};
		if (before) query.before = before;
		try {
			const response = await http.get<ThreadSearchResponseBody>(Endpoints.CHANNEL_THREADS_ARCHIVED_PUBLIC(forum.id), {
				query,
			});
			if (this.generations.get(key) !== generation) return;
			const body = response.body;
			runInAction(() => {
				const ids = body ? this.ingestSearch(forum, body) : [];
				this.lists.set(key, {
					ids: append ? [...base.ids, ...ids.filter((id) => !base.ids.includes(id))] : ids,
					hasMore: body?.has_more ?? false,
					loading: false,
					loaded: true,
					indexing: false,
					failed: false,
				});
			});
		} catch (error) {
			if (this.generations.get(key) !== generation) return;
			logger.warn(`Failed to load archived posts for forum ${forum.id}`, error);
			runInAction(() => {
				this.lists.set(key, {...base, loading: false, indexing: false, failed: true});
			});
		}
	}

	private scheduleRetry(
		forumId: string,
		kind: ForumListKind,
		append: boolean,
		base: ForumListState,
		retryAfterSeconds: number | undefined,
	): void {
		const key = listKey(forumId, kind);
		const attempt = this.retryAttempts.get(key) ?? 0;
		runInAction(() => {
			if (attempt >= SEARCH_RETRY_MAX_ATTEMPTS) {
				this.retryAttempts.delete(key);
				this.lists.set(key, {...base, loading: false, indexing: false, failed: true});
				return;
			}
			this.retryAttempts.set(key, attempt + 1);
			this.lists.set(key, {...base, loading: false, indexing: true, failed: false});
		});
		if (attempt >= SEARCH_RETRY_MAX_ATTEMPTS) return;
		const generation = this.generations.get(key);
		this.retryTimers.set(
			key,
			setTimeout(
				() => {
					this.retryTimers.delete(key);
					if (this.generations.get(key) !== generation) return;
					const forum = Channels.getChannel(forumId);
					if (forum) void this.fetchList(forum, kind, append);
				},
				searchRetryDelayMs(retryAfterSeconds, attempt),
			),
		);
	}

	private ingestSearch(forum: Channel, body: ThreadSearchResponseBody): Array<string> {
		const members = new Map<string, ThreadMemberResponse>();
		for (const member of body.members ?? []) {
			if (member.id) members.set(member.id, member);
		}
		const ids: Array<string> = [];
		for (const thread of body.threads ?? []) {
			const member = members.get(thread.id);
			const post = ChannelThreads.upsert(member ? {...thread, member} : thread, forum.guildId);
			if (post?.parentId === forum.id) ids.push(post.id);
		}
		for (const message of body.first_messages ?? []) {
			this.firstMessages.set(message.channel_id, message);
		}
		return ids;
	}

	private flushPostData(): void {
		this.postDataTimer = null;
		const batches = Array.from(this.pendingPostData);
		this.pendingPostData.clear();
		for (const [forumId, pending] of batches) {
			const forum = Channels.getChannel(forumId);
			const ids = Array.from(pending);
			if (!forum || !canRequestForumPosts(forum)) {
				for (const id of ids) this.requestedPostData.delete(id);
				continue;
			}
			for (let index = 0; index < ids.length; index += POST_DATA_MAX_IDS) {
				void this.fetchPostData(forum, ids.slice(index, index + POST_DATA_MAX_IDS));
			}
		}
	}

	private async fetchPostData(forum: Channel, threadIds: Array<string>): Promise<void> {
		try {
			const response = await http.post<PostDataResponseBody>(Endpoints.CHANNEL_POST_DATA(forum.id), {
				body: {thread_ids: threadIds},
			});
			runInAction(() => {
				for (const threadId of threadIds) {
					if (!this.requestedPostData.has(threadId)) continue;
					this.firstMessages.set(threadId, response.body?.threads?.[threadId]?.first_message ?? null);
				}
			});
		} catch (error) {
			for (const threadId of threadIds) this.requestedPostData.delete(threadId);
			logger.warn(`Failed to load post data for forum ${forum.id}`, error);
		}
	}
}

export default new ForumPosts();

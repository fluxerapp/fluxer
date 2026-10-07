// SPDX-License-Identifier: AGPL-3.0-or-later

import {Channel, type ChannelWire} from '@app/features/channel/models/Channel';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const channels = new Map<string, Channel>();
const activeGuilds = new Set<string>();
const readableChannels = new Set<string>();

vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {localInstanceDomain: 'fluxer.test'}}));
vi.mock('@app/features/user/state/Users', () => ({default: {getUser: () => undefined, cacheUsers: () => {}}}));
vi.mock('@app/features/channel/state/Channels', () => ({
	default: {getChannel: (id: string) => channels.get(id)},
}));
vi.mock('@app/features/threads/state/ThreadGuilds', () => ({
	default: {isActive: (guildId: string | null | undefined) => guildId != null && activeGuilds.has(guildId)},
}));
vi.mock('@app/features/threads/state/ChannelThreads', () => ({
	default: {
		upsert: (wire: ChannelWire, guildId?: string) => {
			const channel = new Channel({...wire, guild_id: wire.guild_id ?? guildId});
			channels.set(channel.id, channel);
			return channel;
		},
	},
}));
vi.mock('@app/features/permissions/state/Permission', () => ({
	default: {
		can: (permission: bigint, channel: {id: string}) =>
			permission === Permissions.READ_MESSAGE_HISTORY && readableChannels.has(channel.id),
	},
}));
vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: vi.fn(), post: vi.fn()},
}));

const {
	default: ForumPosts,
	FORUM_SEARCH_DEBOUNCE_MS,
	POST_DATA_DEBOUNCE_MS,
	searchRetryDelayMs,
} = await import('@app/features/forum/state/ForumPosts');
const {http} = await import('@app/features/platform/transport/RestTransport');

const GUILD = '1500000000000000001';
const FORUM = '1500000000000000002';
const POST = '1500000000000000003';

function seedForum(): Channel {
	const forum = new Channel({id: FORUM, type: ChannelTypes.GUILD_FORUM, guild_id: GUILD, name: 'forum'});
	channels.set(FORUM, forum);
	return forum;
}

function searchBody(threadIds: Array<string>) {
	return {
		threads: threadIds.map((id) => ({id, type: ChannelTypes.PUBLIC_THREAD, guild_id: GUILD, parent_id: FORUM})),
		members: [],
		has_more: false,
		total_results: threadIds.length,
		first_messages: [],
	};
}

function requestedPath(call: number): URL {
	const path = vi.mocked(http.get).mock.calls[call][0] as string;
	return new URL(path, 'https://fluxer.test');
}

async function flush(): Promise<void> {
	await vi.advanceTimersByTimeAsync(0);
}

beforeEach(() => {
	vi.useFakeTimers();
	channels.clear();
	activeGuilds.clear();
	readableChannels.clear();
	activeGuilds.add(GUILD);
	readableChannels.add(FORUM);
	vi.mocked(http.get).mockReset();
	vi.mocked(http.post).mockReset();
	ForumPosts.reset();
});

afterEach(() => {
	ForumPosts.reset();
	vi.useRealTimers();
});

describe('ForumPosts search debounce', () => {
	it('sends one search after the user stops typing', async () => {
		const forum = seedForum();
		vi.mocked(http.get).mockResolvedValue({ok: true, status: 200, headers: {}, body: searchBody([POST])});
		ForumPosts.setQuery(forum, 'he');
		await vi.advanceTimersByTimeAsync(FORUM_SEARCH_DEBOUNCE_MS - 1);
		ForumPosts.setQuery(forum, 'hello');
		await vi.advanceTimersByTimeAsync(FORUM_SEARCH_DEBOUNCE_MS - 1);
		expect(http.get).not.toHaveBeenCalled();
		await vi.advanceTimersByTimeAsync(1);
		expect(http.get).toHaveBeenCalledTimes(1);
		expect(requestedPath(0).pathname).toBe(`/channels/${FORUM}/threads/search`);
		expect(requestedPath(0).searchParams.get('name')).toBe('hello');
		expect(ForumPosts.getList(FORUM, 'search').ids).toEqual([POST]);
	});

	it('cancels a pending search when the query is cleared', async () => {
		const forum = seedForum();
		ForumPosts.setQuery(forum, 'hello');
		ForumPosts.setQuery(forum, '');
		await vi.advanceTimersByTimeAsync(FORUM_SEARCH_DEBOUNCE_MS * 2);
		expect(http.get).not.toHaveBeenCalled();
		expect(ForumPosts.isSearching(FORUM)).toBe(false);
	});

	it('sends every selected tag and the tag setting', async () => {
		const forum = seedForum();
		vi.mocked(http.get).mockResolvedValue({ok: true, status: 200, headers: {}, body: searchBody([])});
		ForumPosts.setTagFilter(forum, ['11', '12']);
		ForumPosts.loadMore(forum, 'archived');
		await flush();
		const url = requestedPath(0);
		expect(url.searchParams.getAll('tag')).toEqual(['11', '12']);
		expect(url.searchParams.get('tag_setting')).toBe('match_some');
		expect(url.searchParams.get('archived')).toBe('true');
	});
});

describe('ForumPosts gate', () => {
	it('sends nothing for a guild outside the experiment', async () => {
		const forum = seedForum();
		activeGuilds.clear();
		ForumPosts.setQuery(forum, 'hello');
		ForumPosts.loadMore(forum, 'archived');
		ForumPosts.requestPostData(forum, [POST]);
		await vi.advanceTimersByTimeAsync(FORUM_SEARCH_DEBOUNCE_MS + POST_DATA_DEBOUNCE_MS);
		expect(http.get).not.toHaveBeenCalled();
		expect(http.post).not.toHaveBeenCalled();
	});

	it('sends nothing for a channel that is not a forum', async () => {
		const text = new Channel({id: FORUM, type: ChannelTypes.GUILD_TEXT, guild_id: GUILD, name: 'text'});
		channels.set(FORUM, text);
		ForumPosts.loadMore(text, 'archived');
		ForumPosts.requestPostData(text, [POST]);
		await vi.advanceTimersByTimeAsync(POST_DATA_DEBOUNCE_MS);
		expect(http.get).not.toHaveBeenCalled();
		expect(http.post).not.toHaveBeenCalled();
	});

	it('sends nothing without read message history', async () => {
		const forum = seedForum();
		readableChannels.clear();
		ForumPosts.setQuery(forum, 'hello');
		ForumPosts.loadMore(forum, 'archived');
		ForumPosts.requestPostData(forum, [POST]);
		await vi.advanceTimersByTimeAsync(FORUM_SEARCH_DEBOUNCE_MS + POST_DATA_DEBOUNCE_MS);
		expect(http.get).not.toHaveBeenCalled();
		expect(http.post).not.toHaveBeenCalled();
	});
});

describe('ForumPosts index backoff', () => {
	it('computes a doubling delay with a ceiling', () => {
		expect(searchRetryDelayMs(2, 0)).toBe(2000);
		expect(searchRetryDelayMs(2, 1)).toBe(4000);
		expect(searchRetryDelayMs(undefined, 0)).toBe(1000);
		expect(searchRetryDelayMs(2, 10)).toBe(60_000);
	});

	it('retries a 202 after retry_after and doubles the wait', async () => {
		const forum = seedForum();
		const notReady = {ok: true, status: 202, headers: {}, body: {code: 'SEARCH_INDEX_NOT_READY', retry_after: 2}};
		vi.mocked(http.get)
			.mockResolvedValueOnce(notReady)
			.mockResolvedValueOnce(notReady)
			.mockResolvedValueOnce({ok: true, status: 200, headers: {}, body: searchBody([POST])});
		ForumPosts.loadMore(forum, 'archived');
		await flush();
		expect(http.get).toHaveBeenCalledTimes(1);
		expect(ForumPosts.getList(FORUM, 'archived').indexing).toBe(true);
		await vi.advanceTimersByTimeAsync(1999);
		expect(http.get).toHaveBeenCalledTimes(1);
		await vi.advanceTimersByTimeAsync(1);
		expect(http.get).toHaveBeenCalledTimes(2);
		await vi.advanceTimersByTimeAsync(3999);
		expect(http.get).toHaveBeenCalledTimes(2);
		await vi.advanceTimersByTimeAsync(1);
		expect(http.get).toHaveBeenCalledTimes(3);
		const list = ForumPosts.getList(FORUM, 'archived');
		expect(list.indexing).toBe(false);
		expect(list.ids).toEqual([POST]);
	});

	it('gives up with a failed state after too many 202 answers', async () => {
		const forum = seedForum();
		vi.mocked(http.get).mockResolvedValue({ok: true, status: 202, headers: {}, body: {retry_after: 1}});
		ForumPosts.loadMore(forum, 'archived');
		await vi.advanceTimersByTimeAsync(10 * 60_000);
		const list = ForumPosts.getList(FORUM, 'archived');
		expect(list.failed).toBe(true);
		expect(list.indexing).toBe(false);
		const calls = vi.mocked(http.get).mock.calls.length;
		await vi.advanceTimersByTimeAsync(10 * 60_000);
		expect(http.get).toHaveBeenCalledTimes(calls);
	});

	it('drops a retry when the filter changes', async () => {
		const forum = seedForum();
		vi.mocked(http.get).mockResolvedValue({ok: true, status: 202, headers: {}, body: {retry_after: 1}});
		ForumPosts.loadMore(forum, 'archived');
		await flush();
		ForumPosts.setTagFilter(forum, ['11']);
		await vi.advanceTimersByTimeAsync(60_000);
		expect(http.get).toHaveBeenCalledTimes(1);
	});
});

describe('ForumPosts post data', () => {
	it('batches ids within the debounce and never asks twice', async () => {
		const forum = seedForum();
		vi.mocked(http.post).mockResolvedValue({
			ok: true,
			status: 200,
			headers: {},
			body: {threads: {[POST]: {owner: null, first_message: null}}},
		});
		ForumPosts.requestPostData(forum, [POST]);
		ForumPosts.requestPostData(forum, [POST, '1500000000000000004']);
		await vi.advanceTimersByTimeAsync(POST_DATA_DEBOUNCE_MS);
		expect(http.post).toHaveBeenCalledTimes(1);
		expect(vi.mocked(http.post).mock.calls[0][1]).toEqual({body: {thread_ids: [POST, '1500000000000000004']}});
		expect(ForumPosts.getFirstMessage(POST)).toBeNull();
		ForumPosts.requestPostData(forum, [POST]);
		await vi.advanceTimersByTimeAsync(POST_DATA_DEBOUNCE_MS);
		expect(http.post).toHaveBeenCalledTimes(1);
	});
});

describe('ForumPosts without a search provider', () => {
	it('pages archived posts through the archived list and turns search off', async () => {
		const forum = seedForum();
		const older = '1500000000000000004';
		const archived = (id: string, stamp: string) => ({
			id,
			type: ChannelTypes.PUBLIC_THREAD,
			guild_id: GUILD,
			parent_id: FORUM,
			thread_metadata: {archived: true, archive_timestamp: stamp, auto_archive_duration: 4320, locked: false},
		});
		vi.mocked(http.get)
			.mockRejectedValueOnce(
				new HttpError({
					method: 'GET',
					path: `/channels/${FORUM}/threads/search`,
					status: 403,
					body: {code: APIErrorCodes.FEATURE_TEMPORARILY_DISABLED},
				}),
			)
			.mockResolvedValueOnce({
				ok: true,
				status: 200,
				headers: {},
				body: {threads: [archived(POST, '2026-09-28T00:00:00.000Z')], members: [], has_more: true},
			})
			.mockResolvedValueOnce({
				ok: true,
				status: 200,
				headers: {},
				body: {threads: [archived(older, '2026-09-27T00:00:00.000Z')], members: [], has_more: false},
			});
		ForumPosts.loadMore(forum, 'archived');
		await flush();
		expect(ForumPosts.searchUnavailable).toBe(true);
		expect(vi.mocked(http.get).mock.calls[1][0]).toBe(`/channels/${FORUM}/threads/archived/public`);
		expect(ForumPosts.getList(FORUM, 'archived')).toMatchObject({ids: [POST], hasMore: true, failed: false});
		ForumPosts.loadMore(forum, 'archived');
		await flush();
		expect(http.get).toHaveBeenCalledTimes(3);
		expect(vi.mocked(http.get).mock.calls[2][1]).toEqual({
			query: {limit: '25', before: '2026-09-28T00:00:00.000Z'},
		});
		expect(ForumPosts.getList(FORUM, 'archived')).toMatchObject({ids: [POST, older], hasMore: false});
	});
});

describe('ForumPosts first message sync', () => {
	function seedFirstMessage() {
		ForumPosts.setFirstMessage({
			id: POST,
			channel_id: POST,
			content: 'hello',
			reactions: [{emoji: {id: null, name: '👍'}, count: 1}],
		} as never);
	}

	it('follows edits, deletes and reactions on the starter only', () => {
		seedFirstMessage();
		ForumPosts.handleReaction(POST, POST, {id: null, name: '👍'}, true, true);
		expect(ForumPosts.getFirstMessage(POST)?.reactions).toEqual([{emoji: {id: null, name: '👍'}, count: 2, me: true}]);
		ForumPosts.handleReaction(POST, POST, {id: null, name: '👍'}, true, true);
		expect(ForumPosts.getFirstMessage(POST)?.reactions?.[0].count).toBe(2);
		ForumPosts.handleReaction(POST, POST, {id: null, name: '👍'}, false, false);
		expect(ForumPosts.getFirstMessage(POST)?.reactions).toEqual([{emoji: {id: null, name: '👍'}, count: 1, me: true}]);
		ForumPosts.handleMessageUpdate({id: '1500000000000000009', channel_id: POST, content: 'reply'});
		ForumPosts.handleMessageUpdate({id: POST, channel_id: POST, content: 'edited'});
		expect(ForumPosts.getFirstMessage(POST)?.content).toBe('edited');
		ForumPosts.handleMessageDelete(POST, POST);
		expect(ForumPosts.getFirstMessage(POST)).toBeNull();
	});

	it('forgets the forum lists on READY', async () => {
		const forum = seedForum();
		seedFirstMessage();
		vi.mocked(http.get).mockResolvedValue({ok: true, status: 200, headers: {}, body: searchBody([POST])});
		ForumPosts.loadMore(forum, 'archived');
		await flush();
		ForumPosts.handleGatewayReady();
		expect(ForumPosts.getList(FORUM, 'archived').loaded).toBe(false);
		expect(ForumPosts.getFirstMessage(POST)).toBeUndefined();
	});
});

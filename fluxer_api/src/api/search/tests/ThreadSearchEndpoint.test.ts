// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {getSnowflakeService} from '@app/api/middleware/ServiceRegistry';
import {getMessageSearchService, getThreadSearchService} from '@app/api/SearchFactory';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import refreshSearchIndex from '@app/api/worker/tasks/RefreshSearchIndex';
import {backfillThreadSearch, syncThreadSearchDocument} from '@app/api/worker/tasks/ThreadSearchTasks';
import {clearWorkerDependencies, setWorkerDependencies} from '@app/api/worker/WorkerContext';
import {initializeWorkerDependencies} from '@app/api/worker/WorkerDependencies';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {
	StartForumThreadResponse,
	ThreadSearchResponse,
} from '@fluxer/schema/src/domains/channel/ForumRequestSchemas';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import {createSnowflake, MAX_WORKER_ID, snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {WorkerTaskHandler, WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const HANDLERS: Record<string, WorkerTaskHandler> = {
	syncThreadSearchDocument,
	backfillThreadSearch,
	refreshSearchIndex,
};

function helpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 1n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
		attempt: {isLastAttempt: true},
	};
}

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	guildId: string;
	forumId: string;
	textId: string;
}

describe('thread search', () => {
	let harness: ApiTestHarness;
	let pending: Array<Promise<unknown>>;

	beforeAll(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
		setWorkerDependencies(await initializeWorkerDependencies(getSnowflakeService()));
		pending = [];
		vi.spyOn(NoopWorkerService.prototype, 'addJob').mockImplementation((async (
			task: string,
			payload: Record<string, unknown>,
		) => {
			const handler = HANDLERS[task];
			if (handler) pending.push(handler(payload, helpers()));
			return 0n;
		}) as () => Promise<bigint>);
		await getThreadSearchService()!.deleteAllDocuments();
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		clearWorkerDependencies();
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function settle(): Promise<void> {
		while (pending.length > 0) await Promise.all(pending.splice(0));
	}

	async function setup(): Promise<Setup> {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		await ensureSessionStarted(harness, member.token);
		const guild = await createGuild(harness, owner.token, 'search');
		const text = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, text.id);
		await acceptInvite(harness, member.token, invite.code);
		const forum = await threadsRequest<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'forum', type: ChannelTypes.GUILD_FORUM, available_tags: [{name: 'a'}, {name: 'b'}]})
			.execute();
		return {owner, member, guildId: guild.id, forumId: forum.id, textId: text.id};
	}

	async function post(s: Setup, name: string, appliedTags: Array<string> = []): Promise<StartForumThreadResponse> {
		return threadsRequest<StartForumThreadResponse>(harness, s.member.token)
			.post(`/channels/${s.forumId}/threads`)
			.body({name, applied_tags: appliedTags, message: {content: `${name} body`}})
			.expect(201)
			.execute();
	}

	async function search(s: Setup, channelId: string, query = ''): Promise<ThreadSearchResponse> {
		return threadsRequest<ThreadSearchResponse>(harness, s.member.token)
			.get(`/channels/${channelId}/threads/search${query}`)
			.execute();
	}

	it('answers 202 until the guild is backfilled, then searches', async () => {
		const s = await setup();
		const created = await post(s, 'first post');
		await settle();
		const response = await harness.requestJson({
			path: `/channels/${s.forumId}/threads/search`,
			headers: {Authorization: s.member.token, 'X-Fluxer-Features': 'channel_threads'},
		});
		expect(response.status).toBe(202);
		expect(await response.json()).toMatchObject({
			code: APIErrorCodes.SEARCH_INDEX_NOT_READY,
			documents_indexed: 0,
			retry_after: 2,
		});
		await settle();
		const result = await search(s, s.forumId);
		expect(result.threads.map((thread) => thread.id)).toEqual([created.id]);
		expect(result.total_results).toBe(1);
		expect(result.has_more).toBe(false);
		expect(result.first_messages?.map((message) => message.id)).toEqual([created.id]);
		expect(result.members.map((member) => member.id)).toEqual([created.id]);
	});

	async function refreshThreadsIndex(
		guildId: string,
		status: number,
		code?: string,
	): Promise<{errors?: Array<{path: string}>}> {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup']);
		return createBuilder<{errors?: Array<{path: string}>}>(harness, admin.token)
			.post('/admin/search/indexes/threads/refreshes')
			.body({guild_id: guildId})
			.expect(status, code)
			.execute();
	}

	it('rebuilds a dropped index through the admin threads reindex', async () => {
		const s = await setup();
		const created = await post(s, 'kept post');
		await search(s, s.forumId).catch(() => null);
		await settle();
		await getThreadSearchService()!.deleteAllDocuments();
		expect((await search(s, s.forumId)).threads).toEqual([]);
		await refreshThreadsIndex(s.guildId, HTTP_STATUS.OK);
		await settle();
		expect((await search(s, s.forumId)).threads.map((thread) => thread.id)).toEqual([created.id]);
	});

	it('rejects the threads reindex type while the experiment is off', async () => {
		const s = await setup();
		await settle();
		resetChannelThreadsConfig();
		const response = await refreshThreadsIndex(s.guildId, HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY');
		expect(response.errors?.[0]?.path).toBe('index_name');
		expect(pending).toEqual([]);
	});

	it('keeps thread message documents when a message reindex runs while the guild is inactive', async () => {
		const s = await setup();
		const thread = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
			.post(`/channels/${s.textId}/threads`)
			.body({name: 'kept', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
		await settle();
		await setChannelThreadsConfig({enabled: false});
		const deleted = vi.spyOn(getMessageSearchService()!, 'deleteChannelMessages');
		const queued: Array<string> = [];
		await refreshSearchIndex(
			{index_type: 'channel_messages', job_id: '1', admin_user_id: s.owner.userId, guild_id: s.guildId},
			{
				...helpers(),
				addJob: async (_task: string, payload: Record<string, unknown>) => {
					queued.push(String(payload.channelId));
					return 0n;
				},
			} as WorkerTaskHelpers,
		);
		const deletedIds = deleted.mock.calls.map(([channelId]) => channelId.toString());
		expect(deletedIds).toContain(s.textId);
		expect(deletedIds).not.toContain(thread.id);
		expect(deletedIds.sort()).toEqual(queued.sort());
	});

	it('filters by name, tags and archive state and pages results', async () => {
		const s = await setup();
		const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token).get(`/channels/${s.forumId}`).execute();
		const [a, b] = forum.available_tags!;
		const apple = await post(s, 'apple pie', [a!.id]);
		const banana = await post(s, 'banana bread', [a!.id, b!.id]);
		const cherry = await post(s, 'cherry tart', [b!.id]);
		await threadsRequest(harness, s.owner.token).patch(`/channels/${cherry.id}`).body({archived: true}).execute();
		await search(s, s.forumId).catch(() => null);
		await settle();
		expect((await search(s, s.forumId, '?name=banana')).threads.map((thread) => thread.id)).toEqual([banana.id]);
		const some = await search(s, s.forumId, `?tag=${a!.id}&tag=${b!.id}&sort_by=creation_time&sort_order=asc`);
		expect(some.threads.map((thread) => thread.id)).toEqual([apple.id, banana.id, cherry.id]);
		const all = await search(s, s.forumId, `?tag=${a!.id}&tag=${b!.id}&tag_setting=match_all`);
		expect(all.threads.map((thread) => thread.id)).toEqual([banana.id]);
		const archived = await search(s, s.forumId, '?archived=true');
		expect(archived.threads.map((thread) => thread.id)).toEqual([cherry.id]);
		const page = await search(s, s.forumId, '?sort_by=creation_time&limit=2');
		expect(page.threads.map((thread) => thread.id)).toEqual([cherry.id, banana.id]);
		expect(page.has_more).toBe(true);
		expect(page.total_results).toBe(3);
	});

	it('ignores deleted tags in results and filters', async () => {
		const s = await setup();
		const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token).get(`/channels/${s.forumId}`).execute();
		const [a, b] = forum.available_tags!;
		const tagged = await post(s, 'tagged', [a!.id, b!.id]);
		await search(s, s.forumId).catch(() => null);
		await settle();
		await threadsRequest(harness, s.owner.token).delete(`/channels/${s.forumId}/tags/${a!.id}`).execute();
		const all = await search(s, s.forumId);
		expect(all.threads.map((thread) => thread.applied_tags)).toEqual([[b!.id]]);
		expect((await search(s, s.forumId, `?tag=${a!.id}`)).threads).toEqual([]);
		expect((await search(s, s.forumId, `?tag=${a!.id}&tag=${b!.id}&tag_setting=match_all`)).threads).toEqual([]);
		const some = await search(s, s.forumId, `?tag=${a!.id}&tag=${b!.id}`);
		expect(some.threads.map((thread) => thread.id)).toEqual([tagged.id]);
	});

	it('keeps documents in sync with renames and deletes', async () => {
		const s = await setup();
		const created = await post(s, 'old name');
		await search(s, s.forumId).catch(() => null);
		await settle();
		await threadsRequest(harness, s.owner.token).patch(`/channels/${created.id}`).body({name: 'new name'}).execute();
		await settle();
		expect((await search(s, s.forumId, '?name=new')).threads.map((thread) => thread.id)).toEqual([created.id]);
		expect((await search(s, s.forumId, '?name=old')).threads).toEqual([]);
		await threadsRequest(harness, s.owner.token).delete(`/channels/${created.id}`).expect(204).execute();
		await settle();
		expect((await search(s, s.forumId)).threads).toEqual([]);
	});

	it('searches text channel threads without first messages and hides private threads from non-moderators', async () => {
		const s = await setup();
		const open = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
			.post(`/channels/${s.textId}/threads`)
			.body({name: 'open', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
		await threadsRequest(harness, s.owner.token)
			.post(`/channels/${s.textId}/threads`)
			.body({name: 'secret', type: ChannelTypes.PRIVATE_THREAD})
			.expect(201)
			.execute();
		const joined = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
			.post(`/channels/${s.textId}/threads`)
			.body({name: 'joined', type: ChannelTypes.PRIVATE_THREAD})
			.expect(201)
			.execute();
		await threadsRequest(harness, s.owner.token)
			.put(`/channels/${joined.id}/thread-members/${s.member.userId}`)
			.expect(204)
			.execute();
		await search(s, s.textId).catch(() => null);
		await settle();
		const result = await search(s, s.textId, '?sort_by=creation_time&sort_order=asc');
		expect(result.threads.map((thread) => thread.id)).toEqual([open.id, joined.id]);
		expect(result.total_results).toBe(2);
		expect(result).not.toHaveProperty('first_messages');
		const moderator = await threadsRequest<ThreadSearchResponse>(harness, s.owner.token)
			.get(`/channels/${s.textId}/threads/search`)
			.execute();
		expect(moderator.threads).toHaveLength(3);
	});
	it('pages text channel threads by thread id, including threads started from older messages', async () => {
		const s = await setup();
		const source = await sendMessage(harness, s.owner.token, s.textId, 'source');
		const standalone = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
			.post(`/channels/${s.textId}/threads`)
			.body({name: 'standalone', type: ChannelTypes.PUBLIC_THREAD})
			.expect(201)
			.execute();
		const fromMessage = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
			.post(`/channels/${s.textId}/messages/${source.id}/threads`)
			.body({name: 'from message'})
			.expect(201)
			.execute();
		await search(s, s.textId).catch(() => null);
		await settle();
		const ordered = await search(s, s.textId, '?sort_by=creation_time&sort_order=asc');
		expect(ordered.threads.map((thread) => thread.id)).toEqual([fromMessage.id, standalone.id]);
		const before = await search(s, s.textId, `?sort_by=creation_time&max_id=${standalone.id}`);
		expect(before.threads.map((thread) => thread.id)).toEqual([fromMessage.id]);
		const after = await search(s, s.textId, `?sort_by=creation_time&min_id=${fromMessage.id}`);
		expect(after.threads.map((thread) => thread.id)).toEqual([standalone.id]);
	});

	it('pages threads created in the same millisecond by thread id', async () => {
		const s = await setup();
		const snowflakes = getSnowflakeService();
		const timestamp = Date.now() - 1000;
		let sequence = 0;
		const sameMillisecond = async () =>
			createSnowflake({timestamp, workerId: Number(MAX_WORKER_ID), sequence: sequence++});
		vi.spyOn(snowflakes, 'generate').mockImplementation(sameMillisecond);
		vi.spyOn(snowflakes, 'generateForChannel').mockImplementation(sameMillisecond);
		const first = await post(s, 'first');
		const second = await post(s, 'second');
		const third = await post(s, 'third');
		vi.mocked(snowflakes.generate).mockRestore();
		vi.mocked(snowflakes.generateForChannel).mockRestore();
		await search(s, s.forumId).catch(() => null);
		await settle();
		const ids = [first.id, second.id, third.id];
		expect(new Set(ids.map((id) => snowflakeToDate(BigInt(id)).getTime())).size).toBe(1);
		const pages: Array<string> = [];
		let maxId: string | null = null;
		for (let i = 0; i < 3; i++) {
			const page = await search(s, s.forumId, `?sort_by=creation_time&limit=1${maxId ? `&max_id=${maxId}` : ''}`);
			pages.push(...page.threads.map((thread) => thread.id));
			maxId = page.threads.at(-1)?.id ?? null;
		}
		expect(pages).toEqual([third.id, second.id, first.id]);
		const after = await search(s, s.forumId, `?sort_by=creation_time&sort_order=asc&min_id=${first.id}`);
		expect(after.threads.map((thread) => thread.id)).toEqual([second.id, third.id]);
		const offsetPages = await Promise.all(
			[0, 1, 2].map((offset) => search(s, s.forumId, `?sort_by=last_message_time&limit=1&offset=${offset}`)),
		);
		expect(new Set(offsetPages.flatMap((page) => page.threads.map((thread) => thread.id))).size).toBe(3);
	});
});

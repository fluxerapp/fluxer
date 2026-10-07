// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {ThreadRepository} from '@app/api/channel/repositories/ThreadRepository';
import {
	acceptInvite,
	addMemberRole,
	createChannel,
	createChannelInvite,
	createGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	THREADS_FEATURE,
	THREADS_FEATURE_HEADER,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags, ThreadMemberFlags} from '@fluxer/constants/src/ThreadConstants';
import {ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import {ThreadAlreadyCreatedForMessageError} from '@fluxer/errors/src/domains/channel/ThreadAlreadyCreatedForMessageError';
import type {
	ActiveThreadsResponse,
	ArchivedThreadsResponse,
	ThreadChannelResponse,
} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {RateLimitService} from '@pkgs/rate_limit/src/RateLimitService';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	guildId: string;
	channelId: string;
}

const repository = new ChannelRepository();

async function archive(threadId: string, archiveTimestamp?: Date): Promise<void> {
	await repository.threads.updateState(createChannelID(BigInt(threadId)), () => ({
		archived: true,
		...(archiveTimestamp ? {archive_timestamp: archiveTimestamp} : {}),
	}));
}

async function lock(threadId: string): Promise<void> {
	await repository.threads.updateState(createChannelID(BigInt(threadId)), () => ({locked: true}));
}

describe('thread routes', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup(): Promise<Setup> {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'threads');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);
		return {owner, member, guildId: guild.id, channelId: channel.id};
	}

	async function addBot(
		s: Setup,
		permissions = Permissions.ADMINISTRATOR,
	): Promise<Awaited<ReturnType<typeof createTestBotAccount>>> {
		const bot = await createTestBotAccount(harness);
		await threadsRequest(harness, s.owner.token)
			.post('/oauth2/authorize/consent')
			.body({client_id: bot.appId, scope: 'bot', guild_id: s.guildId, permissions: permissions.toString()})
			.execute();
		return bot;
	}

	async function startThread(
		token: string,
		channelId: string,
		body: Record<string, unknown> = {name: 'topic', type: ChannelTypes.PUBLIC_THREAD},
	): Promise<ThreadChannelResponse> {
		return threadsRequest<ThreadChannelResponse>(harness, token)
			.post(`/channels/${channelId}/threads`)
			.body(body)
			.expect(201)
			.execute();
	}

	describe('control arm', () => {
		async function snapshot(path: string, method: string, token: string, headers: Record<string, string>) {
			const response = await harness.requestJson({
				path,
				method,
				headers: {Authorization: token, 'x-fluxer-test-enable-rate-limits': 'true', ...headers},
				body: method === 'GET' || method === 'DELETE' || method === 'PUT' ? undefined : {name: 'x', type: 11},
			});
			return {
				status: response.status,
				body: await response.text(),
				headers: [...response.headers].filter(([name]) => name !== 'x-request-id'),
			};
		}

		function routes(s: Setup): Array<[string, string]> {
			const c = s.channelId;
			return [
				['POST', `/channels/${c}/messages/${c}/threads`],
				['POST', `/channels/${c}/threads`],
				['GET', `/guilds/${s.guildId}/threads/active`],
				['GET', `/channels/${c}/threads/archived/public`],
				['GET', `/channels/${c}/threads/archived/private`],
				['GET', `/channels/${c}/users/@me/threads/archived/private`],
				['GET', `/channels/${c}/thread-members`],
				['GET', `/channels/${c}/thread-members/${s.member.userId}`],
				['PUT', `/channels/${c}/thread-members/@me`],
				['PUT', `/channels/${c}/thread-members/${s.member.userId}`],
				['DELETE', `/channels/${c}/thread-members/@me`],
				['DELETE', `/channels/${c}/thread-members/${s.member.userId}`],
			];
		}

		async function expectEveryRouteUnregistered(s: Setup, token: string, headers: Record<string, string>) {
			for (const [method, path] of routes(s)) {
				const unregistered = path.startsWith('/guilds/')
					? `/guilds/${s.guildId}/threads-unregistered`
					: `/channels/${s.channelId}/threads-unregistered`;
				const expected = await snapshot(unregistered, method, token, headers);
				const actual = await snapshot(path, method, token, headers);
				expect(actual.status, `${method} ${path}`).toBe(404);
				expect(actual, `${method} ${path}`).toEqual(expected);
				expect(actual.headers.some(([name]) => name.startsWith('x-ratelimit'))).toBe(false);
			}
		}

		it('answers every thread route like an unregistered path while the experiment is off', async () => {
			const s = await setup();
			await setChannelThreadsConfig({enabled: false});
			await expectEveryRouteUnregistered(s, s.owner.token, {[THREADS_FEATURE_HEADER]: THREADS_FEATURE});
		});

		it('answers every thread route like an unregistered path without the client capability', async () => {
			const s = await setup();
			await expectEveryRouteUnregistered(s, s.owner.token, {});
		});

		it('answers every thread route like an unregistered path for a guild outside the experiment', async () => {
			const s = await setup();
			await setChannelThreadsConfig({enabled: true, user_basis_points: 10000, disabled_guild_ids: [s.guildId]});
			await expectEveryRouteUnregistered(s, s.owner.token, {[THREADS_FEATURE_HEADER]: THREADS_FEATURE});
		});

		it('answers bot-only routes like an unregistered path for users', async () => {
			const s = await setup();
			const headers = {Authorization: s.owner.token, [THREADS_FEATURE_HEADER]: THREADS_FEATURE};
			for (const path of [
				`/guilds/${s.guildId}/threads/active`,
				`/channels/${s.channelId}/thread-members`,
				`/channels/${s.channelId}/thread-members/${s.member.userId}`,
			]) {
				const response = await harness.requestJson({path, method: 'GET', headers});
				expect(response.status).toBe(404);
				expect(((await response.json()) as {code: string}).code).toBe('NOT_FOUND');
			}
		});

		it('writes no thread data and dispatches nothing when the gate is off', async () => {
			const s = await setup();
			await setChannelThreadsConfig({enabled: false});
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			const message = await sendMessage(harness, s.owner.token, s.channelId, 'hello');
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
				.body({name: 'nope'})
				.expect(404)
				.execute();
			expect(await repository.threads.getState(createChannelID(BigInt(message.id)))).toBeNull();
			expect(dispatch.mock.calls.filter(([params]) => params.event.startsWith('THREAD_'))).toEqual([]);
		});
	});

	describe('start thread from message', () => {
		it('creates a public thread that shares the message id', async () => {
			const s = await setup();
			const message = await sendMessage(harness, s.member.token, s.channelId, 'start here');
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			const thread = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
				.body({name: 'Discussion', auto_archive_duration: 1440})
				.expect(201)
				.execute();
			expect(thread).toMatchObject({
				id: message.id,
				type: ChannelTypes.PUBLIC_THREAD,
				guild_id: s.guildId,
				parent_id: s.channelId,
				owner_id: s.owner.userId,
				name: 'Discussion',
				member_count: 1,
				message_count: 0,
				total_message_sent: 0,
				rate_limit_per_user: 0,
				flags: 0,
			});
			expect(thread.thread_metadata).toMatchObject({archived: false, locked: false, auto_archive_duration: 1440});
			expect(thread.thread_metadata?.invitable).toBeUndefined();
			expect(thread.member).toBeUndefined();
			expect('position' in thread).toBe(false);
			expect('permission_overwrites' in thread).toBe(false);
			const threadEvents = dispatch.mock.calls
				.map(([params]) => params)
				.filter((params) => params.event !== 'GUILD_AUDIT_LOG_ENTRY_CREATE');
			expect(threadEvents.map((params) => params.event)).toEqual(['THREAD_CREATE', 'MESSAGE_UPDATE', 'MESSAGE_CREATE']);
			expect(threadEvents[0]!.data).toMatchObject({id: message.id, newly_created: true});
			const starter = threadEvents[2]!.data as MessageResponse;
			expect(starter).toMatchObject({
				id: message.id,
				channel_id: message.id,
				type: MessageTypes.THREAD_STARTER_MESSAGE,
				message_reference: {channel_id: s.channelId, message_id: message.id},
			});
			expect(starter.referenced_message?.id).toBe(message.id);
			expect(starter.referenced_message?.thread?.id).toBe(message.id);
			expect(starter.referenced_message?.thread && 'member' in starter.referenced_message.thread).toBe(false);
			const update = threadEvents[1]!.data as MessageResponse & {__thread_only_update?: boolean};
			expect(update.flags & ServerMessageFlags.HAS_THREAD).toBe(ServerMessageFlags.HAS_THREAD);
			expect(update.__thread_only_update).toBe(true);
			expect(update.thread?.id).toBe(message.id);
			const stored = await repository.messages.getMessage(
				createChannelID(BigInt(s.channelId)),
				createMessageID(BigInt(message.id)),
			);
			expect((stored?.flags ?? 0) & ServerMessageFlags.HAS_THREAD).toBe(ServerMessageFlags.HAS_THREAD);
			const member = await repository.threads.getMember(
				createChannelID(BigInt(message.id)),
				createUserID(BigInt(s.owner.userId)),
			);
			expect(member?.flags).toBe(ThreadMemberFlags.HAS_INTERACTED);
		});

		it('announces threads started from older messages in the parent without moving its last message', async () => {
			const s = await setup();
			const message = await sendMessage(harness, s.owner.token, s.channelId, 'old news');
			const now = Date.now();
			vi.spyOn(Date, 'now').mockReturnValue(now + 6 * 60 * 1000);
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
				.body({name: 'Later'})
				.expect(201)
				.execute();
			const announced = dispatch.mock.calls
				.map(([params]) => params.data as MessageResponse)
				.filter((data) => data.type === MessageTypes.THREAD_CREATED);
			expect(announced).toHaveLength(1);
			expect(announced[0]).toMatchObject({channel_id: s.channelId, content: 'Later'});
			expect(announced[0]!.message_reference?.channel_id).toBe(message.id);
			const parent = await repository.findUnique(createChannelID(BigInt(s.channelId)));
			expect(parent?.lastMessageId?.toString()).toBe(message.id);
		});

		it('refuses a second thread on the same message', async () => {
			const s = await setup();
			const message = await sendMessage(harness, s.owner.token, s.channelId, 'once');
			const path = `/channels/${s.channelId}/messages/${message.id}/threads`;
			await threadsRequest(harness, s.owner.token).post(path).body({name: 'a'}).expect(201).execute();
			await threadsRequest(harness, s.owner.token)
				.post(path)
				.body({name: 'b'})
				.expect(400, APIErrorCodes.THREAD_ALREADY_CREATED_FOR_MESSAGE)
				.execute();
		});

		it('rejects unknown messages, bad names and non-text parents', async () => {
			const s = await setup();
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.channelId}/messages/1/threads`)
				.body({name: 'a'})
				.expect(404, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
			const message = await sendMessage(harness, s.owner.token, s.channelId, 'x');
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
				.body({name: ''})
				.expect(400)
				.execute();
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${thread.id}/messages/${message.id}/threads`)
				.body({name: 'nested'})
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
		});

		it('requires create public threads and read message history on the parent', async () => {
			const s = await setup();
			const message = await sendMessage(harness, s.owner.token, s.channelId, 'x');
			await createBuilder(harness, s.owner.token)
				.patch(`/guilds/${s.guildId}/roles/${s.guildId}`)
				.header(THREADS_FEATURE_HEADER, THREADS_FEATURE)
				.body({permissions: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString()})
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
				.body({name: 'denied'})
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});

		it('spends the parent slowmode only on a thread that gets created', async () => {
			const s = await setup();
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.channelId}`)
				.body({rate_limit_per_user: 60})
				.expect(200)
				.execute();
			const message = await sendMessage(harness, s.owner.token, s.channelId, 'x');
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/messages/1/threads`)
				.body({name: 'missing'})
				.expect(404, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
				.body({name: 'first'})
				.expect(201)
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
				.body({name: 'again'})
				.expect(400, APIErrorCodes.THREAD_ALREADY_CREATED_FOR_MESSAGE)
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/threads`)
				.body({name: 'second', type: ChannelTypes.PUBLIC_THREAD})
				.expect(400, APIErrorCodes.SLOWMODE_RATE_LIMITED)
				.execute();
		});

		it('refunds the parent slowmode when the thread write loses a race', async () => {
			const s = await setup();
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.channelId}`)
				.body({rate_limit_per_user: 60})
				.expect(200)
				.execute();
			const message = await sendMessage(harness, s.owner.token, s.channelId, 'x');
			vi.spyOn(ThreadRepository.prototype, 'create').mockRejectedValueOnce(new ThreadAlreadyCreatedForMessageError());
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
				.body({name: 'raced'})
				.expect(400, APIErrorCodes.THREAD_ALREADY_CREATED_FOR_MESSAGE)
				.execute();
			await startThread(s.member.token, s.channelId, {name: 'next', type: ChannelTypes.PUBLIC_THREAD});
		});

		it('resets the thread create slowmode when the parent rate limit changes', async () => {
			const s = await setup();
			const setRateLimit = (seconds: number) =>
				threadsRequest(harness, s.owner.token)
					.patch(`/channels/${s.channelId}`)
					.body({rate_limit_per_user: seconds})
					.expect(200)
					.execute();
			await setRateLimit(60);
			await startThread(s.member.token, s.channelId, {name: 'first', type: ChannelTypes.PUBLIC_THREAD});
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/threads`)
				.body({name: 'blocked', type: ChannelTypes.PUBLIC_THREAD})
				.expect(400, APIErrorCodes.SLOWMODE_RATE_LIMITED)
				.execute();
			const clear = vi.spyOn(RateLimitService.prototype, 'clearLimitsByIdentifierPrefix');
			await setRateLimit(30);
			expect(clear.mock.calls).toEqual([[`slowmode:${s.channelId}:`], [`slowmode-thread:${s.channelId}:`]]);
			await startThread(s.member.token, s.channelId, {name: 'second', type: ChannelTypes.PUBLIC_THREAD});
		});

		it('clears only the message slowmode on a rate limit change in a guild without threads', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'control');
			const channel = await createChannel(harness, owner.token, guild.id, 'general');
			const clear = vi.spyOn(RateLimitService.prototype, 'clearLimitsByIdentifierPrefix');
			const arms: Array<[Parameters<typeof setChannelThreadsConfig>[0], number]> = [
				[{enabled: false}, 60],
				[{enabled: true, included_user_ids: [owner.userId]}, 30],
			];
			for (const [config, seconds] of arms) {
				await setChannelThreadsConfig(config);
				clear.mockClear();
				await threadsRequest(harness, owner.token)
					.patch(`/channels/${channel.id}`)
					.body({rate_limit_per_user: seconds})
					.expect(200)
					.execute();
				expect(clear.mock.calls).toEqual([[`slowmode:${channel.id}:`]]);
			}
		});
	});

	it('refuses a limited account on both thread create routes without dispatching', async () => {
		const s = await setup();
		const message = await sendMessage(harness, s.owner.token, s.channelId, 'start here');
		await createBuilder(harness, '')
			.post(`/test/users/${s.member.userId}/security-flags`)
			.body({set_flags: ['ACCOUNT_LIMITED']})
			.execute();
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		await threadsRequest(harness, s.member.token)
			.post(`/channels/${s.channelId}/threads`)
			.body({name: 'limited', type: ChannelTypes.PUBLIC_THREAD})
			.expect(403, APIErrorCodes.ACCOUNT_LIMITED)
			.execute();
		await threadsRequest(harness, s.member.token)
			.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
			.body({name: 'limited'})
			.expect(403, APIErrorCodes.ACCOUNT_LIMITED)
			.execute();
		expect(await repository.threads.getState(createChannelID(BigInt(message.id)))).toBeNull();
		expect(dispatch.mock.calls.filter(([params]) => params.event.startsWith('THREAD_'))).toEqual([]);
	});

	describe('start thread without a message', () => {
		it('requires a thread type', async () => {
			const s = await setup();
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.channelId}/threads`)
				.body({name: 'no type'})
				.expect(400)
				.execute();
		});

		it('accepts the body as payload_json in multipart form data', async () => {
			const s = await setup();
			const multipart = (payload: string) => {
				const boundary = 'ThreadBoundary';
				return harness.app.request(`/channels/${s.channelId}/threads`, {
					method: 'POST',
					headers: {
						Authorization: s.owner.token,
						[THREADS_FEATURE_HEADER]: THREADS_FEATURE,
						'Content-Type': `multipart/form-data; boundary=${boundary}`,
						'x-forwarded-for': '127.0.0.1',
					},
					body: `--${boundary}\r\nContent-Disposition: form-data; name="payload_json"\r\n\r\n${payload}\r\n--${boundary}--\r\n`,
				});
			};
			const created = await multipart(JSON.stringify({name: 'form', type: ChannelTypes.PRIVATE_THREAD}));
			expect(created.status).toBe(201);
			const thread = (await created.json()) as ThreadChannelResponse;
			expect(thread.type).toBe(ChannelTypes.PRIVATE_THREAD);
			expect(thread.name).toBe('form');
			expect((await multipart('{not json')).status).toBe(400);
		});

		it('creates public and private threads', async () => {
			const s = await setup();
			const publicThread = await startThread(s.owner.token, s.channelId);
			expect(publicThread.type).toBe(ChannelTypes.PUBLIC_THREAD);
			expect(BigInt(publicThread.id)).toBeGreaterThan(BigInt(s.channelId));
			const privateThread = await startThread(s.owner.token, s.channelId, {
				name: 'secret',
				type: ChannelTypes.PRIVATE_THREAD,
				invitable: false,
				rate_limit_per_user: 30,
			});
			expect(privateThread.type).toBe(ChannelTypes.PRIVATE_THREAD);
			expect(privateThread.thread_metadata?.invitable).toBe(false);
			expect(privateThread.rate_limit_per_user).toBe(30);
		});

		it('posts a thread created message in the parent for public threads only', async () => {
			const s = await setup();
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			const publicThread = await startThread(s.owner.token, s.channelId);
			const created = dispatch.mock.calls
				.map(([params]) => params)
				.filter((params) => params.event === 'MESSAGE_CREATE')
				.map((params) => params.data as MessageResponse);
			expect(created).toHaveLength(1);
			expect(created[0]).toMatchObject({type: MessageTypes.THREAD_CREATED, channel_id: s.channelId, content: 'topic'});
			expect(created[0]!.message_reference?.channel_id).toBe(publicThread.id);
			dispatch.mockClear();
			await startThread(s.owner.token, s.channelId, {name: 'quiet', type: ChannelTypes.PRIVATE_THREAD});
			expect(dispatch.mock.calls.filter(([params]) => params.event === 'MESSAGE_CREATE')).toEqual([]);
		});

		it('refuses private threads without create private threads', async () => {
			const s = await setup();
			await createBuilder(harness, s.owner.token)
				.patch(`/guilds/${s.guildId}/roles/${s.guildId}`)
				.header(THREADS_FEATURE_HEADER, THREADS_FEATURE)
				.body({
					permissions: (
						Permissions.VIEW_CHANNEL |
						Permissions.SEND_MESSAGES |
						ThreadPermissionFlags.CREATE_PUBLIC_THREADS
					).toString(),
				})
				.execute();
			await startThread(s.member.token, s.channelId);
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/threads`)
				.body({name: 'p', type: ChannelTypes.PRIVATE_THREAD})
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});
	});

	describe('membership', () => {
		it('joins and leaves a thread', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/@me`)
				.expect(204)
				.execute();
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/@me`)
				.expect(204)
				.execute();
			const joined = dispatch.mock.calls.filter(([params]) => params.event === 'THREAD_MEMBERS_UPDATE');
			expect(joined).toHaveLength(1);
			expect(joined[0]![0].data).toMatchObject({
				id: thread.id,
				guild_id: s.guildId,
				member_count: 2,
				added_members: [{id: thread.id, user_id: s.member.userId, flags: 0}],
			});
			await threadsRequest(harness, s.member.token)
				.delete(`/channels/${thread.id}/thread-members/@me`)
				.expect(204)
				.execute();
			await threadsRequest(harness, s.member.token)
				.delete(`/channels/${thread.id}/thread-members/@me`)
				.expect(404, APIErrorCodes.UNKNOWN_THREAD_MEMBER)
				.execute();
		});

		it('joins and leaves through a percent-encoded @me', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/%40me`)
				.expect(204)
				.execute();
			await threadsRequest(harness, s.member.token)
				.delete(`/channels/${thread.id}/thread-members/%40me`)
				.expect(204)
				.execute();
			await threadsRequest(harness, s.member.token)
				.delete(`/channels/${thread.id}/thread-members/%40me`)
				.expect(404, APIErrorCodes.UNKNOWN_THREAD_MEMBER)
				.execute();
		});
		it('refuses joins on archived threads and non-moderator joins on locked threads', async () => {
			const s = await setup();
			const archived = await startThread(s.owner.token, s.channelId);
			await archive(archived.id);
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${archived.id}/thread-members/@me`)
				.expect(400, APIErrorCodes.THREAD_ARCHIVED)
				.execute();
			const locked = await startThread(s.owner.token, s.channelId, {name: 'locked', type: ChannelTypes.PUBLIC_THREAD});
			await lock(locked.id);
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${locked.id}/thread-members/@me`)
				.expect(400, APIErrorCodes.THREAD_LOCKED)
				.execute();
		});

		it('hides private threads from non-members', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId, {name: 'p', type: ChannelTypes.PRIVATE_THREAD});
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/@me`)
				.expect(403, APIErrorCodes.MISSING_ACCESS)
				.execute();
		});

		it('adds and removes other members with recipient system messages', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${thread.id}/thread-members/${s.member.userId}`)
				.expect(204)
				.execute();
			const recipientAdd = dispatch.mock.calls
				.map(([params]) => params)
				.find((params) => params.event === 'MESSAGE_CREATE')?.data as MessageResponse | undefined;
			expect(recipientAdd).toMatchObject({type: MessageTypes.RECIPIENT_ADD, channel_id: thread.id});
			expect(recipientAdd?.mentions.map((user) => user.id)).toEqual([s.member.userId]);
			await threadsRequest(harness, s.member.token)
				.delete(`/channels/${thread.id}/thread-members/${s.owner.userId}`)
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
			dispatch.mockClear();
			await threadsRequest(harness, s.owner.token)
				.delete(`/channels/${thread.id}/thread-members/${s.member.userId}`)
				.expect(204)
				.execute();
			const events = dispatch.mock.calls.map(([params]) => params.event);
			expect(events).toContain('THREAD_MEMBERS_UPDATE');
			const removed = dispatch.mock.calls.find(([params]) => params.event === 'THREAD_MEMBERS_UPDATE')![0].data;
			expect(removed).toMatchObject({removed_member_ids: [s.member.userId], member_count: 1});
			await threadsRequest(harness, s.owner.token)
				.delete(`/channels/${thread.id}/thread-members/${s.member.userId}`)
				.expect(404, APIErrorCodes.UNKNOWN_THREAD_MEMBER)
				.execute();
		});

		it('posts one recipient message when concurrent adds or removes race past the membership check', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			const realGetMember = ThreadRepository.prototype.getMember;
			let stale: Awaited<ReturnType<ThreadRepository['getMember']>> = null;
			vi.spyOn(ThreadRepository.prototype, 'getMember').mockImplementation(function (this: ThreadRepository, ...args) {
				return args[1].toString() === s.member.userId ? Promise.resolve(stale) : realGetMember.apply(this, args);
			});
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			const recipientMessages = (type: number) =>
				dispatch.mock.calls.filter(
					([params]) => params.event === 'MESSAGE_CREATE' && (params.data as MessageResponse).type === type,
				);
			const put = () =>
				threadsRequest(harness, s.owner.token)
					.put(`/channels/${thread.id}/thread-members/${s.member.userId}`)
					.expect(204)
					.execute();
			await Promise.all([put(), put()]);
			expect(recipientMessages(MessageTypes.RECIPIENT_ADD)).toHaveLength(1);
			stale = await realGetMember.call(
				repository.threads as ThreadRepository,
				createChannelID(BigInt(thread.id)),
				createUserID(BigInt(s.member.userId)),
			);
			expect(stale).not.toBeNull();
			const del = () =>
				threadsRequest(harness, s.owner.token)
					.delete(`/channels/${thread.id}/thread-members/${s.member.userId}`)
					.expect(204)
					.execute();
			await Promise.all([del(), del()]);
			expect(recipientMessages(MessageTypes.RECIPIENT_REMOVE)).toHaveLength(1);
		});

		it('refuses adding a user who is not a guild member', async () => {
			const s = await setup();
			const outsider = await createTestAccount(harness);
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${thread.id}/thread-members/${outsider.userId}`)
				.expect(404, APIErrorCodes.UNKNOWN_MEMBER)
				.execute();
		});

		it('refuses adding a member who is outside the experiment like one who cannot view the parent', async () => {
			const s = await setup();
			await setChannelThreadsConfig({...ALL_THREADS_ACTIVE, excluded_user_ids: [s.member.userId]});
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${thread.id}/thread-members/${s.member.userId}`)
				.expect(403, APIErrorCodes.MISSING_ACCESS)
				.execute();
		});

		it('lets only moderators add non-moderators to a private thread that is not invitable', async () => {
			const s = await setup();
			const third = await createTestAccount(harness);
			const invite = await createChannelInvite(harness, s.owner.token, s.channelId);
			await acceptInvite(harness, third.token, invite.code);
			const thread = await startThread(s.owner.token, s.channelId, {
				name: 'closed',
				type: ChannelTypes.PRIVATE_THREAD,
				invitable: false,
			});
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${thread.id}/thread-members/${s.member.userId}`)
				.expect(204)
				.execute();
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/${third.userId}`)
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
			const role = await threadsRequest<GuildRoleResponse>(harness, s.owner.token)
				.post(`/guilds/${s.guildId}/roles`)
				.body({name: 'mods', permissions: ThreadPermissionFlags.MANAGE_THREADS.toString()})
				.execute();
			await addMemberRole(harness, s.owner.token, s.guildId, third.userId, role.id);
			const setTimeout = (until: string | null) =>
				threadsRequest(harness, s.owner.token)
					.patch(`/guilds/${s.guildId}/members/${third.userId}`)
					.body({communication_disabled_until: until})
					.expect(200)
					.execute();
			await setTimeout(new Date(Date.now() + 10 * 60 * 1000).toISOString());
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/${third.userId}`)
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
			await setTimeout(null);
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/${third.userId}`)
				.expect(204)
				.execute();
		});

		it('refuses member changes on archived threads', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/@me`)
				.expect(204)
				.execute();
			await archive(thread.id);
			await threadsRequest(harness, s.owner.token)
				.delete(`/channels/${thread.id}/thread-members/${s.member.userId}`)
				.expect(400, APIErrorCodes.THREAD_ARCHIVED)
				.execute();
			await threadsRequest(harness, s.member.token)
				.delete(`/channels/${thread.id}/thread-members/@me`)
				.expect(400, APIErrorCodes.THREAD_ARCHIVED)
				.execute();
		});

		it('rejects thread member routes on channels that are not threads', async () => {
			const s = await setup();
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${s.channelId}/thread-members/@me`)
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
		});
	});

	describe('bot-only member reads', () => {
		it('lists thread members paginated with and without guild members', async () => {
			const s = await setup();
			const bot = await addBot(s);
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/@me`)
				.expect(204)
				.execute();
			const token = `Bot ${bot.botToken}`;
			const all = await createBuilder<Array<ThreadMemberResponse>>(harness, token)
				.get(`/channels/${thread.id}/thread-members`)
				.execute();
			const expectedOrder = [s.owner.userId, s.member.userId].sort((a, b) => (BigInt(a) < BigInt(b) ? -1 : 1));
			expect(all.map((member) => member.user_id)).toEqual(expectedOrder);
			expect(all.every((member) => member.member === undefined && member.muted === undefined)).toBe(true);
			const firstPage = await createBuilder<Array<ThreadMemberResponse>>(harness, token)
				.get(`/channels/${thread.id}/thread-members?limit=1&with_member=true`)
				.execute();
			expect(firstPage).toHaveLength(1);
			expect(firstPage[0]!.member?.user.id).toBe(expectedOrder[0]);
			const secondPage = await createBuilder<Array<ThreadMemberResponse>>(harness, token)
				.get(`/channels/${thread.id}/thread-members?limit=1&with_member=true&after=${expectedOrder[0]}`)
				.execute();
			expect(secondPage.map((member) => member.user_id)).toEqual([expectedOrder[1]]);
			const plainFirst = await createBuilder<Array<ThreadMemberResponse>>(harness, token)
				.get(`/channels/${thread.id}/thread-members?limit=1`)
				.execute();
			expect(plainFirst.map((member) => member.user_id)).toEqual([expectedOrder[0]]);
			const plainSecond = await createBuilder<Array<ThreadMemberResponse>>(harness, token)
				.get(`/channels/${thread.id}/thread-members?limit=1&after=${expectedOrder[0]}`)
				.execute();
			expect(plainSecond.map((member) => member.user_id)).toEqual([expectedOrder[1]]);
			expect(plainSecond[0]!.member).toBeUndefined();
			await createBuilder(harness, token).get(`/channels/${thread.id}/thread-members?limit=101`).expect(400).execute();
		});

		it('gets one thread member and 404s for non-members', async () => {
			const s = await setup();
			const bot = await addBot(s);
			const thread = await startThread(s.owner.token, s.channelId);
			const token = `Bot ${bot.botToken}`;
			const owner = await createBuilder<ThreadMemberResponse>(harness, token)
				.get(`/channels/${thread.id}/thread-members/${s.owner.userId}?with_member=true`)
				.execute();
			expect(owner).toMatchObject({id: thread.id, user_id: s.owner.userId, flags: ThreadMemberFlags.HAS_INTERACTED});
			expect(owner.member?.user.id).toBe(s.owner.userId);
			await createBuilder(harness, token)
				.get(`/channels/${thread.id}/thread-members/${s.member.userId}`)
				.expect(404, APIErrorCodes.UNKNOWN_THREAD_MEMBER)
				.execute();
		});

		it('lists active guild threads the bot can see, newest first', async () => {
			const s = await setup();
			const bot = await addBot(s);
			const first = await startThread(s.owner.token, s.channelId);
			const second = await startThread(s.owner.token, s.channelId, {name: 'p', type: ChannelTypes.PRIVATE_THREAD});
			const archived = await startThread(s.owner.token, s.channelId, {name: 'old', type: ChannelTypes.PUBLIC_THREAD});
			await archive(archived.id);
			const token = `Bot ${bot.botToken}`;
			await threadsRequest(harness, token).put(`/channels/${first.id}/thread-members/@me`).expect(204).execute();
			const response = await createBuilder<ActiveThreadsResponse>(harness, token)
				.get(`/guilds/${s.guildId}/threads/active`)
				.execute();
			expect(response.threads.map((thread) => thread.id)).toEqual([second.id, first.id]);
			expect(response.members.map((member) => [member.id, member.user_id])).toEqual([[first.id, bot.botUserId]]);
		});

		it('hides private threads from bots without manage threads that are not members', async () => {
			const s = await setup();
			const bot = await addBot(s, Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY);
			await startThread(s.owner.token, s.channelId, {name: 'p', type: ChannelTypes.PRIVATE_THREAD});
			const open = await startThread(s.owner.token, s.channelId);
			const response = await createBuilder<ActiveThreadsResponse>(harness, `Bot ${bot.botToken}`)
				.get(`/guilds/${s.guildId}/threads/active`)
				.execute();
			expect(response.threads.map((thread) => thread.id)).toEqual([open.id]);
		});

		it('hides private threads from timed out thread moderators that are not members', async () => {
			const s = await setup();
			const bot = await addBot(
				s,
				Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY | ThreadPermissionFlags.MANAGE_THREADS,
			);
			const hidden = await startThread(s.owner.token, s.channelId, {name: 'p', type: ChannelTypes.PRIVATE_THREAD});
			const open = await startThread(s.owner.token, s.channelId);
			const list = async () =>
				(
					await createBuilder<ActiveThreadsResponse>(harness, `Bot ${bot.botToken}`)
						.get(`/guilds/${s.guildId}/threads/active`)
						.execute()
				).threads.map((thread) => thread.id);
			expect(await list()).toEqual([open.id, hidden.id]);
			await threadsRequest(harness, s.owner.token)
				.patch(`/guilds/${s.guildId}/members/${bot.botUserId}`)
				.body({communication_disabled_until: new Date(Date.now() + 10 * 60 * 1000).toISOString()})
				.expect(200)
				.execute();
			expect(await list()).toEqual([open.id]);
		});
	});

	describe('archived lists', () => {
		it('pages public archived threads by archive timestamp', async () => {
			const s = await setup();
			const ids: Array<string> = [];
			const archivedAt = Date.now() - 60_000;
			for (const [index, name] of ['a', 'b', 'c'].entries()) {
				const thread = await startThread(s.owner.token, s.channelId, {name, type: ChannelTypes.PUBLIC_THREAD});
				await archive(thread.id, new Date(archivedAt + index * 1000));
				ids.push(thread.id);
			}
			await startThread(s.owner.token, s.channelId);
			const page = await threadsRequest<ArchivedThreadsResponse>(harness, s.owner.token)
				.get(`/channels/${s.channelId}/threads/archived/public?limit=2`)
				.execute();
			expect(page.threads.map((thread) => thread.id)).toEqual([ids[2], ids[1]]);
			expect(page.has_more).toBe(true);
			expect(page.members.map((member) => member.id)).toEqual([ids[2], ids[1]]);
			expect(page.members[0]!.muted).toBe(false);
			const before = page.threads[1]!.thread_metadata!.archive_timestamp;
			const rest = await threadsRequest<ArchivedThreadsResponse>(harness, s.owner.token)
				.get(`/channels/${s.channelId}/threads/archived/public?before=${encodeURIComponent(before)}`)
				.execute();
			expect(rest.threads.map((thread) => thread.id)).toEqual([ids[0]]);
			expect(rest.has_more).toBe(false);
			await threadsRequest(harness, s.owner.token)
				.get(`/channels/${s.channelId}/threads/archived/public?limit=1`)
				.expect(400)
				.execute();
		});

		it('requires manage threads for private archived threads and lists joined ones for members', async () => {
			const s = await setup();
			await createBuilder(harness, s.owner.token)
				.patch(`/guilds/${s.guildId}/roles/${s.guildId}`)
				.header(THREADS_FEATURE_HEADER, THREADS_FEATURE)
				.body({
					permissions: (
						Permissions.VIEW_CHANNEL |
						Permissions.SEND_MESSAGES |
						Permissions.READ_MESSAGE_HISTORY |
						ThreadPermissionFlags.CREATE_PRIVATE_THREADS |
						ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS
					).toString(),
				})
				.execute();
			const mine = await startThread(s.member.token, s.channelId, {name: 'mine', type: ChannelTypes.PRIVATE_THREAD});
			const theirs = await startThread(s.owner.token, s.channelId, {name: 'x', type: ChannelTypes.PRIVATE_THREAD});
			await archive(mine.id);
			await archive(theirs.id);
			await threadsRequest(harness, s.member.token)
				.get(`/channels/${s.channelId}/threads/archived/private`)
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
			const all = await threadsRequest<ArchivedThreadsResponse>(harness, s.owner.token)
				.get(`/channels/${s.channelId}/threads/archived/private`)
				.execute();
			expect(all.threads.map((thread) => thread.id).sort()).toEqual([mine.id, theirs.id].sort());
			const joined = await threadsRequest<ArchivedThreadsResponse>(harness, s.member.token)
				.get(`/channels/${s.channelId}/users/@me/threads/archived/private`)
				.execute();
			expect(joined.threads.map((thread) => thread.id)).toEqual([mine.id]);
			expect(joined.members.map((member) => member.user_id)).toEqual([s.member.userId]);
			const empty = await threadsRequest<ArchivedThreadsResponse>(harness, s.member.token)
				.get(`/channels/${s.channelId}/users/@me/threads/archived/private?before=${mine.id}`)
				.execute();
			expect(empty).toEqual({threads: [], members: [], has_more: false});
		});

		it('rejects private lists on threads', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.owner.token)
				.get(`/channels/${thread.id}/threads/archived/private`)
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
		});
	});

	describe('announcement parents', () => {
		it('refuses a thread whose parent became an announcement channel mid-create', async () => {
			const s = await setup();
			const parentId = createChannelID(BigInt(s.channelId));
			const count = ThreadRepository.prototype.countActiveThreads;
			vi.spyOn(ThreadRepository.prototype, 'countActiveThreads').mockImplementationOnce(async function (
				this: ThreadRepository,
				guildId,
			) {
				const parent = await repository.channelData.findUnique(parentId);
				await repository.channelData.upsert({...parent!.toRow(), type: ChannelTypes.GUILD_ANNOUNCEMENT});
				return count.call(this, guildId);
			});
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.channelId}/threads`)
				.body({name: 'late', type: ChannelTypes.PUBLIC_THREAD})
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
			expect(await repository.threads.listThreadIdsByParent(parentId, {limit: 1})).toEqual([]);
		});
	});

	describe('admin', () => {
		it('lists every thread of a guild and deletes threads even after the experiment is off', async () => {
			const s = await setup();
			const admin = await setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
			const open = await startThread(s.owner.token, s.channelId);
			const archived = await startThread(s.owner.token, s.channelId, {name: 'old', type: ChannelTypes.PUBLIC_THREAD});
			await archive(archived.id);
			await setChannelThreadsConfig({enabled: false});
			const listed = await createBuilder<{threads: Array<ThreadChannelResponse>}>(harness, admin.token)
				.get(`/admin/guilds/${s.guildId}/threads`)
				.execute();
			expect(listed.threads.map((thread) => thread.id)).toEqual([archived.id, open.id]);
			await createBuilder(harness, admin.token).delete(`/admin/channels/${s.channelId}`).expect(400).execute();
			await createBuilder(harness, admin.token).delete(`/admin/channels/${open.id}`).expect(204).execute();
			expect(await repository.threads.getState(createChannelID(BigInt(open.id)))).toBeNull();
			expect(await repository.findUnique(createChannelID(BigInt(open.id)))).toBeNull();
			const after = await createBuilder<{threads: Array<ThreadChannelResponse>}>(harness, admin.token)
				.get(`/admin/guilds/${s.guildId}/threads`)
				.execute();
			expect(after.threads.map((thread) => thread.id)).toEqual([archived.id]);
		});
	});
});

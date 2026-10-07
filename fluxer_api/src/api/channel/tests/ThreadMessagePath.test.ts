// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {MessageSearchService} from '@app/api/channel/services/message/MessageSearchService';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {upsertOne} from '@app/api/database/CassandraQueryExecution';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {getKVThreadAutoArchiveQueue} from '@app/api/middleware/ServiceSingletons';
import {ThreadState as ThreadStateTable} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_THREAD_MEMBERS, ServerMessageFlags, ThreadMemberFlags} from '@fluxer/constants/src/ThreadConstants';
import {ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	guildId: string;
	channelId: string;
}

const repository = new ChannelRepository();

describe('thread message path', () => {
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
		await ensureSessionStarted(harness, owner.token);
		await ensureSessionStarted(harness, member.token);
		return {owner, member, guildId: guild.id, channelId: channel.id};
	}

	async function startThread(token: string, channelId: string, type: number = ChannelTypes.PUBLIC_THREAD) {
		return threadsRequest<ThreadChannelResponse>(harness, token)
			.post(`/channels/${channelId}/threads`)
			.body({name: 'topic', type})
			.expect(201)
			.execute();
	}

	async function send(token: string, channelId: string, content: string, status = 200, code?: string) {
		return threadsRequest<MessageResponse>(harness, token)
			.post(`/channels/${channelId}/messages`)
			.body({content})
			.expect(status, code)
			.execute();
	}

	async function getThread(token: string, threadId: string) {
		return threadsRequest<ThreadChannelResponse>(harness, token).get(`/channels/${threadId}`).execute();
	}

	async function listMessages(token: string, channelId: string, capable = true) {
		return threadsRequest<Array<MessageResponse>>(harness, token, {capable})
			.get(`/channels/${channelId}/messages`)
			.execute();
	}

	function threadState(threadId: string) {
		return repository.threads.getState(createChannelID(BigInt(threadId)));
	}

	describe('sending', () => {
		it('auto-joins the sender, counts the message and keeps ids above the thread id', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			const message = await send(s.member.token, thread.id, 'hello');
			expect(BigInt(message.id)).toBeGreaterThan(BigInt(thread.id));
			const member = await repository.threads.getMember(
				createChannelID(BigInt(thread.id)),
				createUserID(BigInt(s.member.userId)),
			);
			expect(member?.flags).toBe(ThreadMemberFlags.HAS_INTERACTED);
			const fetched = await getThread(s.member.token, thread.id);
			expect(fetched.message_count).toBe(1);
			expect(fetched.total_message_sent).toBe(1);
			expect(fetched.last_message_id).toBe(message.id);
		});

		it('never auto-joins a bot that sends into a thread', async () => {
			const s = await setup();
			const bot = await createTestBotAccount(harness);
			await authorizeBot(harness, s.owner.token, bot.appId, ['bot'], s.guildId, Permissions.ADMINISTRATOR.toString());
			const thread = await startThread(s.owner.token, s.channelId);
			await send(`Bot ${bot.botToken}`, thread.id, 'bot line');
			const member = await repository.threads.getMember(
				createChannelID(BigInt(thread.id)),
				createUserID(BigInt(bot.botUserId)),
			);
			expect(member).toBeNull();
		});

		it('unarchives an archived unlocked thread on send and refuses a locked one', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await repository.threads.updateState(createChannelID(BigInt(thread.id)), () => ({archived: true}));
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			await send(s.member.token, thread.id, 'wake up');
			expect((await threadState(thread.id))?.archived).toBe(false);
			const events = dispatch.mock.calls.map(([params]) => params.event);
			expect(events.indexOf('THREAD_UPDATE')).toBeLessThan(events.indexOf('MESSAGE_CREATE'));
			const update = dispatch.mock.calls.find(([params]) => params.event === 'THREAD_UPDATE')?.[0].data as {
				_fluxer_members?: Array<{user_id: string}>;
			};
			expect(update._fluxer_members?.map((member) => member.user_id)).toEqual([s.owner.userId]);
			await repository.threads.updateState(createChannelID(BigInt(thread.id)), () => ({archived: true, locked: true}));
			await send(s.member.token, thread.id, 'locked out', 400, APIErrorCodes.THREAD_LOCKED);
			await send(s.owner.token, thread.id, 'moderator');
			expect((await threadState(thread.id))?.archived).toBe(false);
		});

		it('checks SEND_MESSAGES_IN_THREADS instead of SEND_MESSAGES', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${s.channelId}/permissions/${s.guildId}`)
				.body({
					type: 0,
					allow: '0',
					deny: ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS.toString(),
				})
				.expect(204)
				.execute();
			await send(s.member.token, thread.id, 'nope', 403, APIErrorCodes.MISSING_PERMISSIONS);
			await sendMessage(harness, s.member.token, s.channelId, 'parent still works');
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${s.channelId}/permissions/${s.guildId}`)
				.body({type: 0, allow: '0', deny: Permissions.SEND_MESSAGES.toString()})
				.expect(204)
				.execute();
			await send(s.member.token, thread.id, 'thread works');
		});

		it('applies the thread slowmode and refuses forwarding thread created messages', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${thread.id}`)
				.body({rate_limit_per_user: 30})
				.execute();
			await send(s.member.token, thread.id, 'first');
			await send(s.member.token, thread.id, 'second', 400, APIErrorCodes.SLOWMODE_RATE_LIMITED);
			const typing = await threadsRequest<{message_send_cooldown_ms?: number}>(harness, s.member.token)
				.post(`/channels/${thread.id}/typing`)
				.expect(200)
				.execute();
			expect(typing.message_send_cooldown_ms).toBeGreaterThan(0);
			const created = (await listMessages(s.owner.token, s.channelId)).find(
				(message) => message.type === MessageTypes.THREAD_CREATED,
			);
			expect(created).toBeDefined();
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.channelId}/messages`)
				.body({message_reference: {type: 1, channel_id: s.channelId, message_id: created!.id}})
				.expect(400)
				.execute();
		});

		it('adds mentioned members who can see the parent', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await send(s.owner.token, thread.id, `hey <@${s.member.userId}>`);
			const member = await repository.threads.getMember(
				createChannelID(BigInt(thread.id)),
				createUserID(BigInt(s.member.userId)),
			);
			expect(member?.flags).toBe(0);
			const bot = await createTestBotAccount(harness);
			await authorizeBot(harness, s.owner.token, bot.appId, ['bot'], s.guildId, Permissions.ADMINISTRATOR.toString());
			await setChannelThreadsConfig({...ALL_THREADS_ACTIVE, user_basis_points: 0, included_user_ids: [s.owner.userId]});
			await send(s.owner.token, thread.id, `hey <@${bot.botUserId}>`);
			const botMember = await repository.threads.getMember(
				createChannelID(BigInt(thread.id)),
				createUserID(BigInt(bot.botUserId)),
			);
			expect(botMember?.flags).toBe(0);
		});

		it('indexes thread messages for search while the parent is still unindexed', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			expect((await repository.findUnique(createChannelID(BigInt(s.channelId))))?.indexedAt).toBeNull();
			const indexMessage = vi.spyOn(MessageSearchService.prototype, 'indexMessage');
			const message = await send(s.member.token, thread.id, 'searchable');
			expect(indexMessage).toHaveBeenCalledWith(
				expect.objectContaining({id: createMessageID(BigInt(message.id))}),
				false,
				{includeDefault: true},
			);
		});
	});

	describe('member cap', () => {
		async function fillThread(threadId: string, memberCount: number) {
			await upsertOne(
				ThreadStateTable.patchByPk(
					{thread_id: createChannelID(BigInt(threadId))},
					{member_count: {kind: 'set', value: memberCount}},
				),
			);
		}

		it('sends into a full thread without auto-joining the sender', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await repository.threads.updateState(createChannelID(BigInt(thread.id)), () => ({archived: true}));
			await fillThread(thread.id, MAX_THREAD_MEMBERS);
			await send(s.member.token, thread.id, 'still here');
			const member = await repository.threads.getMember(
				createChannelID(BigInt(thread.id)),
				createUserID(BigInt(s.member.userId)),
			);
			expect(member).toBeNull();
			const state = await threadState(thread.id);
			expect(state?.archived).toBe(false);
			expect(state?.memberCount).toBe(MAX_THREAD_MEMBERS);
		});

		it('adds mentioned members up to the remaining capacity', async () => {
			const s = await setup();
			const third = await createTestAccount(harness);
			await acceptInvite(harness, third.token, (await createChannelInvite(harness, s.owner.token, s.channelId)).code);
			const thread = await startThread(s.owner.token, s.channelId);
			await fillThread(thread.id, MAX_THREAD_MEMBERS - 1);
			await send(s.owner.token, thread.id, `hey <@${s.member.userId}> <@${third.userId}>`);
			const members = await repository.threads.getMembers(createChannelID(BigInt(thread.id)), [
				createUserID(BigInt(s.member.userId)),
				createUserID(BigInt(third.userId)),
			]);
			expect(members).toHaveLength(1);
			expect((await threadState(thread.id))?.memberCount).toBe(MAX_THREAD_MEMBERS);
		});
	});

	describe('reading', () => {
		it('attaches thread to the source for viewers and masks it for everyone else', async () => {
			const s = await setup();
			const source = await sendMessage(harness, s.member.token, s.channelId, 'source');
			const thread = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.post(`/channels/${s.channelId}/messages/${source.id}/threads`)
				.body({name: 'discussion'})
				.expect(201)
				.execute();
			const viewerSource = (await listMessages(s.member.token, s.channelId)).find((m) => m.id === source.id);
			expect(viewerSource?.flags ?? 0).toBe(ServerMessageFlags.HAS_THREAD);
			expect(viewerSource?.thread?.id).toBe(thread.id);
			const controlSource = (await listMessages(s.member.token, s.channelId, false)).find((m) => m.id === source.id);
			expect(controlSource?.flags).toBe(0);
			expect(controlSource?.thread).toBeUndefined();
			const controlReply = await threadsRequest<MessageResponse>(harness, s.member.token, {capable: false})
				.post(`/channels/${s.channelId}/messages`)
				.body({content: 'reply', message_reference: {message_id: source.id}})
				.execute();
			expect(controlReply.referenced_message?.flags).toBe(0);
			const viewerReply = await threadsRequest<MessageResponse>(harness, s.member.token)
				.post(`/channels/${s.channelId}/messages`)
				.body({content: 'reply', message_reference: {message_id: source.id}})
				.execute();
			expect(viewerReply.referenced_message?.flags).toBe(ServerMessageFlags.HAS_THREAD);
			const forward = await threadsRequest<MessageResponse>(harness, s.member.token)
				.post(`/channels/${s.channelId}/messages`)
				.body({message_reference: {type: 1, channel_id: s.channelId, message_id: source.id}})
				.execute();
			expect(forward.message_snapshots?.[0]?.flags).toBe(0);
		});

		it('synthesizes the starter and keeps the thread when the source is deleted', async () => {
			const s = await setup();
			const source = await sendMessage(harness, s.member.token, s.channelId, 'source');
			const thread = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.post(`/channels/${s.channelId}/messages/${source.id}/threads`)
				.body({name: 'discussion'})
				.expect(201)
				.execute();
			await send(s.member.token, thread.id, 'reply');
			const before = await listMessages(s.member.token, thread.id);
			const starter = before.at(-1)!;
			expect(starter).toMatchObject({id: thread.id, type: MessageTypes.THREAD_STARTER_MESSAGE});
			expect(starter.referenced_message?.id).toBe(source.id);
			const older = await threadsRequest<Array<MessageResponse>>(harness, s.member.token)
				.get(`/channels/${thread.id}/messages?before=${thread.id}`)
				.execute();
			expect(older).toEqual([]);
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content: 'reply', message_reference: {message_id: thread.id}})
				.expect(400)
				.execute();
			await createBuilder(harness, s.member.token)
				.delete(`/channels/${s.channelId}/messages/${source.id}`)
				.expect(204)
				.execute();
			const single = await threadsRequest<MessageResponse>(harness, s.member.token)
				.get(`/channels/${thread.id}/messages/${thread.id}`)
				.execute();
			expect(single.type).toBe(MessageTypes.THREAD_STARTER_MESSAGE);
			expect(single.referenced_message).toBeNull();
			await threadsRequest(harness, s.owner.token)
				.delete(`/channels/${thread.id}/messages/${thread.id}`)
				.expect(403)
				.execute();
			expect(await threadState(thread.id)).not.toBeNull();
		});

		it('decrements the message count on delete and counts system messages', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			const message = await send(s.member.token, thread.id, 'bye');
			await threadsRequest(harness, s.member.token)
				.delete(`/channels/${thread.id}/messages/${message.id}`)
				.expect(204)
				.execute();
			const fetched = await getThread(s.member.token, thread.id);
			expect(fetched.message_count).toBe(0);
			expect(fetched.total_message_sent).toBe(1);
			await threadsRequest(harness, s.owner.token)
				.delete(`/channels/${thread.id}/messages/${thread.id}`)
				.expect(404, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
			await threadsRequest(harness, s.owner.token).patch(`/channels/${thread.id}`).body({name: 'renamed'}).execute();
			const renamed = await getThread(s.member.token, thread.id);
			expect(renamed.message_count).toBe(1);
			expect(renamed.total_message_sent).toBe(2);
		});

		it('returns thread channels for bulk fetch viewers only', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await send(s.owner.token, thread.id, 'bulk');
			const body = {requests: [{channel_id: thread.id, limit: 10}]};
			const viewer = await threadsRequest<{channels: Array<{messages: Array<MessageResponse>}>}>(harness, s.owner.token)
				.post('/channels/messages/bulk')
				.body(body)
				.execute();
			expect(viewer.channels[0]?.messages.length).toBeGreaterThan(0);
			await threadsRequest(harness, s.owner.token, {capable: false})
				.post('/channels/messages/bulk')
				.body(body)
				.expect(404, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
		});
	});

	describe('modify and delete', () => {
		it('follows the thread permission rules on PATCH', async () => {
			const s = await setup();
			const thread = await startThread(s.member.token, s.channelId);
			const renamed = await threadsRequest<ThreadChannelResponse>(harness, s.member.token)
				.patch(`/channels/${thread.id}`)
				.body({name: 'renamed', auto_archive_duration: 60})
				.execute();
			expect(renamed).toMatchObject({name: 'renamed', thread_metadata: {auto_archive_duration: 60}});
			expect(Date.parse(renamed.thread_metadata!.archive_timestamp)).toBeGreaterThan(
				Date.parse(thread.thread_metadata!.archive_timestamp),
			);
			await threadsRequest(harness, s.member.token)
				.patch(`/channels/${thread.id}`)
				.body({rate_limit_per_user: 5})
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
			await threadsRequest(harness, s.member.token)
				.patch(`/channels/${thread.id}`)
				.body({flags: 1 << 4})
				.expect(400)
				.execute();
			const locked = await threadsRequest<ThreadChannelResponse>(harness, s.member.token)
				.patch(`/channels/${thread.id}`)
				.body({locked: true, archived: true})
				.execute();
			expect(locked.thread_metadata).toMatchObject({locked: true, archived: true});
			await threadsRequest(harness, s.member.token)
				.patch(`/channels/${thread.id}`)
				.body({archived: false})
				.expect(400, APIErrorCodes.THREAD_LOCKED)
				.execute();
			const reopened = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${thread.id}`)
				.body({archived: false, locked: false})
				.execute();
			expect(reopened.thread_metadata).toMatchObject({locked: false, archived: false});
		});

		it('deletes a thread for moderators only and records audit entries for viewers', async () => {
			const s = await setup();
			const thread = await startThread(s.member.token, s.channelId);
			await threadsRequest(harness, s.member.token).patch(`/channels/${thread.id}`).body({name: 'audited'}).execute();
			await threadsRequest(harness, s.member.token)
				.delete(`/channels/${thread.id}`)
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			await threadsRequest(harness, s.owner.token).delete(`/channels/${thread.id}`).expect(204).execute();
			const deleted = dispatch.mock.calls.find(([params]) => params.event === 'THREAD_DELETE')?.[0];
			expect(deleted?.data).toMatchObject({_fluxer_member_ids: [s.member.userId]});
			expect(await threadState(thread.id)).toBeNull();
			await threadsRequest(harness, s.owner.token).get(`/channels/${thread.id}`).expect(404).execute();
			const viewerLog = await threadsRequest<GuildAuditLogListResponse>(harness, s.owner.token)
				.get(`/guilds/${s.guildId}/audit-logs`)
				.execute();
			const actions = viewerLog.audit_log_entries.map((entry) => entry.action_type);
			expect(actions).toContain(AuditLogActionType.THREAD_UPDATE);
			expect(actions).toContain(AuditLogActionType.THREAD_DELETE);
			expect(viewerLog.threads).toBeDefined();
			const controlLog = await threadsRequest<GuildAuditLogListResponse>(harness, s.owner.token, {capable: false})
				.get(`/guilds/${s.guildId}/audit-logs`)
				.execute();
			expect(controlLog.audit_log_entries.map((entry) => entry.action_type)).not.toContain(
				AuditLogActionType.THREAD_UPDATE,
			);
			expect(controlLog).not.toHaveProperty('threads');
			await threadsRequest(harness, s.owner.token, {capable: false})
				.get(`/guilds/${s.guildId}/audit-logs?action_type=110`)
				.expect(400)
				.execute();
			await threadsRequest(harness, s.owner.token).get(`/guilds/${s.guildId}/audit-logs?action_type=110`).execute();
		});

		it('tags batched thread message deletes and never merges parent deletes across hidden entries', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			const remove = (channelId: string, messageId: string) =>
				threadsRequest(harness, s.owner.token)
					.delete(`/channels/${channelId}/messages/${messageId}`)
					.expect(204)
					.execute();
			const t1 = await send(s.member.token, thread.id, 't1');
			const t2 = await send(s.member.token, thread.id, 't2');
			await remove(thread.id, t1.id);
			await remove(thread.id, t2.id);
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
			await threadsRequest(harness, s.owner.token).get(`/guilds/${s.guildId}/audit-logs`).execute();
			const bulk = dispatch.mock.calls
				.map(([params]) => params)
				.find(
					(params) =>
						params.event === 'GUILD_AUDIT_LOG_ENTRY_CREATE' &&
						(params.data as {action_type: number}).action_type === AuditLogActionType.MESSAGE_BULK_DELETE,
				);
			expect(bulk?.data).toMatchObject({__thread_scoped: s.guildId, options: {channel_id: thread.id}});
			const p1 = await sendMessage(harness, s.member.token, s.channelId, 'p1');
			const p2 = await sendMessage(harness, s.member.token, s.channelId, 'p2');
			const t3 = await send(s.member.token, thread.id, 't3');
			await remove(s.channelId, p1.id);
			await remove(thread.id, t3.id);
			await remove(s.channelId, p2.id);
			const controlLog = await threadsRequest<GuildAuditLogListResponse>(harness, s.owner.token, {capable: false})
				.get(`/guilds/${s.guildId}/audit-logs`)
				.execute();
			const parentDeletes = controlLog.audit_log_entries.filter(
				(entry) =>
					entry.options?.channel_id === s.channelId &&
					(entry.action_type === AuditLogActionType.MESSAGE_DELETE ||
						entry.action_type === AuditLogActionType.MESSAGE_BULK_DELETE),
			);
			expect(parentDeletes.map((entry) => entry.action_type)).toEqual([
				AuditLogActionType.MESSAGE_DELETE,
				AuditLogActionType.MESSAGE_DELETE,
			]);
		});

		it('drops the guild auto-archive queue when the guild is deleted', async () => {
			const s = await setup();
			await startThread(s.owner.token, s.channelId);
			const queue = getKVThreadAutoArchiveQueue();
			const guildId = createGuildID(BigInt(s.guildId));
			expect(await queue.listGuilds()).toContain(guildId);
			await createBuilder(harness, s.owner.token)
				.post(`/guilds/${s.guildId}/delete`)
				.body({password: s.owner.password})
				.expect(204)
				.execute();
			expect(await queue.listGuilds()).not.toContain(guildId);
			expect(await queue.getDue(guildId, Number.MAX_SAFE_INTEGER, 10)).toEqual([]);
		});

		it('keeps message entries of a deleted thread hidden from non-viewers', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			const single = await send(s.member.token, thread.id, 'single');
			await threadsRequest(harness, s.owner.token)
				.delete(`/channels/${thread.id}/messages/${single.id}`)
				.expect(204)
				.execute();
			const t1 = await send(s.member.token, thread.id, 't1');
			const t2 = await send(s.member.token, thread.id, 't2');
			const other = await createChannel(harness, s.owner.token, s.guildId, 'spacer');
			const spacer = await sendMessage(harness, s.member.token, other.id, 'spacer');
			for (const [channelId, messageId] of [
				[other.id, spacer.id],
				[thread.id, t1.id],
				[thread.id, t2.id],
			]) {
				await threadsRequest(harness, s.owner.token)
					.delete(`/channels/${channelId}/messages/${messageId}`)
					.expect(204)
					.execute();
			}
			await threadsRequest(harness, s.owner.token).delete(`/channels/${thread.id}`).expect(204).execute();
			const viewerLog = await threadsRequest<GuildAuditLogListResponse>(harness, s.owner.token)
				.get(`/guilds/${s.guildId}/audit-logs`)
				.execute();
			const threadEntries = (log: GuildAuditLogListResponse) =>
				log.audit_log_entries.filter((entry) => entry.options?.channel_id === thread.id);
			expect(threadEntries(viewerLog).map((entry) => entry.action_type)).toEqual(
				expect.arrayContaining([AuditLogActionType.MESSAGE_DELETE, AuditLogActionType.MESSAGE_BULK_DELETE]),
			);
			const controlLog = await threadsRequest<GuildAuditLogListResponse>(harness, s.owner.token, {capable: false})
				.get(`/guilds/${s.guildId}/audit-logs`)
				.execute();
			expect(threadEntries(controlLog)).toEqual([]);
			expect(controlLog.audit_log_entries.some((entry) => entry.options?.channel_id === other.id)).toBe(true);
		});

		it('refuses invites and overwrites on threads', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${thread.id}/invites`)
				.body({})
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${thread.id}/permissions/${s.member.userId}`)
				.body({type: 1, allow: '0', deny: Permissions.SEND_MESSAGES.toString()})
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
			await threadsRequest(harness, s.owner.token, {capable: false})
				.put(`/channels/${thread.id}/permissions/${s.member.userId}`)
				.body({type: 1, allow: '0', deny: Permissions.SEND_MESSAGES.toString()})
				.expect(404, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
		});
	});

	describe('webhooks', () => {
		it('posts into a child thread and refuses foreign threads', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			const other = await createChannel(harness, s.owner.token, s.guildId, 'other');
			const otherThread = await startThread(s.owner.token, other.id);
			const webhook = await createWebhook(harness, s.channelId, s.owner.token, 'hook');
			await repository.threads.updateState(createChannelID(BigInt(thread.id)), () => ({archived: true}));
			const posted = await createBuilderWithoutAuth<MessageResponse>(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true&thread_id=${thread.id}`)
				.body({content: 'into thread'})
				.expect(200)
				.execute();
			expect(posted.channel_id).toBe(thread.id);
			expect((await threadState(thread.id))?.archived).toBe(false);
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?thread_id=${otherThread.id}`)
				.body({content: 'wrong parent'})
				.expect(404, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
			const fetched = await createBuilderWithoutAuth<MessageResponse>(harness)
				.get(`/webhooks/${webhook.id}/${webhook.token}/messages/${posted.id}?thread_id=${thread.id}`)
				.execute();
			expect(fetched.id).toBe(posted.id);
		});

		it('refuses locked threads and edits archived or locked messages but deletes with thread_id', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			const webhook = await createWebhook(harness, s.channelId, s.owner.token, 'hook');
			const hookPath = `/webhooks/${webhook.id}/${webhook.token}`;
			const threadKey = createChannelID(BigInt(thread.id));
			const posted = await createBuilderWithoutAuth<MessageResponse>(harness)
				.post(`${hookPath}?wait=true&thread_id=${thread.id}`)
				.body({content: 'first'})
				.expect(200)
				.execute();
			const edited = await createBuilderWithoutAuth<MessageResponse>(harness)
				.patch(`${hookPath}/messages/${posted.id}?thread_id=${thread.id}`)
				.body({content: 'edited'})
				.expect(200)
				.execute();
			expect(edited.content).toBe('edited');
			expect(edited.channel_id).toBe(thread.id);
			await createBuilderWithoutAuth(harness)
				.patch(`${hookPath}/messages/${posted.id}`)
				.body({content: 'parent lookup'})
				.expect(404, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
			await repository.threads.updateState(threadKey, () => ({archived: true}));
			await createBuilderWithoutAuth(harness)
				.patch(`${hookPath}/messages/${posted.id}?thread_id=${thread.id}`)
				.body({content: 'archived'})
				.expect(400, APIErrorCodes.THREAD_ARCHIVED)
				.execute();
			await repository.threads.updateState(threadKey, () => ({archived: false, locked: true}));
			await createBuilderWithoutAuth(harness)
				.patch(`${hookPath}/messages/${posted.id}?thread_id=${thread.id}`)
				.body({content: 'locked'})
				.expect(400, APIErrorCodes.THREAD_LOCKED)
				.execute();
			await createBuilderWithoutAuth(harness)
				.post(`${hookPath}?thread_id=${thread.id}`)
				.body({content: 'locked out'})
				.expect(400, APIErrorCodes.THREAD_LOCKED)
				.execute();
			expect((await getThread(s.owner.token, thread.id)).message_count).toBe(1);
			await createBuilderWithoutAuth(harness)
				.delete(`${hookPath}/messages/${posted.id}?thread_id=${thread.id}`)
				.expect(204)
				.execute();
			expect((await getThread(s.owner.token, thread.id)).message_count).toBe(0);
			await createBuilderWithoutAuth(harness)
				.get(`${hookPath}/messages/${posted.id}?thread_id=${thread.id}`)
				.expect(404, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
		});

		it('refuses thread_id on message lookups, edits and deletes in a tainted inactive guild', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			const webhook = await createWebhook(harness, s.channelId, s.owner.token, 'hook');
			const hookPath = `/webhooks/${webhook.id}/${webhook.token}`;
			const posted = await createBuilderWithoutAuth<MessageResponse>(harness)
				.post(`${hookPath}?wait=true&thread_id=${thread.id}`)
				.body({content: 'kept'})
				.expect(200)
				.execute();
			await setChannelThreadsConfig({enabled: false});
			const messagePath = `${hookPath}/messages/${posted.id}?thread_id=${thread.id}`;
			await createBuilderWithoutAuth(harness).get(messagePath).expect(404, APIErrorCodes.UNKNOWN_CHANNEL).execute();
			await createBuilderWithoutAuth(harness)
				.patch(messagePath)
				.body({content: 'refused'})
				.expect(404, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
			await createBuilderWithoutAuth(harness).delete(messagePath).expect(404, APIErrorCodes.UNKNOWN_CHANNEL).execute();
			const message = await repository.messages.getMessage(
				createChannelID(BigInt(thread.id)),
				createMessageID(BigInt(posted.id)),
			);
			expect(message?.content).toBe('kept');
		});

		it('strips thread_id in never-enabled instances and refuses it once the guild is tainted', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'control');
			const channel = await createChannel(harness, owner.token, guild.id, 'general');
			const webhook = await createWebhook(harness, channel.id, owner.token, 'hook');
			const posted = await createBuilderWithoutAuth<MessageResponse>(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true&thread_id=123`)
				.body({content: 'parent'})
				.expect(200)
				.execute();
			expect(posted.channel_id).toBe(channel.id);
			await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
			await startThread(owner.token, channel.id);
			await setChannelThreadsConfig({enabled: false});
			await createBuilderWithoutAuth(harness)
				.post(`/webhooks/${webhook.id}/${webhook.token}?thread_id=123`)
				.body({content: 'refused'})
				.expect(404, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
		});
	});
});

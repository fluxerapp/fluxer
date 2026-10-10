// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createUserID} from '@app/api/BrandedTypes';
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
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, MessageFlags, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_ACTIVE_THREADS_PER_GUILD, ThreadMemberFlags} from '@fluxer/constants/src/ThreadConstants';
import {ThreadPermissionFlags} from '@fluxer/constants/src/ThreadPermissionUtils';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {GuildRoleResponse} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	guildId: string;
	channelId: string;
}

const repository = new ChannelRepository();

function threadId(id: string) {
	return createChannelID(BigInt(id));
}

describe('thread limits and timeouts', () => {
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

	async function joinGuild(owner: TestAccount, channelId: string): Promise<TestAccount> {
		const account = await createTestAccount(harness);
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, account.token, invite.code);
		await ensureSessionStarted(harness, account.token);
		return account;
	}

	async function setup(): Promise<Setup> {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'threads');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		await ensureSessionStarted(harness, owner.token);
		const member = await joinGuild(owner, channel.id);
		return {owner, member, guildId: guild.id, channelId: channel.id};
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

	async function timeOut(s: Setup, userId: string): Promise<void> {
		await threadsRequest(harness, s.owner.token)
			.patch(`/guilds/${s.guildId}/members/${userId}`)
			.body({communication_disabled_until: new Date(Date.now() + 10 * 60 * 1000).toISOString()})
			.expect(200)
			.execute();
	}

	function capActiveThreads() {
		return vi.spyOn(ThreadRepository.prototype, 'countActiveThreads').mockResolvedValue(MAX_ACTIVE_THREADS_PER_GUILD);
	}

	describe('active thread cap', () => {
		it('refuses new threads at the cap without writing a thread, a member or a slowmode charge', async () => {
			const s = await setup();
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.channelId}`)
				.body({rate_limit_per_user: 60})
				.expect(200)
				.execute();
			const message = await sendMessage(harness, s.owner.token, s.channelId, 'x');
			const cap = capActiveThreads();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/threads`)
				.body({name: 'capped', type: ChannelTypes.PUBLIC_THREAD})
				.expect(400, APIErrorCodes.MAX_ACTIVE_THREADS)
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
				.body({name: 'capped'})
				.expect(400, APIErrorCodes.MAX_ACTIVE_THREADS)
				.execute();
			cap.mockRestore();
			expect(await repository.threads.listActiveThreads(createGuildID(BigInt(s.guildId)))).toEqual([]);
			expect(await repository.threads.getState(threadId(message.id))).toBeNull();
			expect(await repository.threads.listMembers(threadId(message.id), {limit: 10})).toEqual([]);
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.channelId}/messages/${message.id}/threads`)
				.body({name: 'allowed'})
				.expect(201)
				.execute();
		});

		it('refuses unarchiving by PATCH and by send at the cap', async () => {
			const s = await setup();
			const thread = await startThread(s.owner.token, s.channelId);
			await repository.threads.updateState(threadId(thread.id), () => ({archived: true}));
			const cap = capActiveThreads();
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${thread.id}`)
				.body({archived: false})
				.expect(400, APIErrorCodes.MAX_ACTIVE_THREADS)
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content: 'wake up'})
				.expect(400, APIErrorCodes.MAX_ACTIVE_THREADS)
				.execute();
			cap.mockRestore();
			expect((await repository.threads.getState(threadId(thread.id)))?.archived).toBe(true);
			expect(await repository.threads.getMember(threadId(thread.id), createUserID(BigInt(s.member.userId)))).toBeNull();
			const messages = await threadsRequest<Array<MessageResponse>>(harness, s.owner.token)
				.get(`/channels/${thread.id}/messages`)
				.execute();
			expect(messages.map((m) => m.content)).not.toContain('wake up');
		});
	});

	describe('timeouts', () => {
		it('blocks thread writes for a timed-out member but still lets them join, leave, change settings and read', async () => {
			const s = await setup();
			const own = await startThread(s.member.token, s.channelId, {name: 'mine', type: ChannelTypes.PUBLIC_THREAD});
			await threadsRequest(harness, s.owner.token).put(`/channels/${own.id}/thread-members/@me`).expect(204).execute();
			const other = await startThread(s.owner.token, s.channelId);
			await timeOut(s, s.member.userId);
			const denied = [
				threadsRequest(harness, s.member.token)
					.post(`/channels/${s.channelId}/threads`)
					.body({name: 'new', type: ChannelTypes.PUBLIC_THREAD}),
				threadsRequest(harness, s.member.token).patch(`/channels/${own.id}`).body({name: 'renamed'}),
				threadsRequest(harness, s.member.token).put(`/channels/${other.id}/thread-members/${s.owner.userId}`),
				threadsRequest(harness, s.member.token).delete(`/channels/${own.id}/thread-members/${s.owner.userId}`),
				threadsRequest(harness, s.member.token).delete(`/channels/${own.id}`),
			];
			for (const request of denied) {
				await request.expect(403, APIErrorCodes.COMMUNICATION_DISABLED).execute();
			}
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${other.id}/thread-members/@me`)
				.expect(204)
				.execute();
			await threadsRequest(harness, s.member.token)
				.patch(`/channels/${other.id}/thread-members/@me/settings`)
				.body({flags: ThreadMemberFlags.ONLY_MENTIONS})
				.expect(200)
				.execute();
			await threadsRequest(harness, s.member.token).get(`/channels/${other.id}/messages`).expect(200).execute();
			await threadsRequest(harness, s.member.token)
				.delete(`/channels/${other.id}/thread-members/@me`)
				.expect(204)
				.execute();
			expect((await repository.threads.getState(threadId(own.id)))?.archived).toBe(false);
		});

		it('blocks adding reactions and editing own messages but not removals or moderator edits', async () => {
			const s = await setup();
			const role = await threadsRequest<GuildRoleResponse>(harness, s.owner.token)
				.post(`/guilds/${s.guildId}/roles`)
				.body({name: 'mods', permissions: Permissions.MANAGE_MESSAGES.toString()})
				.execute();
			await addMemberRole(harness, s.owner.token, s.guildId, s.member.userId, role.id);
			const thread = await startThread(s.owner.token, s.channelId);
			const ownerMessage = await threadsRequest<MessageResponse>(harness, s.owner.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content: 'owner line'})
				.execute();
			const memberMessage = await threadsRequest<MessageResponse>(harness, s.member.token)
				.post(`/channels/${thread.id}/messages`)
				.body({content: 'member line'})
				.execute();
			const reaction = `/channels/${thread.id}/messages/${ownerMessage.id}/reactions/${encodeURIComponent('👍')}/@me`;
			await threadsRequest(harness, s.member.token).put(reaction).expect(204).execute();
			await timeOut(s, s.member.userId);
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/messages/${memberMessage.id}/reactions/${encodeURIComponent('👍')}/@me`)
				.expect(403, APIErrorCodes.COMMUNICATION_DISABLED)
				.execute();
			await threadsRequest(harness, s.member.token)
				.patch(`/channels/${thread.id}/messages/${memberMessage.id}`)
				.body({content: 'edited'})
				.expect(403, APIErrorCodes.COMMUNICATION_DISABLED)
				.execute();
			await threadsRequest(harness, s.member.token).delete(reaction).expect(204).execute();
			const suppressed = await threadsRequest<MessageResponse>(harness, s.member.token)
				.patch(`/channels/${thread.id}/messages/${ownerMessage.id}`)
				.body({flags: MessageFlags.SUPPRESS_EMBEDS})
				.expect(200)
				.execute();
			expect(suppressed.flags & MessageFlags.SUPPRESS_EMBEDS).not.toBe(0);
		});

		it('hides a private thread from a timed-out thread moderator who is not a member', async () => {
			const s = await setup();
			const role = await threadsRequest<GuildRoleResponse>(harness, s.owner.token)
				.post(`/guilds/${s.guildId}/roles`)
				.body({name: 'mods', permissions: ThreadPermissionFlags.MANAGE_THREADS.toString()})
				.execute();
			await addMemberRole(harness, s.owner.token, s.guildId, s.member.userId, role.id);
			const thread = await startThread(s.owner.token, s.channelId, {name: 'p', type: ChannelTypes.PRIVATE_THREAD});
			await threadsRequest(harness, s.member.token).get(`/channels/${thread.id}`).expect(200).execute();
			await timeOut(s, s.member.userId);
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/@me`)
				.expect(403, APIErrorCodes.MISSING_ACCESS)
				.execute();
		});
	});

	describe('null patch fields', () => {
		it('treats null archived, locked and invitable as no change', async () => {
			const s = await setup();
			const thread = await startThread(s.member.token, s.channelId);
			const patched = await threadsRequest<ThreadChannelResponse>(harness, s.member.token)
				.patch(`/channels/${thread.id}`)
				.body({archived: null, locked: null, rate_limit_per_user: null, name: 'renamed'})
				.expect(200)
				.execute();
			expect(patched.name).toBe('renamed');
			expect(patched.thread_metadata?.archived).toBe(false);
			expect(patched.thread_metadata?.locked).toBe(false);
			expect(patched.rate_limit_per_user ?? 0).toBe(thread.rate_limit_per_user ?? 0);
			const privateThread = await startThread(s.owner.token, s.channelId, {
				name: 'private',
				type: ChannelTypes.PRIVATE_THREAD,
				invitable: false,
			});
			const privatePatched = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${privateThread.id}`)
				.body({invitable: null, locked: null})
				.expect(200)
				.execute();
			expect(privatePatched.thread_metadata?.invitable).toBe(false);
			expect(privatePatched.thread_metadata?.locked).toBe(false);
		});

		it('leaves the channel row untouched when the thread state vanishes mid patch', async () => {
			const s = await setup();
			const thread = await startThread(s.member.token, s.channelId);
			vi.spyOn(ThreadRepository.prototype, 'updateState').mockResolvedValueOnce(null);
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${thread.id}`)
				.body({name: 'renamed', rate_limit_per_user: 30})
				.expect(404, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
			const stored = await repository.channelData.findUnique(threadId(thread.id));
			expect(stored?.name).toBe('topic');
			expect(stored?.rateLimitPerUser ?? 0).toBe(thread.rate_limit_per_user ?? 0);
		});

		it('clears slowmode on null rate_limit_per_user for moderators only', async () => {
			const s = await setup();
			const thread = await startThread(s.member.token, s.channelId);
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${thread.id}`)
				.body({rate_limit_per_user: 30})
				.expect(200)
				.execute();
			await threadsRequest(harness, s.member.token)
				.patch(`/channels/${thread.id}`)
				.body({rate_limit_per_user: null})
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
			const cleared = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${thread.id}`)
				.body({rate_limit_per_user: null})
				.expect(200)
				.execute();
			expect(cleared.rate_limit_per_user ?? 0).toBe(0);
		});
	});

	describe('unarchive by members', () => {
		it('lets a member who is not the owner unarchive without send permission and refuses non-members', async () => {
			const s = await setup();
			const outsider = await joinGuild(s.owner, s.channelId);
			const thread = await startThread(s.owner.token, s.channelId);
			await threadsRequest(harness, s.member.token)
				.put(`/channels/${thread.id}/thread-members/@me`)
				.expect(204)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${s.channelId}/permissions/${s.guildId}`)
				.body({type: 0, allow: '0', deny: ThreadPermissionFlags.SEND_MESSAGES_IN_THREADS.toString()})
				.expect(204)
				.execute();
			await repository.threads.updateState(threadId(thread.id), () => ({archived: true}));
			await threadsRequest(harness, outsider.token)
				.patch(`/channels/${thread.id}`)
				.body({archived: false})
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
			const reopened = await threadsRequest<ThreadChannelResponse>(harness, s.member.token)
				.patch(`/channels/${thread.id}`)
				.body({
					archived: false,
					name: thread.name,
					auto_archive_duration: thread.thread_metadata!.auto_archive_duration,
					rate_limit_per_user: thread.rate_limit_per_user ?? 0,
				})
				.expect(200)
				.execute();
			expect(reopened.thread_metadata?.archived).toBe(false);
			await threadsRequest(harness, s.member.token)
				.patch(`/channels/${thread.id}`)
				.body({name: 'renamed'})
				.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});
	});
});

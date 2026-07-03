// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {sendMessage} from '../../message/tests/MessageTestUtils';
import {ChannelDataRepository} from '../repositories/ChannelDataRepository';
import {
	acceptInvite,
	addMemberRole,
	createChannel,
	createChannelInvite,
	createGuild,
	createPermissionOverwrite,
	createRole,
	createThread,
	deleteThread,
	getThread,
	joinThread,
	leaveThread,
	listThreadMessages,
	listThreads,
	updateThread,
} from './ChannelTestUtils';

describe('Thread Lifecycle', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});

	it('guild owner can create a thread in a text channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Test Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const thread = await createThread(harness, owner.token, channel.id, 'My First Thread');

		expect(thread.type).toBe(11);
		expect(thread.name).toBe('My First Thread');
		expect(thread.thread_parent_channel_id).toBe(channel.id);
		expect(thread.thread_state).toBe(0);
	});

	it('created thread appears in list', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread List Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		await createThread(harness, owner.token, channel.id, 'Thread Alpha');
		await createThread(harness, owner.token, channel.id, 'Thread Beta');

		const threads = await listThreads(harness, owner.token, channel.id);

		expect(threads).toHaveLength(2);
		const names = threads.map((t) => t.name as string);
		expect(names).toContain('Thread Alpha');
		expect(names).toContain('Thread Beta');
	});

	it('can fetch a specific thread', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Get Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Fetchable Thread');

		const fetched = await getThread(harness, owner.token, channel.id, thread.id as string);

		expect(fetched.id).toBe(thread.id);
		expect(fetched.name).toBe('Fetchable Thread');
	});

	it('member can join and leave a thread', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Join Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const thread = await createThread(harness, owner.token, channel.id, 'Joinable Thread');

		await joinThread(harness, member.token, channel.id, thread.id as string);
		await leaveThread(harness, member.token, channel.id, thread.id as string);
	});

	it('owner can delete a thread', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Delete Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Deletable Thread');

		await deleteThread(harness, owner.token, channel.id, thread.id as string);

		await createBuilder(harness, owner.token)
			.get(`/channels/${channel.id}/threads/${thread.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	it('member without CREATE_THREADS cannot create a thread', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Perm Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		await createBuilder(harness, member.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'Unauthorized Thread'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	it('member without MANAGE_THREADS cannot delete a thread', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Delete Perm Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const thread = await createThread(harness, owner.token, channel.id, 'Protected Thread');

		await createBuilder(harness, member.token)
			.delete(`/channels/${channel.id}/threads/${thread.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	it('returns 404 for thread belonging to different channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Isolation Guild');
		const channelA = await createChannel(harness, owner.token, guild.id, 'channel-a');
		const channelB = await createChannel(harness, owner.token, guild.id, 'channel-b');
		const thread = await createThread(harness, owner.token, channelA.id, 'Thread in A');

		await createBuilder(harness, owner.token)
			.get(`/channels/${channelB.id}/threads/${thread.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	it('thread name is required', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Validation Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		await createBuilder(harness, owner.token)
			.post(`/channels/${channel.id}/threads`)
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('update thread name', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Update Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Old Name');

		const updated = await createBuilder<Record<string, unknown>>(harness, owner.token)
			.patch(`/channels/${channel.id}/threads/${thread.id}`)
			.body({name: 'New Name'})
			.execute();

		expect(updated.name).toBe('New Name');
	});

	it('cannot create thread in a category channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Category Guild');
		const category = await createChannel(harness, owner.token, guild.id, 'my-category', 4);

		await createBuilder(harness, owner.token)
			.post(`/channels/${category.id}/threads`)
			.body({name: 'Thread in Category'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('member with MANAGE_THREADS can delete a thread', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Manage Threads Grant Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const modRole = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			permissions: Permissions.MANAGE_THREADS.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, modRole.id);

		const thread = await createThread(harness, owner.token, channel.id, 'Deletable by Mod');

		await createBuilder(harness, member.token)
			.delete(`/channels/${channel.id}/threads/${thread.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});

	it('member with MANAGE_THREADS can rename a thread', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Rename Thread Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const modRole = await createRole(harness, owner.token, guild.id, {
			name: 'Moderator',
			permissions: Permissions.MANAGE_THREADS.toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, modRole.id);

		const thread = await createThread(harness, owner.token, channel.id, 'Old Name');

		const updated = await createBuilder<Record<string, unknown>>(harness, member.token)
			.patch(`/channels/${channel.id}/threads/${thread.id}`)
			.body({name: 'Renamed by Mod'})
			.execute();

		expect(updated.name).toBe('Renamed by Mod');
	});

	it('member without MANAGE_THREADS cannot rename a thread', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'No Rename Thread Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const thread = await createThread(harness, owner.token, channel.id, 'Locked Name');

		await createBuilder(harness, member.token)
			.patch(`/channels/${channel.id}/threads/${thread.id}`)
			.body({name: 'Attempted Rename'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	it('member without SEND_MESSAGES_IN_THREADS cannot post in a thread', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Msg Perm Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const thread = await createThread(harness, owner.token, channel.id, 'Restricted Thread');
		await joinThread(harness, member.token, channel.id, thread.id as string);

		await createPermissionOverwrite(harness, owner.token, thread.id as string, member.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.SEND_MESSAGES_IN_THREADS.toString(),
		});

		await createBuilder(harness, member.token)
			.post(`/channels/${thread.id}/messages`)
			.body({content: 'should be blocked'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	it('member with SEND_MESSAGES_IN_THREADS can post in a thread', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Msg Allow Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const thread = await createThread(harness, owner.token, channel.id, 'Open Thread');
		await joinThread(harness, member.token, channel.id, thread.id as string);

		const message = await createBuilder<Record<string, unknown>>(harness, member.token)
			.post(`/channels/${thread.id}/messages`)
			.body({content: 'hello thread'})
			.execute();

		expect(message.content).toBe('hello thread');
		expect(message.channel_id).toBe(thread.id);
	});

	it('GET /channels/:thread_id/thread-members returns creator after creation', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Members Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Members Test Thread');

		const members = await createBuilder<Array<Record<string, unknown>>>(harness, owner.token)
			.get(`/channels/${thread.id}/thread-members`)
			.execute();

		expect(members).toHaveLength(1);
		expect(members[0].user_id).toBe(owner.userId);
		expect(members[0].thread_id).toBe(thread.id);
	});

	it('GET /channels/:thread_id/thread-members reflects joins and leaves', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Thread Members Join Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const thread = await createThread(harness, owner.token, channel.id, 'Join Leave Thread');

		await joinThread(harness, member.token, channel.id, thread.id as string);

		const afterJoin = await createBuilder<Array<Record<string, unknown>>>(harness, owner.token)
			.get(`/channels/${thread.id}/thread-members`)
			.execute();
		expect(afterJoin).toHaveLength(2);

		await leaveThread(harness, member.token, channel.id, thread.id as string);

		const afterLeave = await createBuilder<Array<Record<string, unknown>>>(harness, owner.token)
			.get(`/channels/${thread.id}/thread-members`)
			.execute();
		expect(afterLeave).toHaveLength(1);
		expect(afterLeave[0].user_id).toBe(owner.userId);
	});

	it('GET /users/@me/thread-members returns threads joined by user', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'My Threads Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const threadA = await createThread(harness, owner.token, channel.id, 'Thread A');
		const threadB = await createThread(harness, owner.token, channel.id, 'Thread B');
		await joinThread(harness, member.token, channel.id, threadA.id as string);

		const ownerThreads = await createBuilder<Array<Record<string, unknown>>>(harness, owner.token)
			.get('/users/@me/thread-members')
			.execute();
		const memberThreads = await createBuilder<Array<Record<string, unknown>>>(harness, member.token)
			.get('/users/@me/thread-members')
			.execute();

		const ownerIds = ownerThreads.map((t) => t.id);
		expect(ownerIds).toContain(threadA.id);
		expect(ownerIds).toContain(threadB.id);

		const memberIds = memberThreads.map((t) => t.id);
		expect(memberIds).toContain(threadA.id);
		expect(memberIds).not.toContain(threadB.id);
	});

	it('GET /users/@me/thread-members does not include thread after leaving', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Leave Sync Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const thread = await createThread(harness, owner.token, channel.id, 'Leave Test Thread');
		await joinThread(harness, member.token, channel.id, thread.id as string);

		const beforeLeave = await createBuilder<Array<Record<string, unknown>>>(harness, member.token)
			.get('/users/@me/thread-members')
			.execute();
		expect(beforeLeave.map((t) => t.id)).toContain(thread.id);

		await leaveThread(harness, member.token, channel.id, thread.id as string);

		const afterLeave = await createBuilder<Array<Record<string, unknown>>>(harness, member.token)
			.get('/users/@me/thread-members')
			.execute();
		expect(afterLeave.map((t) => t.id)).not.toContain(thread.id);
	});

	it('creating a thread writes a THREAD_CREATED system message to the parent channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'System Msg Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const thread = await createThread(harness, owner.token, channel.id, 'My Thread');

		const messages = await listThreadMessages(harness, owner.token, channel.id);
		const systemMsg = messages.find((m) => m.type === 18);
		expect(systemMsg).toBeDefined();
		expect(systemMsg?.content).toBe(thread.name);
	});

	it('renaming a thread writes a CHANNEL_NAME_CHANGE message inside the thread', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Rename Msg Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Old Name');

		await updateThread(harness, owner.token, channel.id, thread.id as string, {name: 'New Name'});

		const messages = await listThreadMessages(harness, owner.token, thread.id as string);
		const nameChangeMsg = messages.find((m) => m.type === 4);
		expect(nameChangeMsg).toBeDefined();
		expect(nameChangeMsg?.content).toBe('New Name');
	});

	it('rename with same name does not write CHANNEL_NAME_CHANGE', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'No Rename Msg Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Stable Name');

		await updateThread(harness, owner.token, channel.id, thread.id as string, {name: 'Stable Name'});

		const messages = await listThreadMessages(harness, owner.token, thread.id as string);
		const nameChangeMsgs = messages.filter((m) => m.type === 4);
		expect(nameChangeMsgs).toHaveLength(0);
	});

	it('rate_limit_per_user is accepted in updateThread', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Slowmode Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Slowmode Thread');

		const updated = await updateThread(harness, owner.token, channel.id, thread.id as string, {
			rate_limit_per_user: 30,
		});

		expect(updated.rate_limit_per_user).toBe(30);
	});

	it('creating a thread with a source message writes a THREAD_STARTED message inside the thread', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Starter Msg Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const sourceMsg = await sendMessage(harness, owner.token, channel.id, 'source content') as unknown as Record<string, unknown>;

		const thread = await createThread(
			harness,
			owner.token,
			channel.id,
			'From Source',
			sourceMsg.id as string,
		);

		const messages = await listThreadMessages(harness, owner.token, thread.id as string);
		const starterMsg = messages.find((m) => m.type === 21);
		expect(starterMsg).toBeDefined();
		expect(starterMsg?.content).toBe('source content');
	});

	it('member cannot create a second thread from the same source message', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Duplicate Thread Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const sourceMsg = await sendMessage(harness, owner.token, channel.id, 'original') as unknown as Record<string, unknown>;

		await createThread(harness, owner.token, channel.id, 'First Thread', sourceMsg.id as string);

		await createBuilder(harness, owner.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'Duplicate Thread', source_message_id: sourceMsg.id})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('updateThread rejects invalid rate_limit_per_user', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Invalid Slowmode Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Slowmode Thread 2');

		await createBuilder(harness, owner.token)
			.patch(`/channels/${channel.id}/threads/${thread.id}`)
			.body({rate_limit_per_user: -1})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});

describe('Thread Auto-Archive', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});

	it('open thread with past expiry appears in listExpiredOpenThreads', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Archive Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const thread = await createThread(harness, owner.token, channel.id, 'Expiring Thread');

		const repo = new ChannelDataRepository();
		const farFuture = new Date(Date.now() + 99_999_999_000);
		const expired = await repo.listExpiredOpenThreads(farFuture, 200);
		expect(expired.map(String)).toContain(thread.id);
	});

	it('open thread with future expiry does not appear in listExpiredOpenThreads with current time', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Active Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const thread = await createThread(harness, owner.token, channel.id, 'Active Thread');

		const repo = new ChannelDataRepository();
		const now = new Date();
		const expired = await repo.listExpiredOpenThreads(now, 200);
		expect(expired.map(String)).not.toContain(thread.id);
	});

	it('closing a thread removes it from listExpiredOpenThreads', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Close Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const thread = await createThread(harness, owner.token, channel.id, 'Closed Thread');
		await updateThread(harness, owner.token, channel.id, thread.id as string, {state: 1});

		const repo = new ChannelDataRepository();
		const farFuture = new Date(Date.now() + 99_999_999_000);
		const expired = await repo.listExpiredOpenThreads(farFuture, 200);
		expect(expired.map(String)).not.toContain(thread.id);
	});

	it('deleted thread is not returned by listExpiredOpenThreads', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Deleted Thread Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const thread = await createThread(harness, owner.token, channel.id, 'To Be Deleted');
		await deleteThread(harness, owner.token, channel.id, thread.id as string);

		const repo = new ChannelDataRepository();
		const farFuture = new Date(Date.now() + 99_999_999_000);
		const expired = await repo.listExpiredOpenThreads(farFuture, 200);
		expect(expired.map(String)).not.toContain(thread.id);
	});

	it('reopening a closed thread restores it in listExpiredOpenThreads', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Reopen Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const thread = await createThread(harness, owner.token, channel.id, 'Reopen Thread');
		await updateThread(harness, owner.token, channel.id, thread.id as string, {state: 1});
		await updateThread(harness, owner.token, channel.id, thread.id as string, {state: 0});

		const repo = new ChannelDataRepository();
		const farFuture = new Date(Date.now() + 99_999_999_000);
		const expired = await repo.listExpiredOpenThreads(farFuture, 200);
		expect(expired.map(String)).toContain(thread.id);
	});
});

describe('Thread Phase 2 — member count and message count', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});

	it('thread_member_count_actual increments on join and decrements on leave', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Member Count Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);

		const thread = await createThread(harness, owner.token, channel.id, 'Count Thread');
		const afterCreate = await getThread(harness, owner.token, channel.id, thread.id as string);
		expect(afterCreate.thread_member_count_actual).toBe(1);

		await joinThread(harness, member.token, channel.id, thread.id as string);
		const afterJoin = await getThread(harness, owner.token, channel.id, thread.id as string);
		expect(afterJoin.thread_member_count_actual).toBe(2);

		await leaveThread(harness, member.token, channel.id, thread.id as string);
		const afterLeave = await getThread(harness, owner.token, channel.id, thread.id as string);
		expect(afterLeave.thread_member_count_actual).toBe(1);
	});

	it('thread_total_message_sent increments on each message and never decrements', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Total Msg Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Msg Count Thread');

		const msg = await createBuilder<Record<string, unknown>>(harness, owner.token)
			.post(`/channels/${thread.id}/messages`)
			.body({content: 'first'})
			.execute();

		await createBuilder<Record<string, unknown>>(harness, owner.token)
			.post(`/channels/${thread.id}/messages`)
			.body({content: 'second'})
			.execute();

		const fetched = await getThread(harness, owner.token, channel.id, thread.id as string);
		expect(fetched.thread_total_message_sent).toBeGreaterThanOrEqual(2);

		await createBuilder<void>(harness, owner.token)
			.delete(`/channels/${thread.id}/messages/${msg.id}`)
			.expect(204)
			.execute();

		const afterDelete = await getThread(harness, owner.token, channel.id, thread.id as string);
		expect(afterDelete.thread_total_message_sent).toBeGreaterThanOrEqual(2);
	});

	it('duplicate thread from same source message is rejected', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Dedup Source Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const sourceMsg = await sendMessage(harness, owner.token, channel.id, 'source') as unknown as Record<string, unknown>;

		await createThread(harness, owner.token, channel.id, 'First', sourceMsg.id as string);

		await createBuilder(harness, owner.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'Dupe', source_message_id: sourceMsg.id})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});

describe('Thread Metadata', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});

	it('new thread has thread_metadata with archived=false and locked=false', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Metadata Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');

		const thread = await createThread(harness, owner.token, channel.id, 'Meta Thread') as Record<string, unknown>;

		expect((thread.thread_metadata as Record<string, unknown>).archived).toBe(false);
		expect((thread.thread_metadata as Record<string, unknown>).locked).toBe(false);
		expect((thread.thread_metadata as Record<string, unknown>).auto_archive_duration).toBeGreaterThan(0);
	});

	it('archiving a thread sets thread_metadata.archived=true', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Archive Meta Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Archive Me');

		const updated = await updateThread(harness, owner.token, channel.id, thread.id as string, {archived: true}) as Record<string, unknown>;

		expect((updated.thread_metadata as Record<string, unknown>).archived).toBe(true);
		expect(updated.thread_state).toBe(2);
	});

	it('locking a thread sets thread_metadata.locked=true', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Lock Meta Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Lock Me');

		const updated = await updateThread(harness, owner.token, channel.id, thread.id as string, {locked: true}) as Record<string, unknown>;

		expect((updated.thread_metadata as Record<string, unknown>).locked).toBe(true);
	});

	it('setting auto_archive_duration is stored and reflected in thread_metadata', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Duration Meta Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Duration Thread');

		const updated = await updateThread(harness, owner.token, channel.id, thread.id as string, {auto_archive_duration: 1440}) as Record<string, unknown>;

		expect((updated.thread_metadata as Record<string, unknown>).auto_archive_duration).toBe(1440);
	});

	it('unarchiving sets thread_metadata.archived=false', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Unarchive Meta Guild');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const thread = await createThread(harness, owner.token, channel.id, 'Unarchive Me');

		await updateThread(harness, owner.token, channel.id, thread.id as string, {archived: true});
		const unarchived = await updateThread(harness, owner.token, channel.id, thread.id as string, {archived: false}) as Record<string, unknown>;

		expect((unarchived.thread_metadata as Record<string, unknown>).archived).toBe(false);
		expect(unarchived.thread_state).toBe(0);
	});
});

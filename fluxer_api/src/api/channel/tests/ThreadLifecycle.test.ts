// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {
	ChannelResponse,
	ThreadListResponse,
	ThreadMemberResponse,
} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount, type TestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {
	createChannel,
	getChannel,
	getGuild,
	sendChannelMessage,
	setupTestGuildWithMembers,
	updateRole,
} from './ChannelTestUtils';

async function createThreadFromMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	messageId: string,
	name: string,
): Promise<ChannelResponse> {
	return createBuilder<ChannelResponse>(harness, token)
		.post(`/channels/${channelId}/messages/${messageId}/threads`)
		.body({name})
		.expect(HTTP_STATUS.CREATED)
		.execute();
}

async function listMessages(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
): Promise<Array<MessageResponse>> {
	return createBuilder<Array<MessageResponse>>(harness, token).get(`/channels/${channelId}/messages`).execute();
}

async function setEveryonePermissions(
	harness: ApiTestHarness,
	token: string,
	guildId: string,
	permissions: bigint,
): Promise<void> {
	await updateRole(harness, token, guildId, guildId, {permissions: permissions.toString()});
}

const BASE_MEMBER_PERMISSIONS =
	Permissions.VIEW_CHANNEL |
	Permissions.SEND_MESSAGES |
	Permissions.READ_MESSAGE_HISTORY |
	Permissions.CREATE_PUBLIC_THREADS |
	Permissions.SEND_MESSAGES_IN_THREADS;

describe('Thread lifecycle', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function setupThread(): Promise<{
		owner: TestAccount;
		members: Array<TestAccount>;
		guildId: string;
		channelId: string;
		starter: MessageResponse;
		thread: ChannelResponse;
	}> {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const starter = await sendChannelMessage(harness, owner.token, systemChannel.id, 'thread me');
		const thread = await createThreadFromMessage(harness, owner.token, systemChannel.id, starter.id, 'Bug hunt');
		return {owner, members, guildId: guild.id, channelId: systemChannel.id, starter, thread};
	}

	it('creates a thread from a message that shares the starter message id', async () => {
		const {starter, thread, channelId, owner} = await setupThread();
		expect(thread.id).toBe(starter.id);
		expect(thread.type).toBe(11);
		expect(thread.parent_id).toBe(channelId);
		expect(thread.owner_id).toBe(owner.userId);
		expect(thread.name).toBe('Bug hunt');
		expect(thread.thread_metadata?.archived).toBe(false);
		expect(thread.thread_metadata?.locked).toBe(false);
		expect(thread.thread_metadata?.auto_archive_duration).toBe(10080);
		expect(thread.member_count).toBe(1);
		expect(thread.member?.user_id).toBe(owner.userId);
	});

	it('posts the thread system messages in the parent and the thread', async () => {
		const {owner, channelId, thread} = await setupThread();
		const parentMessages = await listMessages(harness, owner.token, channelId);
		expect(parentMessages.some((message) => message.type === MessageTypes.THREAD_CREATED)).toBe(true);
		const threadMessages = await listMessages(harness, owner.token, thread.id);
		expect(threadMessages.some((message) => message.type === MessageTypes.THREAD_STARTER_MESSAGE)).toBe(true);
	});

	it('rejects starting a second thread from the same message', async () => {
		const {owner, channelId, starter} = await setupThread();
		await createBuilder<unknown>(harness, owner.token)
			.post(`/channels/${channelId}/messages/${starter.id}/threads`)
			.body({name: 'Second thread'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});

	it('creates a standalone thread with its own id', async () => {
		const {owner, guild, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		await sendChannelMessage(harness, owner.token, systemChannel.id, 'hello');
		const thread = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/channels/${systemChannel.id}/threads`)
			.body({name: 'Standalone', auto_archive_duration: 1440})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		expect(thread.type).toBe(11);
		expect(thread.parent_id).toBe(systemChannel.id);
		expect(thread.thread_metadata?.auto_archive_duration).toBe(1440);
		expect(thread.guild_id).toBe(guild.id);
	});

	it('keeps threads out of the guild channel list', async () => {
		const {owner, guildId, thread} = await setupThread();
		const guild = await getGuild(harness, owner.token, guildId);
		const channelIds = (guild.channels ?? []).map((channel) => channel.id);
		expect(channelIds).not.toContain(thread.id);
		const fetched = await getChannel(harness, owner.token, thread.id);
		expect(fetched.id).toBe(thread.id);
	});

	it('renames a thread and records a name-change system message', async () => {
		const {owner, thread} = await setupThread();
		const renamed = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${thread.id}`)
			.body({name: 'Renamed hunt'})
			.execute();
		expect(renamed.name).toBe('Renamed hunt');
		const threadMessages = await listMessages(harness, owner.token, thread.id);
		expect(threadMessages.some((message) => message.type === MessageTypes.CHANNEL_NAME_CHANGE)).toBe(true);
	});

	it('prevents non-creators without MANAGE_THREADS from renaming', async () => {
		const {owner, members, guildId, thread} = await setupThread();
		await setEveryonePermissions(harness, owner.token, guildId, BASE_MEMBER_PERMISSIONS);
		await createBuilder<unknown>(harness, members[0].token)
			.patch(`/channels/${thread.id}`)
			.body({name: 'Hijacked'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	it('archives and unarchives a thread', async () => {
		const {owner, members, thread} = await setupThread();
		const archived = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${thread.id}`)
			.body({archived: true})
			.execute();
		expect(archived.thread_metadata?.archived).toBe(true);
		expect(archived.thread_metadata?.archive_timestamp).toBeTruthy();
		await createBuilder<unknown>(harness, members[0].token)
			.put(`/channels/${thread.id}/thread-members/@me`)
			.body(null)
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		const unarchived = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${thread.id}`)
			.body({archived: false})
			.execute();
		expect(unarchived.thread_metadata?.archived).toBe(false);
	});

	it('requires MANAGE_THREADS to unarchive a locked thread', async () => {
		const {owner, members, guildId, thread} = await setupThread();
		await createBuilder<ChannelResponse>(harness, members[0].token)
			.put(`/channels/${thread.id}/thread-members/@me`)
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${thread.id}`)
			.body({archived: true, locked: true})
			.execute();
		await setEveryonePermissions(harness, owner.token, guildId, BASE_MEMBER_PERMISSIONS);
		const lockedError = await createBuilder<{code: string}>(harness, members[0].token)
			.patch(`/channels/${thread.id}`)
			.body({archived: false})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(lockedError.code).toBe('THREAD_LOCKED');
		const unarchived = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${thread.id}`)
			.body({archived: false})
			.execute();
		expect(unarchived.thread_metadata?.archived).toBe(false);
	});

	it('supports joining, listing, and leaving thread members', async () => {
		const {owner, members, thread} = await setupThread();
		await createBuilder<unknown>(harness, members[0].token)
			.put(`/channels/${thread.id}/thread-members/@me`)
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const threadMembers = await createBuilder<Array<ThreadMemberResponse>>(harness, owner.token)
			.get(`/channels/${thread.id}/thread-members`)
			.execute();
		expect(threadMembers.map((member) => member.user_id)).toContain(members[0].userId);
		const threadMessages = await listMessages(harness, owner.token, thread.id);
		expect(threadMessages.some((message) => message.type === MessageTypes.RECIPIENT_ADD)).toBe(true);
		await createBuilder<unknown>(harness, members[0].token)
			.delete(`/channels/${thread.id}/thread-members/@me`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const remaining = await createBuilder<Array<ThreadMemberResponse>>(harness, owner.token)
			.get(`/channels/${thread.id}/thread-members`)
			.execute();
		expect(remaining.map((member) => member.user_id)).not.toContain(members[0].userId);
	});

	it('requires MANAGE_THREADS to remove another thread member', async () => {
		const {owner, members, guildId, thread} = await setupThread();
		for (const member of members) {
			await createBuilder<unknown>(harness, member.token)
				.put(`/channels/${thread.id}/thread-members/@me`)
				.body(null)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		}
		await setEveryonePermissions(harness, owner.token, guildId, BASE_MEMBER_PERMISSIONS);
		await createBuilder<unknown>(harness, members[0].token)
			.delete(`/channels/${thread.id}/thread-members/${members[1].userId}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder<unknown>(harness, owner.token)
			.delete(`/channels/${thread.id}/thread-members/${members[1].userId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});

	it('counts messages and auto-joins the sender on send', async () => {
		const {owner, members, thread} = await setupThread();
		await sendChannelMessage(harness, members[0].token, thread.id, 'joining by talking');
		const fetched = await getChannel(harness, owner.token, thread.id);
		expect(fetched.message_count).toBe(1);
		expect(fetched.total_message_sent).toBe(1);
		const threadMembers = await createBuilder<Array<ThreadMemberResponse>>(harness, owner.token)
			.get(`/channels/${thread.id}/thread-members`)
			.execute();
		expect(threadMembers.map((member) => member.user_id)).toContain(members[0].userId);
	});

	it('reopens an archived unlocked thread when a message is sent', async () => {
		const {owner, thread} = await setupThread();
		await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${thread.id}`)
			.body({archived: true})
			.execute();
		await sendChannelMessage(harness, owner.token, thread.id, 'rise from your grave');
		const fetched = await getChannel(harness, owner.token, thread.id);
		expect(fetched.thread_metadata?.archived).toBe(false);
	});

	it('gates thread posting on SEND_MESSAGES_IN_THREADS instead of SEND_MESSAGES', async () => {
		const {owner, members, guildId, thread, channelId} = await setupThread();
		// Without SEND_MESSAGES_IN_THREADS the member can post to the parent but not the thread.
		await setEveryonePermissions(
			harness,
			owner.token,
			guildId,
			BASE_MEMBER_PERMISSIONS & ~Permissions.SEND_MESSAGES_IN_THREADS,
		);
		await sendChannelMessage(harness, members[0].token, channelId, 'parent still works');
		await createBuilder<unknown>(harness, members[0].token)
			.post(`/channels/${thread.id}/messages`)
			.body({content: 'blocked'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		// With SEND_MESSAGES_IN_THREADS but without SEND_MESSAGES the thread accepts posts.
		await setEveryonePermissions(harness, owner.token, guildId, BASE_MEMBER_PERMISSIONS & ~Permissions.SEND_MESSAGES);
		const message = await createBuilder<MessageResponse>(harness, members[0].token)
			.post(`/channels/${thread.id}/messages`)
			.body({content: 'SEND_MESSAGES is inert inside threads'})
			.execute();
		expect(message.content).toBe('SEND_MESSAGES is inert inside threads');
	});

	it('lists active and archived threads and searches by name', async () => {
		const {owner, channelId, thread} = await setupThread();
		const second = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/channels/${channelId}/threads`)
			.body({name: 'Feature talk'})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${second.id}`)
			.body({archived: true})
			.execute();
		const active = await createBuilder<ThreadListResponse>(harness, owner.token)
			.get(`/channels/${channelId}/threads/active`)
			.execute();
		expect(active.threads.map((entry) => entry.id)).toContain(thread.id);
		expect(active.threads.map((entry) => entry.id)).not.toContain(second.id);
		const archived = await createBuilder<ThreadListResponse>(harness, owner.token)
			.get(`/channels/${channelId}/threads/archived/public`)
			.execute();
		expect(archived.threads.map((entry) => entry.id)).toContain(second.id);
		const search = await createBuilder<ThreadListResponse>(harness, owner.token)
			.get(`/channels/${channelId}/threads/search?q=feature&archived=true`)
			.execute();
		expect(search.threads.map((entry) => entry.id)).toEqual([second.id]);
	});

	it('rejects adding a user who is not a guild member', async () => {
		const {owner, thread} = await setupThread();
		const outsider = await createTestAccount(harness);
		await createBuilder<unknown>(harness, owner.token)
			.put(`/channels/${thread.id}/thread-members/${outsider.userId}`)
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		const threadMembers = await createBuilder<Array<ThreadMemberResponse>>(harness, owner.token)
			.get(`/channels/${thread.id}/thread-members`)
			.execute();
		expect(threadMembers.map((member) => member.user_id)).not.toContain(outsider.userId);
	});

	it('lists active threads guild-wide for the caller', async () => {
		const {owner, members, guildId, thread} = await setupThread();
		const mine = await createBuilder<ThreadListResponse>(harness, owner.token)
			.get(`/guilds/${guildId}/threads/active`)
			.execute();
		expect(mine.threads.map((entry) => entry.id)).toContain(thread.id);
		const theirs = await createBuilder<ThreadListResponse>(harness, members[0].token)
			.get(`/guilds/${guildId}/threads/active`)
			.execute();
		expect(theirs.threads.map((entry) => entry.id)).toContain(thread.id);
	});

	it('cascades thread deletion when the parent channel is deleted', async () => {
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const parent = await createChannel(harness, owner.token, guild.id, 'thread-parent');
		await sendChannelMessage(harness, owner.token, parent.id, 'seed');
		const thread = await createBuilder<ChannelResponse>(harness, owner.token)
			.post(`/channels/${parent.id}/threads`)
			.body({name: 'Doomed thread'})
			.expect(HTTP_STATUS.CREATED)
			.execute();
		await createBuilder<unknown>(harness, owner.token)
			.delete(`/channels/${parent.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder<unknown>(harness, owner.token)
			.get(`/channels/${thread.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	it('deletes a thread with MANAGE_THREADS and cleans up listings', async () => {
		const {owner, members, guildId, channelId, thread} = await setupThread();
		await setEveryonePermissions(harness, owner.token, guildId, BASE_MEMBER_PERMISSIONS);
		await createBuilder<unknown>(harness, members[0].token)
			.delete(`/channels/${thread.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder<unknown>(harness, owner.token)
			.delete(`/channels/${thread.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder<unknown>(harness, owner.token)
			.get(`/channels/${thread.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
		const active = await createBuilder<ThreadListResponse>(harness, owner.token)
			.get(`/channels/${channelId}/threads/active`)
			.execute();
		expect(active.threads.map((entry) => entry.id)).not.toContain(thread.id);
	});
});

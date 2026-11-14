// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {
	createChannel,
	createFriendship,
	createGroupDmChannel,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import {clearChannelThreadsTaintCacheForTesting} from '@app/api/experiment/ChannelThreadsGate';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {StartForumThreadResponse} from '@fluxer/schema/src/domains/channel/ForumRequestSchemas';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const repository = new ChannelRepository();

async function listMessages(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
): Promise<Array<MessageResponse>> {
	return threadsRequest<Array<MessageResponse>>(harness, token).get(`/channels/${channelId}/messages`).execute();
}

async function sendThreadMessage(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	content: string,
): Promise<MessageResponse> {
	return threadsRequest<MessageResponse>(harness, token)
		.post(`/channels/${channelId}/messages`)
		.body({content})
		.expect(200)
		.execute();
}

async function createThread(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
	type: number = ChannelTypes.PUBLIC_THREAD,
): Promise<ThreadChannelResponse> {
	return threadsRequest<ThreadChannelResponse>(harness, token)
		.post(`/channels/${channelId}/threads`)
		.body({name: 'thread', type})
		.expect(201)
		.execute();
}

async function bulkDeleteMine(harness: ApiTestHarness, account: TestAccount, channelId: string, status = 202) {
	await threadsRequest(harness, account.token)
		.post(`/channels/${channelId}/messages/bulk-delete-mine`)
		.body({password: account.password})
		.expect(status)
		.execute();
}

function expectMessageVisibility(
	messages: Array<MessageResponse>,
	visibleIds: Array<string>,
	hiddenIds: Array<string>,
) {
	const messageIds = messages.map((message) => message.id);
	for (const visibleId of visibleIds) {
		expect(messageIds).toContain(visibleId);
	}
	for (const hiddenId of hiddenIds) {
		expect(messageIds).not.toContain(hiddenId);
	}
}

describe('Bulk delete my messages', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		clearChannelThreadsTaintCacheForTesting();
	});
	afterAll(async () => {
		clearChannelThreadsTaintCacheForTesting();
		await harness?.shutdown();
	});
	it('deletes the caller messages in a single guild channel immediately', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const ownerMessage = await sendChannelMessage(harness, owner.token, systemChannel.id, 'owner cleanup target');
		const memberMessage = await sendChannelMessage(harness, member.token, systemChannel.id, 'member should stay');
		const ownerMessage2 = await sendChannelMessage(harness, owner.token, systemChannel.id, 'owner cleanup target 2');
		await createBuilder(harness, owner.token)
			.post(`/channels/${systemChannel.id}/messages/bulk-delete-mine`)
			.body({password: owner.password})
			.expect(202)
			.execute();
		const messages = await listMessages(harness, member.token, systemChannel.id);
		expectMessageVisibility(messages, [memberMessage.id], [ownerMessage.id, ownerMessage2.id]);
	});
	it('deletes the caller messages across every channel in a guild immediately', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const extraChannel = await createChannel(harness, owner.token, guild.id, 'cleanup');
		const systemOwnerMessage = await sendChannelMessage(
			harness,
			owner.token,
			systemChannel.id,
			'system cleanup target',
		);
		const extraOwnerMessage = await sendChannelMessage(harness, owner.token, extraChannel.id, 'extra cleanup target');
		const systemMemberMessage = await sendChannelMessage(harness, member.token, systemChannel.id, 'system should stay');
		const extraMemberMessage = await sendChannelMessage(harness, member.token, extraChannel.id, 'extra should stay');
		await createBuilder(harness, owner.token)
			.post(`/users/@me/guilds/${guild.id}/messages/bulk-delete-mine`)
			.body({password: owner.password})
			.expect(202)
			.execute();
		expectMessageVisibility(
			await listMessages(harness, member.token, systemChannel.id),
			[systemMemberMessage.id],
			[systemOwnerMessage.id],
		);
		expectMessageVisibility(
			await listMessages(harness, member.token, extraChannel.id),
			[extraMemberMessage.id],
			[extraOwnerMessage.id],
		);
	});
	it('deletes the caller guild messages before leaving a guild', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const memberMessage = await sendChannelMessage(harness, member.token, systemChannel.id, 'leaver cleanup target');
		const ownerMessage = await sendChannelMessage(harness, owner.token, systemChannel.id, 'owner should stay');
		await createBuilder(harness, member.token)
			.delete(`/users/@me/guilds/${guild.id}?delete_messages=true`)
			.body({password: member.password})
			.expect(204)
			.execute();
		const messages = await listMessages(harness, owner.token, systemChannel.id);
		expectMessageVisibility(messages, [ownerMessage.id], [memberMessage.id]);
	});
	it('deletes the caller group DM messages before leaving a group DM', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		await createFriendship(harness, owner, member);
		const groupDm = await createGroupDmChannel(harness, owner.token, [member.userId]);
		const ownerMessage = await sendChannelMessage(harness, owner.token, groupDm.id, 'group cleanup target');
		const memberMessage = await sendChannelMessage(harness, member.token, groupDm.id, 'group should stay');
		await createBuilder(harness, owner.token)
			.delete(`/channels/${groupDm.id}?delete_messages=true&silent=true`)
			.body({password: owner.password})
			.expect(204)
			.execute();
		const messages = await listMessages(harness, member.token, groupDm.id);
		expectMessageVisibility(messages, [memberMessage.id], [ownerMessage.id]);
	});
	it('deletes the caller messages in the active and archived threads of a channel', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const active = await createThread(harness, owner.token, systemChannel.id);
		const archived = await createThread(harness, owner.token, systemChannel.id);
		const privateThread = await createThread(harness, owner.token, systemChannel.id, ChannelTypes.PRIVATE_THREAD);
		const parentOwnerMessage = await sendChannelMessage(harness, owner.token, systemChannel.id, 'parent target');
		const activeOwnerMessage = await sendThreadMessage(harness, owner.token, active.id, 'active target');
		const activeMemberMessage = await sendThreadMessage(harness, member.token, active.id, 'active should stay');
		const archivedOwnerMessage = await sendThreadMessage(harness, owner.token, archived.id, 'archived target');
		const archivedMemberMessage = await sendThreadMessage(harness, member.token, archived.id, 'archived should stay');
		const privateOwnerMessage = await sendThreadMessage(harness, owner.token, privateThread.id, 'private target');
		await repository.threads.updateState(createChannelID(BigInt(archived.id)), () => ({archived: true}));
		await bulkDeleteMine(harness, owner, systemChannel.id);
		expectMessageVisibility(await listMessages(harness, member.token, systemChannel.id), [], [parentOwnerMessage.id]);
		expectMessageVisibility(
			await listMessages(harness, member.token, active.id),
			[activeMemberMessage.id],
			[activeOwnerMessage.id],
		);
		expectMessageVisibility(
			await listMessages(harness, member.token, archived.id),
			[archivedMemberMessage.id],
			[archivedOwnerMessage.id],
		);
		expectMessageVisibility(await listMessages(harness, owner.token, privateThread.id), [], [privateOwnerMessage.id]);
	});
	it('deletes the caller messages only in the thread it is run on', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const thread = await createThread(harness, owner.token, systemChannel.id);
		const sibling = await createThread(harness, owner.token, systemChannel.id);
		const parentOwnerMessage = await sendChannelMessage(harness, owner.token, systemChannel.id, 'parent should stay');
		const threadOwnerMessage = await sendThreadMessage(harness, owner.token, thread.id, 'thread target');
		const threadMemberMessage = await sendThreadMessage(harness, member.token, thread.id, 'thread should stay');
		const siblingOwnerMessage = await sendThreadMessage(harness, owner.token, sibling.id, 'sibling should stay');
		await bulkDeleteMine(harness, owner, thread.id);
		expectMessageVisibility(
			await listMessages(harness, member.token, thread.id),
			[threadMemberMessage.id],
			[threadOwnerMessage.id],
		);
		expectMessageVisibility(await listMessages(harness, member.token, systemChannel.id), [parentOwnerMessage.id], []);
		expectMessageVisibility(await listMessages(harness, member.token, sibling.id), [siblingOwnerMessage.id], []);
	});
	it('deletes the caller messages in the posts of a forum channel', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const forum = await threadsRequest<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'forum', type: ChannelTypes.GUILD_FORUM})
			.execute();
		const post = await threadsRequest<StartForumThreadResponse>(harness, member.token)
			.post(`/channels/${forum.id}/threads`)
			.body({name: 'post', message: {content: 'starter should stay'}})
			.expect(201)
			.execute();
		const ownerReply = await sendThreadMessage(harness, owner.token, post.id, 'reply target');
		const memberReply = await sendThreadMessage(harness, member.token, post.id, 'reply should stay');
		await bulkDeleteMine(harness, owner, forum.id);
		expectMessageVisibility(await listMessages(harness, member.token, post.id), [memberReply.id], [ownerReply.id]);
	});
	it('rejects a caller outside the community and leaves its thread messages', async () => {
		const {owner, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		const outsider = await createTestAccount(harness);
		const thread = await createThread(harness, owner.token, systemChannel.id);
		const ownerMessage = await sendThreadMessage(harness, owner.token, thread.id, 'thread should stay');
		await bulkDeleteMine(harness, outsider, systemChannel.id, 403);
		await bulkDeleteMine(harness, outsider, thread.id, 403);
		expectMessageVisibility(await listMessages(harness, owner.token, thread.id), [ownerMessage.id], []);
	});
});

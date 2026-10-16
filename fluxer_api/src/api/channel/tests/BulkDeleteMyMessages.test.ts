// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createChannel,
	createFriendship,
	createGroupDmChannel,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

async function listMessages(
	harness: ApiTestHarness,
	token: string,
	channelId: string,
): Promise<Array<MessageResponse>> {
	return createBuilder<Array<MessageResponse>>(harness, token).get(`/channels/${channelId}/messages`).execute();
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
	});
	afterAll(async () => {
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
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createChannel,
	createFriendship,
	createGuild,
	createPermissionOverwrite,
	createRole,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, test} from 'vitest';

async function sendTypingIndicator(harness: ApiTestHarness, token: string, channelId: string): Promise<void> {
	await createBuilder<void>(harness, token).post(`/channels/${channelId}/typing`).body({}).expect(204).execute();
}

async function createDmChannel(
	harness: ApiTestHarness,
	token: string,
	recipientId: string,
): Promise<{
	id: string;
}> {
	return createBuilder<{
		id: string;
	}>(harness, token)
		.post('/users/@me/channels')
		.body({recipient_id: recipientId})
		.execute();
}

describe('Typing Indicators', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should send typing indicator in DM channel', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		await createFriendship(harness, user1, user2);
		const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
		await sendTypingIndicator(harness, user1.token, dmChannel.id);
	});
	test('should send typing indicator in guild channel as member', async () => {
		const {members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await sendTypingIndicator(harness, member.token, systemChannel.id);
	});
	test('should reject typing indicator from user not in DM channel', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		const user3 = await createTestAccount(harness);
		await createFriendship(harness, user1, user2);
		const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
		await createBuilder(harness, user3.token)
			.post(`/channels/${dmChannel.id}/typing`)
			.body({})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('should reject typing indicator from non-member in guild channel', async () => {
		const account = await createTestAccount(harness);
		const nonMember = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Typing Test Guild');
		const channelId = guild.system_channel_id!;
		await createBuilder(harness, nonMember.token)
			.post(`/channels/${channelId}/typing`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should require SEND_MESSAGES permission for typing indicator', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.SEND_MESSAGES.toString(),
		});
		await createBuilder(harness, member.token)
			.post(`/channels/${systemChannel.id}/typing`)
			.body({})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('should allow typing with SEND_MESSAGES permission from role', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const typingRole = await createRole(harness, owner.token, guild.id, {
			name: 'Can Type',
			permissions: Permissions.SEND_MESSAGES.toString(),
		});
		await createBuilder<void>(harness, owner.token)
			.put(`/guilds/${guild.id}/members/${member.userId}/roles/${typingRole.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await sendTypingIndicator(harness, member.token, systemChannel.id);
	});
	test('should reject typing indicator for unknown channel', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token)
			.post('/channels/999999999999999999/typing')
			.body({})
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
	test('owner can always send typing indicator', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Owner Typing Test');
		const channelId = guild.system_channel_id!;
		await sendTypingIndicator(harness, account.token, channelId);
	});
	test('both participants can send typing indicator in DM', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		await createFriendship(harness, user1, user2);
		const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
		await sendTypingIndicator(harness, user1.token, dmChannel.id);
		await sendTypingIndicator(harness, user2.token, dmChannel.id);
	});
	test('should send typing indicator in created channel', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Channel Typing Test');
		const channel = await createChannel(harness, account.token, guild.id, 'typing-test');
		await sendTypingIndicator(harness, account.token, channel.id);
	});
	test('should reject typing indicator without authorization', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'No Auth Test');
		const channelId = guild.system_channel_id!;
		await createBuilderWithoutAuth(harness)
			.post(`/channels/${channelId}/typing`)
			.body({})
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
});

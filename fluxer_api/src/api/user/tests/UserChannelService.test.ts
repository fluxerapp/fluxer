// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, unclaimAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createUserID} from '@app/api/BrandedTypes';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {
	acceptInvite,
	blockUser,
	createChannelInvite,
	createDmChannel,
	createFriendship,
	createGroupDmChannel,
	createGuild,
	deleteChannel,
	getChannel,
	type MinimalChannelResponse,
	sendChannelMessage,
} from '@app/api/channel/tests/ChannelTestUtils';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {FLUXERBOT_ID} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface PrivateChannelsResponse extends Array<MinimalChannelResponse> {}

async function setUserFlags(harness: ApiTestHarness, userId: string, flags: bigint): Promise<void> {
	await createBuilder(harness, '')
		.patch(`/test/users/${userId}/flags`)
		.body({flags: flags.toString()})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('UserChannelService', () => {
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
	describe('DM channel creation', () => {
		test('bot can create DM when it shares a guild with the recipient', async () => {
			const botAccount = await createTestBotAccount(harness);
			const recipient = await createTestAccount(harness);
			const guild = await createGuild(harness, botAccount.ownerToken, 'Bot DM mutual guild');
			const systemChannel = await getChannel(harness, botAccount.ownerToken, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, botAccount.ownerToken, systemChannel.id);
			await acceptInvite(harness, recipient.token, invite.code);
			await authorizeBot(harness, botAccount.ownerToken, botAccount.appId, ['bot'], guild.id, '0');
			const channel = await createBuilder<MinimalChannelResponse>(harness, `Bot ${botAccount.botToken}`)
				.post('/users/@me/channels')
				.body({recipient_id: recipient.userId})
				.execute();
			expect(channel.id).toBeDefined();
			expect(channel.type).toBe(ChannelTypes.DM);
		});
		test('bug hunter bot can create and send one-to-one DM without normal recipient limits', async () => {
			const botAccount = await createTestBotAccount(harness);
			const recipient = await createTestAccount(harness);
			await setUserFlags(harness, botAccount.botUserId, UserFlags.BUG_HUNTER | UserFlags.SPAMMER);
			await blockUser(harness, recipient, botAccount.botUserId);
			const channel = await createBuilder<MinimalChannelResponse>(harness, `Bot ${botAccount.botToken}`)
				.post('/users/@me/channels')
				.body({recipient_id: recipient.userId})
				.execute();
			expect(channel.id).toBeDefined();
			expect(channel.type).toBe(ChannelTypes.DM);
			let recipientChannels = await createBuilder<PrivateChannelsResponse>(harness, recipient.token)
				.get('/users/@me/channels')
				.execute();
			expect(recipientChannels.some((recipientChannel) => recipientChannel.id === channel.id)).toBe(false);
			await sendChannelMessage(harness, `Bot ${botAccount.botToken}`, channel.id, 'bug hunter bot dm');
			recipientChannels = await createBuilder<PrivateChannelsResponse>(harness, recipient.token)
				.get('/users/@me/channels')
				.execute();
			expect(recipientChannels.some((recipientChannel) => recipientChannel.id === channel.id)).toBe(true);
		});
		test('can create DM with a user who blocked you', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const guild = await createGuild(harness, user1.token, 'Test Community');
			const systemChannel = await getChannel(harness, user1.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, user1.token, systemChannel.id);
			await acceptInvite(harness, user2.token, invite.code);
			await blockUser(harness, user1, user2.userId);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			expect(channel.id).toBeDefined();
			expect(channel.type).toBe(ChannelTypes.DM);
		});
		test('can create DM with a user who shares no mutual community', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			expect(channel.id).toBeDefined();
			expect(channel.type).toBe(ChannelTypes.DM);
		});
		test('unclaimed account cannot create DM', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const guild = await createGuild(harness, user1.token, 'Test Community');
			const systemChannel = await getChannel(harness, user1.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, user1.token, systemChannel.id);
			await acceptInvite(harness, user2.token, invite.code);
			await unclaimAccount(harness, user1.userId);
			await createBuilder(harness, user1.token)
				.post('/users/@me/channels')
				.body({recipient_id: user2.userId})
				.expect(HTTP_STATUS.BAD_REQUEST, 'UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES')
				.execute();
		});
		test('reopening existing DM returns same channel', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const channel1 = await createDmChannel(harness, user1.token, user2.userId);
			const channel2 = await createDmChannel(harness, user1.token, user2.userId);
			expect(channel1.id).toBe(channel2.id);
		});
		test('reopening existing DM works after blocking user', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const channel1 = await createDmChannel(harness, user1.token, user2.userId);
			await blockUser(harness, user1, user2.userId);
			const channel2 = await createDmChannel(harness, user1.token, user2.userId);
			expect(channel2.id).toBe(channel1.id);
		});
		test('reopening closed system user DM accepts recipient_id 0', async () => {
			const user = await createTestAccount(harness);
			const userId = createUserID(BigInt(user.userId));
			const channelId = createChannelID(1000000000000000001n);
			const userRepository = new UserRepository();
			const channel = await userRepository.createDmChannelAndState(userId, SYSTEM_USER_ID, channelId);
			await userRepository.openPrivateChannelForUser(userId, channel);
			await deleteChannel(harness, user.token, channelId.toString());
			const reopened = await createBuilder<MinimalChannelResponse>(harness, user.token)
				.post('/users/@me/channels')
				.body({recipient_id: FLUXERBOT_ID})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(reopened.id).toBe(channelId.toString());
			expect(reopened.type).toBe(ChannelTypes.DM);
		});
		test('can create new system user DM without friendship or mutual guilds', async () => {
			const user = await createTestAccount(harness);
			const channel = await createBuilder<MinimalChannelResponse>(harness, user.token)
				.post('/users/@me/channels')
				.body({recipient_id: FLUXERBOT_ID})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(channel.id).toBeDefined();
			expect(channel.type).toBe(ChannelTypes.DM);
		});
	});
	describe('Group DM creation', () => {
		test('cannot create group DM with non-friend', async () => {
			const owner = await createTestAccount(harness);
			const friend = await createTestAccount(harness);
			const stranger = await createTestAccount(harness);
			await createFriendship(harness, owner, friend);
			await createBuilder(harness, owner.token)
				.post('/users/@me/channels')
				.body({recipients: [friend.userId, stranger.userId]})
				.expect(HTTP_STATUS.BAD_REQUEST, 'GROUP_DM_RECIPIENTS_NOT_ADDABLE')
				.execute();
		});
	});
	describe('sending messages in DMs', () => {
		test('cannot send message to user who blocked you', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			await ensureSessionStarted(harness, user1.token);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			await createBuilder(harness, user2.token)
				.put(`/users/@me/relationships/${user1.userId}`)
				.body({type: 2})
				.execute();
			await createBuilder(harness, user1.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'Hello!'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_MESSAGES_TO_USER')
				.execute();
		});
	});
	describe('group DM recipient management', () => {
		test('cannot add non-friend to group DM', async () => {
			const owner = await createTestAccount(harness);
			const friend = await createTestAccount(harness);
			const stranger = await createTestAccount(harness);
			await createFriendship(harness, owner, friend);
			const channel = await createGroupDmChannel(harness, owner.token, [friend.userId]);
			await createBuilder(harness, owner.token)
				.put(`/channels/${channel.id}/recipients/${stranger.userId}`)
				.body(null)
				.expect(HTTP_STATUS.BAD_REQUEST, 'NOT_FRIENDS_WITH_USER')
				.execute();
		});
		test('non-owner cannot remove other recipients', async () => {
			const owner = await createTestAccount(harness);
			const member1 = await createTestAccount(harness);
			const member2 = await createTestAccount(harness);
			await createFriendship(harness, owner, member1);
			await createFriendship(harness, owner, member2);
			const channel = await createGroupDmChannel(harness, owner.token, [member1.userId, member2.userId]);
			await createBuilder(harness, member1.token)
				.delete(`/channels/${channel.id}/recipients/${member2.userId}`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
});

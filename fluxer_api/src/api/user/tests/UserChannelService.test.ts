// SPDX-License-Identifier: AGPL-3.0-or-later

import {FLUXERBOT_ID} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, unclaimAccount} from '../../auth/tests/AuthTestUtils';
import {createChannelID, createUserID} from '../../BrandedTypes';
import {authorizeBot, createTestBotAccount} from '../../bot/tests/BotTestUtils';
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
} from '../../channel/tests/ChannelTestUtils';
import {SYSTEM_USER_ID} from '../../constants/Core';
import {ensureSessionStarted} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {UserRepository} from '../../user/repositories/UserRepository';

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
		test('can create DM with a friend', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			expect(channel.id).toBeDefined();
			expect(channel.type).toBe(ChannelTypes.DM);
		});
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
		test('cannot create DM with yourself', async () => {
			const user = await createTestAccount(harness);
			await createBuilder(harness, user.token)
				.post('/users/@me/channels')
				.body({recipient_id: user.userId})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('cannot create DM with unknown user', async () => {
			const user = await createTestAccount(harness);
			await createBuilder(harness, user.token)
				.post('/users/@me/channels')
				.body({recipient_id: '999999999999999999'})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_USER')
				.execute();
		});
		test('cannot create DM with blocked user', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const guild = await createGuild(harness, user1.token, 'Test Community');
			const systemChannel = await getChannel(harness, user1.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, user1.token, systemChannel.id);
			await acceptInvite(harness, user2.token, invite.code);
			await blockUser(harness, user1, user2.userId);
			await createBuilder(harness, user1.token)
				.post('/users/@me/channels')
				.body({recipient_id: user2.userId})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_MESSAGES_TO_USER')
				.execute();
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
		test('can create group DM with friends', async () => {
			const owner = await createTestAccount(harness);
			const friend1 = await createTestAccount(harness);
			const friend2 = await createTestAccount(harness);
			await createFriendship(harness, owner, friend1);
			await createFriendship(harness, owner, friend2);
			const channel = await createGroupDmChannel(harness, owner.token, [friend1.userId, friend2.userId]);
			expect(channel.id).toBeDefined();
			expect(channel.type).toBe(ChannelTypes.GROUP_DM);
			expect(channel.owner_id).toBe(owner.userId);
			expect(channel.recipients.length).toBe(2);
		});
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
		test('cannot add yourself to group DM recipients', async () => {
			const owner = await createTestAccount(harness);
			const friend = await createTestAccount(harness);
			await createFriendship(harness, owner, friend);
			await createBuilder(harness, owner.token)
				.post('/users/@me/channels')
				.body({recipients: [friend.userId, owner.userId]})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('cannot add duplicate recipients to group DM', async () => {
			const owner = await createTestAccount(harness);
			const friend = await createTestAccount(harness);
			await createFriendship(harness, owner, friend);
			await createBuilder(harness, owner.token)
				.post('/users/@me/channels')
				.body({recipients: [friend.userId, friend.userId]})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
		test('cannot create group DM with unknown user', async () => {
			const owner = await createTestAccount(harness);
			const friend = await createTestAccount(harness);
			await createFriendship(harness, owner, friend);
			await createBuilder(harness, owner.token)
				.post('/users/@me/channels')
				.body({recipients: [friend.userId, '999999999999999999']})
				.expect(HTTP_STATUS.BAD_REQUEST, 'GROUP_DM_RECIPIENTS_NOT_ADDABLE')
				.execute();
		});
	});
	describe('private channel listing', () => {
		test('lists all private channels for user', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const user3 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			await createFriendship(harness, user1, user3);
			await createDmChannel(harness, user1.token, user2.userId);
			await createDmChannel(harness, user1.token, user3.userId);
			const channels = await createBuilder<PrivateChannelsResponse>(harness, user1.token)
				.get('/users/@me/channels')
				.execute();
			expect(channels.length).toBe(2);
		});
	});
	describe('DM pinning', () => {
		test('can pin and unpin DM channel', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			await createBuilder(harness, user1.token)
				.put(`/users/@me/channels/${channel.id}/pin`)
				.body(null)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await createBuilder(harness, user1.token)
				.delete(`/users/@me/channels/${channel.id}/pin`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		test('cannot pin non-DM channel', async () => {
			const user = await createTestAccount(harness);
			await createBuilder(harness, user.token)
				.put('/users/@me/channels/999999999999999999/pin')
				.body(null)
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
	});
	describe('preload DM messages', () => {
		test('can preload messages for multiple DM channels', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const user3 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			await createFriendship(harness, user1, user3);
			await ensureSessionStarted(harness, user1.token);
			const channel1 = await createDmChannel(harness, user1.token, user2.userId);
			const channel2 = await createDmChannel(harness, user1.token, user3.userId);
			await sendChannelMessage(harness, user1.token, channel1.id, 'Hello user2');
			await sendChannelMessage(harness, user1.token, channel2.id, 'Hello user3');
			const result = await createBuilder<Record<string, unknown>>(harness, user1.token)
				.post('/users/@me/preload-messages')
				.body({channels: [channel1.id, channel2.id]})
				.execute();
			expect(result).toBeDefined();
		});
		test('cannot preload more than 100 channels', async () => {
			const user = await createTestAccount(harness);
			const tooManyChannels = Array.from({length: 101}, (_, i) => i.toString());
			await createBuilder(harness, user.token)
				.post('/users/@me/preload-messages')
				.body({channels: tooManyChannels})
				.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
				.execute();
		});
	});
	describe('sending messages in DMs', () => {
		test('can send message in DM to friend', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			await ensureSessionStarted(harness, user1.token);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			const message = await sendChannelMessage(harness, user1.token, channel.id, 'Hello!');
			expect(message.id).toBeDefined();
			expect(message.content).toBe('Hello!');
		});
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
		test('owner can add friend to group DM', async () => {
			const owner = await createTestAccount(harness);
			const friend1 = await createTestAccount(harness);
			const friend2 = await createTestAccount(harness);
			await createFriendship(harness, owner, friend1);
			await createFriendship(harness, owner, friend2);
			const channel = await createGroupDmChannel(harness, owner.token, [friend1.userId]);
			await createBuilder(harness, owner.token)
				.put(`/channels/${channel.id}/recipients/${friend2.userId}`)
				.body(null)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
		test('owner can remove recipient from group DM', async () => {
			const owner = await createTestAccount(harness);
			const friend1 = await createTestAccount(harness);
			const friend2 = await createTestAccount(harness);
			await createFriendship(harness, owner, friend1);
			await createFriendship(harness, owner, friend2);
			const channel = await createGroupDmChannel(harness, owner.token, [friend1.userId, friend2.userId]);
			await createBuilder(harness, owner.token)
				.delete(`/channels/${channel.id}/recipients/${friend2.userId}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
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
		test('member can leave group DM', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			await createFriendship(harness, owner, member);
			const channel = await createGroupDmChannel(harness, owner.token, [member.userId]);
			await createBuilder(harness, member.token)
				.delete(`/channels/${channel.id}/recipients/${member.userId}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		});
	});
});

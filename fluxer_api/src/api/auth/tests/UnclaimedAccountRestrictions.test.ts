// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, unclaimAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createDMChannel,
	createFriendship,
	createGuild,
	sendMessage,
} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

async function setBotFlag(harness: ApiTestHarness, userId: string, isBot: boolean): Promise<void> {
	await createBuilder(harness, '').post(`/test/users/${userId}/set-bot-flag`).body({is_bot: isBot}).execute();
}

describe('Unclaimed Account Restrictions', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('unclaimed account cannot add reactions', async () => {
		const owner = await createTestAccount(harness);
		const unclaimed = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, unclaimed.token, invite.code);
		const message = await sendMessage(harness, owner.token, channelId, 'React to this');
		await unclaimAccount(harness, unclaimed.userId);
		const {json: error} = await createBuilder<{
			code: string;
		}>(harness, unclaimed.token)
			.put(`/channels/${channelId}/messages/${message.id}/reactions/%F0%9F%91%8D/@me`)
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
		expect(error.code).toBe('UNCLAIMED_ACCOUNT_CANNOT_ADD_REACTIONS');
	});
	test('unclaimed account cannot create group DM', async () => {
		const unclaimed = await createTestAccount(harness);
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		await unclaimAccount(harness, unclaimed.userId);
		await createBuilder(harness, unclaimed.token)
			.post('/users/@me/channels')
			.body({recipients: [user1.userId, user2.userId]})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('unclaimed account cannot join group DM via invite', async () => {
		const owner = await createTestAccount(harness);
		const friend1 = await createTestAccount(harness);
		const friend2 = await createTestAccount(harness);
		await createFriendship(harness, owner, friend1);
		await createFriendship(harness, owner, friend2);
		const groupDm = await createBuilder<{
			id: string;
		}>(harness, owner.token)
			.post('/users/@me/channels')
			.body({recipients: [friend1.userId]})
			.execute();
		const invite = await createBuilder<{
			code: string;
		}>(harness, owner.token)
			.post(`/channels/${groupDm.id}/invites`)
			.body({})
			.execute();
		await unclaimAccount(harness, friend2.userId);
		const {json: error} = await createBuilder<{
			code: string;
		}>(harness, friend2.token)
			.post(`/invites/${invite.code}`)
			.body({})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
		expect(error.code).toBe('UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS');
	});
	test('unclaimed account cannot send DM', async () => {
		const sender = await createTestAccount(harness);
		const receiver = await createTestAccount(harness);
		await createFriendship(harness, sender, receiver);
		const dmChannel = await createDMChannel(harness, sender.token, receiver.userId);
		await unclaimAccount(harness, sender.userId);
		await createBuilder(harness, sender.token)
			.post(`/channels/${dmChannel.id}/messages`)
			.body({content: 'Hello from unclaimed'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('unclaimed account can receive DM', async () => {
		const sender = await createTestAccount(harness);
		const receiver = await createTestAccount(harness);
		await createFriendship(harness, sender, receiver);
		const dmChannel = await createDMChannel(harness, sender.token, receiver.userId);
		await unclaimAccount(harness, receiver.userId);
		const message = await sendMessage(harness, sender.token, dmChannel.id, 'Hello to unclaimed');
		expect(message.id).toBeTruthy();
		expect(message.content).toBe('Hello to unclaimed');
	});
	test('unclaimed account can join guild by invite', async () => {
		const owner = await createTestAccount(harness);
		const unclaimed = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await unclaimAccount(harness, unclaimed.userId);
		await createBuilder(harness, unclaimed.token).post(`/invites/${invite.code}`).body({}).execute();
	});
	test('unclaimed account cannot send guild messages', async () => {
		const owner = await createTestAccount(harness);
		const unclaimed = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, unclaimed.token, invite.code);
		await unclaimAccount(harness, unclaimed.userId);
		const {json: error} = await createBuilder<{
			code: string;
		}>(harness, unclaimed.token)
			.post(`/channels/${channelId}/messages`)
			.body({content: 'Hello world'})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
		expect(error.code).toBe('UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES');
	});
	test('unclaimed account cannot send friend request', async () => {
		const sender = await createTestAccount(harness);
		const receiver = await createTestAccount(harness);
		await unclaimAccount(harness, sender.userId);
		await createBuilder(harness, sender.token)
			.put(`/users/@me/relationships/${receiver.userId}`)
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('unclaimed account cannot accept friend request', async () => {
		const sender = await createTestAccount(harness);
		const receiver = await createTestAccount(harness);
		await createBuilder(harness, sender.token).post(`/users/@me/relationships/${receiver.userId}`).body({}).execute();
		await unclaimAccount(harness, receiver.userId);
		await createBuilder(harness, receiver.token)
			.put(`/users/@me/relationships/${sender.userId}`)
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('unclaimed account can view their own profile', async () => {
		const unclaimed = await createTestAccount(harness);
		await unclaimAccount(harness, unclaimed.userId);
		await createBuilder(harness, unclaimed.token).get('/users/@me').execute();
	});
	test('unclaimed account can view guild they are member of', async () => {
		const owner = await createTestAccount(harness);
		const unclaimed = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, unclaimed.token, invite.code);
		await unclaimAccount(harness, unclaimed.userId);
		await createBuilder(harness, unclaimed.token).get(`/guilds/${guild.id}`).execute();
	});
	test('unclaimed account can read messages in guild', async () => {
		const owner = await createTestAccount(harness);
		const unclaimed = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Test Guild');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, unclaimed.token, invite.code);
		await sendMessage(harness, owner.token, channelId, 'Test message');
		await unclaimAccount(harness, unclaimed.userId);
		await createBuilder(harness, unclaimed.token).get(`/channels/${channelId}/messages`).execute();
	});
	test('claimed account creates guild without INVITES_DISABLED feature', async () => {
		const user = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, user.token)
			.post('/guilds')
			.body({name: 'Guild'})
			.execute();
		expect(guild.features).not.toContain(GuildFeatures.INVITES_DISABLED);
	});
	test('unclaimed account cannot open DM with another user', async () => {
		const unclaimed = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		const guild = await createGuild(harness, unclaimed.token, 'Test Guild');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, unclaimed.token, channelId);
		await acceptInvite(harness, target.token, invite.code);
		await unclaimAccount(harness, unclaimed.userId);
		const {json: error} = await createBuilder<{
			code: string;
		}>(harness, unclaimed.token)
			.post('/users/@me/channels')
			.body({recipient_id: target.userId})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.executeWithResponse();
		expect(error.code).toBe('UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES');
	});
	test('unclaimed account can use personal notes', async () => {
		const unclaimed = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		await unclaimAccount(harness, unclaimed.userId);
		await createBuilder(harness, unclaimed.token)
			.put(`/users/@me/notes/${target.userId}`)
			.body({note: 'This is a personal note'})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	test('unclaimed account can delete without password', async () => {
		const unclaimed = await createTestAccount(harness);
		await unclaimAccount(harness, unclaimed.userId);
		await createBuilder(harness, unclaimed.token)
			.post('/users/@me/delete')
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	});
	test('unclaimed account cannot create guild', async () => {
		const unclaimed = await createTestAccount(harness);
		await unclaimAccount(harness, unclaimed.userId);
		await createBuilder(harness, unclaimed.token)
			.post('/guilds')
			.body({name: 'Guild'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS)
			.execute();
	});
	test('bot account cannot create guild', async () => {
		const bot = await createTestAccount(harness);
		await setBotFlag(harness, bot.userId, true);
		await createBuilder(harness, bot.token)
			.post('/guilds')
			.body({name: 'Guild'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.BOTS_CANNOT_CREATE_GUILDS)
			.execute();
	});
});

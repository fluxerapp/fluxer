// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {addMemberRole, createRole, removeMemberRole} from '@app/api/guild/tests/GuildTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	sendMessage,
	updateChannelPermissions,
} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {MessageListResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('User recent mentions', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('filters mentions from channels the user can no longer access', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Recent Mentions Access Test');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, member.token, invite.code);
		const accessRole = await createRole(harness, owner.token, guild.id, {
			name: 'Mention Channel Access',
			permissions: (Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY).toString(),
		});
		await updateChannelPermissions(harness, owner.token, channelId, guild.id, {
			type: 0,
			deny: Permissions.VIEW_CHANNEL.toString(),
		});
		await updateChannelPermissions(harness, owner.token, channelId, accessRole.id, {
			type: 0,
			allow: (Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY).toString(),
		});
		await addMemberRole(harness, owner.token, guild.id, member.userId, accessRole.id);
		const message = await sendMessage(harness, owner.token, channelId, `<@${member.userId}> private ping`);
		await new UserRepository().createRecentMention({
			user_id: createUserID(BigInt(member.userId)),
			channel_id: createChannelID(BigInt(channelId)),
			message_id: createMessageID(BigInt(message.id)),
			guild_id: createGuildID(BigInt(guild.id)),
			is_everyone: false,
			is_role: false,
		});
		const visibleMentions = await createBuilder<MessageListResponse>(harness, member.token)
			.get('/users/@me/mentions')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(visibleMentions.map((mention) => mention.id)).toContain(message.id);
		await removeMemberRole(harness, owner.token, guild.id, member.userId, accessRole.id);
		await createBuilder(harness, member.token)
			.get(`/channels/${channelId}/messages/${message.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		const filteredMentions = await createBuilder<MessageListResponse>(harness, member.token)
			.get('/users/@me/mentions')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(filteredMentions.map((mention) => mention.id)).not.toContain(message.id);
	});
	test('marks several recent mentions read in one request', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Recent Mentions Bulk Read Test');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, member.token, invite.code);
		const firstMessage = await sendMessage(harness, owner.token, channelId, `<@${member.userId}> first ping`);
		const secondMessage = await sendMessage(harness, owner.token, channelId, `<@${member.userId}> second ping`);
		const thirdMessage = await sendMessage(harness, owner.token, channelId, `<@${member.userId}> third ping`);
		const repository = new UserRepository();
		for (const message of [firstMessage, secondMessage, thirdMessage]) {
			await repository.createRecentMention({
				user_id: createUserID(BigInt(member.userId)),
				channel_id: createChannelID(BigInt(channelId)),
				message_id: createMessageID(BigInt(message.id)),
				guild_id: createGuildID(BigInt(guild.id)),
				is_everyone: false,
				is_role: false,
			});
		}
		await createBuilder(harness, member.token)
			.post('/users/@me/mentions/read')
			.body({message_ids: [firstMessage.id, secondMessage.id]})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const remainingMentions = await createBuilder<MessageListResponse>(harness, member.token)
			.get('/users/@me/mentions')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(remainingMentions.map((mention) => mention.id)).toEqual([thirdMessage.id]);
	});
	test('filters persisted role and everyone mentions independently', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Recent Mentions Type Filter Test');
		const channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, member.token, invite.code);
		const everyoneMessage = await sendMessage(harness, owner.token, channelId, 'everyone row');
		const roleMessage = await sendMessage(harness, owner.token, channelId, 'role row');
		const directMessage = await sendMessage(harness, owner.token, channelId, 'direct row');
		const repository = new UserRepository();
		await repository.createRecentMention({
			user_id: createUserID(BigInt(member.userId)),
			channel_id: createChannelID(BigInt(channelId)),
			message_id: createMessageID(BigInt(everyoneMessage.id)),
			guild_id: createGuildID(BigInt(guild.id)),
			is_everyone: true,
			is_role: false,
		});
		await repository.createRecentMention({
			user_id: createUserID(BigInt(member.userId)),
			channel_id: createChannelID(BigInt(channelId)),
			message_id: createMessageID(BigInt(roleMessage.id)),
			guild_id: createGuildID(BigInt(guild.id)),
			is_everyone: false,
			is_role: true,
		});
		await repository.createRecentMention({
			user_id: createUserID(BigInt(member.userId)),
			channel_id: createChannelID(BigInt(channelId)),
			message_id: createMessageID(BigInt(directMessage.id)),
			guild_id: createGuildID(BigInt(guild.id)),
			is_everyone: false,
			is_role: false,
		});
		const withoutRoles = await createBuilder<MessageListResponse>(harness, member.token)
			.get('/users/@me/mentions?roles=false&everyone=true')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(withoutRoles.map((mention) => mention.id)).toEqual([directMessage.id, everyoneMessage.id]);
		const withoutEveryone = await createBuilder<MessageListResponse>(harness, member.token)
			.get('/users/@me/mentions?roles=true&everyone=false')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(withoutEveryone.map((mention) => mention.id)).toEqual([directMessage.id, roleMessage.id]);
	});
	test('continues scanning older rows when recent mentions are filtered out', async () => {
		const repository = new UserRepository();
		const userId = createUserID(10n);
		const channelId = createChannelID(20n);
		const guildId = createGuildID(30n);
		const everyoneMessageId = createMessageID(1000n);
		await repository.createRecentMention({
			user_id: userId,
			channel_id: channelId,
			message_id: everyoneMessageId,
			guild_id: guildId,
			is_everyone: true,
			is_role: false,
		});
		for (let i = 0; i < 50; i++) {
			await repository.createRecentMention({
				user_id: userId,
				channel_id: channelId,
				message_id: createMessageID(1001n + BigInt(i)),
				guild_id: guildId,
				is_everyone: false,
				is_role: true,
			});
		}
		const mentions = await repository.listRecentMentions(userId, true, false, true, 1);
		expect(mentions.map((mention) => mention.messageId)).toEqual([everyoneMessageId]);
	});
});

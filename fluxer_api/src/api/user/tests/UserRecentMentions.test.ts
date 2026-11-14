// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createChannelID, createGuildID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {
	createPermissionOverwrite,
	sendChannelMessage,
	setupTestGuildWithMembers,
	updateGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {addMemberRole, createRole, removeMemberRole} from '@app/api/guild/tests/GuildTestUtils';
import {sendMessage, updateChannelPermissions} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import type {MessageListResponse, MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('User recent mentions', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	async function recordMentions(userId: string, guildId: string, messages: Array<MessageResponse>) {
		await new UserRepository().createRecentMentions(
			messages.map((message) => ({
				user_id: createUserID(BigInt(userId)),
				channel_id: createChannelID(BigInt(message.channel_id)),
				message_id: createMessageID(BigInt(message.id)),
				guild_id: createGuildID(BigInt(guildId)),
				is_everyone: false,
				is_role: false,
			})),
		);
	}

	async function createMentionBatch() {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		assert(member, 'Expected a guild member');
		const older = await sendChannelMessage(harness, owner.token, systemChannel.id, `<@${member.userId}> before cutoff`);
		const boundary = await sendChannelMessage(harness, owner.token, systemChannel.id, `<@${member.userId}> at cutoff`);
		const newer = await sendChannelMessage(harness, owner.token, systemChannel.id, `<@${member.userId}> after cutoff`);
		await recordMentions(member.userId, guild.id, [older, boundary, newer]);
		return {owner, member, guild, systemChannel, older, boundary, newer};
	}

	test('filters mentions from channels the user can no longer access', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		assert(member, 'Expected a guild member');
		const channelId = systemChannel.id;
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
		await recordMentions(member.userId, guild.id, [message]);
		const visibleMentions = await createBuilder<MessageListResponse>(harness, member.token)
			.get('/users/@me/mentions')
			.expect(HttpStatus.OK)
			.execute();
		expect(visibleMentions.map((mention) => mention.id)).toContain(message.id);
		await removeMemberRole(harness, owner.token, guild.id, member.userId, accessRole.id);
		await createBuilder(harness, member.token)
			.get(`/channels/${channelId}/messages/${message.id}`)
			.expect(HttpStatus.FORBIDDEN)
			.execute();
		const filteredMentions = await createBuilder<MessageListResponse>(harness, member.token)
			.get('/users/@me/mentions')
			.expect(HttpStatus.OK)
			.execute();
		expect(filteredMentions.map((mention) => mention.id)).not.toContain(message.id);
	});
	test('marks several recent mentions read in one request', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		assert(member, 'Expected a guild member');
		const channelId = systemChannel.id;
		const firstMessage = await sendMessage(harness, owner.token, channelId, `<@${member.userId}> first ping`);
		const secondMessage = await sendMessage(harness, owner.token, channelId, `<@${member.userId}> second ping`);
		const thirdMessage = await sendMessage(harness, owner.token, channelId, `<@${member.userId}> third ping`);
		await recordMentions(member.userId, guild.id, [firstMessage, secondMessage, thirdMessage]);
		await createBuilder(harness, member.token)
			.post('/users/@me/mentions/read')
			.body({message_ids: [firstMessage.id, secondMessage.id]})
			.expect(HttpStatus.NO_CONTENT)
			.execute();
		const remainingMentions = await createBuilder<MessageListResponse>(harness, member.token)
			.get('/users/@me/mentions')
			.expect(HttpStatus.OK)
			.execute();
		expect(remainingMentions.map((mention) => mention.id)).toEqual([thirdMessage.id]);
	});

	test.each([
		['readable history', 0n, ['newer', 'boundary', 'older']],
		['history restricted by cutoff', Permissions.READ_MESSAGE_HISTORY, ['newer', 'boundary']],
	] as const)('returns a mention batch with %s', async (_name, denied, expected) => {
		const world = await createMentionBatch();
		const cutoffTimestamp = extractTimestamp(world.boundary.id);
		expect(extractTimestamp(world.older.id)).toBeLessThan(cutoffTimestamp);
		expect(extractTimestamp(world.newer.id)).toBeGreaterThan(cutoffTimestamp);
		await updateGuild(harness, world.owner.token, world.guild.id, {
			message_history_cutoff: new Date(cutoffTimestamp).toISOString(),
		});
		await createPermissionOverwrite(harness, world.owner.token, world.systemChannel.id, world.member.userId, {
			type: 1,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: denied.toString(),
		});

		const mentions = await createBuilder<MessageListResponse>(harness, world.member.token)
			.get('/users/@me/mentions')
			.execute();

		expect(mentions.map((message) => message.id)).toEqual(expected.map((key) => world[key].id));
	});

	test('omits every guild mention when history is denied without a cutoff', async () => {
		const world = await createMentionBatch();
		await createPermissionOverwrite(harness, world.owner.token, world.systemChannel.id, world.member.userId, {
			type: 1,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});

		const mentions = await createBuilder<MessageListResponse>(harness, world.member.token)
			.get('/users/@me/mentions')
			.execute();

		expect(mentions).toEqual([]);
	});
	test('continues scanning older rows when recent mentions are filtered out', async () => {
		const repository = new UserRepository();
		const userId = createUserID(10n);
		const channelId = createChannelID(20n);
		const guildId = createGuildID(30n);
		const everyoneMessageId = createMessageID(1000n);
		await repository.createRecentMentions([
			{
				user_id: userId,
				channel_id: channelId,
				message_id: everyoneMessageId,
				guild_id: guildId,
				is_everyone: true,
				is_role: false,
			},
		]);
		for (let i = 0; i < 50; i++) {
			await repository.createRecentMentions([
				{
					user_id: userId,
					channel_id: channelId,
					message_id: createMessageID(1001n + BigInt(i)),
					guild_id: guildId,
					is_everyone: false,
					is_role: true,
				},
			]);
		}
		const mentions = await repository.listRecentMentions(userId, true, false, true, 1);
		expect(mentions.map((mention) => mention.messageId)).toEqual([everyoneMessageId]);
	});
});

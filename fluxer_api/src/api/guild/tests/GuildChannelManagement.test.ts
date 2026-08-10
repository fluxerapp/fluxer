// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {
	addMemberRole,
	createChannel,
	createRole,
	getChannel,
	setupTestGuildWithMembers,
	updateChannel,
} from '../../channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createGuild, getGuildChannels} from './GuildTestUtils';

describe('Guild Channel Management', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('Channel Name Updates', () => {
		test('should normalize channel name with spaces to hyphens', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const updated = await updateChannel(harness, account.token, channel.id, {name: 'my new channel'});
			expect(updated.name).toBe('my-new-channel');
		});
		test('should reject channel name exceeding maximum length', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const longName = 'a'.repeat(101);
			await createBuilder(harness, account.token)
				.patch(`/channels/${channel.id}`)
				.body({name: longName, type: ChannelTypes.GUILD_TEXT})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should preserve channel name when update name is empty', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const updated = await updateChannel(harness, account.token, channel.id, {name: ''});
			expect(updated.name).toBe('test-channel');
		});
		test('should convert channel name to lowercase', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const updated = await updateChannel(harness, account.token, channel.id, {name: 'MyChannel'});
			expect(updated.name).toBe('mychannel');
		});
	});
	describe('Channel Topic Updates', () => {
		test('should allow clearing channel topic', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			await updateChannel(harness, account.token, channel.id, {topic: 'Initial topic'});
			const updated = await updateChannel(harness, account.token, channel.id, {topic: null});
			expect(updated.topic).toBeNull();
		});
		test('should reject channel topic exceeding maximum length', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const longTopic = 'a'.repeat(1025);
			await createBuilder(harness, account.token)
				.patch(`/channels/${channel.id}`)
				.body({topic: longTopic, type: ChannelTypes.GUILD_TEXT})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should accept topic at maximum length', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const maxTopic = 'a'.repeat(1024);
			const updated = await updateChannel(harness, account.token, channel.id, {topic: maxTopic});
			expect(updated.topic).toBe(maxTopic);
		});
		test('should expose topic and message fields on voice channel responses', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'voice-topic', ChannelTypes.GUILD_VOICE);
			const updated = await updateChannel(harness, account.token, channel.id, {topic: 'Voice topic'});
			expect(updated.topic).toBe('Voice topic');
			expect(updated.last_message_id).toBeNull();
			expect(updated.rate_limit_per_user).toBe(0);
		});
	});
	describe('Channel Slowmode (rate_limit_per_user)', () => {
		test('should set slowmode when creating a channel', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'We go slow');
			const channel = await createBuilder<ChannelResponse>(harness, account.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'slooooow', type: ChannelTypes.GUILD_TEXT, rate_limit_per_user: 60})
				.execute();
			expect(channel.rate_limit_per_user).toBe(60);
		});
		test('should reject invalid slowmode when creating a channel', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			await createBuilder(harness, account.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'too-fast-omg', type: ChannelTypes.GUILD_TEXT, rate_limit_per_user: Number.MAX_SAFE_INTEGER})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should set slowmode on text channel', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const updated = await updateChannel(harness, account.token, channel.id, {rate_limit_per_user: 60});
			expect(updated.rate_limit_per_user).toBe(60);
		});
		test('should disable slowmode by setting to zero', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			await updateChannel(harness, account.token, channel.id, {rate_limit_per_user: 60});
			const updated = await updateChannel(harness, account.token, channel.id, {rate_limit_per_user: 0});
			expect(updated.rate_limit_per_user).toBe(0);
		});
		test('should reject negative slowmode value', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			await createBuilder(harness, account.token)
				.patch(`/channels/${channel.id}`)
				.body({rate_limit_per_user: -1, type: ChannelTypes.GUILD_TEXT})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject slowmode exceeding maximum (21600 seconds)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			await createBuilder(harness, account.token)
				.patch(`/channels/${channel.id}`)
				.body({rate_limit_per_user: 21601, type: ChannelTypes.GUILD_TEXT})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should accept maximum slowmode value (21600 seconds)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const updated = await updateChannel(harness, account.token, channel.id, {rate_limit_per_user: 21600});
			expect(updated.rate_limit_per_user).toBe(21600);
		});
		test('should set slowmode on voice channel text chat', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'voice-slowmode', ChannelTypes.GUILD_VOICE);
			const updated = await updateChannel(harness, account.token, channel.id, {rate_limit_per_user: 60});
			expect(updated.rate_limit_per_user).toBe(60);
		});
	});
	describe('Channel Position Updates', () => {
		test('should update single channel position via direct PATCH', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			await createChannel(harness, account.token, guild.id, 'channel-1');
			const channel2 = await createChannel(harness, account.token, guild.id, 'channel-2');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}/channels`)
				.body([{id: channel2.id, position: 0}])
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const channels = await getGuildChannels(harness, account.token, guild.id);
			const updatedChannel2 = channels.find((c) => c.id === channel2.id);
			expect(updatedChannel2).toBeDefined();
		});
		test('should update multiple channel positions in bulk via direct PATCH', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel1 = await createChannel(harness, account.token, guild.id, 'channel-1');
			const channel2 = await createChannel(harness, account.token, guild.id, 'channel-2');
			const channel3 = await createChannel(harness, account.token, guild.id, 'channel-3');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}/channels`)
				.body([
					{id: channel3.id, position: 0},
					{id: channel2.id, position: 1},
					{id: channel1.id, position: 2},
				])
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const channels = await getGuildChannels(harness, account.token, guild.id);
			const textChannels = channels.filter((c) => c.type === ChannelTypes.GUILD_TEXT);
			expect(textChannels.some((c) => c.id === channel1.id)).toBe(true);
			expect(textChannels.some((c) => c.id === channel2.id)).toBe(true);
			expect(textChannels.some((c) => c.id === channel3.id)).toBe(true);
		});
		test('should move channel into category via direct PATCH', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const category = await createChannel(harness, account.token, guild.id, 'Category', ChannelTypes.GUILD_CATEGORY);
			const textChannel = await createChannel(harness, account.token, guild.id, 'text-channel');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}/channels`)
				.body([{id: textChannel.id, parent_id: category.id}])
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const updatedChannel = await getChannel(harness, account.token, textChannel.id);
			expect(updatedChannel.parent_id).toBe(category.id);
		});
		test('should move channel out of category via direct PATCH', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const category = await createChannel(harness, account.token, guild.id, 'Category', ChannelTypes.GUILD_CATEGORY);
			const textChannel = await createChannel(harness, account.token, guild.id, 'text-channel');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}/channels`)
				.body([{id: textChannel.id, parent_id: category.id}])
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			let updatedChannel = await getChannel(harness, account.token, textChannel.id);
			expect(updatedChannel.parent_id).toBe(category.id);
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}/channels`)
				.body([{id: textChannel.id, parent_id: null}])
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			updatedChannel = await getChannel(harness, account.token, textChannel.id);
			expect(updatedChannel.parent_id).toBeNull();
		});
		test('should lock permissions when moving to category via direct PATCH', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const category = await createChannel(harness, account.token, guild.id, 'Category', ChannelTypes.GUILD_CATEGORY);
			const textChannel = await createChannel(harness, account.token, guild.id, 'text-channel');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}/channels`)
				.body([{id: textChannel.id, parent_id: category.id, lock_permissions: true}])
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const updatedChannel = await getChannel(harness, account.token, textChannel.id);
			expect(updatedChannel.parent_id).toBe(category.id);
		});
		test('should reject or forbid invalid channel id in position update', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			await createBuilder(harness, account.token)
				.patch(`/guilds/${guild.id}/channels`)
				.body([{id: '999999999999999999', position: 0}])
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
	describe('Channel Parent Validation', () => {
		test('should reject creating a channel under a category from another guild', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const manager = members[0];
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Channel Manager',
				permissions: Permissions.MANAGE_CHANNELS.toString(),
			});
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			const sourceGuild = await createGuild(harness, manager.token, 'Attacker Source Guild');
			const sourceCategory = await createChannel(
				harness,
				manager.token,
				sourceGuild.id,
				'source-category',
				ChannelTypes.GUILD_CATEGORY,
			);
			await createBuilder(harness, manager.token)
				.put(`/channels/${sourceCategory.id}/permissions/${manager.userId}`)
				.body({
					type: 1,
					allow: (Permissions.MENTION_EVERYONE | Permissions.MANAGE_MESSAGES).toString(),
					deny: '0',
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await createBuilder(harness, manager.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({
					type: ChannelTypes.GUILD_TEXT,
					name: 'access',
					parent_id: sourceCategory.id,
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject moving a channel under a category from another guild', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const manager = members[0];
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Channel Manager',
				permissions: Permissions.MANAGE_CHANNELS.toString(),
			});
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			const targetChannel = await createChannel(harness, owner.token, guild.id, 'target-channel');
			const sourceGuild = await createGuild(harness, manager.token, 'Attacker Source Guild');
			const sourceCategory = await createChannel(
				harness,
				manager.token,
				sourceGuild.id,
				'source-category',
				ChannelTypes.GUILD_CATEGORY,
			);
			await createBuilder(harness, manager.token)
				.patch(`/channels/${targetChannel.id}`)
				.body({parent_id: sourceCategory.id})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			const unchangedChannel = await getChannel(harness, owner.token, targetChannel.id);
			expect(unchangedChannel.parent_id).toBeNull();
		});
	});
	describe('Channel Permission Overwrites Operations', () => {
		test('should create permission overwrite for role', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const role = await createRole(harness, account.token, guild.id, {name: 'Test Role'});
			await createBuilder(harness, account.token)
				.put(`/channels/${channel.id}/permissions/${role.id}`)
				.body({
					type: 0,
					allow: Permissions.SEND_MESSAGES.toString(),
					deny: '0',
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const channelData = await getChannel(harness, account.token, channel.id);
			const overwrite = channelData.permission_overwrites?.find((o) => o.id === role.id);
			expect(overwrite).toBeDefined();
			expect(overwrite?.type).toBe(0);
		});
		test('should create permission overwrite for member', async () => {
			const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await createBuilder(harness, owner.token)
				.put(`/channels/${systemChannel.id}/permissions/${member.userId}`)
				.body({
					type: 1,
					allow: Permissions.VIEW_CHANNEL.toString(),
					deny: Permissions.SEND_MESSAGES.toString(),
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const channelData = await getChannel(harness, owner.token, systemChannel.id);
			const overwrite = channelData.permission_overwrites?.find((o) => o.id === member.userId);
			expect(overwrite).toBeDefined();
			expect(overwrite?.type).toBe(1);
		});
		test('should update existing permission overwrite', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const role = await createRole(harness, account.token, guild.id, {name: 'Test Role'});
			await createBuilder(harness, account.token)
				.put(`/channels/${channel.id}/permissions/${role.id}`)
				.body({
					type: 0,
					allow: Permissions.SEND_MESSAGES.toString(),
					deny: '0',
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await createBuilder(harness, account.token)
				.put(`/channels/${channel.id}/permissions/${role.id}`)
				.body({
					type: 0,
					allow: (Permissions.SEND_MESSAGES | Permissions.EMBED_LINKS).toString(),
					deny: '0',
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const channelData = await getChannel(harness, account.token, channel.id);
			const overwrite = channelData.permission_overwrites?.find((o) => o.id === role.id);
			expect(BigInt(overwrite!.allow)).toBe(Permissions.SEND_MESSAGES | Permissions.EMBED_LINKS);
		});
		test('should delete permission overwrite', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const role = await createRole(harness, account.token, guild.id, {name: 'Test Role'});
			await createBuilder(harness, account.token)
				.put(`/channels/${channel.id}/permissions/${role.id}`)
				.body({
					type: 0,
					allow: Permissions.SEND_MESSAGES.toString(),
					deny: '0',
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await createBuilder(harness, account.token)
				.delete(`/channels/${channel.id}/permissions/${role.id}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const channelData = await getChannel(harness, account.token, channel.id);
			const overwrite = channelData.permission_overwrites?.find((o) => o.id === role.id);
			expect(overwrite).toBeUndefined();
		});
		test('should show overwrites in channel response', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			const role = await createRole(harness, account.token, guild.id, {name: 'Test Role'});
			await createBuilder(harness, account.token)
				.put(`/channels/${channel.id}/permissions/${role.id}`)
				.body({
					type: 0,
					allow: Permissions.SEND_MESSAGES.toString(),
					deny: '0',
				})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			const channelData = await getChannel(harness, account.token, channel.id);
			expect(channelData.permission_overwrites).toBeDefined();
			expect(channelData.permission_overwrites?.some((o) => o.id === role.id)).toBe(true);
		});
		test('should reject invalid overwrite type', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const channel = await createChannel(harness, account.token, guild.id, 'test-channel');
			await createBuilder(harness, account.token)
				.put(`/channels/${channel.id}/permissions/123456789`)
				.body({
					type: 999,
					allow: '0',
					deny: '0',
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should require MANAGE_ROLES permission to create overwrites', async () => {
			const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			const role = await createRole(harness, owner.token, guild.id, {name: 'Test Role'});
			await createBuilder(harness, member.token)
				.put(`/channels/${systemChannel.id}/permissions/${role.id}`)
				.body({
					type: 0,
					allow: Permissions.SEND_MESSAGES.toString(),
					deny: '0',
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should require MANAGE_ROLES to set overwrites during channel create', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const manager = members[0];
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Channel Manager',
				permissions: Permissions.MANAGE_CHANNELS.toString(),
			});
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			await createBuilder(harness, manager.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({
					type: ChannelTypes.GUILD_TEXT,
					name: 'deny-on-create',
					permission_overwrites: [
						{
							id: manager.userId,
							type: 1,
							allow: '0',
							deny: Permissions.MANAGE_MESSAGES.toString(),
						},
					],
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
		test('should require MANAGE_ROLES to set overwrites during channel update', async () => {
			const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
			const manager = members[0];
			const managerRole = await createRole(harness, owner.token, guild.id, {
				name: 'Channel Manager',
				permissions: Permissions.MANAGE_CHANNELS.toString(),
			});
			await addMemberRole(harness, owner.token, guild.id, manager.userId, managerRole.id);
			await createBuilder(harness, manager.token)
				.patch(`/channels/${systemChannel.id}`)
				.body({
					permission_overwrites: [
						{
							id: manager.userId,
							type: 1,
							allow: '0',
							deny: Permissions.MANAGE_MESSAGES.toString(),
						},
					],
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});
	describe('Voice Channel Bitrate and User Limit Updates', () => {
		test('should update voice channel bitrate', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, bitrate: 96000})
				.execute();
			expect(data.bitrate).toBe(96000);
		});
		test('should update voice channel user limit', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, user_limit: 10})
				.execute();
			expect(data.user_limit).toBe(10);
		});
		test('should update voice channel connection limit', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, voice_connection_limit: 10})
				.execute();
			expect(data.voice_connection_limit).toBe(10);
		});
		test('should set unlimited user capacity with zero', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, user_limit: 0})
				.execute();
			expect(data.user_limit).toBe(0);
		});
		test('should reject bitrate below minimum (8000)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			await createBuilder(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, bitrate: 7999})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject bitrate above maximum (320000)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			await createBuilder(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, bitrate: 320001})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject negative user limit', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			await createBuilder(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, user_limit: -1})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject connection limit below minimum', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			await createBuilder(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, voice_connection_limit: 0})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject user limit above maximum (99)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			await createBuilder(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, user_limit: 100})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should reject connection limit above maximum', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			await createBuilder(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, voice_connection_limit: 101})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		test('should accept minimum bitrate (8000)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, bitrate: 8000})
				.execute();
			expect(data.bitrate).toBe(8000);
		});
		test('should accept maximum bitrate (320000)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, bitrate: 320000})
				.execute();
			expect(data.bitrate).toBe(320000);
		});
		test('should accept maximum user limit (99)', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, user_limit: 99})
				.execute();
			expect(data.user_limit).toBe(99);
		});
		test('should accept connection limit bounds', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const minData = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, voice_connection_limit: 1})
				.execute();
			const maxData = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, voice_connection_limit: 100})
				.execute();
			expect(minData.voice_connection_limit).toBe(1);
			expect(maxData.voice_connection_limit).toBe(100);
		});
		test('should update both bitrate and user limit together', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, bitrate: 128000, user_limit: 25})
				.execute();
			expect(data.bitrate).toBe(128000);
			expect(data.user_limit).toBe(25);
		});
	});
});

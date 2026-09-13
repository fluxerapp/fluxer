// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannel,
	createChannelInvite,
	createGuild,
	createPermissionOverwrite,
	createRole,
	getChannel,
} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Voice Channel Permissions', () => {
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
	describe('Voice channel creation', () => {
		it('owner can create voice channel', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const voiceChannel = await createChannel(harness, owner.token, guild.id, 'voice-test', ChannelTypes.GUILD_VOICE);
			expect(voiceChannel.type).toBe(ChannelTypes.GUILD_VOICE);
			expect(voiceChannel.name).toBe('voice-test');
		});
		it('member without permission cannot create voice channel', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, member.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);
			await createBuilder(harness, member.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'voice-test', type: ChannelTypes.GUILD_VOICE})
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		});
		it('member with MANAGE_CHANNELS permission can create voice channel', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, member.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);
			const role = await createRole(harness, owner.token, guild.id, {
				name: 'Channel Manager',
				permissions: Permissions.MANAGE_CHANNELS.toString(),
			});
			await addMemberRole(harness, owner.token, guild.id, member.userId, role.id);
			const voiceChannel = await createBuilder<ChannelResponse>(harness, member.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'voice-test', type: ChannelTypes.GUILD_VOICE})
				.execute();
			expect(voiceChannel.type).toBe(ChannelTypes.GUILD_VOICE);
		});
	});
	describe('Voice channel access', () => {
		it('member can view voice channel by default', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, member.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const voiceChannel = await createChannel(harness, owner.token, guild.id, 'voice-test', ChannelTypes.GUILD_VOICE);
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);
			const channel = await getChannel(harness, member.token, voiceChannel.id);
			expect(channel.id).toBe(voiceChannel.id);
			expect(channel.type).toBe(ChannelTypes.GUILD_VOICE);
		});
		it('member cannot view voice channel when VIEW_CHANNEL is denied', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, member.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const voiceChannel = await createChannel(harness, owner.token, guild.id, 'voice-test', ChannelTypes.GUILD_VOICE);
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);
			await createPermissionOverwrite(harness, owner.token, voiceChannel.id, member.userId, {
				type: 1,
				allow: '0',
				deny: Permissions.VIEW_CHANNEL.toString(),
			});
			await createBuilder(harness, member.token)
				.get(`/channels/${voiceChannel.id}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		});
	});
	describe('Voice channel modification', () => {
		it('owner can update voice channel name', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const voiceChannel = await createChannel(harness, owner.token, guild.id, 'voice-test', ChannelTypes.GUILD_VOICE);
			const updated = await createBuilder<ChannelResponse>(harness, owner.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({name: 'updated-voice'})
				.execute();
			expect(updated.name).toBe('updated-voice');
		});
		it('owner can update voice channel bitrate', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const voiceChannel = await createChannel(harness, owner.token, guild.id, 'voice-test', ChannelTypes.GUILD_VOICE);
			const updated = await createBuilder<ChannelResponse>(harness, owner.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({bitrate: 64000})
				.execute();
			expect(updated.bitrate).toBe(64000);
		});
		it('owner can update voice channel user_limit', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const voiceChannel = await createChannel(harness, owner.token, guild.id, 'voice-test', ChannelTypes.GUILD_VOICE);
			const updated = await createBuilder<ChannelResponse>(harness, owner.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({user_limit: 10})
				.execute();
			expect(updated.user_limit).toBe(10);
		});
		it('owner can update voice channel voice_connection_limit', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const voiceChannel = await createChannel(harness, owner.token, guild.id, 'voice-test', ChannelTypes.GUILD_VOICE);
			const updated = await createBuilder<ChannelResponse>(harness, owner.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({voice_connection_limit: 12})
				.execute();
			expect(updated.voice_connection_limit).toBe(12);
		});
		it('member without permission cannot update voice channel', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, member.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const voiceChannel = await createChannel(harness, owner.token, guild.id, 'voice-test', ChannelTypes.GUILD_VOICE);
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);
			await createBuilder(harness, member.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({name: 'updated-voice'})
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		});
	});
	describe('Voice channel deletion', () => {
		it('owner can delete voice channel', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const voiceChannel = await createChannel(harness, owner.token, guild.id, 'voice-test', ChannelTypes.GUILD_VOICE);
			await createBuilder(harness, owner.token)
				.delete(`/channels/${voiceChannel.id}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await createBuilder(harness, owner.token)
				.get(`/channels/${voiceChannel.id}`)
				.expect(HTTP_STATUS.NOT_FOUND)
				.execute();
		});
		it('member without permission cannot delete voice channel', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, member.token);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const voiceChannel = await createChannel(harness, owner.token, guild.id, 'voice-test', ChannelTypes.GUILD_VOICE);
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);
			await createBuilder(harness, member.token)
				.delete(`/channels/${voiceChannel.id}`)
				.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
				.execute();
		});
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID} from '@app/api/BrandedTypes';
import {
	addMemberRole,
	createChannel,
	createRole,
	getChannel,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {getChannelRepository, getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {DEFAULT_VOICE_P2P_CONFIG, type VoiceP2pConfig} from '@fluxer/schema/src/domains/admin/VoiceP2pSchemas';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Guild Channel Management', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	async function setVoiceP2pConfig(config: Partial<VoiceP2pConfig>): Promise<void> {
		await getInstanceConfigRepository().setVoiceP2pConfig({...DEFAULT_VOICE_P2P_CONFIG, ...config});
	}
	async function addGuildFeaturesForTesting(guildId: string, features: Array<string>): Promise<void> {
		await createBuilder<{
			success: boolean;
		}>(harness, '')
			.post(`/test/guilds/${guildId}/features`)
			.body({add_features: features})
			.execute();
	}
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
		test('should default rtc_p2p to false and update it on a voice channel', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			expect(voiceChannel.rtc_p2p).toBe(false);
			await setVoiceP2pConfig({enabled: true, included_user_ids: [account.userId]});
			const enabled = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, rtc_p2p: true})
				.execute();
			expect(enabled.rtc_p2p).toBe(true);
			const untouched = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, user_limit: 4})
				.execute();
			expect(untouched.rtc_p2p).toBe(true);
			const disabled = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, rtc_p2p: false})
				.execute();
			expect(disabled.rtc_p2p).toBe(false);
		});
		test('should refuse enabling rtc_p2p outside the voice_p2p experiment', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			for (const config of [{included_user_ids: [account.userId]}, {enabled: true}]) {
				await setVoiceP2pConfig(config);
				const refusal = await createBuilder<{errors?: Array<{path: string; code: string}>}>(harness, account.token)
					.patch(`/channels/${voiceChannel.id}`)
					.body({type: ChannelTypes.GUILD_VOICE, rtc_p2p: true})
					.expect(HTTP_STATUS.BAD_REQUEST, 'INVALID_FORM_BODY')
					.execute();
				expect(refusal.errors).toContainEqual(
					expect.objectContaining({path: 'rtc_p2p', code: ValidationErrorCodes.GUILD_FEATURE_NOT_TOGGLEABLE}),
				);
			}
			const stored = await getChannelRepository().findUnique(createChannelID(BigInt(voiceChannel.id)));
			expect(stored?.rtcP2p).toBe(false);
			await setVoiceP2pConfig({enabled: true, included_user_ids: [account.userId]});
			await createBuilder(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, rtc_p2p: true})
				.execute();
			await setVoiceP2pConfig({});
			const disabled = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, rtc_p2p: false})
				.execute();
			expect(disabled.rtc_p2p).toBe(false);
		});
		test('should ignore rtc_p2p on a text channel', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const textChannel = await createChannel(harness, account.token, guild.id, 'text-channel');
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${textChannel.id}`)
				.body({type: ChannelTypes.GUILD_TEXT, rtc_p2p: true})
				.execute();
			expect(data.rtc_p2p).toBeUndefined();
			const stored = await getChannelRepository().findUnique(createChannelID(BigInt(textChannel.id)));
			expect(stored?.rtcP2p).toBe(false);
		});
		test('should require MANAGE_CHANNELS to update rtc_p2p', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const voiceChannel = await createChannel(
				harness,
				owner.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			await createBuilder(harness, members[0].token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, rtc_p2p: true})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
			const stored = await getChannelRepository().findUnique(createChannelID(BigInt(voiceChannel.id)));
			expect(stored?.rtcP2p).toBe(false);
		});
		test('should clamp bitrate to 96000 without an audio bitrate feature', async () => {
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
				.body({type: ChannelTypes.GUILD_VOICE, bitrate: 384000})
				.execute();
			expect(data.bitrate).toBe(96000);
		});
		test('should clamp bitrate to the feature the guild holds', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			await addGuildFeaturesForTesting(guild.id, [GuildFeatures.AUDIO_BITRATE_256_KBPS]);
			const voiceChannel = await createChannel(
				harness,
				account.token,
				guild.id,
				'voice-channel',
				ChannelTypes.GUILD_VOICE,
			);
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.patch(`/channels/${voiceChannel.id}`)
				.body({type: ChannelTypes.GUILD_VOICE, bitrate: 384000})
				.execute();
			expect(data.bitrate).toBe(256000);
		});
		test('should clamp bitrate on create without an audio bitrate feature', async () => {
			const account = await createTestAccount(harness);
			const guild = await createGuild(harness, account.token, 'Test Guild');
			const data = await createBuilder<ChannelResponse>(harness, account.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({type: ChannelTypes.GUILD_VOICE, name: 'loud-channel', bitrate: 384000})
				.execute();
			expect(data.bitrate).toBe(96000);
		});
	});
});

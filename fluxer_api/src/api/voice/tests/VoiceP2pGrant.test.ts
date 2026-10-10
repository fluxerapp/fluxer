// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createUserID,
	type GuildID,
	type UserID,
} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {
	getChannelRepository,
	getGuildRepository,
	getInstanceConfigRepository,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {VoiceService} from '@app/api/voice/VoiceService';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {DEFAULT_VOICE_P2P_CONFIG, type VoiceP2pConfig} from '@fluxer/schema/src/domains/admin/VoiceP2pSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const SFU_PATH_REACHED = 'sfu path reached';

function createVoiceService(): VoiceService {
	const sfuOnly = new Proxy(
		{},
		{
			get() {
				throw new Error(SFU_PATH_REACHED);
			},
		},
	);
	return new VoiceService(
		sfuOnly as never,
		getGuildRepository(),
		getUserRepository(),
		getChannelRepository(),
		sfuOnly as never,
		sfuOnly as never,
		getInstanceConfigRepository(),
	);
}

describe('VoiceService peer-to-peer grant', () => {
	let harness: ApiTestHarness;
	let voiceService: VoiceService;
	let params: {guildId: GuildID; channelId: ChannelID; userId: UserID};
	beforeEach(async () => {
		harness = await createApiTestHarness();
		voiceService = createVoiceService();
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'P2P Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'voice', ChannelTypes.GUILD_VOICE);
		params = {
			guildId: createGuildID(BigInt(guild.id)),
			channelId: createChannelID(BigInt(channel.id)),
			userId: createUserID(BigInt(account.userId)),
		};
	});
	afterEach(async () => {
		Config.voice.p2pStunUrls = undefined;
		await harness?.shutdown();
	});

	async function setConfig(config: Partial<VoiceP2pConfig>): Promise<void> {
		await getInstanceConfigRepository().setVoiceP2pConfig({...DEFAULT_VOICE_P2P_CONFIG, ...config});
	}

	test('grants an assigned initiator without selecting a voice server', async () => {
		await setConfig({enabled: true, included_user_ids: [params.userId.toString()]});
		const grant = await voiceService.getVoiceToken({...params, p2p: true, p2pInitiator: true});
		expect(grant).toEqual({
			p2p: true,
			connectionId: expect.stringMatching(/^[a-z]+-[a-z]+$/u),
			iceServers: [],
			maxParticipants: 2,
		});
	});

	test('hands out the configured STUN urls as one entry without credentials', async () => {
		Config.voice.p2pStunUrls = ['stun:stun.example.com:3478', 'stun:stun2.example.com:3478'];
		await setConfig({enabled: true});
		const grant = await voiceService.getVoiceToken({...params, p2p: true, connectionId: 'brisk-forte'});
		expect(grant).toEqual({
			p2p: true,
			connectionId: 'brisk-forte',
			iceServers: [{urls: ['stun:stun.example.com:3478', 'stun:stun2.example.com:3478']}],
			maxParticipants: 2,
		});
	});

	test('carries the configured cap so the call can enforce it on join', async () => {
		await setConfig({enabled: true, max_participants: 3});
		const grant = await voiceService.getVoiceToken({...params, p2p: true, p2pParticipantCount: 2});
		expect(grant).toMatchObject({p2p: true, maxParticipants: 3});
	});

	test('declines a joiner past the configured cap as channel full', async () => {
		await setConfig({enabled: true, max_participants: 3});
		const grant = await voiceService.getVoiceToken({...params, p2p: true, p2pParticipantCount: 3});
		expect(grant).toMatchObject({p2p: true});
		await expect(voiceService.getVoiceToken({...params, p2p: true, p2pParticipantCount: 4})).resolves.toEqual({
			p2p: false,
			declineReason: 'channel_full',
		});
	});

	test('never refuses an initiator for size at the lowest cap', async () => {
		await setConfig({enabled: true, max_participants: 2, included_user_ids: [params.userId.toString()]});
		const grant = await voiceService.getVoiceToken({
			...params,
			p2p: true,
			p2pInitiator: true,
			p2pParticipantCount: 1,
		});
		expect(grant).toMatchObject({p2p: true});
	});

	test('declines every request while disabled whatever the count', async () => {
		await setConfig({enabled: false, max_participants: 2});
		await expect(voiceService.getVoiceToken({...params, p2p: true, p2pParticipantCount: 3})).rejects.toThrow(
			SFU_PATH_REACHED,
		);
	});

	test('keeps the connection id the gateway supplies', async () => {
		await setConfig({enabled: true});
		const grant = await voiceService.getVoiceToken({...params, p2p: true, connectionId: 'brisk-forte'});
		expect(grant).toEqual({p2p: true, connectionId: 'brisk-forte', iceServers: [], maxParticipants: 2});
	});

	test('grants a joiner outside the rollout while the experiment is enabled', async () => {
		await setConfig({enabled: true, excluded_user_ids: [params.userId.toString()]});
		const grant = await voiceService.getVoiceToken({...params, p2p: true, p2pInitiator: false});
		expect(grant).toMatchObject({p2p: true});
	});

	test('declines an initiator outside the rollout', async () => {
		await setConfig({enabled: true});
		await expect(voiceService.getVoiceToken({...params, p2p: true, p2pInitiator: true})).rejects.toThrow(
			SFU_PATH_REACHED,
		);
	});

	test('applies the forwarded country to the initiator rollout', async () => {
		await setConfig({enabled: true, rollout_basis_points: 10_000, rollout_country_codes: ['SE']});
		const grant = await voiceService.getVoiceToken({...params, p2p: true, p2pInitiator: true, countryCode: 'SE'});
		expect(grant).toMatchObject({p2p: true});
		await expect(
			voiceService.getVoiceToken({...params, p2p: true, p2pInitiator: true, countryCode: 'US'}),
		).rejects.toThrow(SFU_PATH_REACHED);
		await expect(voiceService.getVoiceToken({...params, p2p: true, p2pInitiator: true})).rejects.toThrow(
			SFU_PATH_REACHED,
		);
	});

	test('declines every request while the experiment is disabled', async () => {
		await setConfig({enabled: false, included_user_ids: [params.userId.toString()]});
		await expect(voiceService.getVoiceToken({...params, p2p: true, p2pInitiator: false})).rejects.toThrow(
			SFU_PATH_REACHED,
		);
	});

	test('takes the ordinary path when the gateway does not ask for peer-to-peer', async () => {
		await setConfig({enabled: true, included_user_ids: [params.userId.toString()]});
		await expect(voiceService.getVoiceToken(params)).rejects.toThrow(SFU_PATH_REACHED);
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ILiveKitService, LiveKitParticipantRef} from '@app/api/infrastructure/ILiveKitService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import {LiveKitWebhookService} from '@app/api/infrastructure/LiveKitWebhookService';
import type {VoiceTopology} from '@app/api/voice/VoiceTopology';
import type {WebhookEvent} from 'livekit-server-sdk';
import {describe, expect, it} from 'vitest';

const ROOM = 'guild_10_channel_20';
const IDENTITY = 'user_1_conn-1';
const METADATA = JSON.stringify({
	user_id: '1',
	channel_id: '20',
	guild_id: '10',
	connection_id: 'conn-1',
	token_nonce: 'nonce-1',
	issued_at: '1',
	region_id: 'eu',
	server_id: 'eu-1',
});

function createService(confirm: () => Promise<{success: boolean; error?: string}>) {
	const removed: Array<LiveKitParticipantRef> = [];
	let confirmCalls = 0;
	const topology = {
		registerSubscriber: () => {},
		getAllRegions: () => [{id: 'eu'}],
		getServersForRegion: () => [{serverId: 'eu-1', apiKey: 'key', apiSecret: 'secret'}],
	} as unknown as VoiceTopology;
	const gatewayService = {
		confirmVoiceConnection: async () => {
			confirmCalls += 1;
			return confirm();
		},
	} as unknown as IGatewayService;
	const liveKitService = {
		removeParticipant: async (params: LiveKitParticipantRef) => {
			removed.push(params);
		},
	} as unknown as ILiveKitService;
	const service = new LiveKitWebhookService({} as IVoiceRoomStore, gatewayService, liveKitService, topology);
	const join = (metadata: string) =>
		service.processEvent({
			apiKey: 'key',
			event: {
				event: 'participant_joined',
				room: {name: ROOM},
				participant: {identity: IDENTITY, metadata},
			} as unknown as WebhookEvent,
		});
	return {join, removed, confirmCalls: () => confirmCalls};
}

describe('LiveKitWebhookService participant_joined', () => {
	it('disconnects a participant whose metadata cannot be read', async () => {
		const harness = createService(async () => ({success: true}));
		await harness.join('');
		await harness.join('{"user_id":"1"}');
		expect(harness.removed).toEqual([
			{regionId: 'eu', serverId: 'eu-1', roomName: ROOM, identity: IDENTITY},
			{regionId: 'eu', serverId: 'eu-1', roomName: ROOM, identity: IDENTITY},
		]);
		expect(harness.confirmCalls()).toBe(0);
	});

	it('disconnects a participant the gateway rejects', async () => {
		const harness = createService(async () => ({success: false, error: 'voice_nonce_mismatch'}));
		await harness.join(METADATA);
		expect(harness.removed).toEqual([{regionId: 'eu', serverId: 'eu-1', roomName: ROOM, identity: IDENTITY}]);
	});
});

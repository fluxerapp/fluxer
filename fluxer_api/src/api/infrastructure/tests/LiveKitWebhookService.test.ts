// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID} from '@app/api/BrandedTypes';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ILiveKitService} from '@app/api/infrastructure/ILiveKitService';
import type {IVoiceRoomStore} from '@app/api/infrastructure/IVoiceRoomStore';
import {SERVER_MUTE_ATTRIBUTE} from '@app/api/infrastructure/LiveKitService';
import {LiveKitWebhookService} from '@app/api/infrastructure/LiveKitWebhookService';
import type {IVoiceRepository} from '@app/api/voice/IVoiceRepository';
import {VoiceTopology} from '@app/api/voice/VoiceTopology';
import {TrackSource, type WebhookEvent} from 'livekit-server-sdk';
import {describe, expect, it, vi} from 'vitest';

const GUILD_ID = createGuildID(1n);
const CHANNEL_ID = createChannelID(2n);

function roomFinished(name: string): WebhookEvent {
	return {
		event: 'room_finished',
		room: {name, sid: 'RM_test', emptyTimeout: 300, creationTime: 0n},
	} as unknown as WebhookEvent;
}

function harness(pinnedServerId: string | null) {
	const deleteRoomServer = vi.fn(async () => {});
	const voiceRoomStore = {
		getPinnedRoomServer: vi.fn(async () => (pinnedServerId ? {regionId: 'eu', serverId: pinnedServerId} : null)),
		deleteRoomServer,
	} as unknown as IVoiceRoomStore;
	const gatewayService = {} as unknown as IGatewayService;
	const liveKitService = {} as unknown as ILiveKitService;
	const service = new LiveKitWebhookService(
		voiceRoomStore,
		gatewayService,
		liveKitService,
		new VoiceTopology({} as unknown as IVoiceRepository, null),
	);
	return {service, deleteRoomServer};
}

describe('LiveKitWebhookService room_finished', () => {
	it('clears the guild pin without disconnecting anybody', async () => {
		const {service, deleteRoomServer} = harness('eu-1');

		await service.handleRoomFinished(roomFinished(`guild_${GUILD_ID}_channel_${CHANNEL_ID}`), 'unknown-key');

		expect(deleteRoomServer).toHaveBeenCalledTimes(1);
	});

	it('clears the pin even when no server is pinned, and still disconnects nobody', async () => {
		const {service, deleteRoomServer} = harness(null);

		await service.handleRoomFinished(roomFinished(`guild_${GUILD_ID}_channel_${CHANNEL_ID}`), 'unknown-key');

		expect(deleteRoomServer).toHaveBeenCalledTimes(1);
	});

	it('ignores a room name it cannot parse', async () => {
		const {service, deleteRoomServer} = harness('eu-1');

		await service.handleRoomFinished(roomFinished('not_a_voice_room'), 'unknown-key');

		expect(deleteRoomServer).not.toHaveBeenCalled();
	});
});

function trackPublished(source: TrackSource, attributes: Record<string, string>): WebhookEvent {
	return {
		event: 'track_published',
		room: {name: `guild_${GUILD_ID}_channel_${CHANNEL_ID}`},
		participant: {
			identity: 'user_3_conn-1',
			attributes,
			metadata: JSON.stringify({
				user_id: '3',
				channel_id: CHANNEL_ID.toString(),
				guild_id: GUILD_ID.toString(),
				connection_id: 'conn-1',
				region_id: 'eu',
				server_id: 'eu-1',
				token_nonce: 'nonce',
				issued_at: '0',
			}),
		},
		track: {sid: 'TR_new', source},
	} as unknown as WebhookEvent;
}

function trackHarness() {
	const muteMicrophoneTrack = vi.fn(async () => {});
	const service = new LiveKitWebhookService(
		{} as unknown as IVoiceRoomStore,
		{} as unknown as IGatewayService,
		{muteMicrophoneTrack} as unknown as ILiveKitService,
		new VoiceTopology({} as unknown as IVoiceRepository, null),
	);
	return {service, muteMicrophoneTrack};
}

describe('LiveKitWebhookService track_published', () => {
	it('mutes a microphone published by a participant the guild has muted', async () => {
		const {service, muteMicrophoneTrack} = trackHarness();

		await service.processEvent({
			event: trackPublished(TrackSource.MICROPHONE, {[SERVER_MUTE_ATTRIBUTE]: 'true'}),
			apiKey: 'key',
		});

		expect(muteMicrophoneTrack).toHaveBeenCalledWith({
			guildId: GUILD_ID,
			channelId: CHANNEL_ID,
			userId: 3n,
			connectionId: 'conn-1',
			regionId: 'eu',
			serverId: 'eu-1',
			trackSid: 'TR_new',
		});
	});

	it('leaves a microphone alone when the participant is not muted by the guild', async () => {
		const {service, muteMicrophoneTrack} = trackHarness();

		await service.processEvent({event: trackPublished(TrackSource.MICROPHONE, {}), apiKey: 'key'});

		expect(muteMicrophoneTrack).not.toHaveBeenCalled();
	});

	it('leaves other track sources alone', async () => {
		const {service, muteMicrophoneTrack} = trackHarness();

		await service.processEvent({
			event: trackPublished(TrackSource.CAMERA, {[SERVER_MUTE_ATTRIBUTE]: 'true'}),
			apiKey: 'key',
		});

		expect(muteMicrophoneTrack).not.toHaveBeenCalled();
	});
});

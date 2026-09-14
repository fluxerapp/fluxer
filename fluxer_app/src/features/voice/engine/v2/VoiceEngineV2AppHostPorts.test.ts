// SPDX-License-Identifier: AGPL-3.0-or-later

import {createVoiceEngineV2AppTestControllerHost} from '@app/features/voice/engine/v2/VoiceEngineV2AppControllerHostTestUtils';
import {
	createVoiceEngineV2AppHostPorts,
	createVoiceEngineV2AppIngestionPort,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppHostPorts';
import type {VoiceEngineV2Event, VoiceEngineV2Stats} from '@fluxer/voice_engine_v2';
import {waitForRuntime} from '@fluxer/voice_engine_v2/testing';
import {describe, expect, it} from 'vitest';

type AppHostPortCall =
	| {type: 'gateway.writeVoiceState'}
	| {type: 'gateway.clearVoiceState'}
	| {type: 'media.prewarm'}
	| {type: 'media.connect'}
	| {type: 'media.publishMicrophone'}
	| {type: 'media.publishScreen'}
	| {type: 'subscription.setParticipantVolume'}
	| {type: 'subscription.setRemoteTrackSubscription'}
	| {type: 'stats.collect'};

function createFakeStats(): VoiceEngineV2Stats {
	return {
		rttMs: 42,
		outbound: [],
		inbound: [],
	};
}

describe('VoiceEngineV2AppHostPorts', () => {
	it('executes v2 commands through named app ports without the legacy liveKit umbrella port', async () => {
		const calls: Array<AppHostPortCall> = [];
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2AppHostPorts({
				gateway: {
					async writeVoiceState(): Promise<void> {
						calls.push({type: 'gateway.writeVoiceState'});
					},
					async clearVoiceState(): Promise<void> {
						calls.push({type: 'gateway.clearVoiceState'});
					},
				},
				media: {
					async prewarm(): Promise<void> {
						calls.push({type: 'media.prewarm'});
					},
					async connect(): Promise<void> {
						calls.push({type: 'media.connect'});
					},
					async disconnect(): Promise<void> {},
					async publishMicrophone(): Promise<void> {
						calls.push({type: 'media.publishMicrophone'});
					},
					async unpublishMicrophone(): Promise<void> {},
					async setMicrophoneEnabled(): Promise<void> {},
					async publishCamera(): Promise<void> {},
					async updateCameraEncoding(): Promise<void> {},
					async unpublishCamera(): Promise<void> {},
					async publishScreen(): Promise<void> {
						calls.push({type: 'media.publishScreen'});
					},
					async updateScreenEncoding(): Promise<void> {},
					async unpublishScreen(): Promise<void> {},
					async publishScreenAudio(): Promise<void> {},
					async unpublishScreenAudio(): Promise<void> {},
					async setOutputDevice(): Promise<void> {},
					async publishData(): Promise<void> {},
				},
				subscriptions: {
					async setParticipantVolume(): Promise<void> {
						calls.push({type: 'subscription.setParticipantVolume'});
					},
					async setRemoteTrackSubscription(): Promise<void> {
						calls.push({type: 'subscription.setRemoteTrackSubscription'});
					},
				},
				stats: {
					async collectStats(): Promise<VoiceEngineV2Stats> {
						calls.push({type: 'stats.collect'});
						return createFakeStats();
					},
				},
			}),
		});

		host.controller.prewarm();
		await waitForRuntime();
		host.controller.writeGatewayVoiceState({
			guildId: 'guild-1',
			channelId: 'channel-1',
			selfMute: false,
			selfDeaf: false,
		});
		await waitForRuntime();
		host.controller.connect({url: 'wss://voice.example.test', token: 'token'});
		await waitForRuntime();
		host.controller.publishMicrophone({deviceId: 'mic-1'});
		await waitForRuntime();
		host.controller.publishScreen({
			captureId: 'screen:1',
			width: 1920,
			height: 1080,
			zeroCopyRequired: false,
		});
		await waitForRuntime();
		host.controller.setParticipantVolume({participantIdentity: 'user-2', volume: 0.5});
		await waitForRuntime();
		host.controller.setRemoteTrackSubscription({
			participantIdentity: 'user-2',
			source: 'camera',
			subscribed: true,
			quality: 'high',
		});
		await waitForRuntime();
		host.controller.collectStats();
		await waitForRuntime();
		host.controller.clearGatewayVoiceState('guild-1');
		await waitForRuntime();

		expect(calls).toEqual([
			{type: 'media.prewarm'},
			{type: 'gateway.writeVoiceState'},
			{type: 'media.connect'},
			{type: 'media.publishMicrophone'},
			{type: 'media.publishScreen'},
			{type: 'subscription.setParticipantVolume'},
			{type: 'subscription.setRemoteTrackSubscription'},
			{type: 'stats.collect'},
			{type: 'gateway.clearVoiceState'},
		]);
		expect(host.model.stats).toEqual(createFakeStats());

		host.dispose();
	});

	it('ingests voice-state and participant projection events through app event-source ports', () => {
		const voiceState = createVoiceEngineV2AppIngestionPort();
		const participantProjection = createVoiceEngineV2AppIngestionPort();
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2AppHostPorts({
				voiceState,
				participantProjection,
			}),
		});

		voiceState.ingest({
			type: 'gateway.voiceStateUpdated',
			voiceState: {
				guildId: 'guild-1',
				channelId: 'channel-1',
				userId: 'user-1',
				sessionId: 'session-1',
				selfMute: false,
				selfDeaf: false,
				selfVideo: false,
				selfStream: false,
				suppress: false,
				requestToSpeakTimestamp: null,
			},
		});
		const participantEvent: VoiceEngineV2Event = {
			type: 'room.participantJoined',
			participant: {
				sid: 'participant-1',
				identity: 'user-1',
				name: 'Ada',
			},
		};
		participantProjection.ingest(participantEvent);

		expect(host.model.connection.gateway.selfVoiceState?.channelId).toBe('channel-1');
		expect(host.model.participants).toEqual([
			{
				sid: 'participant-1',
				identity: 'user-1',
				name: 'Ada',
			},
		]);

		host.dispose();
	});

	it('detaches app ingestion event sources when the host is disposed', () => {
		const voiceState = createVoiceEngineV2AppIngestionPort();
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2AppHostPorts({voiceState}),
		});

		voiceState.ingest({
			type: 'gateway.voiceStateUpdated',
			voiceState: {
				guildId: 'guild-1',
				channelId: 'channel-1',
				userId: 'user-1',
				sessionId: 'session-1',
				selfMute: false,
				selfDeaf: false,
				selfVideo: false,
				selfStream: false,
				suppress: false,
				requestToSpeakTimestamp: null,
			},
		});

		expect(host.model.connection.gateway.selfVoiceState?.channelId).toBe('channel-1');

		host.dispose();
		voiceState.ingest({
			type: 'gateway.voiceStateUpdated',
			voiceState: {
				guildId: 'guild-1',
				channelId: 'channel-2',
				userId: 'user-1',
				sessionId: 'session-2',
				selfMute: true,
				selfDeaf: false,
				selfVideo: false,
				selfStream: false,
				suppress: false,
				requestToSpeakTimestamp: null,
			},
		});

		expect(host.model.connection.gateway.selfVoiceState?.channelId).toBe('channel-1');
	});
});

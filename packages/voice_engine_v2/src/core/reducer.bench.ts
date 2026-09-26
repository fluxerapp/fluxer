// SPDX-License-Identifier: AGPL-3.0-or-later

import basicSessionFixture from '@fluxer/voice_engine_v2/fixtures/basic_session.json';
import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2InboundVideoFrame,
	VoiceEngineV2InboundVideoFrameStats,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2Participant,
	VoiceEngineV2ScreenOptions,
	VoiceEngineV2Stats,
} from '@fluxer/voice_engine_v2/src/protocol/types';
import {test} from 'vitest';

interface VoiceEngineV2BenchFixture {
	events: Array<VoiceEngineV2Event>;
}

function buildConnectedSnapshotWithVideoTrack(): VoiceEngineV2Snapshot {
	const fixture = basicSessionFixture as VoiceEngineV2BenchFixture;
	let snapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
	for (const event of fixture.events) {
		snapshot = transitionVoiceEngineV2(snapshot, event).snapshot;
	}
	snapshot = transitionVoiceEngineV2(snapshot, {
		type: 'inboundVideo.trackSubscribed',
		track: {
			participantSid: 'sid-remote',
			participantIdentity: 'identity-remote',
			trackSid: 'track-video-1',
			source: 'camera',
			width: 1280,
			height: 720,
		},
	}).snapshot;
	return snapshot;
}

const connectedSnapshot = buildConnectedSnapshotWithVideoTrack();

const frameEvent: VoiceEngineV2Event = {
	type: 'inboundVideo.frameReceived',
	frame: Object.freeze({
		participantSid: 'sid-remote',
		participantIdentity: 'identity-remote',
		trackSid: 'track-video-1',
		width: 1280,
		height: 720,
		timestampUs: 1_000_000,
		byteLength: 320_000,
	}) satisfies VoiceEngineV2InboundVideoFrame,
};

const frameStatsEvent: VoiceEngineV2Event = {
	type: 'inboundVideo.frameStats',
	stats: Object.freeze({
		participantSid: 'sid-remote',
		participantIdentity: 'identity-remote',
		trackSid: 'track-video-1',
		width: 1280,
		height: 720,
		frameCount: 900,
		lastFrameTimestampUs: 30_000_000,
		lastFrameByteLength: 320_000,
	}) satisfies VoiceEngineV2InboundVideoFrameStats,
};

const microphoneEvent: VoiceEngineV2Event = {
	type: 'microphone.publishRequested',
	options: Object.freeze({deviceId: 'mic-alt', echoCancellation: true}) satisfies VoiceEngineV2MicrophoneOptions,
};

const screenEvent: VoiceEngineV2Event = {
	type: 'screen.publishRequested',
	options: Object.freeze({
		captureId: 'capture-1',
		width: 1920,
		height: 1080,
		codec: 'h264',
		maxFramerate: 30,
		maxBitrateBps: 4_000_000,
	}) satisfies VoiceEngineV2ScreenOptions,
};

const participantEvent: VoiceEngineV2Event = {
	type: 'room.participantJoined',
	participant: Object.freeze({
		sid: 'sid-joiner',
		identity: 'identity-joiner',
		name: 'Joiner',
	}) satisfies VoiceEngineV2Participant,
};

const statsPayload: VoiceEngineV2Stats = Object.freeze({rttMs: 42, outbound: [], inbound: []});
const statsEvent: VoiceEngineV2Event = {type: 'stats.collected', operationId: 9_999_999, stats: statsPayload};

test('voice engine v2 reducer hot paths', async ({bench}) => {
	await bench('inboundVideo.frameReceived', () => {
		transitionVoiceEngineV2(connectedSnapshot, frameEvent);
	}).run();

	await bench('inboundVideo.frameStats', () => {
		transitionVoiceEngineV2(connectedSnapshot, frameStatsEvent);
	}).run();

	await bench('microphone.publishRequested', () => {
		transitionVoiceEngineV2(connectedSnapshot, microphoneEvent);
	}).run();

	await bench('screen.publishRequested', () => {
		transitionVoiceEngineV2(connectedSnapshot, screenEvent);
	}).run();

	await bench('room.participantJoined', () => {
		transitionVoiceEngineV2(connectedSnapshot, participantEvent);
	}).run();

	await bench('stats.collected', () => {
		transitionVoiceEngineV2(connectedSnapshot, statsEvent);
	}).run();
});

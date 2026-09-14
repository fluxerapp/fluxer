// SPDX-License-Identifier: AGPL-3.0-or-later

import basicSessionFixture from '@fluxer/voice_engine_v2/fixtures/basic_session.json';
import {availableVoiceEngineV2Capabilities} from '@fluxer/voice_engine_v2/src/core/state';
import type {
	VoiceEngineV2CommandResult,
	VoiceEngineV2ExternalEventListener,
	VoiceEngineV2Implementation,
} from '@fluxer/voice_engine_v2/src/implementations';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2InboundVideoFrame,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2Participant,
	VoiceEngineV2ScreenOptions,
	VoiceEngineV2Stats,
} from '@fluxer/voice_engine_v2/src/protocol/types';
import {createVoiceEngineV2MemoryEventLogSpillSink} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
import {VoiceEngineV2Runtime} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
import {bench, describe} from 'vitest';

interface VoiceEngineV2BenchFixture {
	events: Array<VoiceEngineV2Event>;
}

class NoopImplementation implements VoiceEngineV2Implementation {
	readonly kind = 'js' as const;

	execute(_command: VoiceEngineV2Command): Promise<VoiceEngineV2CommandResult> {
		return Promise.resolve({ok: true});
	}

	subscribe(_listener: VoiceEngineV2ExternalEventListener): () => void {
		return () => {};
	}
}

function buildRuntimeWithConnectedSession(): VoiceEngineV2Runtime {
	const runtime = new VoiceEngineV2Runtime(new NoopImplementation(), {
		capabilities: availableVoiceEngineV2Capabilities(),
		clock: {now: () => 0},
		eventLogSpillSink: createVoiceEngineV2MemoryEventLogSpillSink(),
	});
	const fixture = basicSessionFixture as VoiceEngineV2BenchFixture;
	for (const event of fixture.events) {
		runtime.dispatch(event);
	}
	runtime.dispatch({
		type: 'inboundVideo.trackSubscribed',
		track: {
			participantSid: 'sid-remote',
			participantIdentity: 'identity-remote',
			trackSid: 'track-video-1',
			source: 'camera',
			width: 1280,
			height: 720,
		},
	});
	return runtime;
}

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

const microphoneEvent: VoiceEngineV2Event = {
	type: 'microphone.publishRequested',
	options: Object.freeze({deviceId: 'mic-alt'}) satisfies VoiceEngineV2MicrophoneOptions,
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

const statsPayload: VoiceEngineV2Stats = Object.freeze({rttMs: 7, outbound: [], inbound: []});
const statsEvent: VoiceEngineV2Event = {type: 'stats.collected', operationId: 9_999_999, stats: statsPayload};

const runtimeForFrames = buildRuntimeWithConnectedSession();
const runtimeForMicrophone = buildRuntimeWithConnectedSession();
const runtimeForScreen = buildRuntimeWithConnectedSession();
const runtimeForParticipant = buildRuntimeWithConnectedSession();
const runtimeForStats = buildRuntimeWithConnectedSession();

describe('voice engine v2 runtime dispatch round-trip', () => {
	bench('dispatch inboundVideo.frameReceived', () => {
		runtimeForFrames.dispatch(frameEvent);
	});

	bench('dispatch microphone.publishRequested', () => {
		runtimeForMicrophone.dispatch(microphoneEvent);
	});

	bench('dispatch screen.publishRequested', () => {
		runtimeForScreen.dispatch(screenEvent);
	});

	bench('dispatch room.participantJoined', () => {
		runtimeForParticipant.dispatch(participantEvent);
	});

	bench('dispatch stats.collected', () => {
		runtimeForStats.dispatch(statsEvent);
	});
});

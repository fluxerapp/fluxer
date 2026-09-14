// SPDX-License-Identifier: AGPL-3.0-or-later

import fixtureJson from '@fluxer/voice_engine_v2/fixtures/basic_session.json';
import cameraInPlaceUpdateFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/camera_in_place_update.json';
import codecRepublishFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/codec_republish.json';
import nativeFrameSinkFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/native_frame_sink.json';
import nativeZeroCopyScreenFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/native_zero_copy_screen.json';
import screenAudioInclusionToggleFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/screen_audio_inclusion_toggle.json';
import screenAudioRoutingFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/screen_audio_routing.json';
import screenShareStartUpdateStopFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/screen_share_start_update_stop.json';
import externallyEstablishedFixtureJson from '@fluxer/voice_engine_v2/fixtures/externally_established_session.json';
import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Command, VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol';
import {describe, expect, it} from 'vitest';

interface VoiceEngineV2FixtureExpected {
	nextOperationId: number;
	connectionStatus: string;
	activeUrl: string;
	microphoneStatus: string;
	microphoneDeviceId: string;
	gatewayChannelId: string;
}

interface VoiceEngineV2Fixture {
	name: string;
	events: Array<VoiceEngineV2Event>;
	expected: VoiceEngineV2FixtureExpected;
}

interface VoiceEngineV2ScreenFixtureExpected {
	nextOperationId: number;
	cameraStatus?: string;
	cameraDeviceId?: string | null;
	cameraWidth?: number;
	cameraFrameRate?: number;
	cameraMirror?: boolean;
	cameraBackgroundMode?: string;
	screenStatus?: string;
	screenCaptureId?: string | null;
	screenCodec?: string;
	screenHardwareEncoding?: boolean;
	screenZeroCopyRequired?: boolean;
	screenAudioStatus?: string;
	screenAudioRoute?: string;
	screenAudioTapId?: string;
	nativeCaptureCount?: number;
	nativeFrameSinkCount?: number;
	lastFailureCode?: string | null;
}

interface VoiceEngineV2CommandReplayFixture {
	name: string;
	events: Array<VoiceEngineV2Event>;
	expectedCommands: Array<VoiceEngineV2Command>;
	expected: VoiceEngineV2ScreenFixtureExpected;
}

const screenEventLogFixtures = [
	screenShareStartUpdateStopFixtureJson,
	cameraInPlaceUpdateFixtureJson,
	screenAudioInclusionToggleFixtureJson,
	screenAudioRoutingFixtureJson,
	codecRepublishFixtureJson,
	nativeZeroCopyScreenFixtureJson,
	nativeFrameSinkFixtureJson,
] as Array<VoiceEngineV2CommandReplayFixture>;

const sessionFixtures = [fixtureJson, externallyEstablishedFixtureJson] as Array<VoiceEngineV2Fixture>;

describe('voice engine v2 shared fixtures', () => {
	it.each(sessionFixtures)('replays the session event log fixture: $name', (fixture) => {
		let snapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());

		for (const event of fixture.events) {
			snapshot = transitionVoiceEngineV2(snapshot, event).snapshot;
		}

		expect(snapshot.nextOperationId).toBe(fixture.expected.nextOperationId);
		expect(snapshot.connection.status).toBe(fixture.expected.connectionStatus);
		expect(snapshot.connection.active?.url).toBe(fixture.expected.activeUrl);
		expect(snapshot.microphone.status).toBe(fixture.expected.microphoneStatus);
		expect(snapshot.microphone.published?.deviceId).toBe(fixture.expected.microphoneDeviceId);
		expect(snapshot.gateway.selfVoiceState?.channelId).toBe(fixture.expected.gatewayChannelId);
	});

	it.each(screenEventLogFixtures)('replays screen/native event log fixture: $name', (fixture) => {
		let snapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
		const commands: Array<VoiceEngineV2Command> = [];

		for (const event of fixture.events) {
			const transition = transitionVoiceEngineV2(snapshot, event);
			commands.push(...transition.commands);
			snapshot = transition.snapshot;
		}

		expect(commands).toEqual(fixture.expectedCommands);
		expect(snapshot.nextOperationId).toBe(fixture.expected.nextOperationId);
		if (fixture.expected.cameraStatus !== undefined) {
			expect(snapshot.camera.status).toBe(fixture.expected.cameraStatus);
		}
		if (fixture.expected.cameraDeviceId !== undefined) {
			expect(snapshot.camera.published?.deviceId ?? null).toBe(fixture.expected.cameraDeviceId);
		}
		if (fixture.expected.cameraWidth !== undefined) {
			expect(snapshot.camera.published?.width).toBe(fixture.expected.cameraWidth);
		}
		if (fixture.expected.cameraFrameRate !== undefined) {
			expect(snapshot.camera.published?.frameRate).toBe(fixture.expected.cameraFrameRate);
		}
		if (fixture.expected.cameraMirror !== undefined) {
			expect(snapshot.camera.published?.mirror).toBe(fixture.expected.cameraMirror);
		}
		if (fixture.expected.cameraBackgroundMode !== undefined) {
			expect(snapshot.camera.published?.backgroundMode).toBe(fixture.expected.cameraBackgroundMode);
		}
		if (fixture.expected.screenStatus !== undefined) {
			expect(snapshot.screen.status).toBe(fixture.expected.screenStatus);
		}
		if (fixture.expected.screenCaptureId !== undefined) {
			expect(snapshot.screen.published?.captureId ?? null).toBe(fixture.expected.screenCaptureId);
		}
		if (fixture.expected.screenCodec !== undefined) {
			expect(snapshot.screen.published?.codec).toBe(fixture.expected.screenCodec);
		}
		if (fixture.expected.screenHardwareEncoding !== undefined) {
			expect(snapshot.screen.published?.hardwareEncoding).toBe(fixture.expected.screenHardwareEncoding);
		}
		if (fixture.expected.screenZeroCopyRequired !== undefined) {
			expect(snapshot.screen.published?.zeroCopyRequired).toBe(fixture.expected.screenZeroCopyRequired);
		}
		if (fixture.expected.screenAudioStatus !== undefined) {
			expect(snapshot.screenAudio.status).toBe(fixture.expected.screenAudioStatus);
		}
		if (fixture.expected.screenAudioRoute !== undefined) {
			expect(snapshot.screenAudio.published?.route).toBe(fixture.expected.screenAudioRoute);
		}
		if (fixture.expected.screenAudioTapId !== undefined) {
			expect(snapshot.screenAudio.published?.tapId).toBe(fixture.expected.screenAudioTapId);
		}
		if (fixture.expected.nativeCaptureCount !== undefined) {
			expect(Object.keys(snapshot.nativeCapture.captures)).toHaveLength(fixture.expected.nativeCaptureCount);
		}
		if (fixture.expected.nativeFrameSinkCount !== undefined) {
			expect(Object.keys(snapshot.nativeFrameSink.sinks)).toHaveLength(fixture.expected.nativeFrameSinkCount);
		}
		if (fixture.expected.lastFailureCode !== undefined) {
			expect(snapshot.lastFailure?.code ?? null).toBe(fixture.expected.lastFailureCode);
		}
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	isVoiceEngineV2AppParticipantSpeaking,
	selectVoiceEngineV2AppConnection,
	selectVoiceEngineV2AppConnectionWithFallback,
	selectVoiceEngineV2AppDevices,
	selectVoiceEngineV2AppE2ee,
	selectVoiceEngineV2AppEffectiveSelfMuteForVoiceStatePayload,
	selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls,
	selectVoiceEngineV2AppInboundVideoTrack,
	selectVoiceEngineV2AppIntentSelfMuteForVoiceStatePayload,
	selectVoiceEngineV2AppIntentSelfMuteFromAudioControls,
	selectVoiceEngineV2AppLocalMedia,
	selectVoiceEngineV2AppMuteReason,
	selectVoiceEngineV2AppParticipant,
	selectVoiceEngineV2AppParticipants,
	selectVoiceEngineV2AppStats,
	selectVoiceEngineV2AppTrackForSource,
	selectVoiceEngineV2AppTracks,
	selectVoiceEngineV2AppTracksForParticipant,
	selectVoiceEngineV2AppView,
	selectVoiceEngineV2AppWatchedStream,
	selectVoiceEngineV2AppWatchedStreams,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	transitionVoiceEngineV2,
	type VoiceEngineV2Event,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2';
import appVoiceSessionFixtureJson from '@fluxer/voice_engine_v2/fixtures/event_logs/app_voice_session.json';
import {replayVoiceEngineV2EventLogFixture, type VoiceEngineV2EventLogFixture} from '@fluxer/voice_engine_v2/testing';
import {describe, expect, it} from 'vitest';

function replayEvents(events: Array<VoiceEngineV2Event>): VoiceEngineV2Snapshot {
	let snapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
	for (const event of events) {
		snapshot = transitionVoiceEngineV2(snapshot, event).snapshot;
	}
	return snapshot;
}

describe('VoiceEngineV2AppSelectors', () => {
	it('projects the canonical app voice-session fixture from snapshots and models', () => {
		const replay = replayVoiceEngineV2EventLogFixture(appVoiceSessionFixtureJson as VoiceEngineV2EventLogFixture);
		const snapshotView = selectVoiceEngineV2AppView(replay.finalSnapshot);
		const modelView = selectVoiceEngineV2AppView(replay.finalModel);

		expect(snapshotView).toEqual(modelView);
		expect(snapshotView.connection).toMatchObject({
			status: 'connected',
			connected: true,
			canPublishMedia: true,
			guildId: 'guild-1',
			channelId: 'channel-1',
			userId: 'user-1',
			sessionId: 'session-1',
			roomSid: 'room-1',
			roomName: 'guild-1/channel-1',
			serverRegion: 'us-east',
		});
		expect(snapshotView.localMedia).toMatchObject({
			microphone: 'published',
			camera: 'published',
			screen: 'published',
			screenAudio: 'published',
			audioMode: 'pushToTalk',
			hasActiveLocalMedia: true,
			effectiveMicrophoneEnabled: true,
			localSpeakingOverride: true,
			pushToTalkActive: true,
			screenCaptureId: 'screen-1',
		});
		expect(snapshotView.participants.participantIdentities).toEqual(['user-2']);
		expect(snapshotView.tracks.microphoneTracks.map((track) => track.trackSid)).toEqual(['track-remote-mic-1']);
		expect(snapshotView.tracks.screenTracks.map((track) => track.trackSid)).toEqual(['track-remote-screen-1']);
		expect(snapshotView.devices.selectedAudioInput?.label).toBe('Studio Mic');
		expect(snapshotView.devices.selectedAudioOutput?.label).toBe('Desk Speakers');
		expect(snapshotView.devices.selectedCamera?.label).toBe('Face Camera');
		expect(snapshotView.stats).toMatchObject({
			hasStats: true,
			rttMs: 42,
			outboundTrackCount: 2,
			inboundTrackCount: 1,
			droppedNativeVideoFrames: 0,
			failureCode: null,
		});
		expect(snapshotView.stats.summary?.network).toMatchObject({
			audioSendBitrateKbps: 48,
			audioRecvBitrateKbps: 40,
			videoSendBitrateKbps: 2500,
			rttMs: 42,
		});
		expect(snapshotView.e2ee).toMatchObject({status: 'disabled', enabled: false, pending: false, failed: false});
	});

	it('selects participants, tracks, watched streams, and inbound video from v2 events', () => {
		const snapshot = replayEvents([
			{type: 'connection.connectRequested', options: {url: 'wss://voice.example.test', token: 'token-1'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{
				type: 'watchedStream.watchRequested',
				stream: {
					participantIdentity: 'alice',
					source: 'screen',
					trackSid: null,
					quality: 'high',
					enabled: true,
				},
			},
			{type: 'room.participantJoined', participant: {sid: 'PA_alice', identity: 'alice', name: 'Alice'}},
			{
				type: 'room.trackPublished',
				track: {
					participantIdentity: 'alice',
					participantSid: 'PA_alice',
					trackSid: 'TR_screen',
					trackName: 'screen',
					kind: 'video',
					source: 'screen',
					muted: false,
				},
			},
			{
				type: 'room.trackPublished',
				track: {
					participantIdentity: 'alice',
					participantSid: 'PA_alice',
					trackSid: 'TR_microphone',
					trackName: 'microphone',
					kind: 'audio',
					source: 'microphone',
					muted: false,
				},
			},
			{
				type: 'inboundVideo.trackSubscribed',
				track: {
					participantSid: 'PA_alice',
					participantIdentity: 'alice',
					trackSid: 'TR_screen',
					source: 'screen',
				},
			},
			{
				type: 'inboundVideo.frameReceived',
				frame: {
					participantSid: 'PA_alice',
					participantIdentity: 'alice',
					trackSid: 'TR_screen',
					width: 1280,
					height: 720,
					timestampUs: 2000,
					byteLength: 1_382_400,
				},
			},
		]);

		expect(selectVoiceEngineV2AppConnection(snapshot).canPublishMedia).toBe(true);
		expect(selectVoiceEngineV2AppParticipants(snapshot).participantIdentities).toEqual(['alice']);
		expect(selectVoiceEngineV2AppParticipant(snapshot, 'alice')?.name).toBe('Alice');
		expect(selectVoiceEngineV2AppTracks(snapshot)).toMatchObject({
			audioTracks: [{trackSid: 'TR_microphone'}],
			videoTracks: [{trackSid: 'TR_screen'}],
			screenTracks: [{trackSid: 'TR_screen'}],
		});
		expect(selectVoiceEngineV2AppTracksForParticipant(snapshot, 'alice').map((track) => track.trackSid)).toEqual([
			'TR_microphone',
			'TR_screen',
		]);
		expect(selectVoiceEngineV2AppTrackForSource(snapshot, 'alice', 'screen')?.trackSid).toBe('TR_screen');
		expect(selectVoiceEngineV2AppWatchedStreams(snapshot).enabledStreams).toEqual([
			{
				participantIdentity: 'alice',
				source: 'screen',
				trackSid: 'TR_screen',
				quality: 'high',
				enabled: true,
			},
		]);
		expect(selectVoiceEngineV2AppWatchedStream(snapshot, 'alice', 'screen')?.trackSid).toBe('TR_screen');
		expect(selectVoiceEngineV2AppInboundVideoTrack(snapshot, 'TR_screen')).toMatchObject({
			participantIdentity: 'alice',
			width: 1280,
			height: 720,
			frameCount: 1,
			lastFrameByteLength: 1_382_400,
		});
	});

	it('projects selected devices, permissions, stats failures, and E2EE without exposing reducer bookkeeping', () => {
		const snapshot = replayEvents([
			{type: 'connection.connectRequested', options: {url: 'wss://voice.example.test', token: 'token-1'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{
				type: 'devices.changed',
				operationId: null,
				reason: 'initial',
				devices: {
					audioInputs: [{deviceId: 'mic-1', label: 'Studio Mic', isDefault: true, role: 'communications'}],
					audioOutputs: [{deviceId: 'speaker-1', label: 'Desk Speakers', isDefault: true}],
					cameras: [{deviceId: 'camera-1', label: 'Face Camera'}],
					selectedAudioInputId: 'mic-1',
					selectedAudioOutputId: 'speaker-1',
					selectedCameraId: 'camera-1',
				},
			},
			{
				type: 'permissions.result',
				operationId: null,
				result: {name: 'microphone', status: 'denied', canPrompt: true},
			},
			{type: 'stats.collectRequested'},
			{
				type: 'stats.collectFailed',
				operationId: 2,
				error: {code: 'implementationError', message: 'stats failed'},
			},
			{type: 'e2ee.setEnabledRequested', enabled: true, keyId: 'key-a'},
			{type: 'e2ee.enabled', operationId: 3, keyId: 'key-a'},
			{
				type: 'e2ee.failed',
				operationId: null,
				error: {code: 'implementationError', message: 'key rotation failed', capability: 'e2ee'},
			},
		]);
		const devices = selectVoiceEngineV2AppDevices(snapshot);
		const stats = selectVoiceEngineV2AppStats(snapshot);
		const e2ee = selectVoiceEngineV2AppE2ee(snapshot);

		expect(devices).toMatchObject({
			hasAudioInput: true,
			hasAudioOutput: true,
			hasCamera: true,
			selectedAudioInput: {deviceId: 'mic-1', label: 'Studio Mic', role: 'communications'},
			selectedAudioOutput: {deviceId: 'speaker-1', label: 'Desk Speakers'},
			selectedCamera: {deviceId: 'camera-1', label: 'Face Camera'},
			microphonePermission: {name: 'microphone', status: 'denied', canPrompt: true},
		});
		expect(stats).toMatchObject({
			hasStats: false,
			summary: null,
			rttMs: null,
			outboundTrackCount: 0,
			inboundTrackCount: 0,
			failureCode: 'implementationError',
		});
		expect(e2ee).toMatchObject({
			status: 'failed',
			keyId: 'key-a',
			enabled: false,
			pending: false,
			failed: true,
			failure: {code: 'implementationError', capability: 'e2ee'},
		});
		expect('operationId' in e2ee).toBe(false);
	});

	it('keeps model-only selectors usable when no snapshot failure state is available', () => {
		const replay = replayVoiceEngineV2EventLogFixture(appVoiceSessionFixtureJson as VoiceEngineV2EventLogFixture);
		const localMedia = selectVoiceEngineV2AppLocalMedia(replay.finalModel);
		const stats = selectVoiceEngineV2AppStats(replay.finalModel);

		expect(localMedia.screenCaptureId).toBe('screen-1');
		expect(stats.failureCode).toBeNull();
		expect(stats.summary?.localScreenShare?.trackIdentifier).toBe('local-screen');
	});

	it('uses app-shell connection fallback only while v2 has no active connection projection', () => {
		const initialSnapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
		expect(
			selectVoiceEngineV2AppConnectionWithFallback(initialSnapshot, {
				connected: true,
				connecting: false,
				guildId: 'guild-fallback',
				channelId: 'channel-fallback',
				sessionId: 'connection-fallback',
			}),
		).toMatchObject({
			status: 'connected',
			connected: true,
			connecting: false,
			guildId: 'guild-fallback',
			channelId: 'channel-fallback',
			sessionId: 'connection-fallback',
		});

		const connectedSnapshot = replayEvents([
			{type: 'connection.connectRequested', options: {url: 'wss://voice.example.test', token: 'token-1'}},
			{type: 'connection.connectSucceeded', operationId: 1},
			{
				type: 'gateway.voiceStateUpdated',
				voiceState: {
					guildId: 'guild-v2',
					channelId: 'channel-v2',
					userId: 'user-v2',
					sessionId: 'session-v2',
					selfMute: false,
					selfDeaf: false,
					selfVideo: false,
					selfStream: false,
					suppress: false,
					requestToSpeakTimestamp: null,
				},
			},
		]);
		expect(
			selectVoiceEngineV2AppConnectionWithFallback(connectedSnapshot, {
				connected: true,
				channelId: 'channel-fallback',
			}),
		).toMatchObject({
			status: 'connected',
			connected: true,
			channelId: 'channel-v2',
			sessionId: 'session-v2',
		});
	});

	it('checks participant speaking from the v2 app selector helper', () => {
		expect(isVoiceEngineV2AppParticipantSpeaking(null)).toBe(false);
		expect(isVoiceEngineV2AppParticipantSpeaking({isSpeaking: false, isAudioLevelSpeaking: false})).toBe(false);
		expect(isVoiceEngineV2AppParticipantSpeaking({isSpeaking: true, isAudioLevelSpeaking: false})).toBe(true);
		expect(isVoiceEngineV2AppParticipantSpeaking({isSpeaking: false, isAudioLevelSpeaking: true})).toBe(true);
	});

	it('projects the effective self-mute payload from v2 audio controls', () => {
		const pushToTalkMutedSnapshot = replayEvents([
			{
				type: 'audioControls.changed',
				controls: {
					mode: 'pushToTalk',
					locallyMuted: false,
					pushToTalkActive: false,
				},
			},
		]);
		const pushToMuteMutedSnapshot = replayEvents([
			{
				type: 'audioControls.changed',
				controls: {
					mode: 'pushToMute',
					locallyMuted: false,
					pushToMuteActive: true,
				},
			},
		]);
		const voiceActivityUnmutedSnapshot = replayEvents([
			{
				type: 'audioControls.changed',
				controls: {
					mode: 'voiceActivity',
					locallyMuted: false,
					pushToTalkActive: false,
					pushToMuteActive: false,
				},
			},
		]);

		expect(selectVoiceEngineV2AppEffectiveSelfMuteForVoiceStatePayload(pushToTalkMutedSnapshot)).toBe(true);
		expect(selectVoiceEngineV2AppEffectiveSelfMuteForVoiceStatePayload(pushToMuteMutedSnapshot)).toBe(true);
		expect(selectVoiceEngineV2AppEffectiveSelfMuteForVoiceStatePayload(voiceActivityUnmutedSnapshot)).toBe(false);
		expect(
			selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls({
				mode: 'pushToTalk',
				locallyMuted: false,
				preferredLocallyMuted: false,
				locallyDeafened: false,
				mutedByPermission: false,
				hasUserSetMute: false,
				hasUserSetDeaf: false,
				shouldUnmuteOnUndeafen: false,
				pushToTalkActive: false,
				pushToMuteActive: false,
				inputVolume: 1,
				outputVolume: 1,
			}),
		).toBe(true);
	});

	it('projects the gateway self-mute payload from intent, not push-to-talk silence (canonical bug)', () => {
		const pushToTalkUnmutedIdle = replayEvents([
			{
				type: 'audioControls.changed',
				controls: {
					mode: 'pushToTalk',
					locallyMuted: false,
					pushToTalkActive: false,
					pushToMuteActive: false,
				},
			},
		]);
		const pushToTalkMutedIdle = replayEvents([
			{
				type: 'localAudio.muteRequested',
				muted: true,
			},
			{
				type: 'audioControls.changed',
				controls: {
					mode: 'pushToTalk',
					pushToTalkActive: false,
				},
			},
		]);

		expect(selectVoiceEngineV2AppEffectiveSelfMuteForVoiceStatePayload(pushToTalkUnmutedIdle)).toBe(true);
		expect(selectVoiceEngineV2AppIntentSelfMuteForVoiceStatePayload(pushToTalkUnmutedIdle)).toBe(false);
		expect(selectVoiceEngineV2AppIntentSelfMuteForVoiceStatePayload(pushToTalkMutedIdle)).toBe(true);
		expect(
			selectVoiceEngineV2AppIntentSelfMuteFromAudioControls({
				mode: 'pushToTalk',
				locallyMuted: false,
				preferredLocallyMuted: false,
				locallyDeafened: false,
				mutedByPermission: true,
				hasUserSetMute: false,
				hasUserSetDeaf: false,
				shouldUnmuteOnUndeafen: false,
				pushToTalkActive: false,
				pushToMuteActive: false,
				inputVolume: 1,
				outputVolume: 1,
			}),
		).toBe(true);
	});

	it('selects app mute reasons from v2 audio controls and server permission state', () => {
		const audio = {
			mode: 'voiceActivity' as const,
			locallyMuted: false,
			preferredLocallyMuted: false,
			locallyDeafened: false,
			mutedByPermission: false,
			hasUserSetMute: false,
			hasUserSetDeaf: false,
			shouldUnmuteOnUndeafen: false,
			pushToTalkActive: false,
			pushToMuteActive: false,
			inputVolume: 1,
			outputVolume: 1,
		};

		expect(selectVoiceEngineV2AppMuteReason({voiceState: {mute: true}, permissionMuted: false, audio})).toBe('guild');
		expect(selectVoiceEngineV2AppMuteReason({voiceState: {mute: false}, permissionMuted: true, audio})).toBe(
			'permission',
		);
		expect(
			selectVoiceEngineV2AppMuteReason({
				voiceState: {mute: false},
				permissionMuted: false,
				audio: {...audio, locallyMuted: true},
			}),
		).toBe('self');
		expect(
			selectVoiceEngineV2AppMuteReason({
				voiceState: {mute: false},
				permissionMuted: false,
				audio: {...audio, mode: 'pushToTalk', pushToTalkActive: false},
			}),
		).toBe('voice_push_to_talk');
		expect(selectVoiceEngineV2AppMuteReason({voiceState: {mute: false}, permissionMuted: false, audio})).toBeNull();
	});

	it('asserts non-null source for projection selectors (negative space)', () => {
		expect(() => selectVoiceEngineV2AppConnection(null as never)).toThrow();
		expect(() => selectVoiceEngineV2AppView(undefined as never)).toThrow();
		expect(() => selectVoiceEngineV2AppParticipant(null as never, 'identity')).toThrow();
	});

	it('treats push-to-mute active as effective self mute via split predicate', () => {
		const audio = {
			mode: 'pushToMute' as const,
			locallyMuted: false,
			preferredLocallyMuted: false,
			locallyDeafened: false,
			mutedByPermission: false,
			hasUserSetMute: false,
			hasUserSetDeaf: false,
			shouldUnmuteOnUndeafen: false,
			pushToTalkActive: false,
			pushToMuteActive: true,
			inputVolume: 1,
			outputVolume: 1,
		};
		expect(selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls(audio)).toBe(true);
	});
});

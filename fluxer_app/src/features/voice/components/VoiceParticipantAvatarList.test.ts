// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {resolveVoiceParticipantAvatarEntryVoiceState} from '@app/features/voice/components/VoiceParticipantDisplayState';
import type {LivekitParticipantSnapshot} from '@app/features/voice/engine/VoiceParticipantStateMachine';
import {VoiceConnectionQuality} from '@app/features/voice/engine/VoiceTrackSource';
import {describe, expect, it} from 'vitest';

function createParticipantSnapshot(overrides: Partial<LivekitParticipantSnapshot> = {}): LivekitParticipantSnapshot {
	return {
		identity: 'user_100_connection-local',
		userId: '100',
		connectionId: 'connection-local',
		sid: 'PA_local',
		isLocal: true,
		isSpeaking: false,
		isAudioLevelSpeaking: true,
		connectionQuality: VoiceConnectionQuality.Excellent,
		attributes: {},
		audioTrackSids: [],
		videoTrackSids: [],
		isMicrophoneEnabled: true,
		isCameraEnabled: false,
		isScreenShareEnabled: false,
		isScreenShareAudioEnabled: false,
		joinedAt: null,
		lastSpokeAt: null,
		...overrides,
	};
}

function createVoiceState(overrides: Partial<VoiceState> = {}): VoiceState {
	return {
		guild_id: 'guild-1',
		channel_id: 'channel-1',
		user_id: '100',
		connection_id: 'connection-local',
		mute: false,
		deaf: false,
		self_mute: false,
		self_deaf: false,
		self_video: false,
		self_stream: false,
		...overrides,
	};
}

describe('resolveVoiceParticipantAvatarEntryVoiceState', () => {
	it('keeps local avatar stack speaking when stale voice state self mute is true but effective v2 mute is false', () => {
		const state = resolveVoiceParticipantAvatarEntryVoiceState({
			snapshot: createParticipantSnapshot(),
			voiceState: createVoiceState({self_mute: true}),
			permissionMuted: false,
			localEffectiveSelfMute: false,
			localSelfDeaf: false,
		});

		expect(state).toMatchObject({
			speaking: true,
			selfMute: false,
			selfDeaf: false,
		});
	});

	it('suppresses local avatar stack speaking when effective v2 mute is true', () => {
		const state = resolveVoiceParticipantAvatarEntryVoiceState({
			snapshot: createParticipantSnapshot(),
			voiceState: createVoiceState({self_mute: false}),
			permissionMuted: false,
			localEffectiveSelfMute: true,
			localSelfDeaf: false,
		});

		expect(state).toMatchObject({
			speaking: false,
			selfMute: true,
		});
	});

	it('keeps speaking when the native engine reports a stale disabled microphone flag', () => {
		const state = resolveVoiceParticipantAvatarEntryVoiceState({
			snapshot: createParticipantSnapshot({isMicrophoneEnabled: false}),
			voiceState: createVoiceState(),
			permissionMuted: false,
			localEffectiveSelfMute: false,
			localSelfDeaf: false,
		});

		expect(state).toMatchObject({
			speaking: true,
			selfMute: false,
		});
	});

	it('keeps remote speaking when the stale microphone flag is false but voice state self mute is false', () => {
		const state = resolveVoiceParticipantAvatarEntryVoiceState({
			snapshot: createParticipantSnapshot({
				identity: 'user_200_connection-remote',
				userId: '200',
				connectionId: 'connection-remote',
				sid: 'PA_remote',
				isLocal: false,
				isMicrophoneEnabled: false,
			}),
			voiceState: createVoiceState({
				user_id: '200',
				connection_id: 'connection-remote',
				self_mute: false,
			}),
			permissionMuted: false,
			localEffectiveSelfMute: false,
			localSelfDeaf: false,
		});

		expect(state).toMatchObject({
			speaking: true,
			selfMute: false,
		});
	});

	it('falls back to the microphone flag for remote mute when voice state is missing', () => {
		const state = resolveVoiceParticipantAvatarEntryVoiceState({
			snapshot: createParticipantSnapshot({
				identity: 'user_200_connection-remote',
				userId: '200',
				connectionId: 'connection-remote',
				sid: 'PA_remote',
				isLocal: false,
				isMicrophoneEnabled: false,
			}),
			voiceState: null,
			permissionMuted: false,
			localEffectiveSelfMute: false,
			localSelfDeaf: false,
		});

		expect(state).toMatchObject({
			speaking: false,
			selfMute: true,
		});
	});

	it('continues to suppress remote avatar stack speaking from voice state self mute', () => {
		const state = resolveVoiceParticipantAvatarEntryVoiceState({
			snapshot: createParticipantSnapshot({
				identity: 'user_200_connection-remote',
				userId: '200',
				connectionId: 'connection-remote',
				sid: 'PA_remote',
				isLocal: false,
			}),
			voiceState: createVoiceState({
				user_id: '200',
				connection_id: 'connection-remote',
				self_mute: true,
			}),
			permissionMuted: false,
			localEffectiveSelfMute: false,
			localSelfDeaf: false,
		});

		expect(state).toMatchObject({
			speaking: false,
			selfMute: true,
		});
	});
});

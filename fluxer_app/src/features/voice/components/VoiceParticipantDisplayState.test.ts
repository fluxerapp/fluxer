// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {
	resolveVoiceParticipantDisplayState,
	resolveVoiceParticipantSpeaking,
} from '@app/features/voice/components/VoiceParticipantDisplayState';
import {describe, expect, it} from 'vitest';

function voiceState(overrides: Partial<VoiceState>): VoiceState {
	return overrides as VoiceState;
}

function speakingParticipant(isMicrophoneEnabled = true) {
	return {isSpeaking: false, isAudioLevelSpeaking: true, isMicrophoneEnabled};
}

describe('resolveVoiceParticipantSpeaking', () => {
	it('returns false when the participant is not speaking', () => {
		expect(
			resolveVoiceParticipantSpeaking({
				participant: {isSpeaking: false, isAudioLevelSpeaking: false},
				voiceState: voiceState({}),
				isLocalConnection: false,
				localSelfMute: false,
				permissionMuted: false,
			}),
		).toBe(false);
	});

	it('suppresses remote participants when gateway self mute is set', () => {
		expect(
			resolveVoiceParticipantSpeaking({
				participant: speakingParticipant(),
				voiceState: voiceState({self_mute: true}),
				isLocalConnection: false,
				localSelfMute: false,
				permissionMuted: false,
			}),
		).toBe(false);
	});

	it('lets the local connection ignore a stale gateway self mute echo', () => {
		expect(
			resolveVoiceParticipantSpeaking({
				participant: speakingParticipant(),
				voiceState: voiceState({self_mute: true}),
				isLocalConnection: true,
				localSelfMute: false,
				permissionMuted: false,
			}),
		).toBe(true);
	});

	it('suppresses the local connection when the effective local mute is set', () => {
		expect(
			resolveVoiceParticipantSpeaking({
				participant: speakingParticipant(),
				voiceState: voiceState({self_mute: false}),
				isLocalConnection: true,
				localSelfMute: true,
				permissionMuted: false,
			}),
		).toBe(false);
	});

	it('ignores a stale disabled microphone flag when gateway self mute is present', () => {
		expect(
			resolveVoiceParticipantSpeaking({
				participant: speakingParticipant(false),
				voiceState: voiceState({self_mute: false}),
				isLocalConnection: false,
				localSelfMute: false,
				permissionMuted: false,
			}),
		).toBe(true);
	});

	it('falls back to the microphone flag when gateway self mute is missing', () => {
		expect(
			resolveVoiceParticipantSpeaking({
				participant: speakingParticipant(false),
				voiceState: voiceState({}),
				isLocalConnection: false,
				localSelfMute: false,
				permissionMuted: false,
			}),
		).toBe(false);
	});

	it('suppresses moderator mute regardless of connection locality', () => {
		expect(
			resolveVoiceParticipantSpeaking({
				participant: speakingParticipant(),
				voiceState: voiceState({self_mute: false, mute: true}),
				isLocalConnection: true,
				localSelfMute: false,
				permissionMuted: false,
			}),
		).toBe(false);
	});

	it('suppresses permission-muted participants', () => {
		expect(
			resolveVoiceParticipantSpeaking({
				participant: speakingParticipant(),
				voiceState: voiceState({self_mute: false}),
				isLocalConnection: false,
				localSelfMute: false,
				permissionMuted: true,
			}),
		).toBe(false);
	});
});

describe('resolveVoiceParticipantDisplayState', () => {
	it('derives remote display state from the gateway voice state first', () => {
		const state = resolveVoiceParticipantDisplayState({
			participant: {
				isSpeaking: false,
				isAudioLevelSpeaking: false,
				isMicrophoneEnabled: true,
				isCameraEnabled: true,
				isScreenShareEnabled: true,
			},
			voiceState: voiceState({
				self_mute: true,
				self_deaf: true,
				self_video: false,
				self_stream: false,
				mute: true,
				deaf: true,
			}),
			isLocalConnection: false,
			localSelfMute: false,
			permissionMuted: false,
		});

		expect(state).toEqual({
			speaking: false,
			selfMute: true,
			selfDeaf: true,
			guildMute: true,
			guildDeaf: true,
			cameraOn: false,
			streaming: false,
		});
	});

	it('falls back to participant track flags when gateway fields are missing', () => {
		const state = resolveVoiceParticipantDisplayState({
			participant: {
				isSpeaking: false,
				isAudioLevelSpeaking: false,
				isMicrophoneEnabled: true,
				isCameraEnabled: true,
				isScreenShareEnabled: true,
			},
			voiceState: voiceState({}),
			isLocalConnection: false,
			localSelfMute: false,
			permissionMuted: false,
		});

		expect(state).toMatchObject({
			selfMute: false,
			selfDeaf: false,
			cameraOn: true,
			streaming: true,
		});
	});

	it('uses the local overrides for the local connection', () => {
		const state = resolveVoiceParticipantDisplayState({
			participant: speakingParticipant(),
			voiceState: voiceState({self_mute: true, self_deaf: true, self_video: false, self_stream: false}),
			isLocalConnection: true,
			localSelfMute: false,
			localSelfDeaf: false,
			localSelfVideo: true,
			localSelfStream: true,
			permissionMuted: false,
		});

		expect(state).toMatchObject({
			speaking: true,
			selfMute: false,
			selfDeaf: false,
			cameraOn: true,
			streaming: true,
		});
	});

	it('includes permission mute in the self mute display flag', () => {
		const state = resolveVoiceParticipantDisplayState({
			participant: speakingParticipant(),
			voiceState: voiceState({self_mute: false}),
			isLocalConnection: false,
			localSelfMute: false,
			permissionMuted: true,
		});

		expect(state).toMatchObject({
			speaking: false,
			selfMute: true,
		});
	});

	it('handles a missing participant and missing voice state without throwing', () => {
		const state = resolveVoiceParticipantDisplayState({
			participant: null,
			voiceState: null,
			isLocalConnection: false,
			localSelfMute: false,
			permissionMuted: false,
		});

		expect(state).toEqual({
			speaking: false,
			selfMute: false,
			selfDeaf: false,
			guildMute: false,
			guildDeaf: false,
			cameraOn: false,
			streaming: false,
		});
	});
});

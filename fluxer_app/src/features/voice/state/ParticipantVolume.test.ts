// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {getStreamKey} from '@app/features/voice/components/StreamKeys';
import {VoiceTrackKind, VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {
	clearRemoteVoicePlaybackBoost,
	setRemoteVoicePlaybackBoost,
} from '@app/features/voice/state/RemoteVoicePlaybackBoost';
import StreamAudioPrefs from '@app/features/voice/state/StreamAudioPrefs';
import type {RemoteParticipant} from 'livekit-client';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const outputVolumePercent = vi.fn<() => number>();
const effectiveDeaf = vi.fn<() => boolean>();

vi.mock('@app/features/platform/utils/MobXPersistence', () => ({
	makePersistent: async () => {},
}));

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {getOutputVolume: () => outputVolumePercent()},
}));

vi.mock('@app/features/voice/engine/VoiceEffectiveAudioState', () => ({
	getEffectiveAudioState: () => ({effectiveDeaf: effectiveDeaf()}),
}));

const GUILD_ID = 'guild-1';
const CHANNEL_ID = 'channel-1';
const LOCAL_CONNECTION_ID = 'local-connection';
const REMOTE_CONNECTION_ID = 'remote-connection';
const USER_ID = '42';
const IDENTITY = `user_${USER_ID}_${REMOTE_CONNECTION_ID}`;
const STREAM_KEY = getStreamKey(GUILD_ID, CHANNEL_ID, REMOTE_CONNECTION_ID);

interface FakePublication {
	source: string;
	trackName: string;
	trackSid: string;
	isDesired: boolean;
	track: {kind: string; setVolume: (volume: number) => void};
	setEnabled: (enabled: boolean) => void;
	volumes: Array<number>;
	enabled: Array<boolean>;
}

(
	window as typeof window & {
		_mediaEngineFacade?: {guildId: string; channelId: string; connectionId: string};
	}
)._mediaEngineFacade = {guildId: GUILD_ID, channelId: CHANNEL_ID, connectionId: LOCAL_CONNECTION_ID};

const {default: ParticipantVolume} = await import('@app/features/voice/state/ParticipantVolume');

function createPublication(source: string, trackSid: string, throwOnSetVolume: boolean): FakePublication {
	const volumes: Array<number> = [];
	const enabled: Array<boolean> = [];
	return {
		source,
		trackName: source,
		trackSid,
		isDesired: true,
		track: {
			kind: VoiceTrackKind.Audio,
			setVolume: (volume: number) => {
				volumes.push(volume);
				if (throwOnSetVolume) {
					throw new Error('setVolume failed');
				}
			},
		},
		setEnabled: (value: boolean) => {
			enabled.push(value);
		},
		volumes,
		enabled,
	};
}

function applyToParticipant(options?: {throwOnMicrophoneVolume?: boolean}): {
	microphone: FakePublication;
	screenShareAudio: FakePublication;
} {
	const microphone = createPublication(
		VoiceTrackSource.Microphone,
		'microphone-sid',
		options?.throwOnMicrophoneVolume === true,
	);
	const screenShareAudio = createPublication(VoiceTrackSource.ScreenShareAudio, 'screen-share-audio-sid', false);
	const audioTrackPublications = new Map<string, FakePublication>([
		[microphone.trackSid, microphone],
		[screenShareAudio.trackSid, screenShareAudio],
	]);
	ParticipantVolume.applySettingsToParticipant({
		identity: IDENTITY,
		audioTrackPublications,
	} as unknown as RemoteParticipant);
	return {microphone, screenShareAudio};
}

describe('ParticipantVolume.applySettingsToParticipant', () => {
	beforeEach(() => {
		outputVolumePercent.mockReturnValue(100);
		effectiveDeaf.mockReturnValue(false);
		clearRemoteVoicePlaybackBoost(IDENTITY);
		ParticipantVolume.resetUserSettings(USER_ID);
		StreamAudioPrefs.entries = {};
	});

	it('gives a saturated auto-leveller participant strictly more gain at output 200% than at 100%', () => {
		setRemoteVoicePlaybackBoost(IDENTITY, 3);
		const atUnity = applyToParticipant().microphone.volumes;
		outputVolumePercent.mockReturnValue(200);
		const atCeiling = applyToParticipant().microphone.volumes;
		expect(atUnity).toEqual([3]);
		expect(atCeiling).toEqual([12]);
		expect(atCeiling[0]).toBeGreaterThan(atUnity[0]);
	});

	it('clamps the total, not each control', () => {
		ParticipantVolume.setVolume(USER_ID, 200);
		outputVolumePercent.mockReturnValue(200);
		setRemoteVoicePlaybackBoost(IDENTITY, 3);
		expect(applyToParticipant().microphone.volumes).toEqual([12]);
	});

	it('the per-user slider still moves with the leveller idle', () => {
		ParticipantVolume.setVolume(USER_ID, 100);
		expect(applyToParticipant().microphone.volumes).toEqual([1]);
		ParticipantVolume.setVolume(USER_ID, 200);
		expect(applyToParticipant().microphone.volumes).toEqual([4]);
	});

	it('screen-share audio composes stream and output volume', () => {
		setRemoteVoicePlaybackBoost(IDENTITY, 3);
		StreamAudioPrefs.setVolume(STREAM_KEY, 100);
		expect(applyToParticipant().screenShareAudio.volumes).toEqual([1]);
		StreamAudioPrefs.setVolume(STREAM_KEY, 200);
		outputVolumePercent.mockReturnValue(200);
		expect(applyToParticipant().screenShareAudio.volumes).toEqual([12]);
	});

	it('leaves the microphone gain untouched when the stream volume changes', () => {
		StreamAudioPrefs.setVolume(STREAM_KEY, 50);
		const atQuietStream = applyToParticipant().microphone.volumes;
		StreamAudioPrefs.setVolume(STREAM_KEY, 200);
		const atLoudStream = applyToParticipant().microphone.volumes;
		expect(atQuietStream).toEqual([1]);
		expect(atLoudStream).toEqual(atQuietStream);
	});

	it('leaves the screen share gain untouched when the per-user volume changes', () => {
		StreamAudioPrefs.setVolume(STREAM_KEY, 100);
		ParticipantVolume.setVolume(USER_ID, 50);
		const atQuietUser = applyToParticipant().screenShareAudio.volumes;
		ParticipantVolume.setVolume(USER_ID, 200);
		const atLoudUser = applyToParticipant().screenShareAudio.volumes;
		expect(atQuietUser).toEqual([1]);
		expect(atLoudUser).toEqual(atQuietUser);
	});

	it('leaves the microphone publication enabled when the stream is muted', () => {
		StreamAudioPrefs.setMuted(STREAM_KEY, true);
		expect(applyToParticipant().microphone.enabled).toEqual([true]);
	});

	it('leaves screen share audio enabled when the participant is locally muted', () => {
		ParticipantVolume.setLocalMute(USER_ID, true);
		expect(applyToParticipant().screenShareAudio.enabled).toEqual([true]);
	});

	it('disables screen share audio while deafened', () => {
		effectiveDeaf.mockReturnValue(true);
		expect(applyToParticipant().screenShareAudio.enabled).toEqual([false]);
	});

	it('still applies the enabled state when a track volume write throws', () => {
		ParticipantVolume.setLocalMute(USER_ID, true);
		const {microphone} = applyToParticipant({throwOnMicrophoneVolume: true});
		expect(microphone.volumes).toEqual([1]);
		expect(microphone.enabled).toEqual([false]);
	});
});

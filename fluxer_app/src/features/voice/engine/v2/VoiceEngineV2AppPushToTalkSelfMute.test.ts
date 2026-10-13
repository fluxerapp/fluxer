// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Room} from 'livekit-client';
import {Track} from 'livekit-client';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const syncLocalVoiceStateWithServer = vi.fn();
const setVoiceEngineV2ParticipantAudioLevelSpeaking = vi.fn(() => true);

const keybind = {
	pushToTalkEnabled: true,
	pushToTalkHeld: false,
	pushToMuteHeld: false,
	isPushToTalkEffective: () => keybind.pushToTalkEnabled,
	isPushToMuteEffective: () => false,
	setPushToTalkHeld: (held: boolean) => {
		keybind.pushToTalkHeld = held;
	},
	setPushToMuteHeld: (held: boolean) => {
		keybind.pushToMuteHeld = held;
	},
	resetPushToTalkState: () => {
		keybind.pushToTalkHeld = false;
	},
	resetPushToMuteState: () => {
		keybind.pushToMuteHeld = false;
	},
};

const localVoiceState = {
	selfMute: false,
	selfDeaf: false,
	shouldUnmuteOnUndeafen: false,
	getSelfMute: () => localVoiceState.selfMute,
	getSelfDeaf: () => localVoiceState.selfDeaf,
	getMutedByPermission: () => false,
	getHasUserSetMute: () => true,
	getHasUserSetDeaf: () => false,
	updateSelfMute: (muted: boolean) => {
		localVoiceState.selfMute = muted;
	},
	updateSelfDeaf: (deafened: boolean) => {
		localVoiceState.selfDeaf = deafened;
	},
};

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? ''}}),
}));

vi.mock('@app/app/I18n', () => ({default: {_: (descriptor: {message?: string}) => descriptor.message ?? ''}}));

vi.mock('@app/features/input/state/InputKeybind', () => ({default: keybind}));

vi.mock('@app/features/voice/state/LocalVoiceState', () => ({default: localVoiceState}));

vi.mock('@app/features/voice/engine/MediaEngineFacade', () => ({default: {}, useMediaEngineVersion: () => 0}));

vi.mock('@app/features/voice/engine/VoiceMediaEngineBridge', () => ({
	getRoomFromMediaEngine: () => null,
	getVoiceConnectionContextFromMediaEngine: () => null,
	getVoiceEngineV2SnapshotFromMediaEngine: () => null,
	getVoiceStateByConnectionIdFromMediaEngine: () => null,
	setVoiceEngineV2ParticipantAudioLevelSpeaking: (...args: Array<unknown>) =>
		setVoiceEngineV2ParticipantAudioLevelSpeaking(...(args as [])),
	syncLocalVoiceStateWithServer: (...args: Array<unknown>) => syncLocalVoiceStateWithServer(...args),
	updateLocalParticipantFromRoom: () => undefined,
}));

const {installRuntimeBootstrap} = await import('@app/features/platform/state/__fixtures__/RuntimeBootstrapFixture');
installRuntimeBootstrap('https://primary.test/api');

const {default: adapter} = await import('@app/features/voice/engine/v2/VoiceEngineV2AppMediaExecutionAdapter');

interface FakeMicrophonePublication {
	source: Track.Source;
	isMuted: boolean;
	track: {mediaStreamTrack: {readyState: string}};
	mute: ReturnType<typeof vi.fn>;
	unmute: ReturnType<typeof vi.fn>;
}

function createMicrophonePublication(): FakeMicrophonePublication {
	const publication: FakeMicrophonePublication = {
		source: Track.Source.Microphone,
		isMuted: true,
		track: {mediaStreamTrack: {readyState: 'live'}},
		mute: vi.fn(async () => {
			publication.isMuted = true;
		}),
		unmute: vi.fn(async () => {
			publication.isMuted = false;
		}),
	};
	return publication;
}

function createRoom(): {room: Room; publications: Map<string, FakeMicrophonePublication>} {
	const publications = new Map<string, FakeMicrophonePublication>();
	const room = {
		localParticipant: {
			identity: 'user_1_connection',
			audioTrackPublications: publications,
			trackPublications: publications,
		},
		remoteParticipants: new Map(),
	} as unknown as Room;
	return {room, publications};
}

function lastSyncedSelfMute(): unknown {
	const call = syncLocalVoiceStateWithServer.mock.calls.at(-1);
	return (call?.[0] as {self_mute?: boolean} | undefined)?.self_mute;
}

function lastSpeakingOverride(): unknown {
	return setVoiceEngineV2ParticipantAudioLevelSpeaking.mock.calls.at(-1)?.[1 as never];
}

const noServerVoiceState = () => null;

describe('push to talk with self-mute', () => {
	beforeEach(() => {
		keybind.pushToTalkEnabled = true;
		keybind.pushToTalkHeld = false;
		localVoiceState.selfMute = false;
		localVoiceState.selfDeaf = false;
		syncLocalVoiceStateWithServer.mockClear();
		setVoiceEngineV2ParticipantAudioLevelSpeaking.mockClear();
	});

	it('keeps the microphone muted and reports self_mute while the key is held and the user is self-muted', () => {
		const {room, publications} = createRoom();
		const publication = createMicrophonePublication();
		publications.set('mic', publication);
		localVoiceState.selfMute = true;
		adapter.applyPushToTalkHold(true, room, noServerVoiceState);
		expect(keybind.pushToTalkHeld).toBe(true);
		expect(publication.unmute).not.toHaveBeenCalled();
		expect(publication.mute).toHaveBeenCalled();
		expect(publication.isMuted).toBe(true);
		expect(lastSyncedSelfMute()).toBe(true);
		expect(lastSpeakingOverride()).toBe(false);
		adapter.applyPushToTalkHold(false, room, noServerVoiceState);
		expect(publication.unmute).not.toHaveBeenCalled();
		expect(lastSyncedSelfMute()).toBe(true);
	});

	it('gates the microphone on the key again once the user unmutes', () => {
		const {room, publications} = createRoom();
		const publication = createMicrophonePublication();
		publications.set('mic', publication);
		adapter.applyPushToTalkHold(true, room, noServerVoiceState);
		expect(publication.unmute).toHaveBeenCalledTimes(1);
		expect(lastSyncedSelfMute()).toBe(false);
		expect(lastSpeakingOverride()).toBe(true);
		adapter.applyPushToTalkHold(false, room, noServerVoiceState);
		expect(publication.isMuted).toBe(true);
		expect(lastSyncedSelfMute()).toBe(true);
		expect(lastSpeakingOverride()).toBe(false);
	});

	it('transmits when the user unmutes while the key is still held', () => {
		const {room, publications} = createRoom();
		const publication = createMicrophonePublication();
		publications.set('mic', publication);
		localVoiceState.selfMute = true;
		adapter.applyPushToTalkHold(true, room, noServerVoiceState);
		expect(publication.isMuted).toBe(true);
		localVoiceState.selfMute = false;
		adapter.handlePushToTalkModeChange(room, noServerVoiceState);
		expect(keybind.pushToTalkHeld).toBe(false);
		adapter.applyPushToTalkHold(true, room, noServerVoiceState);
		expect(publication.isMuted).toBe(false);
		expect(lastSyncedSelfMute()).toBe(false);
	});

	it('keeps self-mute when switching into and out of push to talk', () => {
		const {room, publications} = createRoom();
		const publication = createMicrophonePublication();
		publications.set('mic', publication);
		localVoiceState.selfMute = true;
		keybind.pushToTalkEnabled = false;
		adapter.handlePushToTalkModeChange(room, noServerVoiceState);
		expect(lastSyncedSelfMute()).toBe(true);
		keybind.pushToTalkEnabled = true;
		adapter.handlePushToTalkModeChange(room, noServerVoiceState);
		expect(localVoiceState.selfMute).toBe(true);
		expect(publication.unmute).not.toHaveBeenCalled();
		expect(lastSyncedSelfMute()).toBe(true);
		adapter.applyPushToTalkHold(true, room, noServerVoiceState);
		expect(publication.unmute).not.toHaveBeenCalled();
		expect(lastSyncedSelfMute()).toBe(true);
		keybind.pushToTalkEnabled = false;
		adapter.handlePushToTalkModeChange(room, noServerVoiceState);
		expect(localVoiceState.selfMute).toBe(true);
		expect(publication.unmute).not.toHaveBeenCalled();
		expect(lastSyncedSelfMute()).toBe(true);
	});

	it('publishes a muted microphone when a self-muted push to talk user unmutes without holding the key', async () => {
		const {room, publications} = createRoom();
		const enableMicrophone = vi.spyOn(adapter, 'enableMicrophone').mockImplementation(async () => {
			publications.set('mic', createMicrophonePublication());
		});
		localVoiceState.selfMute = true;
		await adapter.reconcileEffectiveAudioState(room, {channelId: '1', serverMute: false, serverDeaf: false});
		expect(enableMicrophone).not.toHaveBeenCalled();
		expect(publications.size).toBe(0);
		localVoiceState.selfMute = false;
		await adapter.reconcileEffectiveAudioState(room, {channelId: '1', serverMute: false, serverDeaf: false});
		expect(enableMicrophone).toHaveBeenCalledTimes(1);
		const publication = publications.get('mic');
		expect(publication).toBeDefined();
		expect(publication?.isMuted).toBe(true);
		expect(publication?.unmute).not.toHaveBeenCalled();
		adapter.applyPushToTalkHold(true, room, noServerVoiceState);
		expect(publication?.unmute).toHaveBeenCalledTimes(1);
		expect(publication?.isMuted).toBe(false);
		expect(lastSyncedSelfMute()).toBe(false);
		enableMicrophone.mockRestore();
	});
});

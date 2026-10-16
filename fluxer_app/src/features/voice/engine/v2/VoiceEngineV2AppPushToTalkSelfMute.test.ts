// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Room} from 'livekit-client';
import {Track} from 'livekit-client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const syncLocalVoiceStateWithServer = vi.fn();
const setVoiceEngineV2ParticipantAudioLevelSpeaking = vi.fn(() => true);
const outgoingAudio = vi.hoisted(() => ({rms: 0}));

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

vi.mock('@app/features/voice/engine/VoiceSharedAudioContext', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/features/voice/engine/VoiceSharedAudioContext')>()),
	getSharedVoiceAudioContext: () => ({
		state: 'running',
		createMediaStreamSource: () => ({connect: () => undefined, disconnect: () => undefined}),
		createAnalyser: () => ({
			disconnect: () => undefined,
			getFloatTimeDomainData: (target: Float32Array) => target.fill(outgoingAudio.rms),
		}),
	}),
}));

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
	track: {
		mediaStreamTrack: {readyState: string; getSettings(): MediaTrackSettings; getConstraints(): MediaTrackConstraints};
		getProcessor(): undefined;
		on(): void;
		off(): void;
	};
	mute: ReturnType<typeof vi.fn>;
	unmute: ReturnType<typeof vi.fn>;
}

function createMicrophonePublication(): FakeMicrophonePublication {
	const publication: FakeMicrophonePublication = {
		source: Track.Source.Microphone,
		isMuted: true,
		track: {
			mediaStreamTrack: {readyState: 'live', getSettings: () => ({}), getConstraints: () => ({})},
			getProcessor: () => undefined,
			on: () => undefined,
			off: () => undefined,
		},
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

function lastSpeaking(): unknown {
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
		expect(lastSpeaking()).toBe(false);
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
		expect(lastSpeaking()).toBe(false);
		adapter.applyPushToTalkHold(false, room, noServerVoiceState);
		expect(publication.isMuted).toBe(true);
		expect(lastSyncedSelfMute()).toBe(true);
		expect(lastSpeaking()).toBe(false);
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

interface LocalSpeakingMeterProbe {
	startLocalSpeakingDetector(room: Room): void;
	stopLocalSpeakingDetector(room?: Room | null): void;
	localSpeakingMeter: {tick(nowMs: number): void} | null;
}

describe('local speaking indicator in push to talk', () => {
	const probe = adapter as unknown as LocalSpeakingMeterProbe;
	let nowMs = 0;

	function tick(rms: number, ticks = 1): void {
		outgoingAudio.rms = rms;
		for (let index = 0; index < ticks; index++) {
			nowMs += 50;
			probe.localSpeakingMeter?.tick(nowMs);
		}
	}

	beforeEach(() => {
		vi.stubGlobal('MediaStream', class {});
		keybind.pushToTalkEnabled = true;
		keybind.pushToTalkHeld = false;
		localVoiceState.selfMute = false;
		localVoiceState.selfDeaf = false;
		outgoingAudio.rms = 0;
		setVoiceEngineV2ParticipantAudioLevelSpeaking.mockClear();
	});

	afterEach(() => {
		probe.stopLocalSpeakingDetector();
		vi.unstubAllGlobals();
	});

	function startWithMicrophone(): {room: Room; publication: FakeMicrophonePublication} {
		const {room, publications} = createRoom();
		const publication = createMicrophonePublication();
		publications.set('mic', publication);
		probe.startLocalSpeakingDetector(room);
		return {room, publication};
	}

	it('stays dark while the key is held and nothing audible is sent', () => {
		const {room} = startWithMicrophone();
		adapter.applyPushToTalkHold(true, room, noServerVoiceState);
		tick(0.001, 10);
		expect(setVoiceEngineV2ParticipantAudioLevelSpeaking).not.toHaveBeenCalledWith(expect.anything(), true);
	});

	it('lights for speech sent while the key is held and goes dark the moment it is released', () => {
		const {room} = startWithMicrophone();
		adapter.applyPushToTalkHold(true, room, noServerVoiceState);
		tick(0.05, 2);
		expect(lastSpeaking()).toBe(true);
		adapter.applyPushToTalkHold(false, room, noServerVoiceState);
		expect(lastSpeaking()).toBe(false);
		tick(0.05, 4);
		expect(setVoiceEngineV2ParticipantAudioLevelSpeaking).toHaveBeenLastCalledWith(expect.anything(), false);
	});

	it('never lights for a self-muted user, however loud the microphone is', () => {
		const {room} = startWithMicrophone();
		localVoiceState.selfMute = true;
		adapter.applyPushToTalkHold(true, room, noServerVoiceState);
		tick(0.2, 10);
		expect(setVoiceEngineV2ParticipantAudioLevelSpeaking).not.toHaveBeenCalledWith(expect.anything(), true);
	});
});

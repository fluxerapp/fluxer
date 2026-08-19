// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Room} from 'livekit-client';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const updateSettings = vi.fn();
let outputDevices: Array<{deviceId: string}> = [];

vi.mock('@app/features/voice/state/VoiceSettings', () => ({
	default: {
		getOutputDeviceId: () => 'default',
		updateSettings: (update: unknown) => updateSettings(update),
		subscribe: () => () => undefined,
	},
}));

vi.mock('@app/features/voice/utils/VoiceDeviceManager', () => ({
	voiceDeviceManager: {
		ensureDevices: async () => ({
			inputDevices: [],
			outputDevices,
			videoDevices: [],
			permissionStatus: 'granted',
		}),
	},
}));

const {applyOutputDeviceToRoom} = await import('./VoiceOutputDeviceSync');

class FakeMediaElement {
	setSinkId = vi.fn(async () => undefined);
}

Object.defineProperty(FakeMediaElement.prototype, 'setSinkId', {
	configurable: true,
	writable: true,
	value: async (): Promise<void> => undefined,
});

vi.stubGlobal('HTMLMediaElement', FakeMediaElement);

interface FakeRoomOptions {
	webAudioMix?: boolean;
	audioContextSupportsSetSinkId?: boolean;
	switchActiveDevice?: () => Promise<boolean>;
}

function createRoom(options: FakeRoomOptions = {}) {
	const {webAudioMix = true, audioContextSupportsSetSinkId = true} = options;
	const switchActiveDevice = vi.fn(options.switchActiveDevice ?? (async () => true));
	const mixerSetSinkId = vi.fn(async () => undefined);
	const trackSetSinkId = vi.fn(async () => undefined);
	const element = new FakeMediaElement();
	const audioContext = audioContextSupportsSetSinkId ? {setSinkId: mixerSetSinkId} : {};
	const room = {
		options: {webAudioMix},
		audioContext,
		switchActiveDevice,
		remoteParticipants: new Map([
			[
				'participant-with-track-sink',
				{
					audioTrackPublications: new Map([
						['pub-track-sink', {kind: 'audio', track: {kind: 'audio', setSinkId: trackSetSinkId}}],
					]),
				},
			],
			[
				'participant-with-elements',
				{
					audioTrackPublications: new Map([
						['pub-elements', {kind: 'audio', track: {kind: 'audio', attachedElements: [element]}}],
					]),
				},
			],
		]),
	};
	return {room: room as unknown as Room, switchActiveDevice, mixerSetSinkId, trackSetSinkId, element};
}

describe('applyOutputDeviceToRoom', () => {
	beforeEach(() => {
		updateSettings.mockClear();
		outputDevices = [{deviceId: 'speaker-1'}];
	});

	it('hands the Web Audio mixer room the literal default id so LiveKit resolves it to a real device', async () => {
		const {room, switchActiveDevice} = createRoom();

		await applyOutputDeviceToRoom(room, 'default');

		expect(switchActiveDevice).toHaveBeenCalledWith('audiooutput', 'default');
		expect(switchActiveDevice).not.toHaveBeenCalledWith('audiooutput', '');
	});

	it('passes a concrete device id through verbatim', async () => {
		const {room, switchActiveDevice} = createRoom();

		await applyOutputDeviceToRoom(room, 'speaker-1');

		expect(switchActiveDevice).toHaveBeenCalledWith('audiooutput', 'speaker-1');
	});

	it('applies the normalized sink id to the Web Audio mixer and the attached elements', async () => {
		const {room, mixerSetSinkId, trackSetSinkId, element} = createRoom();

		await applyOutputDeviceToRoom(room, 'default');

		expect(mixerSetSinkId).toHaveBeenCalledWith('');
		expect(trackSetSinkId).toHaveBeenCalledWith('');
		expect(element.setSinkId).toHaveBeenCalledWith('');
	});

	it('applies a concrete sink id to the Web Audio mixer and the attached elements', async () => {
		const {room, mixerSetSinkId, trackSetSinkId, element} = createRoom();

		await applyOutputDeviceToRoom(room, 'speaker-1');

		expect(mixerSetSinkId).toHaveBeenCalledWith('speaker-1');
		expect(trackSetSinkId).toHaveBeenCalledWith('speaker-1');
		expect(element.setSinkId).toHaveBeenCalledWith('speaker-1');
	});

	it('still runs the mixer and element paths when LiveKit rejects with NotFoundError', async () => {
		const notFound = new Error('Requested device not found');
		notFound.name = 'NotFoundError';
		const {room, switchActiveDevice, mixerSetSinkId, trackSetSinkId, element} = createRoom({
			switchActiveDevice: async () => {
				throw notFound;
			},
		});

		await expect(applyOutputDeviceToRoom(room, 'default')).resolves.toBeUndefined();

		expect(switchActiveDevice).toHaveBeenCalledWith('audiooutput', 'default');
		expect(mixerSetSinkId).toHaveBeenCalledWith('');
		expect(trackSetSinkId).toHaveBeenCalledWith('');
		expect(element.setSinkId).toHaveBeenCalledWith('');
	});

	it('skips the LiveKit switch when the Web Audio mixer context cannot switch sinks', async () => {
		const {room, switchActiveDevice, trackSetSinkId, element} = createRoom({
			audioContextSupportsSetSinkId: false,
		});

		await applyOutputDeviceToRoom(room, 'default');

		expect(switchActiveDevice).not.toHaveBeenCalled();
		expect(trackSetSinkId).toHaveBeenCalledWith('');
		expect(element.setSinkId).toHaveBeenCalledWith('');
	});

	it('normalizes the sink id only when LiveKit forwards it straight to the browser', async () => {
		const {room, switchActiveDevice} = createRoom({
			webAudioMix: false,
			audioContextSupportsSetSinkId: false,
		});

		await applyOutputDeviceToRoom(room, 'default');

		expect(switchActiveDevice).toHaveBeenCalledWith('audiooutput', '');
	});

	it('falls back to the default device when the stored output device disappeared', async () => {
		outputDevices = [{deviceId: 'speaker-2'}];
		const {room, switchActiveDevice} = createRoom();

		await applyOutputDeviceToRoom(room, 'speaker-1');

		expect(updateSettings).toHaveBeenCalledWith({outputDeviceId: 'default'});
		expect(switchActiveDevice).toHaveBeenCalledWith('audiooutput', 'default');
	});
});

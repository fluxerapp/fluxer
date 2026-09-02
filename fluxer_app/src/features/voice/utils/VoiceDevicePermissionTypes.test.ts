// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

interface FakeMediaDevices {
	enumerateDevices: () => Promise<Array<MediaDeviceInfo>>;
	getUserMedia: (constraints: MediaStreamConstraints) => Promise<MediaStream>;
	addEventListener: () => void;
	removeEventListener: () => void;
}

const makeDevice = (kind: MediaDeviceKind, deviceId: string, label: string): MediaDeviceInfo =>
	({
		deviceId,
		groupId: `${deviceId}-group`,
		kind,
		label,
		toJSON: () => ({deviceId, kind, label}),
	}) as MediaDeviceInfo;

const makeStream = (kinds: Array<'audio' | 'video'>): MediaStream => {
	const tracks = kinds.map((kind) => ({kind, label: `${kind}-track`, stop: vi.fn()}));
	return {getTracks: () => tracks} as unknown as MediaStream;
};

const getUserMediaCalls: Array<MediaStreamConstraints> = [];
let labelledDevices = false;

const fakeMediaDevices: FakeMediaDevices = {
	enumerateDevices: async () =>
		labelledDevices
			? [makeDevice('audioinput', 'mic-1', 'Studio Mic'), makeDevice('videoinput', 'cam-1', 'Studio Camera')]
			: [makeDevice('audioinput', 'mic-1', ''), makeDevice('videoinput', 'cam-1', '')],
	getUserMedia: async (constraints) => {
		getUserMediaCalls.push(constraints);
		labelledDevices = true;
		const kinds: Array<'audio' | 'video'> = [];
		if (constraints.audio) kinds.push('audio');
		if (constraints.video) kinds.push('video');
		return makeStream(kinds);
	},
	addEventListener: () => {},
	removeEventListener: () => {},
};

let originalNavigator: PropertyDescriptor | undefined;
let originalWindow: PropertyDescriptor | undefined;

beforeEach(() => {
	getUserMediaCalls.length = 0;
	labelledDevices = false;
	originalNavigator = Object.getOwnPropertyDescriptor(globalThis, 'navigator');
	originalWindow = Object.getOwnPropertyDescriptor(globalThis, 'window');
	Object.defineProperty(globalThis, 'navigator', {
		configurable: true,
		value: {userAgent: 'node', mediaDevices: fakeMediaDevices},
	});
	Object.defineProperty(globalThis, 'window', {
		configurable: true,
		value: {matchMedia: () => ({matches: false})},
	});
	vi.resetModules();
});

afterEach(() => {
	if (originalNavigator) {
		Object.defineProperty(globalThis, 'navigator', originalNavigator);
	} else {
		Reflect.deleteProperty(globalThis, 'navigator');
	}
	if (originalWindow) {
		Object.defineProperty(globalThis, 'window', originalWindow);
	} else {
		Reflect.deleteProperty(globalThis, 'window');
	}
});

const loadManager = async () => {
	const module = await import('@app/features/voice/utils/VoiceDeviceManager');
	return module.voiceDeviceManager;
};

describe('voiceDeviceManager permission types', () => {
	test('an audio-only request never asks the browser for video', async () => {
		const manager = await loadManager();
		const state = await manager.ensureDevices({requestPermissionTypes: ['audio'], forceRefresh: true});
		expect(getUserMediaCalls).toEqual([{audio: true, video: false}]);
		expect(state.permissionStatus.audio).toBe('granted');
		expect(state.permissionStatus.video).toBe('idle');
	});

	test('a video-only request never asks the browser for audio', async () => {
		const manager = await loadManager();
		const state = await manager.ensureDevices({requestPermissionTypes: ['video'], forceRefresh: true});
		expect(getUserMediaCalls).toEqual([{audio: false, video: true}]);
		expect(state.permissionStatus.video).toBe('granted');
		expect(state.permissionStatus.audio).toBe('idle');
	});

	test('the legacy requestPermissions boolean still asks for both types', async () => {
		const manager = await loadManager();
		const state = await manager.ensureDevices({requestPermissions: true, forceRefresh: true});
		expect(getUserMediaCalls).toEqual([{audio: true, video: true}]);
		expect(state.permissionStatus).toEqual({audio: 'granted', video: 'granted'});
	});

	test('a passive enumeration never prompts', async () => {
		const manager = await loadManager();
		const state = await manager.ensureDevices({requestPermissions: false, forceRefresh: true});
		expect(getUserMediaCalls).toEqual([]);
		expect(state.permissionStatus).toEqual({audio: 'idle', video: 'idle'});
		expect(state.inputDevices.length).toBeGreaterThan(0);
	});
});

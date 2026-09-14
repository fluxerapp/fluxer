// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const mediaPermission = vi.hoisted(() => ({
	isInitialized: () => true,
	isMicrophoneGranted: () => true,
	isCameraGranted: () => true,
	getMicrophonePermissionState: (): PermissionState => 'granted',
	getCameraPermissionState: (): PermissionState => 'granted',
	addChangeListener: (listener: () => void): (() => void) => {
		listener();
		return () => {};
	},
}));

vi.mock('@app/features/permissions/system/state/MediaPermission', () => ({default: mediaPermission}));

const makeDevice = (kind: MediaDeviceKind, deviceId: string, label: string): MediaDeviceInfo =>
	({
		deviceId,
		groupId: `${deviceId}-group`,
		kind,
		label,
		toJSON: () => ({deviceId, kind, label}),
	}) as MediaDeviceInfo;

const getUserMediaCalls: Array<MediaStreamConstraints> = [];
let browserExposesLabels = false;

const fakeMediaDevices = {
	enumerateDevices: async (): Promise<Array<MediaDeviceInfo>> => [
		makeDevice('audioinput', 'mic-1', browserExposesLabels ? 'Studio Mic' : ''),
		makeDevice('videoinput', 'cam-1', browserExposesLabels ? 'Studio Camera' : ''),
	],
	getUserMedia: async (constraints: MediaStreamConstraints): Promise<MediaStream> => {
		getUserMediaCalls.push(constraints);
		return {getTracks: () => []} as unknown as MediaStream;
	},
	addEventListener: () => {},
	removeEventListener: () => {},
};

let originalNavigator: PropertyDescriptor | undefined;
let originalWindow: PropertyDescriptor | undefined;

beforeEach(() => {
	getUserMediaCalls.length = 0;
	browserExposesLabels = false;
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

const runStartupPreload = async () => {
	const {startMediaDeviceStartupPreload} = await import('@app/features/voice/utils/MediaDeviceStartupPreload');
	const {default: VoiceDevicePermissionState} = await import('@app/features/voice/engine/VoiceDevicePermissionState');
	const stopPreload = startMediaDeviceStartupPreload();
	const state = await VoiceDevicePermissionState.ensureDevices();
	stopPreload();
	return state;
};

describe('startMediaDeviceStartupPreload', () => {
	test('never opens capture for a granted microphone and camera when the browser hides device labels', async () => {
		const state = await runStartupPreload();
		expect(getUserMediaCalls).toEqual([]);
		expect(state.permissionStatus).toEqual({audio: 'idle', video: 'idle'});
	});

	test('confirms a granted microphone and camera from exposed device labels without opening capture', async () => {
		browserExposesLabels = true;
		const state = await runStartupPreload();
		expect(getUserMediaCalls).toEqual([]);
		expect(state.permissionStatus).toEqual({audio: 'granted', video: 'granted'});
	});
});

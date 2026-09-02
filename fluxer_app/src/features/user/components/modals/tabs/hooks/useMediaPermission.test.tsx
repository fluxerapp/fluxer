// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	EnsureVoiceDevicesOptions,
	VoiceDeviceState,
	VoiceMediaPermissionType,
} from '@app/features/voice/utils/VoiceDeviceManager';
import {act, createElement, useEffect} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const deviceManagerMock = vi.hoisted(() => {
	const idleState = (): VoiceDeviceState => ({
		inputDevices: [],
		outputDevices: [],
		videoDevices: [],
		permissionStatus: {audio: 'idle', video: 'idle'},
	});
	const listeners = new Set<(state: VoiceDeviceState) => void>();
	const mock = {
		state: idleState(),
		permissionRequests: [] as Array<ReadonlyArray<VoiceMediaPermissionType>>,
		pending: [] as Array<(state: VoiceDeviceState) => void>,
		reset(): void {
			mock.state = idleState();
			mock.permissionRequests = [];
			mock.pending = [];
			listeners.clear();
		},
		settlePendingWith(next: VoiceDeviceState): void {
			const pending = mock.pending;
			mock.pending = [];
			mock.state = next;
			for (const listener of listeners) listener(next);
			for (const resolve of pending) resolve(next);
		},
		manager: {
			getState: (): VoiceDeviceState => mock.state,
			subscribe: (listener: (state: VoiceDeviceState) => void): (() => void) => {
				listeners.add(listener);
				listener(mock.state);
				return () => listeners.delete(listener);
			},
			ensureDevices: (options: EnsureVoiceDevicesOptions = {}): Promise<VoiceDeviceState> => {
				const requestPermissionTypes =
					options.requestPermissionTypes ?? (options.requestPermissions === true ? ['audio', 'video'] : []);
				if (requestPermissionTypes.length === 0) return Promise.resolve(mock.state);
				mock.permissionRequests.push([...requestPermissionTypes]);
				return new Promise<VoiceDeviceState>((resolve) => {
					mock.pending.push(resolve);
				});
			},
		},
	};
	return mock;
});

const mediaPermissionMock = vi.hoisted(() => ({
	microphoneExplicitlyDenied: false,
	cameraExplicitlyDenied: false,
	microphonePermissionState: null as PermissionState | null,
	cameraPermissionState: null as PermissionState | null,
	isMicrophoneGranted: vi.fn(() => false),
	isCameraGranted: vi.fn(() => false),
	updateMicrophonePermissionGranted: vi.fn(),
	updateCameraPermissionGranted: vi.fn(),
	markMicrophoneExplicitlyDenied: vi.fn(),
	markCameraExplicitlyDenied: vi.fn(),
}));

const handleMediaPermissionBlocked = vi.hoisted(() => vi.fn());

vi.mock('@app/features/voice/utils/VoiceDeviceManager', () => ({voiceDeviceManager: deviceManagerMock.manager}));
vi.mock('@app/features/permissions/system/state/MediaPermission', () => ({default: mediaPermissionMock}));
vi.mock('@app/features/permissions/system/commands/MacPermissionsModalCommands', () => ({
	handleMediaPermissionBlocked,
}));

const {useMediaPermission} = await import('@app/features/user/components/modals/tabs/hooks/useMediaPermission');

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

const MICROPHONE: MediaDeviceInfo = {
	deviceId: 'mic-1',
	groupId: 'group-1',
	kind: 'audioinput',
	label: 'Microphone',
	toJSON: () => ({}),
};

const grantedState = (): VoiceDeviceState => ({
	inputDevices: [MICROPHONE],
	outputDevices: [],
	videoDevices: [],
	permissionStatus: {audio: 'granted', video: 'idle'},
});

const deniedState = (): VoiceDeviceState => ({
	inputDevices: [],
	outputDevices: [],
	videoDevices: [],
	permissionStatus: {audio: 'denied', video: 'idle'},
});

let host: HTMLDivElement;
let root: Root;
const requesters: Array<() => Promise<boolean>> = [];

function Consumer(): null {
	const {requestPermission} = useMediaPermission('audio');
	useEffect(() => {
		requesters.push(requestPermission);
		return () => {
			const index = requesters.indexOf(requestPermission);
			if (index !== -1) requesters.splice(index, 1);
		};
	}, [requestPermission]);
	return null;
}

function renderConsumers(count: number): void {
	act(() => {
		root.render(Array.from({length: count}, (_, index) => createElement(Consumer, {key: index})));
	});
}

beforeEach(() => {
	deviceManagerMock.reset();
	requesters.length = 0;
	mediaPermissionMock.microphoneExplicitlyDenied = false;
	mediaPermissionMock.microphonePermissionState = null;
	mediaPermissionMock.isMicrophoneGranted.mockReturnValue(false);
	host = document.createElement('div');
	document.body.append(host);
	root = createRoot(host);
});

afterEach(() => {
	act(() => {
		root.unmount();
	});
	document.body.replaceChildren();
	vi.clearAllMocks();
});

describe('useMediaPermission', () => {
	it('raises a single device permission request when two consumers ask at once', async () => {
		renderConsumers(2);
		expect(requesters).toHaveLength(2);

		const results = await act(async () => {
			const pending = Promise.all([requesters[0](), requesters[1]()]);
			await Promise.resolve();
			deviceManagerMock.settlePendingWith(grantedState());
			return pending;
		});

		expect(deviceManagerMock.permissionRequests).toEqual([['audio']]);
		expect(results).toEqual([true, true]);
		expect(mediaPermissionMock.updateMicrophonePermissionGranted).toHaveBeenCalledTimes(1);
	});

	it('reports a denied request through the shared permission state', async () => {
		renderConsumers(1);

		const granted = await act(async () => {
			const pending = requesters[0]();
			await Promise.resolve();
			deviceManagerMock.settlePendingWith(deniedState());
			return pending;
		});

		expect(granted).toBe(false);
		expect(mediaPermissionMock.markMicrophoneExplicitlyDenied).toHaveBeenCalledTimes(1);
		expect(handleMediaPermissionBlocked).toHaveBeenCalledWith('microphone');
	});
});

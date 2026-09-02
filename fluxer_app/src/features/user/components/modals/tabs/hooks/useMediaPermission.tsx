// SPDX-License-Identifier: AGPL-3.0-or-later

import {handleMediaPermissionBlocked} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import type {VoiceDeviceState} from '@app/features/voice/utils/VoiceDeviceManager';
import {useCallback, useEffect, useLayoutEffect, useRef, useState} from 'react';

const logger = new Logger('useMediaPermission');

type PermissionType = 'audio' | 'video';
type BrowserPermissionState = 'denied' | 'granted' | 'prompt';

interface PermissionState {
	status: 'idle' | 'loading' | 'granted' | 'denied';
	devices: Array<MediaDeviceInfo>;
	deviceState: VoiceDeviceState;
}

interface UseMediaPermissionOptions {
	autoRequest?: boolean;
}

const devicesFromVoiceState = (deviceState: VoiceDeviceState, type: PermissionType): Array<MediaDeviceInfo> => {
	if (type === 'audio') {
		return [...deviceState.inputDevices, ...deviceState.outputDevices];
	}
	return deviceState.videoDevices;
};
const hasPrimaryDeviceForType = (devices: Array<MediaDeviceInfo>, type: PermissionType) => {
	const requiredKind = type === 'audio' ? 'audioinput' : 'videoinput';
	return devices.some((device) => device.kind === requiredKind);
};
const resolveStatusFromVoiceState = ({
	cachedPermissionState,
	deviceState,
	isExplicitlyDenied,
	osPermissionLooksGranted,
	type,
}: {
	cachedPermissionState: BrowserPermissionState | null;
	deviceState: VoiceDeviceState;
	isExplicitlyDenied: boolean;
	osPermissionLooksGranted: boolean;
	type: PermissionType;
}): PermissionState['status'] => {
	if (isExplicitlyDenied) return 'denied';
	if (cachedPermissionState === 'denied') return 'denied';
	const permissionStatus = deviceState.permissionStatus[type];
	if (permissionStatus === 'denied') return 'denied';
	if (cachedPermissionState === 'granted' || osPermissionLooksGranted) {
		return permissionStatus === 'loading' ? 'loading' : 'granted';
	}
	if (permissionStatus === 'loading') return 'loading';
	if (permissionStatus === 'granted') return 'granted';
	return 'idle';
};
export const useMediaPermission = (type: PermissionType, options: UseMediaPermissionOptions = {}) => {
	const {autoRequest = false} = options;
	const micExplicitlyDenied = MediaPermission.microphoneExplicitlyDenied;
	const cameraExplicitlyDenied = MediaPermission.cameraExplicitlyDenied;
	const isExplicitlyDenied = type === 'audio' ? micExplicitlyDenied : cameraExplicitlyDenied;
	const cachedPermissionState =
		type === 'audio' ? MediaPermission.microphonePermissionState : MediaPermission.cameraPermissionState;
	const osPermissionLooksGranted = cachedPermissionState === 'granted';
	const initialDeviceState = VoiceDevicePermissionState.getState();
	const [state, setState] = useState<PermissionState>(() => ({
		status: resolveStatusFromVoiceState({
			cachedPermissionState,
			deviceState: initialDeviceState,
			isExplicitlyDenied,
			osPermissionLooksGranted,
			type,
		}),
		devices: devicesFromVoiceState(initialDeviceState, type),
		deviceState: initialDeviceState,
	}));
	const applyVoiceDeviceState = useCallback(
		(deviceState: VoiceDeviceState): PermissionState => {
			const nextState = {
				status: resolveStatusFromVoiceState({
					cachedPermissionState,
					deviceState,
					isExplicitlyDenied,
					osPermissionLooksGranted,
					type,
				}),
				devices: devicesFromVoiceState(deviceState, type),
				deviceState,
			};
			setState(nextState);
			return nextState;
		},
		[cachedPermissionState, isExplicitlyDenied, osPermissionLooksGranted, type],
	);
	const unlockDevices = useCallback(async (): Promise<PermissionState> => {
		setState((prev) => ({...prev, status: 'loading'}));
		const granted = await VoiceDevicePermissionState.requestPermissionFor(type);
		const deviceState = VoiceDevicePermissionState.getState();
		const nextState = {
			status: granted ? 'granted' : deviceState.permissionStatus[type] === 'denied' ? 'denied' : 'idle',
			devices: devicesFromVoiceState(deviceState, type),
			deviceState,
		} satisfies PermissionState;
		setState(nextState);
		return nextState;
	}, [type]);
	const requestPermission = useCallback(async () => {
		if (isExplicitlyDenied) {
			handleMediaPermissionBlocked(type === 'audio' ? 'microphone' : 'camera');
			return false;
		}
		const currentDeviceState = VoiceDevicePermissionState.getState();
		const currentDevices = devicesFromVoiceState(currentDeviceState, type);
		const permissionAlreadyGranted =
			type === 'audio' ? MediaPermission.isMicrophoneGranted() : MediaPermission.isCameraGranted();
		if (permissionAlreadyGranted && hasPrimaryDeviceForType(currentDevices, type)) {
			applyVoiceDeviceState(currentDeviceState);
			return true;
		}
		try {
			const nextState = await unlockDevices();
			if (nextState.status === 'denied') {
				handleMediaPermissionBlocked(type === 'audio' ? 'microphone' : 'camera');
				return false;
			}
			return nextState.status === 'granted' && hasPrimaryDeviceForType(nextState.devices, type);
		} catch (error) {
			logger.error('Media permission request failed', {type, error});
			return false;
		}
	}, [type, isExplicitlyDenied, applyVoiceDeviceState, unlockDevices]);
	const unlockDevicesRef = useRef(unlockDevices);
	useLayoutEffect(() => {
		unlockDevicesRef.current = unlockDevices;
	}, [unlockDevices]);
	useEffect(() => VoiceDevicePermissionState.subscribe(applyVoiceDeviceState), [applyVoiceDeviceState]);
	useLayoutEffect(() => {
		if (isExplicitlyDenied) {
			setState((prev) => ({...prev, status: 'denied'}));
			return;
		}
		if (!autoRequest) {
			void VoiceDevicePermissionState.ensureDevices({requestPermissions: false})
				.then(applyVoiceDeviceState)
				.catch((error) => logger.warn('Passive media device enumeration failed', {type, error}));
			return;
		}
		void unlockDevicesRef
			.current()
			.catch((error) => logger.error('Automatic media permission request failed', {type, error}));
	}, [applyVoiceDeviceState, isExplicitlyDenied, type, autoRequest]);
	return {
		...state,
		isExplicitlyDenied,
		requestPermission,
	};
};

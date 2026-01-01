/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {CameraPermissionDeniedModal} from '~/components/alerts/CameraPermissionDeniedModal';
import {MicrophonePermissionDeniedModal} from '~/components/alerts/MicrophonePermissionDeniedModal';
import {mediaDeviceCache} from '~/lib/MediaDeviceCache';
import {Platform} from '~/lib/Platform';
import MediaPermissionStore from '~/stores/MediaPermissionStore';
import NativePermissionStore from '~/stores/NativePermissionStore';
import VoiceDevicePermissionStore from '~/stores/voice/VoiceDevicePermissionStore';
import {ensureNativePermission} from '~/utils/NativePermissions';

type PermissionType = 'audio' | 'video';

interface PermissionState {
	status: 'idle' | 'loading' | 'granted' | 'denied';
	devices: Array<MediaDeviceInfo>;
}

interface UseMediaPermissionOptions {
	autoRequest?: boolean;
}

const filterDevicesForType = (devices: Array<MediaDeviceInfo>, type: PermissionType) => {
	if (type === 'audio') {
		return devices.filter((d) => d.kind === 'audioinput' || d.kind === 'audiooutput');
	}
	return devices.filter((d) => d.kind === 'videoinput');
};

const needsBrowserPermission = (devices: Array<MediaDeviceInfo>, type: PermissionType) => {
	if (devices.length === 0) return true;
	const requiredKind = type === 'audio' ? 'audioinput' : 'videoinput';
	const hasPrimaryDevice = devices.some((device) => device.kind === requiredKind);
	const hasLabels = devices.some((device) => (device.label ?? '').trim().length > 0);
	return !hasPrimaryDevice || !hasLabels;
};

export const useMediaPermission = (type: PermissionType, options: UseMediaPermissionOptions = {}) => {
	const {autoRequest = true} = options;

	const micExplicitlyDenied = MediaPermissionStore.microphoneExplicitlyDenied;
	const cameraExplicitlyDenied = MediaPermissionStore.cameraExplicitlyDenied;
	const isExplicitlyDenied = type === 'audio' ? micExplicitlyDenied : cameraExplicitlyDenied;

	const cachedPermissionState =
		type === 'audio' ? MediaPermissionStore.microphonePermissionState : MediaPermissionStore.cameraPermissionState;

	const storeInitialized = MediaPermissionStore.initialized;

	const getInitialStatus = (): PermissionState['status'] => {
		if (isExplicitlyDenied) return 'denied';
		if (cachedPermissionState === 'granted') return 'loading';
		if (!storeInitialized && autoRequest) return 'loading';
		return 'idle';
	};

	const [state, setState] = React.useState<PermissionState>(() => ({
		status: getInitialStatus(),
		devices: [],
	}));

	const isDesktop = NativePermissionStore.isDesktop;

	const fetchDevices = React.useCallback(async (): Promise<Array<MediaDeviceInfo>> => {
		const ensureBrowserPermission = async () => {
			const constraints = type === 'audio' ? {audio: true} : {video: true};
			const stream = await navigator.mediaDevices.getUserMedia(constraints);
			stream.getTracks().forEach((track) => track.stop());
		};

		const enumerateFiltered = async () => {
			const rawDevices = await navigator.mediaDevices.enumerateDevices();
			return filterDevicesForType(rawDevices, type);
		};

		const isIOSPWA = Platform.isIOSWeb && Platform.isPWA;
		if (isIOSPWA && navigator.permissions) {
			try {
				const permissionName = type === 'audio' ? 'microphone' : 'camera';
				const permission = await navigator.permissions.query({name: permissionName as PermissionName});
				if (permission.state === 'granted') {
					const devices = await enumerateFiltered();
					if (devices.length > 0 && devices.some((d) => (d.label ?? '').trim().length > 0)) {
						return devices;
					}
				}
			} catch {}
		}

		let shouldPrimeBrowserPermission = !isDesktop;

		if (isDesktop) {
			const nativeResult = await ensureNativePermission(type === 'audio' ? 'microphone' : 'camera');
			if (nativeResult === 'denied') {
				const error = new DOMException('PermissionDenied', 'NotAllowedError');
				throw error;
			}
			shouldPrimeBrowserPermission = nativeResult === 'unsupported';
		}

		let filteredDevices = await enumerateFiltered();

		if (shouldPrimeBrowserPermission || needsBrowserPermission(filteredDevices, type)) {
			await ensureBrowserPermission();
			filteredDevices = await enumerateFiltered();
		}

		return filteredDevices;
	}, [type, isDesktop]);

	const requestPermission = React.useCallback(async () => {
		if (isExplicitlyDenied) {
			const Modal = type === 'audio' ? MicrophonePermissionDeniedModal : CameraPermissionDeniedModal;
			ModalActionCreators.push(modal(() => <Modal />));
			return false;
		}

		try {
			setState((prev) => ({...prev, status: 'loading'}));

			const {devices} = await mediaDeviceCache.getDevices(type, fetchDevices);

			setState({
				status: 'granted',
				devices,
			});

			if (type === 'audio') {
				MediaPermissionStore.updateMicrophonePermissionGranted();
			} else {
				MediaPermissionStore.updateCameraPermissionGranted();
			}

			void VoiceDevicePermissionStore.ensureDevices({requestPermissions: true});

			return true;
		} catch (error) {
			if (error instanceof Error && (error.name === 'NotAllowedError' || error.name === 'PermissionDeniedError')) {
				const Modal = type === 'audio' ? MicrophonePermissionDeniedModal : CameraPermissionDeniedModal;

				if (type === 'audio') {
					MediaPermissionStore.markMicrophoneExplicitlyDenied();
				} else {
					MediaPermissionStore.markCameraExplicitlyDenied();
				}

				ModalActionCreators.push(modal(() => <Modal />));
				setState((prev) => ({...prev, status: 'denied'}));
			}
			return false;
		}
	}, [type, isExplicitlyDenied, fetchDevices]);

	const requestPermissionRef = React.useRef(requestPermission);
	React.useLayoutEffect(() => {
		requestPermissionRef.current = requestPermission;
	}, [requestPermission]);

	React.useLayoutEffect(() => {
		if (isExplicitlyDenied) {
			setState((prev) => ({...prev, status: 'denied'}));
			return;
		}

		if (!autoRequest) {
			return;
		}

		if (cachedPermissionState === 'granted') {
			void requestPermissionRef.current();
			return;
		}

		if (!storeInitialized) {
			return;
		}

		const currentPermissionState =
			type === 'audio' ? MediaPermissionStore.microphonePermissionState : MediaPermissionStore.cameraPermissionState;

		if (currentPermissionState === 'granted') {
			void requestPermissionRef.current();
		} else {
			setState((prev) => ({...prev, status: 'idle', devices: []}));
		}
	}, [isExplicitlyDenied, type, autoRequest, cachedPermissionState, storeInitialized]);

	React.useEffect(() => {
		const cleanup = mediaDeviceCache.startDeviceChangeListener();

		const handleDeviceChange = () => {
			if (state.status === 'granted') {
				void requestPermissionRef.current();
			}
		};

		if (navigator.mediaDevices?.addEventListener) {
			navigator.mediaDevices.addEventListener('devicechange', handleDeviceChange);
		} else if ('ondevicechange' in (navigator.mediaDevices ?? {})) {
			const previous = (navigator.mediaDevices as any).ondevicechange;
			(navigator.mediaDevices as any).ondevicechange = (...args: Array<unknown>) => {
				handleDeviceChange();
				previous?.(...args);
			};
		}

		return () => {
			if (navigator.mediaDevices?.removeEventListener) {
				navigator.mediaDevices.removeEventListener('devicechange', handleDeviceChange);
			} else if (navigator.mediaDevices && 'ondevicechange' in navigator.mediaDevices) {
				(navigator.mediaDevices as any).ondevicechange = null;
			}
			cleanup();
		};
	}, [state.status]);

	return {
		...state,
		isExplicitlyDenied,
		requestPermission,
	};
};

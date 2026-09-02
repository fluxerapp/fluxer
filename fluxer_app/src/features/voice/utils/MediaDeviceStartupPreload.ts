// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import type {VoiceMediaPermissionType} from '@app/features/voice/utils/VoiceDeviceManager';

const logger = new Logger('MediaDeviceStartupPreload');

export function startMediaDeviceStartupPreload(): () => void {
	let stopped = false;
	let lastPermissionStateKey: string | null = null;
	const preloadDevices = () => {
		if (stopped) return;
		const requestPermissionTypes: Array<VoiceMediaPermissionType> = [];
		if (MediaPermission.isMicrophoneGranted()) requestPermissionTypes.push('audio');
		if (MediaPermission.isCameraGranted()) requestPermissionTypes.push('video');
		const permissionStateKey = [
			MediaPermission.isInitialized() ? 'initialized' : 'pending',
			MediaPermission.getMicrophonePermissionState() ?? 'unknown',
			MediaPermission.getCameraPermissionState() ?? 'unknown',
		].join(':');
		const deviceState = VoiceDevicePermissionState.getState();
		const forceRefresh = lastPermissionStateKey !== null && lastPermissionStateKey !== permissionStateKey;
		const requestedPermissionStatesSettled = requestPermissionTypes.every(
			(type) => deviceState.permissionStatus[type] !== 'idle',
		);
		if (!forceRefresh && lastPermissionStateKey === permissionStateKey && requestedPermissionStatesSettled) {
			return;
		}
		lastPermissionStateKey = permissionStateKey;
		void VoiceDevicePermissionState.ensureDevices({forceRefresh, requestPermissionTypes}).catch((error) => {
			logger.debug('Failed to preload media devices', {error});
		});
	};
	const disposePermissionListener = MediaPermission.addChangeListener(preloadDevices);
	return () => {
		stopped = true;
		disposePermissionListener();
	};
}

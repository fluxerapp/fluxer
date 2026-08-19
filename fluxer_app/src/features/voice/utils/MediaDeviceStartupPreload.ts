// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaPermission from '@app/features/permissions/system/state/MediaPermission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';

const logger = new Logger('MediaDeviceStartupPreload');

export function startMediaDeviceStartupPreload(): () => void {
	let stopped = false;
	let lastPermissionStateKey: string | null = null;
	const preloadDevices = () => {
		if (stopped) return;
		const requestPermissions = MediaPermission.isMicrophoneGranted() || MediaPermission.isCameraGranted();
		const permissionStateKey = [
			MediaPermission.isInitialized() ? 'initialized' : 'pending',
			MediaPermission.getMicrophonePermissionState() ?? 'unknown',
			MediaPermission.getCameraPermissionState() ?? 'unknown',
		].join(':');
		const deviceState = VoiceDevicePermissionState.getState();
		const forceRefresh = lastPermissionStateKey !== null && lastPermissionStateKey !== permissionStateKey;
		if (!forceRefresh && lastPermissionStateKey === permissionStateKey && deviceState.permissionStatus !== 'idle') {
			return;
		}
		lastPermissionStateKey = permissionStateKey;
		void VoiceDevicePermissionState.ensureDevices({forceRefresh, requestPermissions}).catch((error) => {
			logger.debug('Failed to preload media devices', {error});
		});
	};
	const disposePermissionListener = MediaPermission.addChangeListener(preloadDevices);
	return () => {
		stopped = true;
		disposePermissionListener();
	};
}

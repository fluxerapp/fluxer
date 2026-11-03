// SPDX-License-Identifier: AGPL-3.0-or-later

import VoiceDevicePermissionState from '@app/features/voice/engine/VoiceDevicePermissionState';
import type {VoiceDeviceState} from '@app/features/voice/utils/VoiceDeviceManager';
import {useCallback, useEffect, useState} from 'react';

interface UseMediaDevicesOptions {
	requestPermissions?: boolean;
	autoRefresh?: boolean;
}

interface RefreshOptions {
	requestPermissions?: boolean;
}

type UseMediaDevicesResult = VoiceDeviceState & {
	refreshDevices: (options?: RefreshOptions) => Promise<void>;
};

export const useMediaDevices = (options: UseMediaDevicesOptions = {}): UseMediaDevicesResult => {
	const {requestPermissions = false, autoRefresh = true} = options;
	const [state, setState] = useState<VoiceDeviceState>(() => VoiceDevicePermissionState.getState());
	useEffect(() => VoiceDevicePermissionState.subscribe(setState), []);
	useEffect(() => {
		if (!autoRefresh) return;
		void VoiceDevicePermissionState.ensureDevices({requestPermissions}).catch(() => {});
	}, [autoRefresh, requestPermissions]);
	const refreshDevices = useCallback(
		async (refreshOptions?: RefreshOptions) => {
			await VoiceDevicePermissionState.ensureDevices({
				forceRefresh: true,
				requestPermissions: refreshOptions?.requestPermissions ?? requestPermissions,
			});
		},
		[requestPermissions],
	);
	return {
		...state,
		refreshDevices,
	};
};

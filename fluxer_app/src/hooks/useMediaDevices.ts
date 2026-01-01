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

import {useCallback, useEffect, useState} from 'react';
import VoiceDevicePermissionStore, {type VoiceDeviceState} from '~/stores/voice/VoiceDevicePermissionStore';

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
	const [state, setState] = useState<VoiceDeviceState>(() => VoiceDevicePermissionStore.getState());

	useEffect(() => VoiceDevicePermissionStore.subscribe(setState), []);

	useEffect(() => {
		if (!autoRefresh) return;
		void VoiceDevicePermissionStore.ensureDevices({requestPermissions}).catch(() => {});
	}, [autoRefresh, requestPermissions]);

	const refreshDevices = useCallback(
		async (refreshOptions?: RefreshOptions) => {
			await VoiceDevicePermissionStore.ensureDevices({
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

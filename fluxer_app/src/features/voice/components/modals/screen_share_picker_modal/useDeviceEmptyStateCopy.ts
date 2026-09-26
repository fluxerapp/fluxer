// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import type {DisplayShareEnvironment} from '@app/features/voice/utils/ScreenShareEnvironment';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';

const NO_CAMERAS_OR_VIRTUAL_DEVICES_FOUND_DESCRIPTOR = msg({
	message: 'No cameras or virtual devices found',
	comment: 'Empty-state title in the screen-share picker devices tab when no cameras / capture devices are detected.',
});
const BROWSERS_DO_NOT_LIST_CAMERAS_IN_THE_SCREEN_DESCRIPTOR = msg({
	message:
		'Browsers do not list cameras in the screen-share picker. Choose them from the Devices tab instead. If nothing appears here, allow camera access or connect a device.',
	comment: 'Empty-state explanation in the screen-share picker devices tab on web.',
});
const CAMERAS_AND_VIRTUAL_CAPTURE_DEVICES_ARE_SELECTED_HERE_DESCRIPTOR = msg({
	message:
		'Cameras and virtual capture devices are selected here in {productName}, not in the Wayland system picker. If nothing appears here, check permissions or connect a device.',
	comment: 'Empty-state explanation in the screen-share picker devices tab on Linux/Wayland. {productName} is Fluxer.',
});
const NO_CAPTURE_DEVICES_FOUND_DESCRIPTOR = msg({
	message: 'No capture devices found',
	comment:
		'Empty-state title in the screen-share picker devices tab when no capture devices are detected (desktop, non-Wayland).',
});
const CONNECT_A_CAMERA_OR_VIRTUAL_CAPTURE_DEVICE_THEN_DESCRIPTOR = msg({
	message: 'Connect a camera or virtual capture device, then try again.',
	comment:
		'Empty-state explanation in the screen-share picker devices tab. Prompts user to connect hardware and retry.',
});

export interface DeviceEmptyStateCopy {
	title: string;
	description: string;
}

export function useDeviceEmptyStateCopy(displayShareEnvironment: DisplayShareEnvironment): DeviceEmptyStateCopy {
	const {i18n} = useLingui();
	return useMemo(() => {
		if (displayShareEnvironment === 'web') {
			return {
				title: i18n._(NO_CAMERAS_OR_VIRTUAL_DEVICES_FOUND_DESCRIPTOR),
				description: i18n._(BROWSERS_DO_NOT_LIST_CAMERAS_IN_THE_SCREEN_DESCRIPTOR),
			};
		}
		if (displayShareEnvironment === 'desktop-wayland') {
			return {
				title: i18n._(NO_CAMERAS_OR_VIRTUAL_DEVICES_FOUND_DESCRIPTOR),
				description: i18n._(CAMERAS_AND_VIRTUAL_CAPTURE_DEVICES_ARE_SELECTED_HERE_DESCRIPTOR, {
					productName: PRODUCT_NAME,
				}),
			};
		}
		return {
			title: i18n._(NO_CAPTURE_DEVICES_FOUND_DESCRIPTOR),
			description: i18n._(CONNECT_A_CAMERA_OR_VIRTUAL_CAPTURE_DEVICE_THEN_DESCRIPTOR),
		};
	}, [displayShareEnvironment, i18n.locale]);
}

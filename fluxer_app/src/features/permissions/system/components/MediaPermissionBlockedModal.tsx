// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export type MediaPermissionBlockedKind = 'microphone' | 'camera' | 'screen';

const MICROPHONE_PERMISSION_REQUIRED_DESCRIPTOR = msg({
	message: 'Microphone permission required',
	comment: 'Title of the modal shown when microphone permission is denied. Keep tone plain.',
});
const CAMERA_PERMISSION_REQUIRED_DESCRIPTOR = msg({
	message: 'Camera permission required',
	comment: 'Title of the modal shown when camera permission is denied. Keep tone plain.',
});
const SCREEN_PERMISSION_REQUIRED_DESCRIPTOR = msg({
	message: 'Screen recording permission required',
	comment: 'Title of the modal shown when screen recording permission is denied. Keep tone plain.',
});
const MICROPHONE_DESKTOP_DESCRIPTOR = msg({
	message:
		'{productName} needs access to your microphone. Allow microphone access in your operating system privacy settings and restart the app.',
	comment: 'Body of the microphone-permission-denied modal on Windows and Linux. {productName} is Fluxer.',
});
const MICROPHONE_WEB_DESCRIPTOR = msg({
	message:
		'{productName} needs access to your microphone to enable voice chat. Grant microphone permission in your browser settings and try again.',
	comment: 'Body of the microphone-permission-denied modal on web. {productName} is Fluxer.',
});
const CAMERA_DESKTOP_DESCRIPTOR = msg({
	message:
		'{productName} needs access to your camera. Allow camera access in your operating system privacy settings and restart the app.',
	comment: 'Body of the camera-permission-denied modal on Windows and Linux. {productName} is Fluxer.',
});
const CAMERA_WEB_DESCRIPTOR = msg({
	message:
		'{productName} needs access to your camera to enable video chat. Grant camera permission in your browser settings and try again.',
	comment: 'Body of the camera-permission-denied modal on web. {productName} is Fluxer.',
});
const SCREEN_DESCRIPTOR = msg({
	message:
		'{productName} needs screen recording access. Allow screen recording in your operating system privacy settings and restart the app.',
	comment: 'Body of the screen-permission-denied modal on non-mac desktop platforms. {productName} is Fluxer.',
});

const titleDescriptorForKind = (kind: MediaPermissionBlockedKind) => {
	switch (kind) {
		case 'microphone':
			return MICROPHONE_PERMISSION_REQUIRED_DESCRIPTOR;
		case 'camera':
			return CAMERA_PERMISSION_REQUIRED_DESCRIPTOR;
		case 'screen':
			return SCREEN_PERMISSION_REQUIRED_DESCRIPTOR;
	}
};

const bodyDescriptorForKind = (kind: MediaPermissionBlockedKind, desktop: boolean) => {
	switch (kind) {
		case 'microphone':
			return desktop ? MICROPHONE_DESKTOP_DESCRIPTOR : MICROPHONE_WEB_DESCRIPTOR;
		case 'camera':
			return desktop ? CAMERA_DESKTOP_DESCRIPTOR : CAMERA_WEB_DESCRIPTOR;
		case 'screen':
			return SCREEN_DESCRIPTOR;
	}
};

export const MediaPermissionBlockedModal = observer(({kind}: {kind: MediaPermissionBlockedKind}) => {
	const {i18n} = useLingui();
	const desktop = isDesktop();
	return (
		<GenericErrorModal
			title={i18n._(titleDescriptorForKind(kind))}
			message={i18n._(bodyDescriptorForKind(kind, desktop), {productName: PRODUCT_NAME})}
			data-flx="permissions.media-permission-blocked-modal.confirm-modal"
		/>
	);
});

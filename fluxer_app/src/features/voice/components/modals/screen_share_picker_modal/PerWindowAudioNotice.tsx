// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/voice/components/modals/ScreenSharePickerModal.module.css';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';

const PER_WINDOW_AUDIO_ISN_T_AVAILABLE_ON_THIS_DESCRIPTOR = msg({
	message: "App audio isn't available right now on Windows.",
	comment: 'Inline notice on Windows when per-window audio capture is not supported. Title text.',
});
const WINDOWS_APP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR = msg({
	message: "{productName} cannot capture only one app's audio here without risking unrelated app audio or call audio.",
	comment:
		'Inline notice body on Windows. Explains that app audio is disabled because productName cannot guarantee isolated audio capture on this OS build.',
});
const WINDOWS_DESKTOP_AUDIO_UNSUPPORTED_TITLE_DESCRIPTOR = msg({
	message: "Desktop audio isn't available right now on Windows.",
	comment:
		'Inline notice on Windows when desktop/system audio capture is not supported because Fluxer cannot exclude call audio.',
});
const WINDOWS_DESKTOP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR = msg({
	message: "{productName} cannot capture desktop audio here while excluding {productName}'s call audio.",
	comment:
		'Inline notice body on Windows. Explains that desktop audio is disabled because productName cannot exclude its own WebRTC/call playback on this OS build.',
});
const PER_WINDOW_AUDIO_ISN_T_AVAILABLE_ON_THIS_2_DESCRIPTOR = msg({
	message: "Per-window audio isn't available right now on macOS.",
	comment: 'Inline notice on macOS when per-window audio capture is not supported. Title text.',
});
const MACOS_APP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR = msg({
	message: "{productName} cannot capture only one app's audio here without risking unrelated app audio or call audio.",
	comment:
		'Inline notice body on macOS. Explains that app audio is disabled because productName cannot guarantee isolated audio capture on this OS build.',
});
const MACOS_DESKTOP_AUDIO_UNSUPPORTED_TITLE_DESCRIPTOR = msg({
	message: "Desktop audio isn't available right now on macOS.",
	comment:
		'Inline notice on macOS when desktop/system audio capture is not supported because Fluxer cannot exclude call audio.',
});
const MACOS_DESKTOP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR = msg({
	message: "{productName} cannot capture desktop audio here while excluding {productName}'s call audio.",
	comment:
		'Inline notice body on macOS. Explains that desktop audio is disabled because productName cannot exclude its own WebRTC/call playback on this OS build.',
});
const LINUX_APP_AUDIO_UNSUPPORTED_TITLE_DESCRIPTOR = msg({
	message: "App audio isn't available right now on Linux.",
	comment: 'Inline notice on Linux when isolated per-app audio capture is unavailable.',
});
const LINUX_APP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR = msg({
	message: "{productName} cannot start a safe isolated route for this app's audio.",
	comment: 'Inline notice body on Linux when isolated per-app audio capture is unavailable.',
});
const LINUX_DESKTOP_AUDIO_UNSUPPORTED_TITLE_DESCRIPTOR = msg({
	message: "Desktop audio isn't available right now on Linux.",
	comment: 'Inline notice on Linux when desktop audio capture is unavailable.',
});
const LINUX_DESKTOP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR = msg({
	message: "{productName} cannot start a safe desktop audio route while excluding {productName}'s call audio.",
	comment: 'Inline notice body on Linux when desktop audio capture is unavailable.',
});

interface PerWindowAudioNoticeProps {
	platform: 'win32' | 'darwin' | 'linux';
	mode?: 'app' | 'system';
}

export const PerWindowAudioNotice: React.FC<PerWindowAudioNoticeProps> = ({platform, mode = 'app'}) => {
	const {i18n} = useLingui();
	const copy = (() => {
		switch (platform) {
			case 'win32':
				return {
					title:
						mode === 'system'
							? WINDOWS_DESKTOP_AUDIO_UNSUPPORTED_TITLE_DESCRIPTOR
							: PER_WINDOW_AUDIO_ISN_T_AVAILABLE_ON_THIS_DESCRIPTOR,
					body:
						mode === 'system'
							? WINDOWS_DESKTOP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR
							: WINDOWS_APP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR,
				};
			case 'darwin':
				return {
					title:
						mode === 'system'
							? MACOS_DESKTOP_AUDIO_UNSUPPORTED_TITLE_DESCRIPTOR
							: PER_WINDOW_AUDIO_ISN_T_AVAILABLE_ON_THIS_2_DESCRIPTOR,
					body:
						mode === 'system'
							? MACOS_DESKTOP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR
							: MACOS_APP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR,
				};
			case 'linux':
				return {
					title:
						mode === 'system'
							? LINUX_DESKTOP_AUDIO_UNSUPPORTED_TITLE_DESCRIPTOR
							: LINUX_APP_AUDIO_UNSUPPORTED_TITLE_DESCRIPTOR,
					body:
						mode === 'system'
							? LINUX_DESKTOP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR
							: LINUX_APP_AUDIO_UNSUPPORTED_BODY_DESCRIPTOR,
				};
		}
	})();
	return (
		<div className={styles.osNotice} role="status" data-flx="voice.screen-share-picker-modal.os-notice">
			<strong data-flx="voice.screen-share-picker-modal.strong">{i18n._(copy.title)}</strong>{' '}
			{i18n._(copy.body, {productName: PRODUCT_NAME})}
		</div>
	);
};

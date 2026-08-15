// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MACOS_PRIVACY_AND_SECURITY_SETTINGS_NAME,
	MACOS_SCREEN_RECORDING_PERMISSION_NAME,
	MACOS_SYSTEM_SETTINGS_NAME,
	PRODUCT_NAME,
} from '@app/features/app/config/I18nDisplayConstants';
import {OPEN_SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Spinner} from '@app/features/ui/components/Spinner';
import styles from '@app/features/voice/components/modals/ScreenSharePickerModal.module.css';
import type {ScreenSharePickerDisplayPermissionPrompt as DisplayPermissionPrompt} from '@app/features/voice/components/modals/screen_share_picker_modal/ScreenSharePickerDisplayPermissionStateMachine';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MonitorIcon} from '@phosphor-icons/react';

const SCREEN_RECORDING_PERMISSION_REQUIRED_DESCRIPTOR = msg({
	message: 'Screen recording permission required',
	comment: 'Heading in the screen-share picker when macOS screen recording permission is missing.',
});
const CHECKING_SCREEN_RECORDING_PERMISSION_DESCRIPTOR = msg({
	message: 'Checking screen recording permission...',
	comment: 'Loading state in the screen-share picker while checking macOS screen recording permission.',
});
const SCREEN_RECORDING_PERMISSION_PROMPT_DESCRIPTOR = msg({
	message:
		'Open {macosSystemSettingsName} → {macosPrivacyAndSecuritySettingsName} → {macosScreenRecordingPermissionName}, then allow {productName}. If {productName2} is already enabled, fully quit and restart {productName3} so macOS applies the permission.',
	comment:
		'Body in the screen-share picker when macOS screen recording permission is missing. Placeholders are product and macOS UI names.',
});
const SCREEN_RECORDING_PERMISSION_RESTART_REQUIRED_DESCRIPTOR = msg({
	message: 'Restart {productName} to use screen sharing',
	comment:
		'Heading in the screen-share picker after the user has opened macOS screen recording settings. The app must restart before the permission applies.',
});
const SCREEN_RECORDING_PERMISSION_RESTART_PROMPT_DESCRIPTOR = msg({
	message:
		'After changing {macosScreenRecordingPermissionName}, fully quit and restart {productName} so macOS applies the permission.',
	comment:
		'Body in the screen-share picker after the user has opened macOS screen recording settings. macOS does not apply screen recording permission to a running app.',
});

export function ScreenSharePickerDisplayPermissionPrompt({
	prompt,
	onOpenSettings,
}: {
	prompt: Exclude<DisplayPermissionPrompt, 'none'>;
	onOpenSettings: () => void;
}) {
	const {i18n} = useLingui();
	if (prompt === 'checking') {
		return (
			<div className={styles.state} data-flx="voice.screen-share-picker-modal.screen-recording-permission.checking">
				<Spinner size="large" data-flx="voice.screen-share-picker-modal.screen-recording-permission.spinner" />
				<div
					className={styles.stateTitle}
					data-flx="voice.screen-share-picker-modal.screen-recording-permission.checking-title"
				>
					{i18n._(CHECKING_SCREEN_RECORDING_PERMISSION_DESCRIPTOR)}
				</div>
			</div>
		);
	}
	const restartRequired = prompt === 'restart-required';
	return (
		<div className={styles.state} data-flx="voice.screen-share-picker-modal.screen-recording-permission.state">
			<MonitorIcon
				size={remFromPx(42)}
				className={styles.stateIcon}
				data-flx="voice.screen-share-picker-modal.screen-share-picker-display-permission-prompt.state-icon"
			/>
			<div
				className={styles.stateHeading}
				data-flx="voice.screen-share-picker-modal.screen-recording-permission.heading"
			>
				{restartRequired
					? i18n._(SCREEN_RECORDING_PERMISSION_RESTART_REQUIRED_DESCRIPTOR, {productName: PRODUCT_NAME})
					: i18n._(SCREEN_RECORDING_PERMISSION_REQUIRED_DESCRIPTOR)}
			</div>
			<div className={styles.stateTitle} data-flx="voice.screen-share-picker-modal.screen-recording-permission.copy">
				{restartRequired
					? i18n._(SCREEN_RECORDING_PERMISSION_RESTART_PROMPT_DESCRIPTOR, {
							macosScreenRecordingPermissionName: MACOS_SCREEN_RECORDING_PERMISSION_NAME,
							productName: PRODUCT_NAME,
						})
					: i18n._(SCREEN_RECORDING_PERMISSION_PROMPT_DESCRIPTOR, {
							macosSystemSettingsName: MACOS_SYSTEM_SETTINGS_NAME,
							macosPrivacyAndSecuritySettingsName: MACOS_PRIVACY_AND_SECURITY_SETTINGS_NAME,
							macosScreenRecordingPermissionName: MACOS_SCREEN_RECORDING_PERMISSION_NAME,
							productName: PRODUCT_NAME,
							productName2: PRODUCT_NAME,
							productName3: PRODUCT_NAME,
						})}
			</div>
			<div
				className={styles.stateActions}
				data-flx="voice.screen-share-picker-modal.screen-share-picker-display-permission-prompt.state-actions"
			>
				<Button
					variant="primary"
					onClick={onOpenSettings}
					data-flx="voice.screen-share-picker-modal.screen-recording-permission.button.open-settings"
				>
					{i18n._(OPEN_SETTINGS_DESCRIPTOR)}
				</Button>
			</div>
		</div>
	);
}

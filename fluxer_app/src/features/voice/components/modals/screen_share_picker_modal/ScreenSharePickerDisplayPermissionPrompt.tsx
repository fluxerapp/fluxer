// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';

import {
	OPEN_SETTINGS_DESCRIPTOR,
	RELAUNCH_TO_APPLY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {Spinner} from '@app/features/ui/components/Spinner';
import {relaunchDesktopApp} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
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
	message: 'Open System Settings → Privacy & Security → Screen Recording, then allow {productName}.',
	comment:
		'Body in the screen-share picker when macOS screen recording permission is missing. {productName} is the app name. Keep "System Settings", "Privacy & Security" and the permission name as macOS shows them in this language.',
});
const SCREEN_RECORDING_PERMISSION_STILL_WAITING_DESCRIPTOR = msg({
	message: 'Still waiting for screen recording access',
	comment:
		'Heading in the screen-share picker after the user has opened macOS screen recording settings and the permission is not active yet.',
});
const SCREEN_RECORDING_PERMISSION_RELAUNCH_PROMPT_DESCRIPTOR = msg({
	message:
		'Allow {productName} under Screen Recording in System Settings. If it is already allowed, relaunch {productName2} so macOS applies it.',
	comment:
		'Body in the screen-share picker after the user has opened macOS screen recording settings and the permission is not active yet. {productName} and {productName2} are the app name. Keep "Screen Recording" and "System Settings" as macOS shows them in this language.',
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
					? i18n._(SCREEN_RECORDING_PERMISSION_STILL_WAITING_DESCRIPTOR)
					: i18n._(SCREEN_RECORDING_PERMISSION_REQUIRED_DESCRIPTOR)}
			</div>
			<div className={styles.stateTitle} data-flx="voice.screen-share-picker-modal.screen-recording-permission.copy">
				{restartRequired
					? i18n._(SCREEN_RECORDING_PERMISSION_RELAUNCH_PROMPT_DESCRIPTOR, {
							productName: RuntimeConfig.productName,
							productName2: RuntimeConfig.productName,
						})
					: i18n._(SCREEN_RECORDING_PERMISSION_PROMPT_DESCRIPTOR, {
							productName: RuntimeConfig.productName,
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
				{restartRequired && (
					<Button
						variant="secondary"
						onClick={() => void relaunchDesktopApp()}
						data-flx="voice.screen-share-picker-modal.screen-recording-permission.button.relaunch"
					>
						{i18n._(RELAUNCH_TO_APPLY_DESCRIPTOR)}
					</Button>
				)}
			</div>
		</div>
	);
}

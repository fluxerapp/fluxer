// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {RELAUNCH_TO_APPLY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/permissions/system/components/MacPermissionPrompt.module.css';
import {
	macPermissionNameDescriptor,
	PERMISSION_ALLOWED_DESCRIPTOR,
	QUIT_PROMPT_ADVICE_DESCRIPTOR,
	useMacPermissionControl,
} from '@app/features/permissions/system/components/useMacPermissionControl';
import type {MacPermissionKind} from '@app/features/permissions/system/state/MacPermissions';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button, ButtonVariant} from '@app/features/ui/button/Button';
import {relaunchDesktopApp} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckCircleIcon, KeyboardIcon, MicrophoneIcon, MonitorIcon, VideoCameraIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useRef} from 'react';

const MICROPHONE_PURPOSE_DESCRIPTOR = msg({
	message: 'Lets people hear you in calls.',
	comment: 'One sentence explaining what the macOS Microphone permission is used for.',
});
const CAMERA_PURPOSE_DESCRIPTOR = msg({
	message: 'Lets people see you on video.',
	comment: 'One sentence explaining what the macOS Camera permission is used for.',
});
const SCREEN_RECORDING_PURPOSE_DESCRIPTOR = msg({
	message: 'Lets you share your screen or a window.',
	comment: 'One sentence explaining what the macOS screen recording permission is used for.',
});
const INPUT_MONITORING_PURPOSE_DESCRIPTOR = msg({
	message: 'Lets push-to-talk and shortcuts work in other apps.',
	comment: 'One sentence explaining what the macOS Input Monitoring permission is used for.',
});
const NOT_NOW_DESCRIPTOR = msg({
	message: 'Not now',
	comment: 'Quiet secondary button that skips one macOS permission for now.',
});

const ALLOWED_BEAT_MS = 900;

const KINDS_WITH_QUIT_PROMPT_ADVICE: ReadonlySet<MacPermissionKind> = new Set(['screen', 'input-monitoring']);

interface MacPermissionPromptProps {
	readonly kind: MacPermissionKind;
	readonly onAllowed: () => void;
	readonly onNotNow: () => void;
	readonly titleId?: string;
	readonly actionRef?: React.Ref<HTMLButtonElement>;
}

const purposeDescriptorForKind = (kind: MacPermissionKind) => {
	switch (kind) {
		case 'microphone':
			return MICROPHONE_PURPOSE_DESCRIPTOR;
		case 'camera':
			return CAMERA_PURPOSE_DESCRIPTOR;
		case 'screen':
			return SCREEN_RECORDING_PURPOSE_DESCRIPTOR;
		case 'input-monitoring':
			return INPUT_MONITORING_PURPOSE_DESCRIPTOR;
	}
};

const iconForKind = (kind: MacPermissionKind) => {
	switch (kind) {
		case 'microphone':
			return MicrophoneIcon;
		case 'camera':
			return VideoCameraIcon;
		case 'screen':
			return MonitorIcon;
		case 'input-monitoring':
			return KeyboardIcon;
	}
};

export const MacPermissionPrompt: React.FC<MacPermissionPromptProps> = observer(
	({kind, onAllowed, onNotNow, titleId, actionRef}) => {
		const {i18n} = useLingui();
		const control = useMacPermissionControl(kind);
		const allowed = control.status === 'granted';
		const onAllowedRef = useRef(onAllowed);
		onAllowedRef.current = onAllowed;
		useEffect(() => {
			if (!allowed) return;
			const timer = window.setTimeout(() => onAllowedRef.current(), ALLOWED_BEAT_MS);
			return () => window.clearTimeout(timer);
		}, [allowed]);
		const Icon = iconForKind(kind);
		return (
			<div className={styles.root} data-permission={kind} data-flx="permissions.mac-permission-prompt.root">
				<div className={styles.tile} data-flx="permissions.mac-permission-prompt.tile">
					<Icon
						size={remFromPx(28)}
						weight="fill"
						aria-hidden="true"
						data-flx="permissions.mac-permission-prompt.permission-icon"
					/>
				</div>
				<h1 id={titleId} className={styles.title} data-flx="permissions.mac-permission-prompt.title">
					{i18n._(macPermissionNameDescriptor(kind))}
				</h1>
				<p className={styles.purpose} aria-live="polite" data-flx="permissions.mac-permission-prompt.purpose">
					<span
						className={styles.purposeLine}
						data-hidden={control.quitPromptAdvice}
						data-flx="permissions.mac-permission-prompt.purpose-line"
					>
						{i18n._(purposeDescriptorForKind(kind))}
					</span>
					{KINDS_WITH_QUIT_PROMPT_ADVICE.has(kind) && (
						<span
							className={styles.purposeLine}
							data-hidden={!control.quitPromptAdvice}
							data-flx="permissions.mac-permission-prompt.quit-prompt-advice"
						>
							{i18n._(QUIT_PROMPT_ADVICE_DESCRIPTOR, {productName: PRODUCT_NAME})}
						</span>
					)}
				</p>
				<div className={styles.actions} data-flx="permissions.mac-permission-prompt.actions">
					{allowed ? (
						<div className={styles.allowed} role="status" data-flx="permissions.mac-permission-prompt.allowed">
							<CheckCircleIcon
								size={remFromPx(22)}
								weight="fill"
								aria-hidden="true"
								data-flx="permissions.mac-permission-prompt.check-icon"
							/>
							<flx-i18n data-flx="permissions.mac-permission-prompt.flx-i18n">
								{i18n._(PERMISSION_ALLOWED_DESCRIPTOR)}
							</flx-i18n>
						</div>
					) : (
						<>
							<div className={styles.primaryActions} data-flx="permissions.mac-permission-prompt.primary-actions">
								{control.actionDescriptor && (
									<Button
										ref={actionRef}
										type="button"
										variant={ButtonVariant.PRIMARY}
										small={true}
										className={styles.primary}
										submitting={control.busy}
										onClick={control.runAction}
										data-step-focus="true"
										data-flx="permissions.mac-permission-prompt.button.action"
									>
										{i18n._(control.actionDescriptor)}
									</Button>
								)}
								{control.relaunchMayHelp && (
									<Button
										type="button"
										variant={ButtonVariant.SECONDARY}
										small={true}
										onClick={() => void relaunchDesktopApp()}
										data-flx="permissions.mac-permission-prompt.button.relaunch"
									>
										{i18n._(RELAUNCH_TO_APPLY_DESCRIPTOR)}
									</Button>
								)}
							</div>
							<Button
								type="button"
								variant={ButtonVariant.GHOST}
								compact={true}
								className={styles.notNow}
								onClick={onNotNow}
								data-flx="permissions.mac-permission-prompt.button.not-now"
							>
								{i18n._(NOT_NOW_DESCRIPTOR)}
							</Button>
						</>
					)}
				</div>
			</div>
		);
	},
);

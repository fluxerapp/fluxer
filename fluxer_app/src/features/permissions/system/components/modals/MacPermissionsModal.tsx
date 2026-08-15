// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {
	MACOS_CAMERA_PERMISSION_NAME,
	MACOS_INPUT_MONITORING_PERMISSION_NAME,
	MACOS_MICROPHONE_PERMISSION_NAME,
	MACOS_SCREEN_RECORDING_PERMISSION_NAME,
	PRODUCT_NAME,
} from '@app/features/app/config/I18nDisplayConstants';
import styles from '@app/features/permissions/system/components/modals/MacPermissionsModal.module.css';
import MacPermissions, {
	MAC_PERMISSION_KINDS,
	type MacPermissionKind,
} from '@app/features/permissions/system/state/MacPermissions';
import {
	type NativePermissionResult,
	openNativePermissionSettings,
	requestNativePermission,
} from '@app/features/permissions/system/utils/NativePermissions';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const TITLE_DESCRIPTOR = msg({
	message: 'macOS permissions',
	comment: 'Title for the central macOS permissions modal.',
});
const INTRO_DESCRIPTOR = msg({
	message: 'Review the permissions {productName} uses for voice, video, screen sharing, and shortcuts.',
	comment: 'Intro text for the central macOS permissions modal. {productName} is the app name.',
});
const MICROPHONE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Talk in calls and test your input.',
	comment: 'One-line microphone permission explanation in the macOS permissions modal.',
});
const CAMERA_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Use video and camera previews.',
	comment: 'One-line camera permission explanation in the macOS permissions modal.',
});
const SCREEN_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Share screens and windows.',
	comment: 'One-line screen recording permission explanation in the macOS permissions modal.',
});
const INPUT_MONITORING_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Use system-wide push-to-talk and shortcuts.',
	comment: 'One-line input monitoring permission explanation in the macOS permissions modal.',
});
const STATUS_GRANTED_DESCRIPTOR = msg({
	message: 'Granted',
	comment: 'Status chip for an allowed macOS permission.',
});
const STATUS_DENIED_DESCRIPTOR = msg({
	message: 'Denied',
	comment: 'Status chip for a denied macOS permission.',
});
const STATUS_NOT_DETERMINED_DESCRIPTOR = msg({
	message: 'Not set',
	comment: 'Status chip for a macOS permission that has not been requested.',
});
const STATUS_UNSUPPORTED_DESCRIPTOR = msg({
	message: 'Unavailable',
	comment: 'Status chip for a macOS permission API that is unavailable.',
});
const GRANT_DESCRIPTOR = msg({
	message: 'Grant',
	comment: 'Button label that starts the native macOS permission request.',
});
const OPEN_SETTINGS_DESCRIPTOR = msg({
	message: 'Open settings',
	comment: 'Button label that opens macOS System Settings for a denied permission.',
});
const RESTART_REQUIRED_DESCRIPTOR = msg({
	message: 'Fully quit and restart {productName} so macOS applies this permission.',
	comment: 'Restart note after a macOS permission change. {productName} is the app name.',
});

interface MacPermissionsModalProps {
	focus?: MacPermissionKind;
}

const labelForKind = (kind: MacPermissionKind): string => {
	switch (kind) {
		case 'microphone':
			return MACOS_MICROPHONE_PERMISSION_NAME;
		case 'camera':
			return MACOS_CAMERA_PERMISSION_NAME;
		case 'screen':
			return MACOS_SCREEN_RECORDING_PERMISSION_NAME;
		case 'input-monitoring':
			return MACOS_INPUT_MONITORING_PERMISSION_NAME;
	}
};

const descriptionDescriptorForKind = (kind: MacPermissionKind) => {
	switch (kind) {
		case 'microphone':
			return MICROPHONE_DESCRIPTION_DESCRIPTOR;
		case 'camera':
			return CAMERA_DESCRIPTION_DESCRIPTOR;
		case 'screen':
			return SCREEN_DESCRIPTION_DESCRIPTOR;
		case 'input-monitoring':
			return INPUT_MONITORING_DESCRIPTION_DESCRIPTOR;
	}
};

const statusDescriptorForStatus = (status: NativePermissionResult) => {
	switch (status) {
		case 'granted':
			return STATUS_GRANTED_DESCRIPTOR;
		case 'denied':
			return STATUS_DENIED_DESCRIPTOR;
		case 'not-determined':
			return STATUS_NOT_DETERMINED_DESCRIPTOR;
		case 'unsupported':
			return STATUS_UNSUPPORTED_DESCRIPTOR;
	}
};

const statusChipClass = (status: NativePermissionResult): string | undefined => {
	if (status === 'granted') return styles.chipGranted;
	if (status === 'denied') return styles.chipDenied;
	if (status === 'not-determined') return styles.chipNotDetermined;
	if (status === 'unsupported') return styles.chipUnsupported;
	return undefined;
};

const reapplyGlobalShortcuts = async (): Promise<void> => {
	const module = await import('@app/features/app/keybindings/KeybindManager');
	await module.default.reapplyGlobalShortcuts();
};

export const MacPermissionsModal: React.FC<MacPermissionsModalProps> = observer(({focus}) => {
	const {i18n} = useLingui();
	const [busyKind, setBusyKind] = useState<MacPermissionKind | null>(null);
	const closedRef = useRef(false);
	const rowRefs = useRef<Partial<Record<MacPermissionKind, HTMLDivElement | null>>>({});
	const initialFocusRef = useRef<HTMLButtonElement | null>(null);
	const focusedStatus = focus ? MacPermissions.statuses[focus] : null;
	const shouldUseInitialFocusRef = focusedStatus === 'not-determined' || focusedStatus === 'denied';
	const kinds = MAC_PERMISSION_KINDS;
	useEffect(() => {
		void MacPermissions.refreshAll();
	}, []);
	useEffect(() => {
		const handleFocus = () => {
			void MacPermissions.refreshAll();
		};
		window.addEventListener('focus', handleFocus);
		return () => window.removeEventListener('focus', handleFocus);
	}, []);
	useEffect(() => {
		if (!focus) return;
		rowRefs.current[focus]?.scrollIntoView({block: 'nearest'});
	}, [focus]);
	const close = useCallback(() => {
		if (closedRef.current) return;
		closedRef.current = true;
		MacPermissions.recordModalClosed(focus);
		ModalCommands.popWithKey('mac-permissions');
	}, [focus]);
	const requestPermission = useCallback(async (kind: MacPermissionKind) => {
		setBusyKind(kind);
		try {
			const result = await requestNativePermission(kind);
			MacPermissions.applyPermissionResult(kind, result);
			const refreshed = await MacPermissions.refreshKind(kind);
			if (kind === 'input-monitoring' && refreshed === 'granted') {
				await reapplyGlobalShortcuts();
			}
		} finally {
			setBusyKind(null);
		}
	}, []);
	const openSettings = useCallback(async (kind: MacPermissionKind) => {
		setBusyKind(kind);
		try {
			await openNativePermissionSettings(kind);
			await MacPermissions.refreshKind(kind);
		} finally {
			setBusyKind(null);
		}
	}, []);
	return (
		<Modal.Root
			size="medium"
			centered
			onClose={close}
			initialFocusRef={shouldUseInitialFocusRef ? initialFocusRef : undefined}
			data-flx="permissions.mac-permissions-modal.root"
		>
			<Modal.Header
				title={i18n._(TITLE_DESCRIPTOR)}
				onClose={close}
				data-flx="permissions.mac-permissions-modal.header"
			/>
			<Modal.Content data-flx="permissions.mac-permissions-modal.content">
				<Modal.ContentLayout data-flx="permissions.mac-permissions-modal.content-layout">
					<p className={styles.intro} data-flx="permissions.mac-permissions-modal.intro">
						{i18n._(INTRO_DESCRIPTOR, {productName: PRODUCT_NAME})}
					</p>
					<div className={styles.list} data-flx="permissions.mac-permissions-modal.list">
						{kinds.map((kind) => {
							const status = MacPermissions.statuses[kind];
							const isFocused = focus === kind;
							const hasAction = status === 'not-determined' || status === 'denied';
							return (
								<div
									key={kind}
									ref={(node) => {
										rowRefs.current[kind] = node;
									}}
									className={clsx(styles.row, isFocused && styles.rowFocused)}
									data-flx="permissions.mac-permissions-modal.row"
								>
									<div className={styles.text} data-flx="permissions.mac-permissions-modal.row-text">
										<div className={styles.name} data-flx="permissions.mac-permissions-modal.row-name">
											{labelForKind(kind)}
										</div>
										<p className={styles.description} data-flx="permissions.mac-permissions-modal.row-description">
											{i18n._(descriptionDescriptorForKind(kind))}
										</p>
										{MacPermissions.restartRequired[kind] && (
											<p className={styles.restart} data-flx="permissions.mac-permissions-modal.restart">
												{i18n._(RESTART_REQUIRED_DESCRIPTOR, {productName: PRODUCT_NAME})}
											</p>
										)}
									</div>
									<div className={styles.controls} data-flx="permissions.mac-permissions-modal.controls">
										<div
											className={clsx(styles.chip, statusChipClass(status))}
											data-flx="permissions.mac-permissions-modal.status"
										>
											{status === 'granted' && (
												<CheckIcon
													size={remFromPx(16)}
													weight="bold"
													aria-hidden="true"
													data-flx="permissions.system.mac-permissions-modal.check-icon"
												/>
											)}
											{i18n._(statusDescriptorForStatus(status))}
										</div>
										{hasAction && (
											<div className={styles.action} data-flx="permissions.mac-permissions-modal.action">
												{status === 'not-determined' ? (
													<Button
														ref={isFocused ? initialFocusRef : undefined}
														variant="primary"
														small={true}
														submitting={busyKind === kind}
														onClick={() => void requestPermission(kind)}
														data-flx="permissions.mac-permissions-modal.button.grant"
													>
														{i18n._(GRANT_DESCRIPTOR)}
													</Button>
												) : (
													<Button
														ref={isFocused ? initialFocusRef : undefined}
														variant="secondary"
														small={true}
														submitting={busyKind === kind}
														onClick={() => void openSettings(kind)}
														data-flx="permissions.mac-permissions-modal.button.settings"
													>
														{i18n._(OPEN_SETTINGS_DESCRIPTOR)}
													</Button>
												)}
											</div>
										)}
									</div>
								</div>
							);
						})}
					</div>
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {RELAUNCH_TO_APPLY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/permissions/system/components/MacPermissionsSettingsRow.module.css';
import {
	macPermissionNameDescriptor,
	PERMISSION_ALLOWED_DESCRIPTOR,
	QUIT_PROMPT_ADVICE_DESCRIPTOR,
	useMacPermissionControl,
} from '@app/features/permissions/system/components/useMacPermissionControl';
import MacPermissions, {type MacPermissionKind} from '@app/features/permissions/system/state/MacPermissions';
import {MAC_PERMISSION_KINDS} from '@app/features/permissions/system/utils/NativePermissions';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {relaunchDesktopApp} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {getNativePlatformSync, isDesktop} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CheckIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useId} from 'react';

const DESCRIPTION_DESCRIPTOR = msg({
	message: 'Review the permissions {productName} uses for voice, video, screen sharing, and shortcuts.',
	comment: 'Description of the macOS permissions section in user settings. {productName} is the app name.',
});
const NOT_ALLOWED_DESCRIPTOR = msg({
	message: 'Not allowed',
	comment: 'Screen reader status for a macOS permission that has not been granted.',
});

const MacPermissionSettingsLine: React.FC<{readonly kind: MacPermissionKind}> = observer(({kind}) => {
	const {i18n} = useLingui();
	const labelId = useId();
	const control = useMacPermissionControl(kind);
	const allowed = control.status === 'granted';
	return (
		<div
			className={styles.line}
			data-permission={kind}
			data-status={control.status}
			data-flx="permissions.mac-permissions-settings-row.line"
		>
			<span
				className={clsx(styles.dot, allowed && styles.dotAllowed, control.status === 'denied' && styles.dotDenied)}
				aria-hidden="true"
				data-flx="permissions.mac-permissions-settings-row.dot"
			/>
			<span id={labelId} className={styles.label} data-flx="permissions.mac-permissions-settings-row.label">
				{i18n._(macPermissionNameDescriptor(kind))}
			</span>
			{!allowed && (
				<span className={styles.screenReaderOnly} data-flx="permissions.mac-permissions-settings-row.status">
					{i18n._(NOT_ALLOWED_DESCRIPTOR)}
				</span>
			)}
			<div className={styles.trailing} aria-live="polite" data-flx="permissions.mac-permissions-settings-row.trailing">
				{control.relaunchMayHelp && (
					<Button
						variant="ghost"
						compact={true}
						fitContent={true}
						className={styles.action}
						aria-describedby={labelId}
						onClick={() => void relaunchDesktopApp()}
						data-flx="permissions.mac-permissions-settings-row.button.relaunch"
					>
						{i18n._(RELAUNCH_TO_APPLY_DESCRIPTOR)}
					</Button>
				)}
				{control.actionDescriptor ? (
					<Button
						variant="ghost"
						compact={true}
						fitContent={true}
						className={styles.action}
						submitting={control.busy}
						aria-describedby={labelId}
						onClick={control.runAction}
						data-flx="permissions.mac-permissions-settings-row.button.action"
					>
						{i18n._(control.actionDescriptor)}
					</Button>
				) : (
					allowed && (
						<span className={styles.allowed} data-flx="permissions.mac-permissions-settings-row.allowed">
							<CheckIcon
								size={remFromPx(14)}
								weight="bold"
								aria-hidden="true"
								data-flx="permissions.mac-permissions-settings-row.check-icon"
							/>
							<flx-i18n data-flx="permissions.mac-permissions-settings-row.flx-i18n">
								{i18n._(PERMISSION_ALLOWED_DESCRIPTOR)}
							</flx-i18n>
						</span>
					)
				)}
			</div>
		</div>
	);
});

export const MacPermissionsSettingsRow: React.FC<React.HTMLAttributes<HTMLDivElement>> = observer((props) => {
	const {i18n} = useLingui();
	if (!isDesktop() || getNativePlatformSync() !== 'macos') return null;
	return (
		<div className={styles.root} data-flx="permissions.mac-permissions-settings-row.root" {...props}>
			<p className={styles.description} data-flx="permissions.mac-permissions-settings-row.description">
				{i18n._(DESCRIPTION_DESCRIPTOR, {productName: PRODUCT_NAME})}
			</p>
			<div className={styles.lines} data-flx="permissions.mac-permissions-settings-row.lines">
				{MAC_PERMISSION_KINDS.map((kind) => (
					<MacPermissionSettingsLine
						key={kind}
						kind={kind}
						data-flx="permissions.mac-permissions-settings-row.mac-permission-settings-line"
					/>
				))}
			</div>
			<p
				className={styles.advice}
				aria-live="polite"
				data-flx="permissions.mac-permissions-settings-row.quit-prompt-advice"
			>
				{MacPermissions.anyQuitPromptAdvice && i18n._(QUIT_PROMPT_ADVICE_DESCRIPTOR, {productName: PRODUCT_NAME})}
			</p>
		</div>
	);
});

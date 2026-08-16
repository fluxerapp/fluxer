// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import KeybindManager from '@app/features/app/keybindings/KeybindManager';
import NativePermission from '@app/features/permissions/system/state/NativePermission';
import {getUserSettingsTabLabel} from '@app/features/user/components/settings_utils/SettingsConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const LINUX_INPUT_ACCESS_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Enable input access to use system-wide shortcuts on Wayland. After changing input access, fully quit and restart {productName} so shortcuts can use it.',
	comment: 'Description for a Linux Wayland input-access nagbar. {productName} is the app name.',
});
const LINUX_INPUT_ACCESS_RELOGIN_DESCRIPTOR = msg({
	message: 'Input access was changed. Fully quit and restart {productName} so system-wide shortcuts can use it.',
	comment:
		'Description for a Linux Wayland input-access nagbar after access was changed. {productName} is the app name.',
});
const LINUX_INPUT_ACCESS_ERROR_DESCRIPTOR = msg({
	message:
		'Input access could not be enabled. Try again from {settingsMenuName} settings, then fully quit and restart {productName}.',
	comment:
		'Description for a Linux Wayland input-access nagbar after automatic permission setup failed. {settingsMenuName} is the shared user settings tab label for shortcuts and {productName} is the app name.',
});
const LINUX_INPUT_ACCESS_ENABLE_DESCRIPTOR = msg({
	message: 'Enable',
	comment: 'Button label in the Linux Wayland input-access nagbar.',
});
const LINUX_INPUT_ACCESS_RECHECK_DESCRIPTOR = msg({
	message: 'Recheck',
	comment: 'Button label in the Linux Wayland input-access nagbar after the user changes OS permissions.',
});
export const LinuxInputAccessNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const [submitting, setSubmitting] = useState(false);
	const needsRelogin = NativePermission.linuxInputAccessGrantNeedsRelogin;
	const hasError = NativePermission.linuxInputAccessGrantError !== null;
	const shortcutsSettingsMenuName = getUserSettingsTabLabel(i18n, 'keybinds');
	const handleEnable = async () => {
		setSubmitting(true);
		try {
			await NativePermission.grantLinuxInputAccess();
			await KeybindManager.reapplyGlobalShortcuts();
		} finally {
			setSubmitting(false);
		}
	};
	const handleRecheck = async () => {
		setSubmitting(true);
		try {
			await NativePermission.recheckLinuxInputAccess();
			await KeybindManager.reapplyGlobalShortcuts();
		} finally {
			setSubmitting(false);
		}
	};
	const message = needsRelogin
		? i18n._(LINUX_INPUT_ACCESS_RELOGIN_DESCRIPTOR, {productName: PRODUCT_NAME})
		: hasError
			? i18n._(LINUX_INPUT_ACCESS_ERROR_DESCRIPTOR, {
					settingsMenuName: shortcutsSettingsMenuName,
					productName: PRODUCT_NAME,
				})
			: i18n._(LINUX_INPUT_ACCESS_DESCRIPTION_DESCRIPTOR, {productName: PRODUCT_NAME});
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={NativePermission.dismissLinuxInputAccessNagbar}
			data-flx="app.app-layout.nagbars.linux-input-access-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={NativePermission.dismissLinuxInputAccessNagbar}
				message={message}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={needsRelogin ? handleRecheck : handleEnable}
						submitting={submitting}
						data-flx="app.app-layout.nagbars.linux-input-access-nagbar.nagbar-button.enable"
					>
						{needsRelogin
							? i18n._(LINUX_INPUT_ACCESS_RECHECK_DESCRIPTOR)
							: i18n._(LINUX_INPUT_ACCESS_ENABLE_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.linux-input-access-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});

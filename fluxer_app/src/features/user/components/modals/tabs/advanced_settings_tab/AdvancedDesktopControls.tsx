// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {
	getCachedDesktopTroubleshootingSettings,
	getDesktopTroubleshootingSettings,
	setDesktopDisableHardwareAcceleration,
} from '@app/features/devtools/utils/DesktopTroubleshootingUtils';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {
	getCachedDesktopWindowBehaviorSettings,
	getDesktopWindowBehaviorPendingRestart,
	getDesktopWindowBehaviorSettings,
	relaunchDesktopApp,
	setDesktopWindowBehaviorSettings,
} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {getElectronAPI} from '@app/features/ui/utils/NativeUtils';
import type {DesktopTroubleshootingSettings, DesktopWindowBehaviorSettings} from '@app/types/electron.d';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useLayoutEffect, useState} from 'react';

const USE_NATIVE_TITLE_BAR_DESCRIPTOR = msg({
	message: 'Use native title bar',
	comment: 'Short label for an advanced desktop preference.',
});
const USE_HARDWARE_ACCELERATION_DESCRIPTOR = msg({
	message: 'Use hardware acceleration',
	comment: 'Short label for an advanced desktop troubleshooting preference.',
});
const ENABLE_HARDWARE_ACCELERATION_DESCRIPTOR = msg({
	message: 'Enable hardware acceleration?',
	comment: 'Confirmation prompt in advanced settings.',
});
const DISABLE_HARDWARE_ACCELERATION_DESCRIPTOR = msg({
	message: 'Disable hardware acceleration?',
	comment: 'Confirmation prompt in advanced settings.',
});
const RESTART_NOW_DESCRIPTOR = msg({
	message: 'Restart now',
	comment: 'Short confirmation button label in advanced settings.',
});
const RESTART_PRODUCT_DESCRIPTOR = msg({
	message: 'Restart {productName}?',
	comment: 'Confirmation prompt in advanced settings. Preserve {productName}; it is inserted by code.',
});
const LATER_DESCRIPTOR = msg({
	message: 'Later',
	comment: 'Short confirmation button label in advanced settings.',
});

function useDesktopWindowBehaviorSettings() {
	const cachedDesktopWindowBehavior = getCachedDesktopWindowBehaviorSettings();
	const [desktopWindowBehavior, setDesktopWindowBehavior] = useState<DesktopWindowBehaviorSettings | null>(
		cachedDesktopWindowBehavior,
	);
	const [desktopWindowBehaviorBusy, setDesktopWindowBehaviorBusy] = useState(cachedDesktopWindowBehavior === null);
	useLayoutEffect(() => {
		let mounted = true;
		if (getCachedDesktopWindowBehaviorSettings() !== null) {
			setDesktopWindowBehaviorBusy(false);
			return () => {
				mounted = false;
			};
		}
		const initDesktopWindowBehavior = async () => {
			const settings = await getDesktopWindowBehaviorSettings();
			if (!mounted) return;
			if (settings !== null) {
				setDesktopWindowBehavior(settings);
			}
			setDesktopWindowBehaviorBusy(false);
		};
		void initDesktopWindowBehavior();
		return () => {
			mounted = false;
		};
	}, []);
	const updateDesktopWindowBehavior = useCallback(async (settings: Partial<DesktopWindowBehaviorSettings>) => {
		setDesktopWindowBehaviorBusy(true);
		const nextSettings = await setDesktopWindowBehaviorSettings(settings);
		if (nextSettings !== null) {
			setDesktopWindowBehavior(nextSettings);
		}
		setDesktopWindowBehaviorBusy(false);
		return nextSettings;
	}, []);
	return {desktopWindowBehavior, desktopWindowBehaviorBusy, updateDesktopWindowBehavior};
}

function useDesktopTroubleshootingSettings() {
	const cachedDesktopTroubleshooting = getCachedDesktopTroubleshootingSettings();
	const [desktopTroubleshooting, setDesktopTroubleshooting] = useState<DesktopTroubleshootingSettings | null>(
		cachedDesktopTroubleshooting,
	);
	const [desktopTroubleshootingBusy, setDesktopTroubleshootingBusy] = useState(cachedDesktopTroubleshooting === null);
	useLayoutEffect(() => {
		let mounted = true;
		if (getCachedDesktopTroubleshootingSettings() !== null) {
			setDesktopTroubleshootingBusy(false);
			return () => {
				mounted = false;
			};
		}
		const initDesktopTroubleshooting = async () => {
			const settings = await getDesktopTroubleshootingSettings();
			if (!mounted) return;
			if (settings !== null) {
				setDesktopTroubleshooting(settings);
			}
			setDesktopTroubleshootingBusy(false);
		};
		void initDesktopTroubleshooting();
		return () => {
			mounted = false;
		};
	}, []);
	return {
		desktopTroubleshooting,
		desktopTroubleshootingBusy,
		setDesktopTroubleshooting,
		setDesktopTroubleshootingBusy,
	};
}

export const NativeTitleBarControl = observer(() => {
	const {i18n} = useLingui();
	const {desktopWindowBehavior, desktopWindowBehaviorBusy, updateDesktopWindowBehavior} =
		useDesktopWindowBehaviorSettings();
	const handleChange = useCallback(
		(value: boolean) => {
			void updateDesktopWindowBehavior({useNativeTitleBar: value}).then(async () => {
				const pending = await getDesktopWindowBehaviorPendingRestart();
				if (!pending) return;
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(RESTART_PRODUCT_DESCRIPTOR, {productName: PRODUCT_NAME})}
							description={<Trans>{PRODUCT_NAME} needs to restart for the title bar change to take effect.</Trans>}
							primaryText={i18n._(RESTART_NOW_DESCRIPTOR)}
							primaryVariant="primary"
							secondaryText={i18n._(LATER_DESCRIPTOR)}
							onPrimary={async () => {
								await relaunchDesktopApp();
							}}
							data-flx="user.advanced-settings-tab.native-title-bar.confirm-modal"
						/>
					)),
				);
			});
		},
		[i18n, updateDesktopWindowBehavior],
	);
	if (getElectronAPI()?.platform === 'darwin') return null;
	return (
		<Switch
			ariaLabel={i18n._(USE_NATIVE_TITLE_BAR_DESCRIPTOR)}
			value={desktopWindowBehavior?.useNativeTitleBar ?? false}
			disabled={desktopWindowBehaviorBusy || desktopWindowBehavior === null}
			onChange={handleChange}
			compact
			data-flx="user.advanced-settings-tab.switch.native-title-bar"
		/>
	);
});

export const HardwareAccelerationControl = observer(() => {
	const {i18n} = useLingui();
	const {desktopTroubleshooting, desktopTroubleshootingBusy, setDesktopTroubleshooting, setDesktopTroubleshootingBusy} =
		useDesktopTroubleshootingSettings();
	const handleChange = useCallback(
		(value: boolean) => {
			const desiredDisableValue = !value;
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={
							value ? i18n._(ENABLE_HARDWARE_ACCELERATION_DESCRIPTOR) : i18n._(DISABLE_HARDWARE_ACCELERATION_DESCRIPTOR)
						}
						description={
							value ? (
								<Trans>{PRODUCT_NAME} needs to restart for hardware acceleration to take effect.</Trans>
							) : (
								<Trans>
									{PRODUCT_NAME} needs to restart to disable Chromium's hardware acceleration. Use this only if you're
									troubleshooting graphics glitches or high GPU usage.
								</Trans>
							)
						}
						primaryText={i18n._(RESTART_NOW_DESCRIPTOR)}
						primaryVariant="primary"
						secondaryText={i18n._(CANCEL_DESCRIPTOR)}
						onPrimary={async () => {
							setDesktopTroubleshootingBusy(true);
							const next = await setDesktopDisableHardwareAcceleration(desiredDisableValue, {restart: true});
							if (next) {
								setDesktopTroubleshooting(next);
							}
							setDesktopTroubleshootingBusy(false);
						}}
						data-flx="user.advanced-settings-tab.hardware-acceleration.confirm-modal"
					/>
				)),
			);
		},
		[i18n, setDesktopTroubleshooting, setDesktopTroubleshootingBusy],
	);
	if (getElectronAPI()?.platform === 'darwin') return null;
	return (
		<Switch
			ariaLabel={i18n._(USE_HARDWARE_ACCELERATION_DESCRIPTOR)}
			value={!(desktopTroubleshooting?.disableHardwareAcceleration ?? false)}
			disabled={desktopTroubleshootingBusy || desktopTroubleshooting === null}
			onChange={handleChange}
			compact
			data-flx="user.advanced-settings-tab.switch.hardware-acceleration"
		/>
	);
});

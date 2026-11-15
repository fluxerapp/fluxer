// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	getCachedDesktopTroubleshootingSettings,
	getDesktopTroubleshootingSettings,
	setDesktopDisableHardwareAcceleration,
	setDesktopDisableModuleUpdates,
} from '@app/features/devtools/utils/DesktopTroubleshootingUtils';
import {CANCEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import SourceMaps from '@app/features/platform/state/SourceMaps';
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
import {
	DISABLE_AUTOMATIC_UPDATES_DESCRIPTOR,
	DOWNLOAD_SOURCE_MAPS_DESCRIPTOR,
	LATER_DESCRIPTOR,
	RESTART_NOW_DESCRIPTOR,
	RESTART_DESCRIPTOR as RESTART_PRODUCT_DESCRIPTOR,
	USE_NATIVE_TITLE_BAR_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import type {DesktopTroubleshootingSettings, DesktopWindowBehaviorSettings} from '@app/types/electron.d';
import {msg, ph} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useLayoutEffect, useState} from 'react';

export const SourceMapsControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(DOWNLOAD_SOURCE_MAPS_DESCRIPTOR)}
			value={SourceMaps.enabled}
			onChange={(value) => {
				void SourceMaps.setEnabled(value);
			}}
			disabled={SourceMaps.downloading}
			compact
			data-flx="user.desktop-settings-tab.switch.source-maps"
		/>
	);
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
const ENABLE_AUTOMATIC_UPDATES_PROMPT_DESCRIPTOR = msg({
	message: 'Enable automatic updates?',
	comment: 'Confirmation prompt in advanced settings.',
});
const DISABLE_AUTOMATIC_UPDATES_PROMPT_DESCRIPTOR = msg({
	message: 'Disable automatic updates?',
	comment: 'Confirmation prompt in advanced settings.',
});

export function useDesktopWindowBehaviorSettings() {
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
							title={i18n._(RESTART_PRODUCT_DESCRIPTOR, {productName: RuntimeConfig.productName})}
							description={
								<Trans>
									{ph({PRODUCT_NAME: RuntimeConfig.productName})} needs to restart for the title bar change to take
									effect.
								</Trans>
							}
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
								<Trans>
									{ph({PRODUCT_NAME: RuntimeConfig.productName})} needs to restart for hardware acceleration to take
									effect.
								</Trans>
							) : (
								<Trans>
									{ph({PRODUCT_NAME: RuntimeConfig.productName})} needs to restart to disable Chromium's hardware
									acceleration. Use this only if you're troubleshooting graphics glitches or high GPU usage.
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

export const AutomaticUpdatesControl = observer(() => {
	const {i18n} = useLingui();
	const {desktopTroubleshooting, desktopTroubleshootingBusy, setDesktopTroubleshooting, setDesktopTroubleshootingBusy} =
		useDesktopTroubleshootingSettings();
	const handleChange = useCallback(
		(value: boolean) => {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={
							value
								? i18n._(DISABLE_AUTOMATIC_UPDATES_PROMPT_DESCRIPTOR)
								: i18n._(ENABLE_AUTOMATIC_UPDATES_PROMPT_DESCRIPTOR)
						}
						description={
							value ? (
								<Trans>
									{ph({PRODUCT_NAME: RuntimeConfig.productName})} needs to restart to stop updating while starting. When
									an update is available, you can install it from the app whenever you like.
								</Trans>
							) : (
								<Trans>
									{ph({PRODUCT_NAME: RuntimeConfig.productName})} needs to restart to install updates automatically
									again.
								</Trans>
							)
						}
						primaryText={i18n._(RESTART_NOW_DESCRIPTOR)}
						primaryVariant="primary"
						secondaryText={i18n._(CANCEL_DESCRIPTOR)}
						onPrimary={async () => {
							setDesktopTroubleshootingBusy(true);
							const next = await setDesktopDisableModuleUpdates(value);
							if (next) {
								setDesktopTroubleshooting(next);
							}
							setDesktopTroubleshootingBusy(false);
						}}
						data-flx="user.advanced-settings-tab.automatic-updates.confirm-modal"
					/>
				)),
			);
		},
		[i18n, setDesktopTroubleshooting, setDesktopTroubleshootingBusy],
	);
	return (
		<Switch
			ariaLabel={i18n._(DISABLE_AUTOMATIC_UPDATES_DESCRIPTOR)}
			value={desktopTroubleshooting?.disableModuleUpdates ?? false}
			disabled={desktopTroubleshootingBusy || desktopTroubleshooting === null}
			onChange={handleChange}
			compact
			data-flx="user.advanced-settings-tab.switch.automatic-updates"
		/>
	);
});

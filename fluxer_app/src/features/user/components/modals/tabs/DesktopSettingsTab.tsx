// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {SettingsTabContainer} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {MacPermissionsSettingsRow} from '@app/features/permissions/system/components/MacPermissionsSettingsRow';
import {
	getAutostartStatus,
	getCachedAutostartStatus,
	setAutostartEnabled,
} from '@app/features/platform/utils/Autostart';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {
	getCachedDesktopWindowBehaviorSettings,
	getDesktopWindowBehaviorPendingRestart,
	getDesktopWindowBehaviorSettings,
	relaunchDesktopApp,
	setDesktopWindowBehaviorSettings,
} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {
	getNativePlatformSync,
	isDesktop,
	isNativeLinux,
	isNativeMacOS,
	isNativeWindows,
} from '@app/features/ui/utils/NativeUtils';
import {
	LATER_DESCRIPTOR,
	LAUNCH_AT_LOGIN_DESCRIPTOR as LAUNCH_PRODUCT_AT_LOGIN_DESCRIPTOR,
	RESTART_DESCRIPTOR,
	RESTART_NOW_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import type {DesktopWindowBehaviorSettings, MonochromeTrayIconTone} from '@app/types/electron.d';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useLayoutEffect, useState} from 'react';

const RESTART_PRODUCT_FOR_TRAY_CHANGE_DESCRIPTOR = msg({
	message:
		"Restart {productName} for this change to take effect because Linux desktops can't reliably add or remove tray icons at runtime.",
	comment: 'Desktop setting description shown when a tray-icon change on Linux requires restart.',
});

const USE_COLORED_TRAY_ICON_DESCRIPTOR = msg({
	message: 'Use colored tray icon',
	comment:
		'Desktop setting toggle label on Windows and Linux. Switches the system tray icon from monochrome to the full-color app icon.',
});

const USE_COLORED_TRAY_ICON_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Show the full-color {productName} icon instead of a monochrome one that matches your taskbar.',
	comment: 'Desktop setting description under the colored tray icon toggle on Windows and Linux.',
});

const USE_DARK_TASKBAR_BADGE_DESCRIPTOR = msg({
	message: 'Use dark taskbar badge',
	comment:
		'Desktop setting toggle label on Windows. Switches the unread badge drawn over the taskbar button from red to dark gray.',
});

const USE_DARK_TASKBAR_BADGE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Draw the unread badge on the {productName} taskbar button in dark gray instead of red.',
	comment: 'Desktop setting description under the dark taskbar badge toggle on Windows.',
});

const SHOW_UNREAD_LAUNCHER_BADGE_DESCRIPTOR = msg({
	message: 'Badge the app icon for unread messages',
	comment:
		'Desktop setting toggle label on Linux. Controls whether unread messages without mentions put a badge on the app icon in the taskbar or dock.',
});

const SHOW_UNREAD_LAUNCHER_BADGE_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Linux taskbars can only show a number, so unread messages without mentions show as 1. Mentions always show their count.',
	comment: 'Desktop setting description under the Linux unread app icon badge toggle.',
});

const TRAY_ICON_COLOR_DESCRIPTOR = msg({
	message: 'Tray icon color',
	comment:
		'Desktop settings section title on Windows and Linux for choosing the color of the monochrome system tray icon.',
});

const TRAY_ICON_COLOR_AUTO_DESCRIPTOR = msg({
	message: 'Match the taskbar',
	comment: 'Tray icon color option. The app picks white or black from the taskbar or panel color.',
});

const TRAY_ICON_COLOR_AUTO_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Pick white or black from the color of your taskbar or panel.',
	comment: 'Description under the automatic tray icon color option.',
});

const TRAY_ICON_COLOR_WHITE_DESCRIPTOR = msg({
	message: 'White',
	comment: 'Tray icon color option for a white monochrome tray icon, for dark taskbars and panels.',
});

const TRAY_ICON_COLOR_BLACK_DESCRIPTOR = msg({
	message: 'Black',
	comment: 'Tray icon color option for a black monochrome tray icon, for light taskbars and panels.',
});

const TRAY_ICON_COLOR_COLORED_NOTICE_DESCRIPTOR = msg({
	message: 'Turn off the colored tray icon to choose a color here.',
	comment: 'Section description shown when the colored tray icon is on, so the monochrome color choice has no effect.',
});

const DesktopSettingsTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const cachedAutostart = getCachedAutostartStatus();
	const cachedWindowBehavior = getCachedDesktopWindowBehaviorSettings();
	const [autostartEnabled, setAutostartEnabledState] = useState(cachedAutostart ?? false);
	const [autostartBusy, setAutostartBusy] = useState(cachedAutostart === null);
	const [desktopWindowBehavior, setDesktopWindowBehaviorState] = useState<DesktopWindowBehaviorSettings | null>(
		cachedWindowBehavior,
	);
	const [desktopWindowBehaviorBusy, setDesktopWindowBehaviorBusy] = useState(cachedWindowBehavior === null);
	const [trayChangePendingRestart, setTrayChangePendingRestart] = useState(false);
	const platform = getNativePlatformSync();
	const isMac = isNativeMacOS(platform);
	const isLinux = isNativeLinux(platform);
	const isWindows = isNativeWindows(platform);
	useLayoutEffect(() => {
		let mounted = true;
		const initDesktopSettings = async () => {
			if (!isDesktop()) {
				if (mounted) {
					setAutostartBusy(false);
					setDesktopWindowBehaviorBusy(false);
				}
				return;
			}
			const needsAutostart = getCachedAutostartStatus() === null;
			const needsWindowBehavior = getCachedDesktopWindowBehaviorSettings() === null;
			if (!needsAutostart && !needsWindowBehavior) return;
			const [enabled, windowBehavior] = await Promise.all([
				needsAutostart ? getAutostartStatus() : Promise.resolve(getCachedAutostartStatus()),
				needsWindowBehavior
					? getDesktopWindowBehaviorSettings()
					: Promise.resolve(getCachedDesktopWindowBehaviorSettings()),
			]);
			if (!mounted) return;
			if (enabled !== null) {
				setAutostartEnabledState(enabled);
			}
			if (windowBehavior !== null) {
				setDesktopWindowBehaviorState(windowBehavior);
			}
			setAutostartBusy(false);
			setDesktopWindowBehaviorBusy(false);
		};
		void initDesktopSettings();
		void getDesktopWindowBehaviorPendingRestart().then((pending) => {
			if (mounted) setTrayChangePendingRestart(pending);
		});
		return () => {
			mounted = false;
		};
	}, []);
	const handleAutostartChange = async (value: boolean) => {
		setAutostartBusy(true);
		const nextState = await setAutostartEnabled(value);
		if (nextState !== null) {
			setAutostartEnabledState(nextState);
		}
		setAutostartBusy(false);
	};
	const handleDesktopWindowBehaviorChange = async (settings: Partial<DesktopWindowBehaviorSettings>) => {
		setDesktopWindowBehaviorBusy(true);
		const nextSettings = await setDesktopWindowBehaviorSettings(settings);
		if (nextSettings !== null) {
			setDesktopWindowBehaviorState(nextSettings);
		}
		const pending = await getDesktopWindowBehaviorPendingRestart();
		setTrayChangePendingRestart(pending);
		setDesktopWindowBehaviorBusy(false);
		return {pending};
	};
	const handleShowTrayIconChange = (value: boolean) => {
		void handleDesktopWindowBehaviorChange({showTrayIcon: value}).then(({pending}) => {
			if (!pending || !isNativeLinux()) return;
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(RESTART_DESCRIPTOR, {productName: RuntimeConfig.productName})}
						description={
							<Trans>
								Tray icon changes only apply after a restart on Linux because the system tray protocol doesn't reliably
								let apps add or remove icons at runtime. Restart now to apply your change.
							</Trans>
						}
						primaryText={i18n._(RESTART_NOW_DESCRIPTOR)}
						primaryVariant="primary"
						secondaryText={i18n._(LATER_DESCRIPTOR)}
						onPrimary={async () => {
							await relaunchDesktopApp();
						}}
						data-flx="user.desktop-settings-tab.handle-show-tray-icon-change.confirm-modal"
					/>
				)),
			);
		});
	};
	const trayIconToneOptions: Array<RadioOption<MonochromeTrayIconTone>> = [
		{
			value: 'auto',
			name: i18n._(TRAY_ICON_COLOR_AUTO_DESCRIPTOR),
			desc: i18n._(TRAY_ICON_COLOR_AUTO_DESCRIPTION_DESCRIPTOR),
		},
		{value: 'white', name: i18n._(TRAY_ICON_COLOR_WHITE_DESCRIPTOR)},
		{value: 'black', name: i18n._(TRAY_ICON_COLOR_BLACK_DESCRIPTOR)},
	];
	return (
		<SettingsTabContainer data-flx="user.desktop-settings-tab.settings-tab-container">
			<SettingsSection
				id="desktop-window"
				title={<Trans>Desktop window</Trans>}
				data-flx="user.desktop-settings-tab.settings-tab-section"
			>
				{isDesktop() && (
					<Switch
						label={i18n._(LAUNCH_PRODUCT_AT_LOGIN_DESCRIPTOR, {productName: RuntimeConfig.productName})}
						value={autostartEnabled}
						disabled={autostartBusy}
						onChange={handleAutostartChange}
						data-flx="user.desktop-settings-tab.switch.autostart-change"
					/>
				)}
				{isDesktop() && desktopWindowBehavior?.startMinimized !== undefined && (
					<Switch
						label={<Trans>Start minimized</Trans>}
						description={
							isMac ? (
								<Trans>When launched at login, start in the menu bar instead of opening the window</Trans>
							) : (
								<Trans>When launched at login, start in the system tray instead of opening the window</Trans>
							)
						}
						value={desktopWindowBehavior.startMinimized}
						disabled={desktopWindowBehaviorBusy || !autostartEnabled || !desktopWindowBehavior.showTrayIcon}
						onChange={(value) => handleDesktopWindowBehaviorChange({startMinimized: value})}
						data-flx="user.desktop-settings-tab.switch.start-minimized-change"
					/>
				)}
				{isDesktop() && desktopWindowBehavior && (
					<>
						<Switch
							label={<Trans>Remember window size and position</Trans>}
							value={desktopWindowBehavior.rememberWindowState}
							disabled={desktopWindowBehaviorBusy}
							onChange={(value) => handleDesktopWindowBehaviorChange({rememberWindowState: value})}
							data-flx="user.desktop-settings-tab.switch.desktop-window-behavior-change"
						/>
						<Switch
							label={isMac ? <Trans>Show menu bar icon</Trans> : <Trans>Show system tray icon</Trans>}
							description={
								trayChangePendingRestart && isLinux
									? i18n._(RESTART_PRODUCT_FOR_TRAY_CHANGE_DESCRIPTOR, {productName: RuntimeConfig.productName})
									: undefined
							}
							value={desktopWindowBehavior.showTrayIcon}
							disabled={desktopWindowBehaviorBusy}
							onChange={handleShowTrayIconChange}
							data-flx="user.desktop-settings-tab.switch.show-tray-icon-change"
						/>
						{!isMac && desktopWindowBehavior.coloredTrayIcon !== undefined && (
							<Switch
								label={i18n._(USE_COLORED_TRAY_ICON_DESCRIPTOR)}
								description={i18n._(USE_COLORED_TRAY_ICON_DESCRIPTION_DESCRIPTOR, {
									productName: RuntimeConfig.productName,
								})}
								value={desktopWindowBehavior.coloredTrayIcon}
								disabled={desktopWindowBehaviorBusy || !desktopWindowBehavior.showTrayIcon}
								onChange={(value) => handleDesktopWindowBehaviorChange({coloredTrayIcon: value})}
								data-flx="user.desktop-settings-tab.switch.colored-tray-icon-change"
							/>
						)}
						{isLinux && desktopWindowBehavior.unreadLauncherBadge !== undefined && (
							<Switch
								label={i18n._(SHOW_UNREAD_LAUNCHER_BADGE_DESCRIPTOR)}
								description={i18n._(SHOW_UNREAD_LAUNCHER_BADGE_DESCRIPTION_DESCRIPTOR)}
								value={desktopWindowBehavior.unreadLauncherBadge}
								disabled={desktopWindowBehaviorBusy}
								onChange={(value) => handleDesktopWindowBehaviorChange({unreadLauncherBadge: value})}
								data-flx="user.desktop-settings-tab.switch.unread-launcher-badge-change"
							/>
						)}
						{isWindows && desktopWindowBehavior.darkTaskbarBadge !== undefined && (
							<Switch
								label={i18n._(USE_DARK_TASKBAR_BADGE_DESCRIPTOR)}
								description={i18n._(USE_DARK_TASKBAR_BADGE_DESCRIPTION_DESCRIPTOR, {
									productName: RuntimeConfig.productName,
								})}
								value={desktopWindowBehavior.darkTaskbarBadge}
								disabled={desktopWindowBehaviorBusy}
								onChange={(value) => handleDesktopWindowBehaviorChange({darkTaskbarBadge: value})}
								data-flx="user.desktop-settings-tab.switch.dark-taskbar-badge-change"
							/>
						)}
						<Switch
							label={isMac ? <Trans>Minimize to menu bar</Trans> : <Trans>Minimize to tray</Trans>}
							value={desktopWindowBehavior.minimizeToTray}
							disabled={desktopWindowBehaviorBusy || !desktopWindowBehavior.showTrayIcon}
							onChange={(value) => handleDesktopWindowBehaviorChange({minimizeToTray: value})}
							data-flx="user.desktop-settings-tab.switch.desktop-window-behavior-change--2"
						/>
						<Switch
							label={isMac ? <Trans>Close to menu bar</Trans> : <Trans>Close to tray</Trans>}
							value={desktopWindowBehavior.closeToTray}
							disabled={desktopWindowBehaviorBusy || !desktopWindowBehavior.showTrayIcon}
							onChange={(value) => handleDesktopWindowBehaviorChange({closeToTray: value})}
							data-flx="user.desktop-settings-tab.switch.desktop-window-behavior-change--3"
						/>
					</>
				)}
			</SettingsSection>
			{isDesktop() && !isMac && desktopWindowBehavior?.monochromeTrayIconTone !== undefined && (
				<SettingsSection
					id="tray-icon-color"
					title={i18n._(TRAY_ICON_COLOR_DESCRIPTOR)}
					description={
						desktopWindowBehavior.coloredTrayIcon ? i18n._(TRAY_ICON_COLOR_COLORED_NOTICE_DESCRIPTOR) : undefined
					}
					data-flx="user.desktop-settings-tab.tray-icon-color-section"
				>
					<RadioGroup
						options={trayIconToneOptions}
						value={desktopWindowBehavior.monochromeTrayIconTone}
						disabled={
							desktopWindowBehaviorBusy ||
							!desktopWindowBehavior.showTrayIcon ||
							desktopWindowBehavior.coloredTrayIcon === true
						}
						onChange={(value) => handleDesktopWindowBehaviorChange({monochromeTrayIconTone: value})}
						aria-label={i18n._(TRAY_ICON_COLOR_DESCRIPTOR)}
						data-flx="user.desktop-settings-tab.radio-group.tray-icon-color-change"
					/>
				</SettingsSection>
			)}
			{isMac && (
				<SettingsSection
					id="macos-permissions"
					title={<Trans>macOS permissions</Trans>}
					data-flx="user.desktop-settings-tab.macos-permissions-section"
				>
					<MacPermissionsSettingsRow data-flx="user.desktop-settings-tab.macos-permissions-row" />
				</SettingsSection>
			)}
		</SettingsTabContainer>
	);
});

export default DesktopSettingsTab;

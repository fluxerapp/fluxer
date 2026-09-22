// SPDX-License-Identifier: AGPL-3.0-or-later

import {setDesktopTroubleshootingSettings} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import {getLaunchDesktopTroubleshootingSettings} from '@electron/main/DesktopDebugInfo';
import {destroyDesktopTray} from '@electron/main/DesktopTray';
import {relaunchStableLaunchPath} from '@electron/main/LinuxLaunchPath';
import {t} from '@electron/main/MainI18n';
import {clearSavedWindowBounds, getMainWindow, setQuitting} from '@electron/main/Window';
import {app, BrowserWindow, dialog, Menu, type MenuItemConstructorOptions, session} from 'electron';

const logger = createChildLogger('Troubleshooting');

let relaunchRequested = false;

export function relaunchAndExit(): void {
	if (relaunchRequested) {
		logger.warn('Ignoring duplicate relaunch request');
		return;
	}
	relaunchRequested = true;
	setQuitting(true);
	destroyDesktopTray();
	relaunchStableLaunchPath();
	app.exit(0);
}

export function setHardwareAccelerationDisabled(disable: boolean): void {
	if (process.platform === 'darwin') {
		setDesktopTroubleshootingSettings({disableHardwareAcceleration: false});
		logger.info('Hardware acceleration setting ignored on macOS');
		return;
	}
	setDesktopTroubleshootingSettings({disableHardwareAcceleration: disable});
	logger.info('Hardware acceleration setting persisted', {disable});
}

export function setHardwareAccelerationDisabledAndRestart(disable: boolean): void {
	setHardwareAccelerationDisabled(disable);
	if (process.platform === 'darwin') {
		return;
	}
	logger.info('Restarting to apply hardware acceleration change', {disable});
	relaunchAndExit();
}

export function reloadMainWindow(): void {
	const mainWindow = getMainWindow();
	if (!mainWindow || mainWindow.isDestroyed()) {
		logger.warn('Reload requested but no main window is available');
		return;
	}
	logger.info('Reloading main window (ignoring cache)');
	mainWindow.webContents.reloadIgnoringCache();
}

export async function resetAppDataAndRestart(options?: {confirm?: boolean}): Promise<void> {
	const mainWindow = getMainWindow();
	if (options?.confirm !== false) {
		const choice = await dialog.showMessageBox(mainWindow ?? new BrowserWindow({show: false}), {
			type: 'warning',
			buttons: [t('desktop.troubleshooting.resetConfirm'), t('desktop.troubleshooting.resetCancel')],
			defaultId: 1,
			cancelId: 1,
			title: t('desktop.troubleshooting.resetTitle'),
			message: t('desktop.troubleshooting.resetMessage'),
			detail: t('desktop.troubleshooting.resetDetail'),
			noLink: true,
		});
		if (choice.response !== 0) {
			logger.info('App data reset cancelled by user');
			return;
		}
	}
	const partition = mainWindow?.webContents.session ?? session.defaultSession;
	try {
		await partition.clearStorageData();
		logger.info('Cleared session storage data');
	} catch (error) {
		logger.warn('Failed to clear session storage data', {error});
	}
	try {
		await partition.clearCache();
		logger.info('Cleared HTTP cache');
	} catch (error) {
		logger.warn('Failed to clear HTTP cache', {error});
	}
	try {
		await partition.clearCodeCaches({urls: []});
		logger.info('Cleared V8 code caches');
	} catch (error) {
		logger.warn('Failed to clear V8 code caches', {error});
	}
	try {
		await partition.clearAuthCache();
		logger.info('Cleared HTTP auth cache');
	} catch (error) {
		logger.warn('Failed to clear HTTP auth cache', {error});
	}
	try {
		clearSavedWindowBounds();
	} catch (error) {
		logger.warn('Failed to clear saved window bounds', {error});
	}
	logger.info('App data reset complete; restarting');
	relaunchAndExit();
}

export function buildTroubleshootingMenuItems(): Array<MenuItemConstructorOptions> {
	const hwAccelDisabled = getLaunchDesktopTroubleshootingSettings().disableHardwareAcceleration;
	const toggleHwAccelLabel = hwAccelDisabled
		? t('desktop.troubleshooting.enableHardwareAccelerationAndRestart')
		: t('desktop.troubleshooting.disableHardwareAccelerationAndRestart');
	return [
		{
			label: toggleHwAccelLabel,
			click: () => setHardwareAccelerationDisabledAndRestart(!hwAccelDisabled),
		},
		{
			label: t('desktop.troubleshooting.reload'),
			click: () => reloadMainWindow(),
		},
		{type: 'separator'},
		{
			label: t('desktop.troubleshooting.resetAppDataAndRestart'),
			click: () => {
				void resetAppDataAndRestart();
			},
		},
	];
}

function buildHelpPopupMenu(): Menu {
	const template: Array<MenuItemConstructorOptions> = [
		{
			label: t('desktop.appMenu.troubleshooting'),
			submenu: buildTroubleshootingMenuItems(),
		},
	];
	return Menu.buildFromTemplate(template);
}

export function popupHelpMenu(window: BrowserWindow | null): void {
	const menu = buildHelpPopupMenu();
	if (window && !window.isDestroyed()) {
		menu.popup({window});
	} else {
		menu.popup();
	}
}

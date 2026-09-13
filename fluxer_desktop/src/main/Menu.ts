// SPDX-License-Identifier: AGPL-3.0-or-later

import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import {onLocaleChange, t} from '@electron/main/MainI18n';
import {openExternalDeduped} from '@electron/main/OpenExternal';
import {buildTroubleshootingMenuItems} from '@electron/main/Troubleshooting';
import {getMainWindow, toggleWindowDevTools} from '@electron/main/Window';
import {type BaseWindow, BrowserWindow, Menu, type MenuItem, type MenuItemConstructorOptions} from 'electron';

const MACOS_HELP_MENU_TITLE_AUTODETECT_OPT_OUT = '\u200C';

function buildTemplate(): Array<MenuItemConstructorOptions> {
	const isCanary = BUILD_CHANNEL === 'canary';
	const appName = isCanary ? 'Fluxer Canary' : 'Fluxer';
	const isMac = process.platform === 'darwin';
	const template: Array<MenuItemConstructorOptions> = [];
	if (isMac) {
		template.push({
			label: appName,
			submenu: [
				{
					role: 'about',
					label: t('desktop.appMenu.about', {appName}),
				},
				{type: 'separator'},
				{
					label: t('desktop.appMenu.preferences'),
					accelerator: 'Cmd+,',
					click: () => {
						const mainWindow = getMainWindow();
						if (mainWindow) {
							mainWindow.webContents.send('open-settings');
						}
					},
				},
				{type: 'separator'},
				{role: 'services'},
				{type: 'separator'},
				{
					role: 'hide',
					label: t('desktop.appMenu.hide', {appName}),
				},
				{role: 'hideOthers'},
				{role: 'unhide'},
				{type: 'separator'},
				{
					role: 'quit',
					label: t('desktop.appMenu.quit', {appName}),
				},
			],
		});
	}
	template.push({
		label: t('desktop.appMenu.file'),
		submenu: isMac
			? [{role: 'close'}]
			: [
					{
						label: t('desktop.appMenu.preferencesPlain'),
						accelerator: 'Ctrl+,',
						click: () => {
							const mainWindow = getMainWindow();
							if (mainWindow) {
								mainWindow.webContents.send('open-settings');
							}
						},
					},
					{type: 'separator'},
					{
						role: 'quit',
						label: t('desktop.appMenu.exit'),
					},
				],
	});
	template.push({
		label: t('desktop.appMenu.edit'),
		submenu: [
			{role: 'undo', label: t('desktop.appMenu.undo')},
			{role: 'redo', label: t('desktop.appMenu.redo')},
			{type: 'separator'},
			{role: 'cut', label: t('desktop.appMenu.cut')},
			{role: 'copy', label: t('desktop.appMenu.copy')},
			{role: 'paste', label: t('desktop.appMenu.paste')},
			...(isMac
				? [
						{role: 'pasteAndMatchStyle' as const, label: t('desktop.appMenu.pasteAndMatchStyle')},
						{role: 'delete' as const, label: t('desktop.appMenu.delete')},
						{role: 'selectAll' as const, label: t('desktop.appMenu.selectAll')},
						{type: 'separator' as const},
						{
							label: t('desktop.appMenu.speech'),
							submenu: [
								{role: 'startSpeaking' as const, label: t('desktop.appMenu.startSpeaking')},
								{role: 'stopSpeaking' as const, label: t('desktop.appMenu.stopSpeaking')},
							],
						},
					]
				: [
						{role: 'delete' as const, label: t('desktop.appMenu.delete')},
						{type: 'separator' as const},
						{role: 'selectAll' as const, label: t('desktop.appMenu.selectAll')},
					]),
		],
	});
	const zoomInHandler = () => {
		const mainWindow = getMainWindow();
		if (mainWindow) {
			mainWindow.webContents.send('zoom-in');
		}
	};
	template.push({
		label: t('desktop.appMenu.view'),
		submenu: [
			{role: 'reload', label: t('desktop.appMenu.reload')},
			{role: 'forceReload', label: t('desktop.appMenu.forceReload')},
			{
				label: t('desktop.appMenu.toggleDeveloperTools'),
				accelerator: isMac ? 'Alt+Command+I' : 'Ctrl+Shift+I',
				click: (_menuItem, browserWindow) => {
					const targetWindow = browserWindow instanceof BrowserWindow ? browserWindow : getMainWindow();
					if (targetWindow) {
						toggleWindowDevTools(targetWindow);
					}
				},
			},
			{type: 'separator'},
			{
				label: t('desktop.appMenu.actualSize'),
				accelerator: 'CmdOrCtrl+0',
				click: () => {
					const mainWindow = getMainWindow();
					if (mainWindow) {
						mainWindow.webContents.send('zoom-reset');
					}
				},
			},
			{
				label: t('desktop.appMenu.zoomIn'),
				accelerator: 'CmdOrCtrl+Plus',
				click: zoomInHandler,
			},
			{
				label: t('desktop.appMenu.zoomIn'),
				accelerator: 'CmdOrCtrl+=',
				visible: false,
				click: zoomInHandler,
			},
			{
				label: t('desktop.appMenu.zoomOut'),
				accelerator: 'CmdOrCtrl+-',
				click: () => {
					const mainWindow = getMainWindow();
					if (mainWindow) {
						mainWindow.webContents.send('zoom-out');
					}
				},
			},
			{type: 'separator'},
			{role: 'togglefullscreen', label: t('desktop.appMenu.toggleFullScreen')},
			...(isMac
				? []
				: [
						{
							role: 'togglefullscreen' as const,
							label: t('desktop.appMenu.toggleFullScreen'),
							accelerator: 'Alt+Enter',
							visible: false,
						},
					]),
		],
	});
	template.push({
		label: t('desktop.appMenu.window'),
		submenu: [
			{role: 'minimize', label: t('desktop.appMenu.minimize')},
			{role: 'zoom', label: t('desktop.appMenu.zoomWindow')},
			...(isMac
				? [
						{type: 'separator' as const},
						{role: 'front' as const},
						{type: 'separator' as const},
						{role: 'window' as const},
					]
				: [
						{
							label: t('desktop.appMenu.close'),
							click: (_menuItem: MenuItem, browserWindow: BaseWindow | undefined) => {
								const targetWindow = browserWindow ?? getMainWindow();
								if (targetWindow && !targetWindow.isDestroyed()) {
									targetWindow.close();
								}
							},
						},
					]),
		],
	});
	template.push({
		label: isMac
			? `${t('desktop.appMenu.help')}${MACOS_HELP_MENU_TITLE_AUTODETECT_OPT_OUT}`
			: t('desktop.appMenu.help'),
		submenu: [
			{
				label: t('desktop.appMenu.website'),
				click: async () => {
					await openExternalDeduped('https://fluxer.app');
				},
			},
			{
				label: t('desktop.appMenu.github'),
				click: async () => {
					await openExternalDeduped('https://github.com/fluxerapp/fluxer');
				},
			},
			{type: 'separator'},
			{
				label: t('desktop.appMenu.reportIssue'),
				click: async () => {
					await openExternalDeduped('https://github.com/fluxerapp/fluxer/issues');
				},
			},
			{type: 'separator'},
			{
				label: t('desktop.appMenu.troubleshooting'),
				submenu: buildTroubleshootingMenuItems(),
			},
		],
	});
	return template;
}

let subscribed = false;

export function createApplicationMenu(): void {
	const menu = Menu.buildFromTemplate(buildTemplate());
	Menu.setApplicationMenu(menu);
	if (!subscribed) {
		subscribed = true;
		onLocaleChange(() => {
			Menu.setApplicationMenu(Menu.buildFromTemplate(buildTemplate()));
		});
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import {getDesktopWindowBehaviorSettings} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import type {
	TrayPresenceStatus as SharedTrayPresenceStatus,
	TrayActionPayload,
	TrayRuntimeStatePayload,
} from '@electron/common/Types';
import {getStableRelaunchOptions} from '@electron/main/LinuxLaunchPath';
import {onLocaleChange, t} from '@electron/main/MainI18n';
import {app, type BrowserWindow, clipboard, Menu, nativeImage, Tray} from 'electron';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const logger = createChildLogger('DesktopTray');
const isCanary = BUILD_CHANNEL === 'canary';
const APP_NAME = isCanary ? 'Fluxer Canary' : 'Fluxer';
const ICON_DIR_NAME = isCanary ? 'icons-canary' : 'icons-stable';
const TRAY_POSITION_GUID = isCanary ? '1a39981b-b4cc-46a4-8f7e-9fce187110f5' : '11c70c9f-a35d-4328-9040-f722dc5fa0a0';

interface DesktopTrayController {
	createWindow: () => BrowserWindow;
	getMainWindow: () => BrowserWindow | null;
	hideWindow: () => void;
	setQuitting: (quitting: boolean) => void;
	showWindow: () => void;
}

let controller: DesktopTrayController | null = null;
let tray: Tray | null = null;
let initialTrayDesired: boolean | null = null;
let linuxTrayCreatedInProcess = false;
let linuxTrayRecreateWarningLogged = false;
let trayRestartRequested = false;
let pendingTrayMenuRefresh: NodeJS.Timeout | null = null;
let pendingTrayActionFlush: NodeJS.Timeout | null = null;
let trayActionBridgeWebContentsId: number | null = null;
let trayForcedExitTimer: NodeJS.Timeout | null = null;
let pendingTrayActions: Array<TrayActionPayload> = [];

const MAX_PENDING_TRAY_ACTIONS = 16;
const LINUX_TRAY_MENU_ACTION_DELAY_MS = 25;
const TRAY_FORCED_EXIT_DELAY_MS = 5000;

type TrayPresenceStatus = SharedTrayPresenceStatus;

const trayState: TrayRuntimeStatePayload = {
	voiceConnected: false,
	voiceChannelLabel: null,
	selfMute: false,
	selfDeaf: false,
	presenceStatus: null,
	buildInfo: null,
};

export function updateTrayRuntimeState(update: Partial<TrayRuntimeStatePayload>, webContentsId?: number): void {
	if (webContentsId !== undefined) {
		trayActionBridgeWebContentsId = webContentsId;
	}
	Object.assign(trayState, update);
	refreshDesktopTrayMenu();
	schedulePendingTrayActionFlush();
}

function schedulePendingTrayActionFlush(): void {
	if (pendingTrayActionFlush) return;
	pendingTrayActionFlush = setTimeout(() => {
		pendingTrayActionFlush = null;
		flushPendingTrayActions();
	}, 0);
	pendingTrayActionFlush.unref?.();
}

function getLiveMainWindow(): BrowserWindow | null {
	const mainWindow = controller?.getMainWindow();
	if (!mainWindow || mainWindow.isDestroyed()) {
		return null;
	}
	return mainWindow;
}

function canSendTrayAction(mainWindow: BrowserWindow): boolean {
	const {webContents} = mainWindow;
	return (
		!webContents.isDestroyed() &&
		trayActionBridgeWebContentsId === webContents.id &&
		!webContents.isLoadingMainFrame() &&
		Boolean(webContents.getURL())
	);
}

function sendTrayAction(mainWindow: BrowserWindow, payload: TrayActionPayload): boolean {
	try {
		mainWindow.webContents.send('tray-action', payload);
		return true;
	} catch (error) {
		logger.warn('Failed to send tray action to renderer', {action: payload.action, error});
		return false;
	}
}

function queueTrayAction(payload: TrayActionPayload): void {
	if (payload.action === 'set-status') {
		pendingTrayActions = pendingTrayActions.filter((action) => action.action !== 'set-status');
	}
	pendingTrayActions.push(payload);
	if (pendingTrayActions.length > MAX_PENDING_TRAY_ACTIONS) {
		const dropped = pendingTrayActions.shift();
		logger.warn('Dropped oldest pending tray action because the renderer bridge is not ready', {
			action: dropped?.action,
			limit: MAX_PENDING_TRAY_ACTIONS,
		});
	}
	const mainWindow = getLiveMainWindow();
	if (!mainWindow) {
		ensureMainWindowVisible();
		return;
	}
	mainWindow.webContents.once('did-finish-load', schedulePendingTrayActionFlush);
}

function flushPendingTrayActions(): void {
	if (pendingTrayActions.length === 0) return;
	const mainWindow = getLiveMainWindow();
	if (!mainWindow || !canSendTrayAction(mainWindow)) return;
	const actions = pendingTrayActions;
	pendingTrayActions = [];
	for (let index = 0; index < actions.length; index += 1) {
		const action = actions[index];
		if (!sendTrayAction(mainWindow, action)) {
			pendingTrayActions.unshift(...actions.slice(index));
			return;
		}
	}
}

function dispatchTrayAction(payload: TrayActionPayload): void {
	const mainWindow = getLiveMainWindow();
	if (!mainWindow || !canSendTrayAction(mainWindow)) {
		logger.debug('Queueing tray action until the renderer bridge is ready', {action: payload.action});
		queueTrayAction(payload);
		return;
	}
	if (!sendTrayAction(mainWindow, payload)) {
		queueTrayAction(payload);
	}
}

export function hasActiveDesktopTray(): boolean {
	return Boolean(tray && !tray.isDestroyed());
}

function getCandidateIconDirs(): Array<string> {
	const candidates = [
		path.join(process.resourcesPath, 'icons'),
		path.join(process.resourcesPath, ICON_DIR_NAME),
		process.resourcesPath,
		path.join(app.getAppPath(), 'build_resources', ICON_DIR_NAME),
		path.resolve(__dirname, '../../build_resources', ICON_DIR_NAME),
		path.resolve(process.cwd(), 'build_resources', ICON_DIR_NAME),
		path.resolve(process.cwd(), 'fluxer_desktop', 'build_resources', ICON_DIR_NAME),
		path.dirname(app.getPath('exe')),
	];
	return [...new Set(candidates)];
}

function createTrayImage(
	fileNames: Array<string>,
	options?: {
		resize?: {
			width: number;
			height: number;
		};
		template?: boolean;
	},
): Electron.NativeImage | null {
	for (const dir of getCandidateIconDirs()) {
		for (const fileName of fileNames) {
			const candidate = path.join(dir, fileName);
			if (!fs.existsSync(candidate)) {
				continue;
			}
			let image = nativeImage.createFromPath(candidate);
			if (image.isEmpty()) {
				logger.warn('Tray icon candidate could not be decoded', {candidate});
				continue;
			}
			if (options?.resize) {
				image = image.resize(options.resize);
			}
			if (options?.template) {
				image.setTemplateImage(true);
			}
			return image;
		}
	}
	return null;
}

function createTrayIcon(): Electron.NativeImage | null {
	if (process.platform === 'win32') {
		return (
			createTrayImage(['16x16.png'], {resize: {width: 16, height: 16}}) ??
			createTrayImage(['32x32.png'], {resize: {width: 16, height: 16}}) ??
			createTrayImage(['icon.ico', 'icon.png'])
		);
	}
	if (process.platform === 'darwin') {
		return (
			createTrayImage(['FluxerTrayTemplate.png'], {
				resize: {width: 16, height: 16},
				template: true,
			}) ??
			createTrayImage(['32x32.png', '16x16.png', '24x24.png', 'icon.png'], {
				resize: {width: 16, height: 16},
				template: true,
			})
		);
	}
	return createTrayImage(['256x256.png', '512x512.png', '128x128.png', '64x64.png', '48x48.png', 'icon.png']);
}

function ensureMainWindowVisible(): void {
	if (!controller) return;
	const mainWindow = controller.getMainWindow();
	if (!mainWindow || mainWindow.isDestroyed()) {
		controller.createWindow();
	}
	controller.showWindow();
	refreshDesktopTrayMenu();
}

function toggleMainWindow(): void {
	if (!controller) return;
	const mainWindow = controller.getMainWindow();
	if (mainWindow && !mainWindow.isDestroyed() && mainWindow.isVisible() && !mainWindow.isMinimized()) {
		controller.hideWindow();
	} else {
		ensureMainWindowVisible();
	}
	refreshDesktopTrayMenu();
}

function armTrayForcedExit(reason: 'quit' | 'restart'): void {
	if (trayForcedExitTimer) return;
	trayForcedExitTimer = setTimeout(() => {
		logger.warn('Tray shutdown did not complete before the forced-exit timeout', {
			reason,
			timeoutMs: TRAY_FORCED_EXIT_DELAY_MS,
		});
		app.exit(0);
	}, TRAY_FORCED_EXIT_DELAY_MS);
	trayForcedExitTimer.unref?.();
}

function quitFromTray(): void {
	if (!controller) {
		destroyDesktopTray();
		app.quit();
		armTrayForcedExit('quit');
		return;
	}
	controller.setQuitting(true);
	destroyDesktopTray();
	app.quit();
	armTrayForcedExit('quit');
}

function restartFromTray(): void {
	if (trayRestartRequested) return;
	trayRestartRequested = true;
	controller?.setQuitting(true);
	destroyDesktopTray();
	app.relaunch(getStableRelaunchOptions());
	app.quit();
	armTrayForcedExit('restart');
}

function runTrayMenuAction(action: () => void): void {
	const run = (): void => {
		try {
			action();
		} catch (error) {
			logger.error('Tray menu action failed', {error});
		}
	};
	if (process.platform !== 'linux') {
		run();
		return;
	}
	const timeout = setTimeout(run, LINUX_TRAY_MENU_ACTION_DELAY_MS);
	timeout.unref?.();
}

function buildTrayMenu(): Menu {
	const mainWindow = controller?.getMainWindow() ?? null;
	const visible = Boolean(
		mainWindow && !mainWindow.isDestroyed() && mainWindow.isVisible() && !mainWindow.isMinimized(),
	);
	const statusItem = (label: string, status: TrayPresenceStatus): Electron.MenuItemConstructorOptions => ({
		label,
		type: 'radio',
		checked: trayState.presenceStatus === status,
		click: () => {
			runTrayMenuAction(() => {
				trayState.presenceStatus = status;
				refreshDesktopTrayMenu();
				dispatchTrayAction({action: 'set-status', status});
			});
		},
	});
	const menuTemplate: Array<Electron.MenuItemConstructorOptions> = [
		{
			label: visible ? t('desktop.tray.hide', {appName: APP_NAME}) : t('desktop.tray.show', {appName: APP_NAME}),
			click: () => {
				runTrayMenuAction(() => {
					if (visible) {
						controller?.hideWindow();
						refreshDesktopTrayMenu();
					} else {
						ensureMainWindowVisible();
					}
				});
			},
		},
		{
			label: t('desktop.tray.openSettings'),
			click: () => {
				runTrayMenuAction(() => {
					ensureMainWindowVisible();
					const mainWindow = controller?.getMainWindow();
					if (!mainWindow || mainWindow.isDestroyed()) return;
					const {webContents} = mainWindow;
					if (webContents.isLoading() || !webContents.getURL()) {
						webContents.once('did-finish-load', () => {
							if (!mainWindow.isDestroyed()) webContents.send('open-settings');
						});
					} else {
						webContents.send('open-settings');
					}
				});
			},
		},
		{type: 'separator'},
		{
			label: t('desktop.tray.status'),
			submenu: [
				statusItem(t('desktop.tray.statusOnline'), 'online'),
				statusItem(t('desktop.tray.statusIdle'), 'idle'),
				statusItem(t('desktop.tray.statusDnd'), 'dnd'),
				statusItem(t('desktop.tray.statusInvisible'), 'invisible'),
			],
		},
	];
	if (trayState.voiceConnected) {
		menuTemplate.push({type: 'separator'});
		menuTemplate.push({
			label: t(trayState.selfMute ? 'desktop.tray.unmuteMic' : 'desktop.tray.muteMic'),
			click: () => runTrayMenuAction(() => dispatchTrayAction({action: 'toggle-mute'})),
		});
		menuTemplate.push({
			label: t(trayState.selfDeaf ? 'desktop.tray.undeafen' : 'desktop.tray.deafen'),
			click: () => runTrayMenuAction(() => dispatchTrayAction({action: 'toggle-deafen'})),
		});
		menuTemplate.push({
			label: trayState.voiceChannelLabel
				? t('desktop.tray.disconnectFrom', {channel: trayState.voiceChannelLabel})
				: t('desktop.tray.disconnectVoice'),
			click: () => runTrayMenuAction(() => dispatchTrayAction({action: 'disconnect-voice'})),
		});
	}
	menuTemplate.push({type: 'separator'});
	menuTemplate.push({
		label: t('desktop.tray.checkForUpdates'),
		click: () => runTrayMenuAction(() => dispatchTrayAction({action: 'check-for-updates'})),
	});
	if (trayState.buildInfo) {
		menuTemplate.push({
			label: t('desktop.tray.copyBuildInfo'),
			click: () => {
				runTrayMenuAction(() => {
					void clipboard.writeText(trayState.buildInfo ?? '').catch((error) => {
						logger.error('Failed to copy build info to the clipboard', {error});
					});
				});
			},
		});
	}
	menuTemplate.push({type: 'separator'});
	menuTemplate.push({
		label: t('desktop.tray.restart', {appName: APP_NAME}),
		click: () => runTrayMenuAction(restartFromTray),
	});
	menuTemplate.push({
		label: t('desktop.tray.quit', {appName: APP_NAME}),
		click: () => runTrayMenuAction(quitFromTray),
	});
	return Menu.buildFromTemplate(menuTemplate);
}

function createTray(): Tray | null {
	const trayIcon = createTrayIcon();
	if (!trayIcon) {
		logger.warn('Tray icon asset could not be resolved; tray disabled');
		return null;
	}
	try {
		const nextTray = process.platform === 'darwin' ? new Tray(trayIcon, TRAY_POSITION_GUID) : new Tray(trayIcon);
		nextTray.setToolTip(APP_NAME);
		nextTray.setContextMenu(buildTrayMenu());
		if (process.platform !== 'darwin') {
			nextTray.on('click', toggleMainWindow);
		}
		nextTray.on('double-click', ensureMainWindowVisible);
		if (process.platform === 'linux') {
			linuxTrayCreatedInProcess = true;
			linuxTrayRecreateWarningLogged = false;
		}
		return nextTray;
	} catch (error) {
		logger.error('Failed to create tray icon:', error);
		return null;
	}
}

function canCreateTray(lifecycleLocked: boolean): boolean {
	if (lifecycleLocked) return false;
	if (process.platform !== 'linux' || !linuxTrayCreatedInProcess) return true;
	if (!linuxTrayRecreateWarningLogged) {
		linuxTrayRecreateWarningLogged = true;
		logger.warn('Suppressing Linux tray recreation after a tray was already created in this process');
	}
	return false;
}

function setCurrentTrayContextMenu(): void {
	if (!tray || tray.isDestroyed()) return;
	try {
		tray.setContextMenu(buildTrayMenu());
	} catch (error) {
		logger.warn('Failed to refresh desktop tray context menu', {error});
	}
}

export function refreshDesktopTrayMenu(): void {
	if (!tray || tray.isDestroyed()) return;
	if (process.platform !== 'linux') {
		setCurrentTrayContextMenu();
		return;
	}
	if (pendingTrayMenuRefresh) return;
	pendingTrayMenuRefresh = setTimeout(() => {
		pendingTrayMenuRefresh = null;
		setCurrentTrayContextMenu();
	}, 0);
	pendingTrayMenuRefresh.unref?.();
}

function revealHiddenWindowIfTrayUnavailable(): void {
	if (hasActiveDesktopTray() || !controller) return;
	const mainWindow = controller.getMainWindow();
	if (mainWindow && !mainWindow.isDestroyed() && !mainWindow.isVisible()) {
		controller.showWindow();
	}
}

export function applyDesktopWindowBehaviorSettings(): void {
	const settings = getDesktopWindowBehaviorSettings();
	if (initialTrayDesired === null) {
		initialTrayDesired = settings.showTrayIcon;
	}
	const lifecycleLocked = process.platform === 'linux' && settings.showTrayIcon !== initialTrayDesired;
	if (!settings.showTrayIcon) {
		if (!lifecycleLocked) {
			if (tray && !tray.isDestroyed()) {
				tray.destroy();
			}
			tray = null;
		}
		revealHiddenWindowIfTrayUnavailable();
		return;
	}
	if (!tray || tray.isDestroyed()) {
		if (canCreateTray(lifecycleLocked)) {
			tray = createTray();
		}
	} else {
		const nextTrayIcon = createTrayIcon();
		if (nextTrayIcon) {
			tray.setImage(nextTrayIcon);
		} else {
			logger.warn('Tray icon asset could not be reloaded; keeping existing tray image');
		}
		tray.setToolTip(APP_NAME);
	}
	refreshDesktopTrayMenu();
	revealHiddenWindowIfTrayUnavailable();
}

export function desktopTrayChangePendingRestart(): boolean {
	if (process.platform !== 'linux' || initialTrayDesired === null) return false;
	return getDesktopWindowBehaviorSettings().showTrayIcon !== initialTrayDesired;
}

let trayLocaleSubscribed = false;

export function initializeDesktopTray(nextController: DesktopTrayController): void {
	controller = nextController;
	applyDesktopWindowBehaviorSettings();
	if (!trayLocaleSubscribed) {
		trayLocaleSubscribed = true;
		onLocaleChange(() => {
			if (tray && !tray.isDestroyed()) {
				tray.setToolTip(APP_NAME);
			}
			refreshDesktopTrayMenu();
		});
	}
}

export function destroyDesktopTray(): void {
	const existingTray = tray;
	tray = null;
	controller = null;
	trayActionBridgeWebContentsId = null;
	pendingTrayActions = [];
	if (pendingTrayMenuRefresh) {
		clearTimeout(pendingTrayMenuRefresh);
		pendingTrayMenuRefresh = null;
	}
	if (pendingTrayActionFlush) {
		clearTimeout(pendingTrayActionFlush);
		pendingTrayActionFlush = null;
	}
	if (existingTray && !existingTray.isDestroyed()) {
		try {
			existingTray.removeAllListeners();
		} catch (error) {
			logger.warn('Failed to remove desktop tray listeners cleanly', {error});
		}
		try {
			existingTray.setContextMenu(null);
		} catch (error) {
			logger.warn('Failed to clear desktop tray context menu cleanly', {error});
		}
		try {
			existingTray.destroy();
		} catch (error) {
			logger.warn('Failed to destroy desktop tray icon cleanly', {error});
		}
	}
}

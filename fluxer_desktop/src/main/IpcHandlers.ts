// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type DesktopTroubleshootingSettings,
	type DesktopWindowBehaviorSettings,
	getDesktopTroubleshootingSettings,
	getDesktopWindowBehaviorSettings,
	setDesktopWindowBehaviorSettings,
} from '@electron/common/DesktopConfig';
import type {
	ClipboardWriteFileResult,
	DownloadFileResult,
	MediaAccessType,
	TrayPresenceStatus,
} from '@electron/common/Types';
import {hasEnabledBlinkFeature, MIDDLE_CLICK_AUTOSCROLL_BLINK_FEATURE} from '@electron/main/ChromiumRuntime';
import {
	applyDesktopWindowBehaviorSettings,
	desktopTrayChangePendingRestart,
	hasActiveDesktopTray,
	updateTrayRuntimeState,
} from '@electron/main/DesktopTray';
import {downloadFile} from '@electron/main/FileDownloads';
import {
	type LinuxAppearanceSnapshot,
	type LinuxAppearanceSubscription,
	readLinuxAppearance,
	subscribeLinuxAppearance,
} from '@electron/main/LinuxAppearance';
import {getTccStatus, registerMacTccIpcHandlers} from '@electron/main/MacTcc';
import {setNativeStrings} from '@electron/main/MainI18n';
import {copyRemoteFileToClipboard, parseClipboardWriteFileOptions} from '@electron/main/MediaClipboard';
import {registerNotificationIpcHandlers} from '@electron/main/NotificationsIpc';
import {openExternalDeduped} from '@electron/main/OpenExternal';
import {getStatus as getOpenH264Status, setEnabled as setOpenH264Enabled} from '@electron/main/OpenH264Manager';
import {registerPasskeyHandlers} from '@electron/main/Passkeys';
import {getAppMetricsSnapshot, getDesktopInfo, getGpuInfo} from '@electron/main/PlatformInfo';
import {requirePrivilegedRendererDocumentSender} from '@electron/main/PrivilegedRendererDocuments';
import {getStreamerModeCaptureAppStatus} from '@electron/main/StreamerModeProcessDetection';
import {
	acquireStreamingPriority,
	getStreamingPriorityDiagnostics,
	releaseStreamingPriority,
	resetStreamingPriority,
} from '@electron/main/StreamingPriority';
import {setTaskbarProgress, type TaskbarProgressMode} from '@electron/main/TaskbarProgress';
import {registerThemeLocalFileHandlers} from '@electron/main/ThemeLocalFiles';
import {
	popupHelpMenu,
	relaunchAndExit,
	reloadMainWindow,
	resetAppDataAndRestart,
	setHardwareAccelerationDisabled,
	setHardwareAccelerationDisabledAndRestart,
} from '@electron/main/Troubleshooting';
import {registerVoiceBackgroundMediaCacheHandlers} from '@electron/main/VoiceBackgroundMediaCache';
import {
	focusVoiceDebugEventSinkPopout,
	registerVoiceDebugEventSinkPopoutIpcHandlers,
	setVoiceDebugEventSinkAlwaysOnTop,
	VOICE_DEBUG_EVENT_SINK_POPOUT_KEY,
} from '@electron/main/VoiceDebugEventSinkPopout';
import {
	clearSavedWindowBounds,
	closeThemeStudioPopoutWindow,
	desktopTransparencyPendingRestart,
	desktopUseNativeTitleBarPendingRestart,
	focusThemeStudioPopoutWindow,
	focusVoicePopoutWindow,
	getActiveAllowTransparency,
	getActiveUseNativeTitleBar,
	getMainWindow,
	setThemeStudioPopoutAlwaysOnTop,
	setVoicePopoutAlwaysOnTop,
	showWindow,
	THEME_STUDIO_POPOUT_KEY,
	toggleWindowDevTools,
} from '@electron/main/Window';
import {flashWindowForAttention, stopFlashingWindow} from '@electron/main/WindowFlash';
import {setWindowsBadgeOverlay} from '@electron/main/WindowsBadge';
import {registerWindowsToastIpcHandlers} from '@electron/main/WindowsToast';
import {app, BrowserWindow, clipboard, dialog, ipcMain, powerMonitor, shell, systemPreferences} from 'electron';

interface TrayRuntimeStateUpdate {
	voiceConnected?: boolean;
	voiceChannelLabel?: string | null;
	selfMute?: boolean;
	selfDeaf?: boolean;
	presenceStatus?: TrayPresenceStatus | null;
	buildInfo?: string | null;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object';
}

function normalizeDesktopWindowBehaviorUpdate(value: unknown): Partial<DesktopWindowBehaviorSettings> {
	if (!isRecord(value)) {
		return {};
	}
	const update: Partial<DesktopWindowBehaviorSettings> = {};
	if (typeof value.showTrayIcon === 'boolean') {
		update.showTrayIcon = value.showTrayIcon;
	}
	if (typeof value.minimizeToTray === 'boolean') {
		update.minimizeToTray = value.minimizeToTray;
	}
	if (typeof value.closeToTray === 'boolean') {
		update.closeToTray = value.closeToTray;
	}
	if (typeof value.useNativeTitleBar === 'boolean') {
		update.useNativeTitleBar = value.useNativeTitleBar;
	}
	if (typeof value.rememberWindowState === 'boolean') {
		update.rememberWindowState = value.rememberWindowState;
	}
	if (typeof value.allowTransparency === 'boolean') {
		update.allowTransparency = value.allowTransparency;
	}
	if (typeof value.smoothScrolling === 'boolean') {
		update.smoothScrolling = value.smoothScrolling;
	}
	if (typeof value.middleClickAutoscroll === 'boolean') {
		update.middleClickAutoscroll = value.middleClickAutoscroll;
	}
	return update;
}

function getActiveSmoothScrolling(): boolean {
	return !app.commandLine.hasSwitch('disable-smooth-scrolling');
}

function getActiveMiddleClickAutoscroll(): boolean {
	return process.platform === 'linux' && hasEnabledBlinkFeature(MIDDLE_CLICK_AUTOSCROLL_BLINK_FEATURE);
}

export function registerIpcHandlers(): void {
	registerVoiceDebugEventSinkPopoutIpcHandlers();
	registerVoiceBackgroundMediaCacheHandlers();
	ipcMain.handle('get-desktop-info', () => getDesktopInfo());
	ipcMain.handle('get-gpu-info', () => getGpuInfo());
	ipcMain.handle('get-app-metrics', () => getAppMetricsSnapshot());
	ipcMain.handle('get-openh264-status', () => getOpenH264Status());
	ipcMain.handle('set-openh264-enabled', (_event, enabled: unknown) => setOpenH264Enabled(Boolean(enabled)));
	ipcMain.handle('streamer-mode:get-capture-app-status', () => getStreamerModeCaptureAppStatus());
	ipcMain.handle('system-idle-time-ms', (): number => {
		return Math.max(0, powerMonitor.getSystemIdleTime() * 1000);
	});
	ipcMain.handle('desktop-window-behavior-get', (): DesktopWindowBehaviorSettings => {
		return {
			...getDesktopWindowBehaviorSettings(),
			activeUseNativeTitleBar: getActiveUseNativeTitleBar(),
			activeAllowTransparency: getActiveAllowTransparency(),
			activeSmoothScrolling: getActiveSmoothScrolling(),
			activeMiddleClickAutoscroll: getActiveMiddleClickAutoscroll(),
		};
	});
	ipcMain.handle('desktop-troubleshooting-get', (): DesktopTroubleshootingSettings => {
		if (process.platform === 'darwin') {
			return {...getDesktopTroubleshootingSettings(), disableHardwareAcceleration: false};
		}
		return getDesktopTroubleshootingSettings();
	});
	ipcMain.handle(
		'desktop-troubleshooting-set-disable-hardware-acceleration',
		(
			_event,
			payload: {
				disable: boolean;
				restart?: boolean;
			},
		): DesktopTroubleshootingSettings => {
			const disable = Boolean(payload?.disable);
			if (payload?.restart) {
				setHardwareAccelerationDisabledAndRestart(disable);
			} else {
				setHardwareAccelerationDisabled(disable);
			}
			if (process.platform === 'darwin') {
				return {...getDesktopTroubleshootingSettings(), disableHardwareAcceleration: false};
			}
			return getDesktopTroubleshootingSettings();
		},
	);
	ipcMain.handle('desktop-troubleshooting-reload', (): void => {
		reloadMainWindow();
	});
	ipcMain.handle(
		'desktop-troubleshooting-reset-app-data',
		async (
			_event,
			options?: {
				confirm?: boolean;
			},
		): Promise<void> => {
			await resetAppDataAndRestart(options);
		},
	);
	ipcMain.handle('desktop-troubleshooting-popup-help-menu', (event): void => {
		popupHelpMenu(BrowserWindow.fromWebContents(event.sender));
	});
	ipcMain.on('streaming-priority-acquire', (event) => {
		acquireStreamingPriority(event.sender);
	});
	ipcMain.on('streaming-priority-release', () => {
		releaseStreamingPriority();
	});
	ipcMain.on('streaming-priority-reset', () => {
		resetStreamingPriority();
	});
	ipcMain.handle('streaming-priority-get-diagnostics', () => getStreamingPriorityDiagnostics());
	ipcMain.on('tray-runtime-state-update', (event, state: unknown) => {
		if (!state || typeof state !== 'object') return;
		const update = state as TrayRuntimeStateUpdate;
		updateTrayRuntimeState(update, event.sender.id);
	});
	ipcMain.on('native-locale-set', (_event, payload: unknown) => {
		if (!payload || typeof payload !== 'object') return;
		const {locale, strings} = payload as {
			locale?: unknown;
			strings?: unknown;
		};
		if (typeof locale !== 'string' || !strings || typeof strings !== 'object') return;
		const sanitized: Record<string, string> = {};
		for (const [key, value] of Object.entries(strings)) {
			if (typeof key === 'string' && typeof value === 'string') {
				sanitized[key] = value;
			}
		}
		setNativeStrings(locale, sanitized);
	});
	ipcMain.on('window-flash', (_event, persistent: unknown) => {
		flashWindowForAttention(persistent === true);
	});
	ipcMain.on('window-stop-flash', () => {
		stopFlashingWindow();
	});
	ipcMain.on('taskbar-progress-set', (_event, payload: unknown) => {
		if (!payload || typeof payload !== 'object') return;
		const {fraction, mode} = payload as {
			fraction?: unknown;
			mode?: unknown;
		};
		const numericFraction = typeof fraction === 'number' ? fraction : -1;
		const validModes: ReadonlyArray<TaskbarProgressMode> = ['normal', 'indeterminate', 'error', 'paused', 'none'];
		const resolvedMode: TaskbarProgressMode =
			typeof mode === 'string' && (validModes as ReadonlyArray<string>).includes(mode)
				? (mode as TaskbarProgressMode)
				: 'normal';
		setTaskbarProgress(numericFraction, resolvedMode);
	});
	ipcMain.handle('desktop-window-behavior-pending-restart', (): boolean => {
		return (
			desktopTrayChangePendingRestart() ||
			desktopUseNativeTitleBarPendingRestart() ||
			desktopTransparencyPendingRestart()
		);
	});
	ipcMain.handle('desktop-app-relaunch', (): void => {
		relaunchAndExit();
	});
	registerThemeLocalFileHandlers(getMainWindow);
	ipcMain.handle('desktop-window-behavior-set', (_event, settings: unknown): DesktopWindowBehaviorSettings => {
		const nextSettings = setDesktopWindowBehaviorSettings(normalizeDesktopWindowBehaviorUpdate(settings));
		applyDesktopWindowBehaviorSettings();
		if (!nextSettings.showTrayIcon) {
			const mainWindow = getMainWindow();
			if (mainWindow && !mainWindow.isDestroyed() && !mainWindow.isVisible()) {
				showWindow();
			}
		}
		if (!nextSettings.rememberWindowState) {
			clearSavedWindowBounds();
		}
		return {
			...nextSettings,
			activeUseNativeTitleBar: getActiveUseNativeTitleBar(),
			activeAllowTransparency: getActiveAllowTransparency(),
			activeSmoothScrolling: getActiveSmoothScrolling(),
			activeMiddleClickAutoscroll: getActiveMiddleClickAutoscroll(),
		};
	});
	ipcMain.on('window-minimize', (event) => {
		const win = BrowserWindow.fromWebContents(event.sender);
		if (!win) return;
		const settings = getDesktopWindowBehaviorSettings();
		if (win === getMainWindow() && hasActiveDesktopTray() && settings.showTrayIcon && settings.minimizeToTray) {
			win.hide();
			return;
		}
		win.minimize();
	});
	ipcMain.on('window-maximize', (event) => {
		const win = BrowserWindow.fromWebContents(event.sender);
		if (win) {
			if (win.isMaximized()) {
				win.unmaximize();
			} else {
				win.maximize();
			}
		}
	});
	ipcMain.on('window-close', (event) => {
		BrowserWindow.fromWebContents(event.sender)?.close();
	});
	ipcMain.handle('window-is-maximized', (event): boolean => {
		return BrowserWindow.fromWebContents(event.sender)?.isMaximized() ?? false;
	});
	ipcMain.handle('theme-studio-popout-focus', (): boolean => {
		return focusThemeStudioPopoutWindow();
	});
	ipcMain.handle('theme-studio-popout-close', (): boolean => {
		return closeThemeStudioPopoutWindow();
	});
	ipcMain.handle('popout:set-always-on-top', (_event, key: unknown, flag: unknown): boolean => {
		if (typeof key !== 'string' || typeof flag !== 'boolean') {
			return false;
		}
		if (key === THEME_STUDIO_POPOUT_KEY) {
			return setThemeStudioPopoutAlwaysOnTop(flag);
		}
		if (key === VOICE_DEBUG_EVENT_SINK_POPOUT_KEY) {
			return setVoiceDebugEventSinkAlwaysOnTop(flag);
		}
		return setVoicePopoutAlwaysOnTop(key, flag);
	});
	ipcMain.handle('popout:focus', (_event, key: unknown): boolean => {
		if (typeof key !== 'string') {
			return false;
		}
		if (key === THEME_STUDIO_POPOUT_KEY) {
			return focusThemeStudioPopoutWindow();
		}
		if (key === VOICE_DEBUG_EVENT_SINK_POPOUT_KEY) {
			return focusVoiceDebugEventSinkPopout();
		}
		return focusVoicePopoutWindow(key);
	});
	ipcMain.handle('open-external', async (_event, url: string): Promise<void> => {
		if (typeof url !== 'string') {
			throw new Error('Invalid URL');
		}
		await openExternalDeduped(url);
	});
	ipcMain.handle('clipboard-write-text', async (_event, text: string): Promise<void> => {
		await clipboard.writeText(text);
	});
	ipcMain.handle('clipboard-read-text', (): Promise<string> => {
		return clipboard.readText();
	});
	ipcMain.handle('clipboard-write-file', async (event, rawOptions: unknown): Promise<ClipboardWriteFileResult> => {
		requirePrivilegedRendererDocumentSender(event, 'clipboard-write-file');
		try {
			return await copyRemoteFileToClipboard(parseClipboardWriteFileOptions(rawOptions));
		} catch (error) {
			return {success: false, error: error instanceof Error ? error.message : 'Invalid clipboard file payload'};
		}
	});
	ipcMain.handle('clipboard-paste', (event): void => {
		requirePrivilegedRendererDocumentSender(event, 'clipboard-paste');
		event.sender.paste();
	});
	ipcMain.handle(
		'app-set-badge',
		(
			_event,
			payload: {
				count: number;
				text?: string;
			},
		) => {
			const count = Math.max(0, Math.floor(payload?.count ?? 0));
			const label = payload?.text ?? String(count);
			app.setBadgeCount(count);
			if (process.platform === 'darwin' && app.dock) {
				app.dock.setBadge(count > 0 ? label : '');
			}
			if (process.platform === 'win32') {
				setWindowsBadgeOverlay(getMainWindow(), count);
			}
		},
	);
	ipcMain.handle(
		'download-file',
		async (
			event,
			options: {
				url: string;
				defaultPath: string;
			},
		): Promise<DownloadFileResult> => {
			requirePrivilegedRendererDocumentSender(event, 'download-file');
			const win = BrowserWindow.fromWebContents(event.sender);
			if (!win) {
				return {success: false, error: 'No window found'};
			}
			try {
				const result = await dialog.showSaveDialog(win, {
					defaultPath: options.defaultPath,
				});
				if (result.canceled || !result.filePath) {
					return {success: false, canceled: true};
				}
				await downloadFile(options.url, result.filePath);
				return {success: true, path: result.filePath};
			} catch (error) {
				return {success: false, error: error instanceof Error ? error.message : 'Unknown error'};
			}
		},
	);
	ipcMain.on('toggle-devtools', (event) => {
		const win = BrowserWindow.fromWebContents(event.sender);
		if (win) {
			toggleWindowDevTools(win);
		}
	});
	ipcMain.handle('check-media-access', (_event, type: MediaAccessType): string => {
		if (process.platform !== 'darwin') {
			return 'granted';
		}
		if (type === 'audio-capture') {
			return 'not-determined';
		}
		if (type === 'screen') {
			return getTccStatus('screen-recording');
		}
		return systemPreferences.getMediaAccessStatus(type);
	});
	ipcMain.handle('request-media-access', async (_event, type: MediaAccessType): Promise<boolean> => {
		if (process.platform !== 'darwin') {
			return true;
		}
		if (type === 'audio-capture') {
			return false;
		}
		if (type === 'screen') {
			return getTccStatus('screen-recording') === 'granted';
		}
		return systemPreferences.askForMediaAccess(type);
	});
	ipcMain.handle('open-media-access-settings', async (_event, type: MediaAccessType): Promise<void> => {
		if (process.platform !== 'darwin') {
			return;
		}
		const privacyKeys: Record<MediaAccessType, string> = {
			microphone: 'Privacy_Microphone',
			camera: 'Privacy_Camera',
			screen: 'Privacy_ScreenCapture',
			'audio-capture': 'Privacy_AudioCapture',
		};
		await shell.openExternal(`x-apple.systempreferences:com.apple.preference.security?${privacyKeys[type]}`);
	});
	ipcMain.handle('open-input-monitoring-settings', async (): Promise<void> => {
		if (process.platform !== 'darwin') {
			return;
		}
		await shell.openExternal('x-apple.systempreferences:com.apple.preference.security?Privacy_ListenEvent');
	});
	registerNotificationIpcHandlers(getMainWindow);
	registerWindowsToastIpcHandlers();
	registerMacTccIpcHandlers();
	ipcMain.on('set-badge-count', (_event, count: number) => {
		if (process.platform === 'darwin') {
			app.setBadgeCount(count);
		} else if (process.platform === 'win32') {
			setWindowsBadgeOverlay(getMainWindow(), count);
		} else {
			app.setBadgeCount(count);
		}
	});
	ipcMain.handle('get-badge-count', (): number => {
		return app.getBadgeCount();
	});
	ipcMain.on('bounce-dock', (event, type: 'critical' | 'informational') => {
		if (process.platform === 'darwin' && app.dock) {
			const id = app.dock.bounce(type);
			event.returnValue = id;
		} else {
			event.returnValue = -1;
		}
	});
	ipcMain.on('cancel-bounce-dock', (_event, id: number) => {
		if (process.platform === 'darwin' && app.dock && id >= 0) {
			app.dock.cancelBounce(id);
		}
	});
	ipcMain.on('set-zoom-factor', (event, factor: number) => {
		const win = BrowserWindow.fromWebContents(event.sender);
		if (win && factor > 0) {
			win.webContents.setZoomFactor(factor);
		}
	});
	ipcMain.handle('get-zoom-factor', (event): number => {
		const win = BrowserWindow.fromWebContents(event.sender);
		return win?.webContents.getZoomFactor() ?? 1;
	});
	ipcMain.handle('get-accessibility-support-enabled', (): boolean => app.accessibilitySupportEnabled);
	app.on('accessibility-support-changed', (_event, accessibilitySupportEnabled) => {
		const mainWindow = getMainWindow();
		if (!mainWindow || mainWindow.isDestroyed()) return;
		mainWindow.webContents.send('accessibility-support-changed', Boolean(accessibilitySupportEnabled));
	});
	registerPasskeyHandlers();
	registerLinuxAppearanceHandlers();
}

let linuxAppearanceSubscription: LinuxAppearanceSubscription | null = null;

function broadcastLinuxAppearance(snapshot: LinuxAppearanceSnapshot): void {
	for (const window of BrowserWindow.getAllWindows()) {
		if (window.isDestroyed()) continue;
		window.webContents.send('linux-appearance-changed', snapshot);
	}
}

function registerLinuxAppearanceHandlers(): void {
	ipcMain.handle('linux-appearance-get', (): LinuxAppearanceSnapshot => {
		if (process.platform !== 'linux') {
			return {colorScheme: 'no-preference', contrast: 'no-preference', accent: null};
		}
		return readLinuxAppearance();
	});
	if (process.platform !== 'linux') return;
	if (linuxAppearanceSubscription) return;
	linuxAppearanceSubscription = subscribeLinuxAppearance((snapshot) => {
		broadcastLinuxAppearance(snapshot);
	});
}

export function cleanupIpcHandlers(_options: {quitting?: boolean} = {}): void {
	if (linuxAppearanceSubscription) {
		try {
			linuxAppearanceSubscription.close();
		} catch {}
		linuxAppearanceSubscription = null;
	}
}

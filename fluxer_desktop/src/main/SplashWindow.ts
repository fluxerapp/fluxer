// SPDX-License-Identifier: AGPL-3.0-or-later

import {pathToFileURL} from 'node:url';
import {createChildLogger} from '@electron/common/Logger';
import {getDesktopDistributionPath} from '@electron/main/DesktopDistributionPath';
import {app, BrowserWindow, type IpcMainEvent, ipcMain, nativeTheme} from 'electron';

const SPLASH_WINDOW_WIDTH = 300;
const SPLASH_WINDOW_HEIGHT_DARWIN = 300;
const SPLASH_WINDOW_HEIGHT_DEFAULT = 350;
const SPLASH_BACKGROUND_COLOR = '#1a1a1a';
const SPLASH_ACTION_LABEL_MAX_LENGTH = 48;
const SPLASH_MESSAGE_MAX_LENGTH = 160;
const SPLASH_VERSION_LABEL_MAX_LENGTH = 64;
const SPLASH_OPTION_LABEL_MAX_LENGTH = 48;
const SPLASH_OPTION_BUTTON_LABEL_MAX_LENGTH = 24;
const SPLASH_OPTION_MAX_COUNT = 8;
const SPLASH_OPTION_VALUE_PATTERN = /^[a-z0-9_]{1,32}$/;
const SPLASH_PROGRESS_MAX = 100;

export const SPLASH_READY_WATCHDOG_MS = 3000;
export const SPLASH_CLOSE_DELAY_MS = 100;

export const DESKTOP_SPLASH_STATE_CHANNEL = 'desktop-splash:state';
export const DESKTOP_SPLASH_READY_CHANNEL = 'desktop-splash:ready';
export const DESKTOP_SPLASH_RETRY_NOW_CHANNEL = 'desktop-splash:retry-now';
export const DESKTOP_SPLASH_QUIT_CHANNEL = 'desktop-splash:quit';
export const DESKTOP_SPLASH_OPEN_DOWNLOAD_CHANNEL = 'desktop-splash:open-download';
export const DESKTOP_SPLASH_NETWORK_ONLINE_CHANNEL = 'desktop-splash:network-online';
export const DESKTOP_SPLASH_OPEN_LOGS_CHANNEL = 'desktop-splash:open-logs';
export const DESKTOP_SPLASH_COPY_DIAGNOSTICS_CHANNEL = 'desktop-splash:copy-diagnostics';

export const SplashLayout = Object.freeze({
	SPLASH: 'splash',
	MANUAL_UPDATE: 'manual-update',
} as const);

export type SplashLayout = (typeof SplashLayout)[keyof typeof SplashLayout];

export const SplashStatus = Object.freeze({
	CHECKING_FOR_UPDATES: 'checking-for-updates',
	DOWNLOADING_UPDATES: 'downloading-updates',
	INSTALLING_UPDATES: 'installing-updates',
	VERIFYING: 'verifying',
	UPDATE_FAILURE: 'update-failure',
	DOWNLOAD_STALLED: 'download-stalled',
	SHELL_UPDATE_DOWNLOADING: 'shell-update-downloading',
	SHELL_UPDATE_RESTARTING: 'shell-update-restarting',
	BLOCKED_UPDATE_REQUIRED: 'blocked-update-required',
	BLOCKED_UPDATE_UNREACHABLE: 'blocked-update-unreachable',
	BLOCKED_SECURITY_UPDATE_REQUIRED: 'blocked-security-update-required',
	BLOCKED_SECURITY_UPDATE_MANAGED: 'blocked-security-update-managed',
	BLOCKED_SHELL_UPDATE: 'blocked-shell-update',
	BLOCKED_SHELL_UPDATE_MANAGED: 'blocked-shell-update-managed',
	BLOCKED_UNSUPPORTED_BUILD: 'blocked-unsupported-build',
	LAUNCHING: 'launching',
	UNREACHABLE_LAUNCH: 'unreachable-launch',
	WAITING_LOCAL_NETWORK: 'waiting-local-network',
} as const);

export type SplashStatus = (typeof SplashStatus)[keyof typeof SplashStatus];

export const SplashActionKind = Object.freeze({
	RETRY: 'retry',
	DOWNLOAD: 'download',
	QUIT: 'quit',
} as const);

export type SplashActionKind = (typeof SplashActionKind)[keyof typeof SplashActionKind];

export interface SplashActionDescriptor {
	readonly kind: SplashActionKind;
	readonly label: string;
}

export interface SplashDownloadOption {
	readonly value: string;
	readonly label: string;
	readonly buttonLabel: string;
	readonly kind: SplashActionKind;
}

export const SplashAction = Object.freeze({
	RETRY: Object.freeze({kind: SplashActionKind.RETRY, label: 'Retry now'}),
	DOWNLOAD: Object.freeze({kind: SplashActionKind.DOWNLOAD, label: 'Download update'}),
	QUIT: Object.freeze({kind: SplashActionKind.QUIT, label: 'Quit'}),
} as const satisfies Readonly<Record<string, SplashActionDescriptor>>);

export interface SplashState {
	readonly status: SplashStatus;
	readonly requiredSecurityUpdate?: boolean;
	readonly layout?: SplashLayout | null;
	readonly current?: number | null;
	readonly total?: number | null;
	readonly progress?: number | null;
	readonly seconds?: number | null;
	readonly receivedBytes?: number | null;
	readonly totalBytes?: number | null;
	readonly bytesPerSecond?: number | null;
	readonly action?: SplashActionDescriptor | null;
	readonly message?: string | null;
	readonly versionLabel?: string | null;
	readonly options?: ReadonlyArray<SplashDownloadOption> | null;
}

export interface SerializedSplashState {
	readonly layout: SplashLayout;
	readonly status: SplashStatus;
	readonly requiredSecurityUpdate: boolean;
	readonly current: number | null;
	readonly total: number | null;
	readonly progress: number | null;
	readonly seconds: number | null;
	readonly receivedBytes: number | null;
	readonly totalBytes: number | null;
	readonly bytesPerSecond: number | null;
	readonly action: SplashActionDescriptor | null;
	readonly message: string | null;
	readonly versionLabel: string | null;
	readonly options: ReadonlyArray<SplashDownloadOption> | null;
}

type SplashListener = () => void;
type SplashDownloadListener = (value: string | null) => void;

const logger = createChildLogger('SplashWindow');
const SPLASH_LAYOUTS = new Set<string>(Object.values(SplashLayout));
const SPLASH_STATUSES = new Set<string>(Object.values(SplashStatus));
const SPLASH_ACTION_KINDS = new Set<string>(Object.values(SplashActionKind));
const readyListeners = new Set<SplashListener>();
const retryListeners = new Set<SplashListener>();
const quitListeners = new Set<SplashListener>();
const networkOnlineListeners = new Set<SplashListener>();
const openDownloadListeners = new Set<SplashDownloadListener>();
const openLogsListeners = new Set<SplashListener>();
const copyDiagnosticsListeners = new Set<SplashListener>();

let splashWindow: BrowserWindow | null = null;
let splashIpcRegistered = false;
let launchLatched = false;
let affordanceLatched = false;
let lastSerializedState: SerializedSplashState | null = null;
let readyWatchdog: NodeJS.Timeout | null = null;
let themeSourceBeforeSplash: typeof nativeTheme.themeSource | null = null;

function toSplashCount(value: number | null | undefined): number | null {
	if (typeof value !== 'number' || !Number.isFinite(value)) return null;
	return Math.max(0, Math.trunc(value));
}

function toSplashProgress(value: number | null | undefined): number | null {
	if (typeof value !== 'number' || !Number.isFinite(value)) return null;
	return Math.min(SPLASH_PROGRESS_MAX, Math.max(0, value));
}

function toSplashText(value: string | null | undefined, maxLength: number): string | null {
	if (typeof value !== 'string') return null;
	const trimmed = value.trim();
	if (trimmed.length === 0) return null;
	return trimmed.slice(0, maxLength);
}

function toSplashAction(action: SplashActionDescriptor | null | undefined): SplashActionDescriptor | null {
	if (action == null || typeof action !== 'object') return null;
	if (!SPLASH_ACTION_KINDS.has(action.kind)) return null;
	const label = toSplashText(action.label, SPLASH_ACTION_LABEL_MAX_LENGTH);
	if (label == null) return null;
	return {kind: action.kind, label};
}

function toSplashOption(option: SplashDownloadOption): SplashDownloadOption | null {
	if (option == null || typeof option !== 'object') return null;
	if (typeof option.value !== 'string' || !SPLASH_OPTION_VALUE_PATTERN.test(option.value)) return null;
	if (!SPLASH_ACTION_KINDS.has(option.kind)) return null;
	const label = toSplashText(option.label, SPLASH_OPTION_LABEL_MAX_LENGTH);
	if (label == null) return null;
	const buttonLabel = toSplashText(option.buttonLabel, SPLASH_OPTION_BUTTON_LABEL_MAX_LENGTH);
	if (buttonLabel == null) return null;
	return {value: option.value, label, buttonLabel, kind: option.kind};
}

function toSplashOptions(
	options: ReadonlyArray<SplashDownloadOption> | null | undefined,
): ReadonlyArray<SplashDownloadOption> | null {
	if (!Array.isArray(options)) return null;
	const accepted: Array<SplashDownloadOption> = [];
	for (const option of options.slice(0, SPLASH_OPTION_MAX_COUNT)) {
		const normalized = toSplashOption(option);
		if (normalized != null) {
			accepted.push(normalized);
		}
	}
	return accepted.length === 0 ? null : accepted;
}

function withManualUpdateAffordance(state: SerializedSplashState): SerializedSplashState {
	if (state.layout !== SplashLayout.MANUAL_UPDATE || hasSplashAffordance(state)) return state;
	return {...state, action: SplashAction.QUIT};
}

export function serializeSplashState(state: SplashState): SerializedSplashState {
	return withManualUpdateAffordance({
		layout: SPLASH_LAYOUTS.has(state.layout as string) ? (state.layout as SplashLayout) : SplashLayout.SPLASH,
		status: SPLASH_STATUSES.has(state.status) ? state.status : SplashStatus.CHECKING_FOR_UPDATES,
		requiredSecurityUpdate: state.requiredSecurityUpdate === true,
		current: toSplashCount(state.current),
		total: toSplashCount(state.total),
		progress: toSplashProgress(state.progress),
		seconds: toSplashCount(state.seconds),
		receivedBytes: toSplashCount(state.receivedBytes),
		totalBytes: toSplashCount(state.totalBytes),
		bytesPerSecond: toSplashCount(state.bytesPerSecond),
		action: toSplashAction(state.action),
		message: toSplashText(state.message, SPLASH_MESSAGE_MAX_LENGTH),
		versionLabel: toSplashText(state.versionLabel, SPLASH_VERSION_LABEL_MAX_LENGTH),
		options: toSplashOptions(state.options),
	});
}

export function hasSplashAffordance(state: SerializedSplashState): boolean {
	return state.action != null || state.options != null;
}

export function toSplashDownloadValue(value: unknown): string | null {
	if (typeof value !== 'string') return null;
	return SPLASH_OPTION_VALUE_PATTERN.test(value) ? value : null;
}

export function getSplashWindowHeight(platform: NodeJS.Platform): number {
	return platform === 'darwin' ? SPLASH_WINDOW_HEIGHT_DARWIN : SPLASH_WINDOW_HEIGHT_DEFAULT;
}

export function shouldQuitOnSplashClosed(platform: NodeJS.Platform, isLaunchLatched: boolean): boolean {
	return platform !== 'darwin' && !isLaunchLatched;
}

function addSplashListener<T extends SplashListener | SplashDownloadListener>(
	listeners: Set<T>,
	listener: T,
): () => void {
	listeners.add(listener);
	return () => {
		listeners.delete(listener);
	};
}

function emitSplashListeners(listeners: Set<SplashListener>): void {
	for (const listener of Array.from(listeners)) {
		try {
			listener();
		} catch (error) {
			logger.error('A splash listener threw', error);
		}
	}
}

function emitSplashDownloadListeners(value: string | null): void {
	for (const listener of Array.from(openDownloadListeners)) {
		try {
			listener(value);
		} catch (error) {
			logger.error('A splash listener threw', error);
		}
	}
}

function isSplashSender(event: IpcMainEvent): boolean {
	return splashWindow != null && !splashWindow.isDestroyed() && event.sender === splashWindow.webContents;
}

function showSplashWindow(reason: 'ready' | 'watchdog'): void {
	if (readyWatchdog != null) {
		clearTimeout(readyWatchdog);
		readyWatchdog = null;
	}
	const window = splashWindow;
	if (window == null || window.isDestroyed() || window.isVisible()) return;
	if (reason === 'watchdog') {
		logger.warn('The splash preload never reported ready, showing the window anyway');
	}
	window.showInactive();
}

function registerSplashIpc(): void {
	if (splashIpcRegistered) return;
	splashIpcRegistered = true;
	ipcMain.on(DESKTOP_SPLASH_READY_CHANNEL, (event) => {
		if (!isSplashSender(event)) return;
		showSplashWindow('ready');
		if (lastSerializedState != null) {
			event.sender.send(DESKTOP_SPLASH_STATE_CHANNEL, lastSerializedState);
		}
		emitSplashListeners(readyListeners);
	});
	ipcMain.on(DESKTOP_SPLASH_RETRY_NOW_CHANNEL, (event) => {
		if (!isSplashSender(event)) return;
		emitSplashListeners(retryListeners);
	});
	ipcMain.on(DESKTOP_SPLASH_QUIT_CHANNEL, (event) => {
		if (!isSplashSender(event)) return;
		emitSplashListeners(quitListeners);
	});
	ipcMain.on(DESKTOP_SPLASH_NETWORK_ONLINE_CHANNEL, (event) => {
		if (!isSplashSender(event)) return;
		emitSplashListeners(networkOnlineListeners);
	});
	ipcMain.on(DESKTOP_SPLASH_OPEN_DOWNLOAD_CHANNEL, (event, value: unknown) => {
		if (!isSplashSender(event)) return;
		emitSplashDownloadListeners(toSplashDownloadValue(value));
	});
	ipcMain.on(DESKTOP_SPLASH_OPEN_LOGS_CHANNEL, (event) => {
		if (!isSplashSender(event)) return;
		emitSplashListeners(openLogsListeners);
	});
	ipcMain.on(DESKTOP_SPLASH_COPY_DIAGNOSTICS_CHANNEL, (event) => {
		if (!isSplashSender(event)) return;
		emitSplashListeners(copyDiagnosticsListeners);
	});
}

function registerSplashDiagnostics(window: BrowserWindow): void {
	window.webContents.on('did-fail-load', (_event, errorCode, errorDescription, validatedUrl) => {
		logger.error('The splash document failed to load', {errorCode, errorDescription, validatedUrl});
	});
	window.webContents.on('preload-error', (_event, preloadPath, error) => {
		logger.error('The splash preload threw', {preloadPath, error});
	});
	window.webContents.on('console-message', (details) => {
		logger.info('The splash renderer logged', {
			level: details.level,
			message: details.message,
			line: details.lineNumber,
		});
	});
}

export function openSplashWindow(): BrowserWindow {
	if (splashWindow != null && !splashWindow.isDestroyed()) return splashWindow;
	registerSplashIpc();
	if (themeSourceBeforeSplash == null) themeSourceBeforeSplash = nativeTheme.themeSource;
	nativeTheme.themeSource = 'dark';
	const window = new BrowserWindow({
		width: SPLASH_WINDOW_WIDTH,
		height: getSplashWindowHeight(process.platform),
		frame: false,
		hasShadow: true,
		resizable: false,
		center: true,
		show: false,
		transparent: false,
		backgroundColor: SPLASH_BACKGROUND_COLOR,
		webPreferences: {
			preload: getDesktopDistributionPath('preload', 'splash.cjs'),
			contextIsolation: true,
			nodeIntegration: false,
			sandbox: true,
			webSecurity: true,
			spellcheck: false,
			backgroundThrottling: false,
		},
	});
	splashWindow = window;
	registerSplashDiagnostics(window);
	window.webContents.on('will-navigate', (event) => {
		event.preventDefault();
	});
	window.webContents.setWindowOpenHandler(() => ({action: 'deny'}));
	window.on('closed', () => {
		if (splashWindow === window) {
			splashWindow = null;
		}
		restoreThemeSource();
		if (!shouldQuitOnSplashClosed(process.platform, launchLatched)) return;
		logger.info('The splash window closed before launch, quitting');
		app.quit();
	});
	readyWatchdog = setTimeout(() => {
		readyWatchdog = null;
		showSplashWindow('watchdog');
	}, SPLASH_READY_WATCHDOG_MS);
	readyWatchdog.unref();
	const documentUrl = pathToFileURL(getDesktopDistributionPath('splash', 'index.html')).href;
	window.loadURL(documentUrl).catch((error) => {
		logger.error('Failed to load the splash document', error);
	});
	return window;
}

export function focusSplashWindow(): void {
	const window = splashWindow;
	if (window == null || window.isDestroyed()) return;
	if (window.isMinimized()) {
		window.restore();
	}
	window.show();
	window.focus();
}

function restoreThemeSource(): void {
	if (themeSourceBeforeSplash == null) return;
	nativeTheme.themeSource = themeSourceBeforeSplash;
	themeSourceBeforeSplash = null;
}

export function closeSplashWindow(): void {
	const window = splashWindow;
	splashWindow = null;
	restoreThemeSource();
	lastSerializedState = null;
	affordanceLatched = false;
	readyListeners.clear();
	retryListeners.clear();
	quitListeners.clear();
	networkOnlineListeners.clear();
	openDownloadListeners.clear();
	openLogsListeners.clear();
	copyDiagnosticsListeners.clear();
	if (readyWatchdog != null) {
		clearTimeout(readyWatchdog);
		readyWatchdog = null;
	}
	if (window == null || window.isDestroyed()) return;
	window.setSkipTaskbar(true);
	const closeTimer = setTimeout(() => {
		if (window.isDestroyed()) return;
		window.hide();
		window.close();
	}, SPLASH_CLOSE_DELAY_MS);
	closeTimer.unref();
}

export function setSplashState(state: SplashState): void {
	const serialized = serializeSplashState(state);
	if (affordanceLatched && !hasSplashAffordance(serialized)) {
		logger.warn('Dropping a splash state that would erase a terminal affordance', {status: serialized.status});
		return;
	}
	affordanceLatched = hasSplashAffordance(serialized);
	lastSerializedState = serialized;
	if (splashWindow == null || splashWindow.isDestroyed() || splashWindow.webContents.isDestroyed()) return;
	if (affordanceLatched) {
		splashWindow.setAlwaysOnTop(true);
	}
	splashWindow.webContents.send(DESKTOP_SPLASH_STATE_CHANNEL, serialized);
}

export function releaseSplashAffordance(): void {
	if (!affordanceLatched) return;
	affordanceLatched = false;
	if (splashWindow == null || splashWindow.isDestroyed()) return;
	splashWindow.setAlwaysOnTop(false);
}

export function markSplashLaunching(): void {
	launchLatched = true;
}

export function onSplashReady(listener: SplashListener): () => void {
	return addSplashListener(readyListeners, listener);
}

export function onSplashRetry(listener: SplashListener): () => void {
	return addSplashListener(retryListeners, listener);
}

export function onSplashQuit(listener: SplashListener): () => void {
	return addSplashListener(quitListeners, listener);
}

export function onSplashNetworkOnline(listener: SplashListener): () => void {
	return addSplashListener(networkOnlineListeners, listener);
}

export function onSplashOpenDownload(listener: SplashDownloadListener): () => void {
	return addSplashListener(openDownloadListeners, listener);
}

export function onSplashOpenLogs(listener: SplashListener): () => void {
	return addSplashListener(openLogsListeners, listener);
}

export function onSplashCopyDiagnostics(listener: SplashListener): () => void {
	return addSplashListener(copyDiagnosticsListeners, listener);
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {
	DEFAULT_WINDOW_HEIGHT,
	DEFAULT_WINDOW_WIDTH,
	DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL,
	MIN_WINDOW_HEIGHT,
	MIN_WINDOW_WIDTH,
} from '@electron/common/Constants';
import {getDesktopWindowBehaviorSettings, getPrebootTheme} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import type {DesktopWindowBehaviorSettings} from '@electron/common/Types';
import {createAppLoadRetry} from '@electron/main/AppLoadRetry';
import {
	shouldForwardRendererConsoleToMainLog,
	shouldIgnoreWindowStateForLaunch,
	shouldOpenDevToolsOnLaunch,
} from '@electron/main/DesktopDebugInfo';
import {getDesktopDistributionPath} from '@electron/main/DesktopDistributionPath';
import {resolveDesktopLandingUrl} from '@electron/main/DesktopLastRoute';
import {DesktopRuntimeSecurity} from '@electron/main/DesktopRuntimeSecurity';
import {hasActiveDesktopTray, refreshDesktopTrayMenu} from '@electron/main/DesktopTray';
import {drainPendingDisplayMediaRequests, registerDisplayMediaRequestHandler} from '@electron/main/DisplayMedia';
import {isNativeGatewayAvailable} from '@electron/main/GatewaySocketNativeBoundary';
import {shouldDisableV8CodeCache} from '@electron/main/LaunchOptions';
import {getDesktopLocalAppAuthorization} from '@electron/main/LocalAppProtocolAuthorization';
import {isLocalAppRendererDocumentURL, isLocalAppURL} from '@electron/main/LocalAppURL';
import {cancelPendingFullScreenHide, hideWindowLeavingFullScreen} from '@electron/main/MacFullScreenHide';
import {t} from '@electron/main/MainI18n';
import {MainWindowRevealGate, MainWindowRevealReason} from '@electron/main/MainWindowReveal';
import {signalMainWindowCreated, signalMainWindowReady} from '@electron/main/ModuleBootHandoff';
import {openExternalDeduped} from '@electron/main/OpenExternal';
import {registerSpellcheck} from '@electron/main/Spellcheck';
import {resetStreamingPriority} from '@electron/main/StreamingPriority';
import {getMainWindowRendererGoneAction} from '@electron/main/WindowRendererLifecycle';
import {refreshWindowsBadgeOverlay} from '@electron/main/WindowsBadge';
import {NATIVE_GATEWAY_TRANSPORT_AVAILABLE_RENDERER_ARG} from '@fluxer/desktop_ipc/src/GatewayTransportContract';
import {app, BrowserWindow, dialog, ipcMain, screen} from 'electron';
import log from 'electron-log';

const logger = createChildLogger('Window');
const runtimeSecurity = new DesktopRuntimeSecurity({
	logger: createChildLogger('DesktopRuntimeSecurity'),
	localAppAuthorization: getDesktopLocalAppAuthorization(),
});
const LIVE_RESIZE_IDLE_MS = 400;
const VISIBILITY_MARGIN = 32;
const RENDERER_GONE_REPEAT_WINDOW_MS = 30000;
const CLOSE_FOR_UPDATE_TIMEOUT_MS = 5000;
const UPDATE_RELOAD_COMMIT_TIMEOUT_MS = 8000;
const THEME_WINDOW_BACKGROUND_COLORS: Readonly<Record<string, string>> = Object.freeze({
	dark: '#1a181e',
	light: '#ebecef',
	coal: '#020203',
	dark_legacy: '#191b20',
});
const DEFAULT_WINDOW_BACKGROUND_COLOR = '#1a181e';
const TRANSPARENT_WINDOW_BACKGROUND_COLOR = '#00000000';
const MEDIA_DEVICE_BLINK_FEATURES = 'EnumerateDevices,AudioOutputDevices';
const ACTIVE_ALLOW_TRANSPARENCY_RENDERER_ARG = '--fluxer-active-allow-transparency=1';
const ACTIVE_USE_NATIVE_TITLEBAR_RENDERER_ARG = '--fluxer-active-use-native-titlebar=1';
const THEME_STUDIO_POPOUT_WINDOW_NAME = 'fluxer_theme_studio';
const THEME_STUDIO_POPOUT_PATHNAME = '/theme-studio';
const THEME_STUDIO_POPOUT_TITLE = 'Fluxer | Theme Studio';
const THEME_STUDIO_POPOUT_MIN_WIDTH = 900;
const THEME_STUDIO_POPOUT_MIN_HEIGHT = 620;
export const THEME_STUDIO_POPOUT_KEY = THEME_STUDIO_POPOUT_WINDOW_NAME;
const VOICE_POPOUT_WINDOW_NAME_PREFIX = 'fluxer-voice-popout:';
const VOICE_POPOUT_WINDOW_NAME_LENGTH_MAX = 256;
const VOICE_POPOUT_MIN_WIDTH = 360;
const VOICE_POPOUT_MIN_HEIGHT = 240;
const VOICE_POPOUT_WINDOWS_MAX = 8;
const CUSTOM_TITLEBAR_HEIGHT_MAC = 32;
const CUSTOM_TITLEBAR_TRAFFIC_LIGHT_DIAMETER = 14;
const CUSTOM_TITLEBAR_TRAFFIC_LIGHT_POSITION = {
	x: 12,
	y: Math.round((CUSTOM_TITLEBAR_HEIGHT_MAC - CUSTOM_TITLEBAR_TRAFFIC_LIGHT_DIAMETER) / 2),
};
const webAuthnDeviceTypes = new Set(['hid', 'usb', 'serial', 'bluetooth']);
const webAuthnPermissionTypes = new Set(['hid', 'usb', 'serial', 'bluetooth']);
const trustedRendererPermissionTypes = new Set([
	'media',
	'display-capture',
	'notifications',
	'fullscreen',
	'pointerLock',
	'speaker-selection',
	'clipboard-sanitized-write',
]);
const POPOUT_NAMESPACE = 'fluxer_';

export function isTrustedOrigin(url?: string): boolean {
	return isLocalAppRendererDocumentURL(url ?? null);
}

function getSanitizedPath(rawUrl: string): string | null {
	try {
		return new URL(rawUrl).pathname;
	} catch (error) {
		log.warn('Invalid URL for path check', {rawUrl, error});
		return null;
	}
}

function preventUntrustedNavigation(event: Electron.Event, url: string, isMainFrame: boolean): void {
	if (isLocalAppURL(url) && !isLocalAppRendererDocumentURL(url)) {
		logger.warn('Blocked a navigation to a reserved local app path', {url, isMainFrame});
		event.preventDefault();
		return;
	}
	if (isMainFrame && !isTrustedOrigin(url)) {
		event.preventDefault();
	}
}

function preventUntrustedChildNavigation(event: Electron.Event, url: string, isMainFrame: boolean): void {
	if (isMainFrame && url === 'about:blank') return;
	preventUntrustedNavigation(event, url, isMainFrame);
}

function attachChildWindowGuards(webContents: Electron.WebContents): void {
	getDesktopLocalAppAuthorization().authorize(webContents);
	webContents.on('will-navigate', (event, url) => {
		preventUntrustedChildNavigation(event, url, true);
	});
	webContents.on('will-frame-navigate', (event) => {
		preventUntrustedChildNavigation(event, event.url, event.isMainFrame);
	});
	webContents.setWindowOpenHandler(({url}) => {
		openExternalDeduped(url).catch((error) => {
			log.warn('Failed to open external URL from a popout window-open:', error);
		});
		return {action: 'deny'};
	});
}

interface WindowBounds {
	x: number;
	y: number;
	width: number;
	height: number;
	isMaximized: boolean;
}

interface CreateWindowOptions {
	startHidden?: boolean;
}

let mainWindow: BrowserWindow | null = null;
let windowStateFile: string;
let isQuitting = false;
let initialUseNativeTitleBar: boolean | null = null;
let initialAllowTransparency: boolean | null = null;
let themeStudioPopoutWindow: BrowserWindow | null = null;
let lastRestorableMainWindowMaximized = false;
let mainWindowRendererGone = false;
let closingMainWindowForUpdate = false;
const SHELL_PAGE_URL_PREFIX = 'file:';
let mainWindowTakeover: (() => void) | null = null;
const takeoverEndedListeners = new Set<() => void>();
let pendingMainWindowReveal: {readonly window: BrowserWindow; readonly requestShow: () => void} | null = null;

const maximizeChangeForwarders = new WeakSet<BrowserWindow>();
const voicePopoutWindows = new Map<string, BrowserWindow>();
const windowsHtmlFullscreenStates = new WeakMap<
	BrowserWindow,
	{resizable: boolean; bounds: Bounds; isMaximized: boolean; customChromeGuardActive: boolean}
>();
let lastGoodWindowBounds: Bounds | null = null;

function getWindowStateFile(): string {
	if (!windowStateFile) {
		const userDataPath = app.getPath('userData');
		windowStateFile = path.join(userDataPath, 'window-state.json');
	}
	return windowStateFile;
}

interface Bounds {
	x: number;
	y: number;
	width: number;
	height: number;
}

function boundsIntersect(a: Bounds, b: Bounds): boolean {
	const aRight = a.x + a.width;
	const bRight = b.x + b.width;
	const aBottom = a.y + a.height;
	const bBottom = b.y + b.height;
	const overlapX = Math.min(aRight, bRight) - Math.max(a.x, b.x);
	const overlapY = Math.min(aBottom, bBottom) - Math.max(a.y, b.y);
	return overlapX > 0 && overlapY > 0;
}

function findVisibleDisplay(displays: Array<Electron.Display>, bounds: Bounds): Electron.Display | undefined {
	return displays.find((display) => {
		const visibleArea = {
			x: display.workArea.x + VISIBILITY_MARGIN,
			y: display.workArea.y + VISIBILITY_MARGIN,
			width: display.workArea.width - 2 * VISIBILITY_MARGIN,
			height: display.workArea.height - 2 * VISIBILITY_MARGIN,
		};
		return boundsIntersect(bounds, visibleArea);
	});
}

function getDefaultBoundsForDisplay(display: Electron.Display): Bounds {
	const {workArea} = display;
	const width = Math.min(DEFAULT_WINDOW_WIDTH, workArea.width);
	const height = Math.min(DEFAULT_WINDOW_HEIGHT, workArea.height);
	return {
		x: Math.round(workArea.x + (workArea.width - width) / 2),
		y: Math.round(workArea.y + (workArea.height - height) / 2),
		width,
		height,
	};
}

function savedBoundsExceedWorkArea(bounds: Bounds, display: Electron.Display): boolean {
	const {workArea} = display;
	return (
		bounds.x < workArea.x ||
		bounds.y < workArea.y ||
		bounds.x + bounds.width > workArea.x + workArea.width ||
		bounds.y + bounds.height > workArea.y + workArea.height
	);
}

function sanitizeLoadedWindowBounds(bounds: WindowBounds, display: Electron.Display): WindowBounds {
	if (!bounds.isMaximized || !savedBoundsExceedWorkArea(bounds, display)) {
		return bounds;
	}
	const defaultBounds = getDefaultBoundsForDisplay(display);
	const sanitizedBounds = {
		...defaultBounds,
		isMaximized: true,
	};
	log.info('Ignoring display-sized normal bounds from saved maximized window state:', {
		savedBounds: bounds,
		sanitizedBounds,
	});
	return sanitizedBounds;
}

function ensureWindowOnScreen(window: BrowserWindow): void {
	const bounds = window.getBounds();
	const displays = screen.getAllDisplays();
	const visibleDisplay = findVisibleDisplay(displays, bounds);
	if (!visibleDisplay && displays.length > 0) {
		const primaryBounds = displays[0].bounds;
		const correctedBounds = {
			x: primaryBounds.x,
			y: primaryBounds.y,
			width: Math.min(bounds.width, primaryBounds.width),
			height: Math.min(bounds.height, primaryBounds.height),
		};
		log.warn('Window is off-screen, repositioning to primary display:', correctedBounds);
		window.setBounds(correctedBounds);
	}
}

function loadWindowBounds(): Partial<WindowBounds> | null {
	if (shouldIgnoreWindowStateForLaunch(process.argv)) {
		log.info('Ignoring saved window bounds for this launch');
		return null;
	}
	if (!getDesktopWindowBehaviorSettings().rememberWindowState) {
		return null;
	}
	try {
		const filePath = getWindowStateFile();
		if (fs.existsSync(filePath)) {
			const data = fs.readFileSync(filePath, 'utf-8');
			const bounds = JSON.parse(data) as WindowBounds;
			const displays = screen.getAllDisplays();
			const display = findVisibleDisplay(displays, bounds);
			if (display != null) {
				const sanitizedBounds = sanitizeLoadedWindowBounds(bounds, display);
				log.info('Restored window bounds:', sanitizedBounds);
				return sanitizedBounds;
			} else {
				log.warn('Saved window position is off-screen, using defaults');
			}
		}
	} catch (error) {
		log.error('Failed to load window bounds:', error);
	}
	return null;
}

function saveWindowBounds(): void {
	if (!mainWindow) return;
	if (!getDesktopWindowBehaviorSettings().rememberWindowState) return;
	if (windowsHtmlFullscreenStates.has(mainWindow)) return;
	try {
		const bounds = mainWindow.getNormalBounds();
		const isMaximized = mainWindow.isMinimized() ? lastRestorableMainWindowMaximized : mainWindow.isMaximized();
		lastRestorableMainWindowMaximized = isMaximized;
		const windowState: WindowBounds = {
			x: bounds.x,
			y: bounds.y,
			width: bounds.width,
			height: bounds.height,
			isMaximized,
		};
		lastGoodWindowBounds = bounds;
		const filePath = getWindowStateFile();
		fs.writeFileSync(filePath, JSON.stringify(windowState, null, 2), 'utf-8');
		log.debug('Saved window bounds:', windowState);
	} catch (error) {
		log.error('Failed to save window bounds:', error);
	}
}

export function clearSavedWindowBounds(): void {
	try {
		const filePath = getWindowStateFile();
		if (fs.existsSync(filePath)) {
			fs.unlinkSync(filePath);
			log.info('Cleared saved window bounds');
		}
	} catch (error) {
		log.error('Failed to clear saved window bounds:', error);
	}
}

function shouldHideMainWindowOnClose(): boolean {
	if (process.platform === 'darwin') {
		return true;
	}
	const settings = getDesktopWindowBehaviorSettings();
	return hasActiveDesktopTray() && settings.showTrayIcon && settings.closeToTray;
}

function shouldHideMainWindowOnMinimize(): boolean {
	const settings = getDesktopWindowBehaviorSettings();
	return hasActiveDesktopTray() && settings.showTrayIcon && settings.minimizeToTray;
}

export function getMainWindow(): BrowserWindow | null {
	return mainWindow;
}

export function getActiveUseNativeTitleBar(): boolean {
	return initialUseNativeTitleBar ?? false;
}

export function getActiveAllowTransparency(): boolean {
	return initialAllowTransparency ?? false;
}

function isAliveWindow(window: BrowserWindow | null): window is BrowserWindow {
	return Boolean(window && !window.isDestroyed());
}

function reloadMainWindowRendererAfterGone(reason: string, details?: Electron.RenderProcessGoneDetails): void {
	if (!isAliveWindow(mainWindow)) return;
	if (mainWindow.webContents.isDestroyed()) return;
	mainWindowRendererGone = false;
	logger.warn('Reloading main window renderer after termination', {reason, details});
	mainWindow.webContents.reloadIgnoringCache();
}

function recoverMainWindowRendererBeforeShow(reason: string): void {
	if (!mainWindowRendererGone) return;
	if (!isAliveWindow(mainWindow)) return;
	if (mainWindow.webContents.isDestroyed() || mainWindow.webContents.isLoadingMainFrame()) return;
	reloadMainWindowRendererAfterGone(reason);
}

export function isAppDocumentWindowContents(contents: Electron.WebContents): boolean {
	return [mainWindow, themeStudioPopoutWindow].some(
		(window) => isAliveWindow(window) && window.webContents === contents,
	);
}

function getThemeStudioPopoutWindow(): BrowserWindow | null {
	if (isAliveWindow(themeStudioPopoutWindow)) {
		return themeStudioPopoutWindow;
	}
	themeStudioPopoutWindow = null;
	for (const window of BrowserWindow.getAllWindows()) {
		if (window === mainWindow || window.isDestroyed()) continue;
		if (getSanitizedPath(window.webContents.getURL()) === THEME_STUDIO_POPOUT_PATHNAME) {
			trackThemeStudioPopoutWindow(window);
			return window;
		}
	}
	return null;
}

export function focusWindow(window: BrowserWindow): void {
	if (window.isMinimized()) {
		window.restore();
	}
	if (!window.isVisible()) {
		window.show();
	}
	try {
		window.moveTop();
	} catch (error) {
		logger.warn('Failed to move window to top before focusing', error);
	}
	window.focus();
}

export function focusThemeStudioPopoutWindow(): boolean {
	const window = getThemeStudioPopoutWindow();
	if (!window) {
		return false;
	}
	focusWindow(window);
	return true;
}

export function closeThemeStudioPopoutWindow(): boolean {
	const window = getThemeStudioPopoutWindow();
	if (!window) {
		return false;
	}
	window.close();
	return true;
}

function isVoicePopoutWindowName(frameName: string | undefined): frameName is string {
	if (typeof frameName !== 'string') return false;
	if (!frameName.startsWith(VOICE_POPOUT_WINDOW_NAME_PREFIX)) return false;
	return frameName.length <= VOICE_POPOUT_WINDOW_NAME_LENGTH_MAX;
}

function pruneDestroyedVoicePopoutWindows(): void {
	for (const [key, window] of voicePopoutWindows) {
		if (!isAliveWindow(window)) {
			voicePopoutWindows.delete(key);
		}
	}
}

function hasVoicePopoutCapacity(): boolean {
	pruneDestroyedVoicePopoutWindows();
	return voicePopoutWindows.size < VOICE_POPOUT_WINDOWS_MAX;
}

function trackVoicePopoutWindow(key: string, window: BrowserWindow): void {
	const existing = voicePopoutWindows.get(key);
	if (existing && existing !== window && isAliveWindow(existing)) {
		existing.close();
	}
	voicePopoutWindows.set(key, window);
	window.setMinimumSize(VOICE_POPOUT_MIN_WIDTH, VOICE_POPOUT_MIN_HEIGHT);
	forwardMaximizeChanges(window);
	window.once('closed', () => {
		if (voicePopoutWindows.get(key) === window) {
			voicePopoutWindows.delete(key);
		}
	});
}

function getVoicePopoutWindow(key: string): BrowserWindow | null {
	const window = voicePopoutWindows.get(key) ?? null;
	if (isAliveWindow(window)) {
		return window;
	}
	if (window) {
		voicePopoutWindows.delete(key);
	}
	return null;
}

export function setVoicePopoutAlwaysOnTop(key: string, flag: boolean): boolean {
	const window = getVoicePopoutWindow(key);
	if (!window) {
		return false;
	}
	window.setAlwaysOnTop(flag);
	return true;
}

export function setThemeStudioPopoutAlwaysOnTop(flag: boolean): boolean {
	const window = getThemeStudioPopoutWindow();
	if (!window) {
		return false;
	}
	window.setAlwaysOnTop(flag);
	return true;
}

export function focusVoicePopoutWindow(key: string): boolean {
	const window = getVoicePopoutWindow(key);
	if (!window) {
		return false;
	}
	focusWindow(window);
	return true;
}

function getEffectiveUseNativeTitleBar(settings: DesktopWindowBehaviorSettings): boolean {
	if (process.platform === 'darwin') return false;
	return settings.useNativeTitleBar && !settings.allowTransparency;
}

function getWindowBackgroundColor(allowTransparency: boolean): string {
	if (allowTransparency) {
		return TRANSPARENT_WINDOW_BACKGROUND_COLOR;
	}
	return THEME_WINDOW_BACKGROUND_COLORS[getPrebootTheme() ?? ''] ?? DEFAULT_WINDOW_BACKGROUND_COLOR;
}

function getWindowShadowOptions(
	allowTransparency: boolean,
): Pick<Electron.BrowserWindowConstructorOptions, 'hasShadow'> {
	if (process.platform !== 'linux' || !allowTransparency) return {};
	return {hasShadow: false};
}

function enterWindowsHtmlFullscreenChromeGuard(window: BrowserWindow): void {
	if (process.platform !== 'win32') return;
	if (windowsHtmlFullscreenStates.has(window)) return;
	const customChromeGuardActive = !getActiveUseNativeTitleBar();
	const bounds = lastGoodWindowBounds ?? window.getNormalBounds();
	const isMaximized = window.isMaximized();
	windowsHtmlFullscreenStates.set(window, {
		resizable: window.isResizable(),
		bounds,
		isMaximized,
		customChromeGuardActive,
	});
	if (!customChromeGuardActive) return;
	window.setBackgroundColor('#000000');
	window.setResizable(false);
}

function restoreWindowsHtmlFullscreenBounds(
	window: BrowserWindow,
	previous: {bounds: Bounds; isMaximized: boolean},
): void {
	if (!isAliveWindow(window) || windowsHtmlFullscreenStates.has(window)) return;
	if (previous.isMaximized || window.isMaximized()) return;
	window.setBounds(previous.bounds);
	saveWindowBounds();
}

function leaveWindowsHtmlFullscreenChromeGuard(window: BrowserWindow): void {
	if (process.platform !== 'win32') return;
	const previous = windowsHtmlFullscreenStates.get(window);
	if (!previous) return;
	windowsHtmlFullscreenStates.delete(window);
	if (previous.customChromeGuardActive) {
		window.setResizable(previous.resizable);
		window.setBackgroundColor(getWindowBackgroundColor(getActiveAllowTransparency()));
	}
	restoreWindowsHtmlFullscreenBounds(window, previous);
}

function installHtmlFullscreenChromeGuard(window: BrowserWindow): void {
	window.on('enter-html-full-screen', () => {
		enterWindowsHtmlFullscreenChromeGuard(window);
	});
	window.on('leave-html-full-screen', () => {
		leaveWindowsHtmlFullscreenChromeGuard(window);
	});
	window.once('closed', () => {
		windowsHtmlFullscreenStates.delete(window);
	});
}

function getRendererAdditionalArguments(
	allowTransparency: boolean,
	useNativeTitleBar: boolean,
	allowNativeGateway: boolean,
): Array<string> {
	const args: Array<string> = [];
	if (allowTransparency) args.push(ACTIVE_ALLOW_TRANSPARENCY_RENDERER_ARG);
	if (useNativeTitleBar) args.push(ACTIVE_USE_NATIVE_TITLEBAR_RENDERER_ARG);
	if (allowNativeGateway && isNativeGatewayAvailable()) args.push(NATIVE_GATEWAY_TRANSPORT_AVAILABLE_RENDERER_ARG);
	return args;
}

function getDevToolsOptions(options: {forceDetach?: boolean} = {}): Electron.OpenDevToolsOptions | undefined {
	return options.forceDetach || getActiveAllowTransparency() ? {mode: 'detach', activate: true} : undefined;
}

function openWindowDevTools(window: BrowserWindow, options?: {forceDetach?: boolean}): void {
	window.webContents.openDevTools(getDevToolsOptions(options));
}

export function toggleWindowDevTools(window: BrowserWindow): void {
	if (window.webContents.isDevToolsOpened()) {
		window.webContents.closeDevTools();
		return;
	}
	openWindowDevTools(window);
}

function forwardMaximizeChanges(window: BrowserWindow): void {
	if (maximizeChangeForwarders.has(window)) {
		return;
	}
	maximizeChangeForwarders.add(window);
	window.on('maximize', () => {
		window.webContents.send('window-maximize-change', true);
	});
	window.on('unmaximize', () => {
		window.webContents.send('window-maximize-change', false);
	});
}

function trackThemeStudioPopoutWindow(window: BrowserWindow): void {
	themeStudioPopoutWindow = window;
	window.setAlwaysOnTop(true);
	window.setTitle(THEME_STUDIO_POPOUT_TITLE);
	window.setMinimumSize(THEME_STUDIO_POPOUT_MIN_WIDTH, THEME_STUDIO_POPOUT_MIN_HEIGHT);
	forwardMaximizeChanges(window);
	window.once('closed', () => {
		if (themeStudioPopoutWindow === window) {
			themeStudioPopoutWindow = null;
		}
	});
}

export function desktopUseNativeTitleBarPendingRestart(): boolean {
	if (process.platform === 'darwin') return false;
	if (initialUseNativeTitleBar === null) return false;
	return getEffectiveUseNativeTitleBar(getDesktopWindowBehaviorSettings()) !== initialUseNativeTitleBar;
}

export function desktopTransparencyPendingRestart(): boolean {
	if (initialAllowTransparency === null) return false;
	return getDesktopWindowBehaviorSettings().allowTransparency !== initialAllowTransparency;
}

function getLinuxWindowIconPath(): string | null {
	const baseIconName = '512x512.png';
	const candidatePaths = [
		path.join(process.resourcesPath, 'icons', baseIconName),
		path.join(process.resourcesPath, baseIconName),
		path.join(path.dirname(app.getPath('exe')), baseIconName),
	];
	for (const candidatePath of candidatePaths) {
		if (fs.existsSync(candidatePath)) {
			return candidatePath;
		}
	}
	return null;
}

function getVoicePopoutWindowOptions(): Electron.BrowserWindowConstructorOptions {
	const isMac = process.platform === 'darwin';
	const isLinux = process.platform === 'linux';
	const options: Electron.BrowserWindowConstructorOptions = {
		...getTitleBarWindowOptions(getActiveUseNativeTitleBar()),
		minWidth: VOICE_POPOUT_MIN_WIDTH,
		minHeight: VOICE_POPOUT_MIN_HEIGHT,
		...(isMac ? {trafficLightPosition: CUSTOM_TITLEBAR_TRAFFIC_LIGHT_POSITION} : {}),
		backgroundColor: getWindowBackgroundColor(false),
		transparent: false,
		...getWindowShadowOptions(false),
		autoHideMenuBar: true,
		show: true,
	};
	if (isLinux) {
		const iconPath = getLinuxWindowIconPath();
		if (iconPath) {
			options.icon = iconPath;
		}
	}
	return options;
}

function getTitleBarWindowOptions(
	useNativeTitleBar: boolean,
): Pick<Electron.BrowserWindowConstructorOptions, 'titleBarStyle' | 'titleBarOverlay' | 'frame'> {
	const isMac = process.platform === 'darwin';
	const isWindows = process.platform === 'win32';
	const useCustomChrome = isMac || !useNativeTitleBar;
	return {
		titleBarStyle: useCustomChrome ? 'hidden' : undefined,
		titleBarOverlay: isWindows && useCustomChrome ? false : undefined,
		frame: isMac ? true : !useCustomChrome,
	};
}

function getSharedWebPreferences(
	allowTransparency: boolean,
	useNativeTitleBar: boolean,
	allowNativeGateway = false,
): Electron.WebPreferences {
	return {
		preload: getDesktopDistributionPath('preload', 'index.cjs'),
		enableBlinkFeatures: MEDIA_DEVICE_BLINK_FEATURES,
		contextIsolation: true,
		nodeIntegration: false,
		sandbox: false,
		webSecurity: true,
		allowRunningInsecureContent: false,
		spellcheck: process.platform !== 'linux',
		transparent: allowTransparency,
		additionalArguments: getRendererAdditionalArguments(allowTransparency, useNativeTitleBar, allowNativeGateway),
		v8CacheOptions: shouldDisableV8CodeCache(process.argv) ? 'none' : 'code',
	};
}

export function createWindow(options: CreateWindowOptions = {}): BrowserWindow {
	const startedAt = Date.now();
	const logPhase = (phase: string): void => {
		logger.info('Create window phase completed', {phase, elapsedMs: Date.now() - startedAt});
	};
	const primaryDisplay = screen.getPrimaryDisplay();
	const {width: screenWidth, height: screenHeight} = primaryDisplay.workAreaSize;
	const savedBounds = loadWindowBounds();
	lastRestorableMainWindowMaximized = Boolean(savedBounds?.isMaximized);
	logPhase('bounds');
	const windowWidth = savedBounds?.width ?? Math.min(DEFAULT_WINDOW_WIDTH, screenWidth);
	const windowHeight = savedBounds?.height ?? Math.min(DEFAULT_WINDOW_HEIGHT, screenHeight);
	const isMac = process.platform === 'darwin';
	const isLinux = process.platform === 'linux';
	const desktopWindowBehavior = getDesktopWindowBehaviorSettings();
	const allowTransparency = desktopWindowBehavior.allowTransparency;
	const useNativeTitleBar = getEffectiveUseNativeTitleBar(desktopWindowBehavior);
	const acceptFirstMouseOnFocus = isMac;
	initialUseNativeTitleBar = useNativeTitleBar;
	initialAllowTransparency = allowTransparency;
	const windowOptions: Electron.BrowserWindowConstructorOptions = {
		width: windowWidth,
		height: windowHeight,
		minWidth: MIN_WINDOW_WIDTH,
		minHeight: MIN_WINDOW_HEIGHT,
		show: false,
		backgroundColor: getWindowBackgroundColor(allowTransparency),
		transparent: allowTransparency,
		...getWindowShadowOptions(allowTransparency),
		...getTitleBarWindowOptions(useNativeTitleBar),
		...(isMac ? {trafficLightPosition: CUSTOM_TITLEBAR_TRAFFIC_LIGHT_POSITION} : {}),
		acceptFirstMouse: acceptFirstMouseOnFocus,
		webPreferences: getSharedWebPreferences(allowTransparency, useNativeTitleBar, true),
	};
	if (isLinux) {
		const iconPath = getLinuxWindowIconPath();
		if (iconPath) {
			windowOptions.icon = iconPath;
		}
	}
	if (savedBounds?.x !== undefined && savedBounds?.y !== undefined) {
		windowOptions.x = savedBounds.x;
		windowOptions.y = savedBounds.y;
	} else {
		windowOptions.center = true;
	}
	mainWindow = new BrowserWindow(windowOptions);
	signalMainWindowCreated(mainWindow);
	mainWindowRendererGone = false;
	installHtmlFullscreenChromeGuard(mainWindow);
	lastGoodWindowBounds = mainWindow.getNormalBounds();
	logPhase('browser-window');
	let pendingMaximize = Boolean(savedBounds?.isMaximized);
	const applyPendingMaximize = (window: BrowserWindow): void => {
		if (!pendingMaximize) return;
		pendingMaximize = false;
		window.maximize();
	};
	const createdWindow = mainWindow;
	createdWindow.once('show', () => applyPendingMaximize(createdWindow));
	let revealShowsWindow = !options.startHidden;
	const releasePendingReveal = (): void => {
		if (pendingMainWindowReveal?.window === createdWindow) {
			pendingMainWindowReveal = null;
		}
	};
	const revealGate = new MainWindowRevealGate({
		onReveal: (reason) => {
			releasePendingReveal();
			logger.info('Main window revealed', {reason, revealShowsWindow, elapsedMs: Date.now() - startedAt});
			if (reason !== MainWindowRevealReason.CONTENT_PAINTED) {
				logger.warn('The renderer never reported painted content, revealing the main window anyway', {reason});
			}
			if (!revealShowsWindow) {
				signalMainWindowReady();
				return;
			}
			if (!isAliveWindow(createdWindow) || createdWindow.isVisible()) return;
			applyPendingMaximize(createdWindow);
			createdWindow.show();
		},
	});
	pendingMainWindowReveal = {
		window: createdWindow,
		requestShow: () => {
			revealShowsWindow = true;
		},
	};
	const onFirstContentPainted = (event: Electron.IpcMainEvent): void => {
		if (createdWindow.isDestroyed() || event.sender !== createdWindow.webContents) return;
		if (event.senderFrame == null || event.senderFrame.parent != null) return;
		revealGate.markContentPainted();
	};
	ipcMain.on(DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL, onFirstContentPainted);
	createdWindow.once('closed', () => {
		ipcMain.removeListener(DESKTOP_FIRST_CONTENT_PAINTED_CHANNEL, onFirstContentPainted);
		releasePendingReveal();
		revealGate.dispose();
	});
	createdWindow.once('show', () => {
		releasePendingReveal();
		revealGate.dispose();
		signalMainWindowReady();
	});
	createdWindow.once('ready-to-show', () => revealGate.markReadyToShow());
	revealGate.start();
	let saveTimeout: NodeJS.Timeout | null = null;
	const debouncedSave = () => {
		if (saveTimeout) clearTimeout(saveTimeout);
		saveTimeout = setTimeout(() => {
			saveWindowBounds();
		}, 500);
	};
	mainWindow.on('resize', debouncedSave);
	mainWindow.on('move', debouncedSave);
	let liveResizeActive = false;
	let liveResizeIdleTimeout: NodeJS.Timeout | null = null;
	const sendLiveResizeState = (active: boolean) => {
		if (liveResizeActive === active) return;
		liveResizeActive = active;
		if (!mainWindow || mainWindow.webContents.isDestroyed()) return;
		mainWindow.webContents.send('window-live-resize-change', active);
	};
	const endLiveResize = () => {
		if (liveResizeIdleTimeout) {
			clearTimeout(liveResizeIdleTimeout);
			liveResizeIdleTimeout = null;
		}
		sendLiveResizeState(false);
	};
	mainWindow.on('will-resize', () => {
		sendLiveResizeState(true);
		if (liveResizeIdleTimeout) clearTimeout(liveResizeIdleTimeout);
		liveResizeIdleTimeout = setTimeout(endLiveResize, LIVE_RESIZE_IDLE_MS);
	});
	mainWindow.on('resized', endLiveResize);
	mainWindow.on('blur', endLiveResize);
	mainWindow.on('maximize', () => {
		lastRestorableMainWindowMaximized = true;
		saveWindowBounds();
		mainWindow?.webContents.send('window-maximize-change', true);
	});
	mainWindow.on('unmaximize', () => {
		const window = mainWindow;
		setTimeout(() => {
			if (mainWindow !== window) return;
			if (!isAliveWindow(window)) return;
			if (!window.isMinimized()) {
				lastRestorableMainWindowMaximized = false;
			}
			saveWindowBounds();
		}, 0);
		mainWindow?.webContents.send('window-maximize-change', false);
	});
	mainWindow.on('minimize', () => {
		if (!isQuitting && shouldHideMainWindowOnMinimize()) {
			mainWindow?.hide();
			refreshDesktopTrayMenu();
		}
	});
	mainWindow.on('restore', refreshDesktopTrayMenu);
	mainWindow.on('show', refreshDesktopTrayMenu);
	mainWindow.on('hide', refreshDesktopTrayMenu);
	mainWindow.on('close', (event) => {
		if (saveTimeout) clearTimeout(saveTimeout);
		endLiveResize();
		saveWindowBounds();
		if (!isQuitting && !closingMainWindowForUpdate && shouldHideMainWindowOnClose()) {
			event.preventDefault();
			logger.info(
				process.platform === 'darwin'
					? 'Window close hid the app. Use Quit to terminate the process'
					: 'Window close hid the app to the tray. Use Quit to terminate the process',
			);
			if (mainWindow) hideWindowLeavingFullScreen(mainWindow);
			refreshDesktopTrayMenu();
		}
	});
	mainWindow.on('closed', () => {
		mainWindow = null;
	});
	mainWindow.setMenuBarVisibility(false);
	if (process.platform === 'win32') {
		mainWindow.on('show', () => {
			refreshWindowsBadgeOverlay(mainWindow);
		});
	}
	const webContents = mainWindow.webContents;
	const session = webContents.session;
	runtimeSecurity.install(session);
	let rendererGoneReloaded = false;
	let lastRendererGoneAt = 0;
	if (shouldOpenDevToolsOnLaunch(process.argv)) {
		logger.info('Opening DevTools for this launch');
		setTimeout(() => {
			if (isAliveWindow(mainWindow)) {
				openWindowDevTools(mainWindow, {forceDetach: true});
			}
		}, 0);
	}
	if (shouldForwardRendererConsoleToMainLog(process.argv)) {
		webContents.on('console-message', (event, _legacyLevel, legacyMessage, legacyLine, legacySourceId) => {
			const details = event as Electron.Event<Electron.WebContentsConsoleMessageEventParams>;
			const level = details.level ?? 'info';
			const message = details.message || legacyMessage;
			const metadata = {
				level,
				sourceId: details.sourceId || legacySourceId || undefined,
				lineNumber: details.lineNumber || legacyLine || undefined,
			};
			if (level === 'error') {
				logger.error('Renderer console:', message, metadata);
			} else if (level === 'warning') {
				logger.warn('Renderer console:', message, metadata);
			} else if (level === 'debug') {
				logger.debug('Renderer console:', message, metadata);
			} else {
				logger.info('Renderer console:', message, metadata);
			}
		});
	}
	webContents.on('preload-error', (_event, preloadPath, error) => {
		logger.error('Preload script failed:', {preloadPath, error});
	});
	webContents.on('unresponsive', () => {
		logger.warn('Renderer became unresponsive', {url: webContents.getURL()});
	});
	webContents.on('responsive', () => {
		logger.info('Renderer became responsive', {url: webContents.getURL()});
	});
	webContents.on('render-process-gone', (_event, details) => {
		logger.error('Render process gone', {url: webContents.getURL(), details});
		drainPendingDisplayMediaRequests(`render-process-gone:${details.reason}`);
		resetStreamingPriority();
		const now = Date.now();
		const repeated = now - lastRendererGoneAt < RENDERER_GONE_REPEAT_WINDOW_MS;
		lastRendererGoneAt = now;
		mainWindowRendererGone = true;
		const action = getMainWindowRendererGoneAction(details, {
			platform: process.platform,
			isQuitting,
			isMainWindowHidden: isAliveWindow(mainWindow) && !mainWindow.isVisible(),
			closeToTrayEnabled: shouldHideMainWindowOnClose(),
			reloadedRecently: rendererGoneReloaded && repeated,
		});
		if (action === 'quit') {
			logger.error('Quitting after renderer process termination', {details, repeated});
			app.quit();
			return;
		}
		if (action === 'defer-reload') {
			logger.warn('Deferring hidden main window renderer recovery until the window is shown', {details, repeated});
			return;
		}
		if (action === 'reload' && isAliveWindow(mainWindow)) {
			rendererGoneReloaded = true;
			reloadMainWindowRendererAfterGone(`render-process-gone:${details.reason}`, details);
		}
	});
	webContents.on('did-start-navigation', (_event, _url, isSameDoc, isMainFrame) => {
		if (isMainFrame && !isSameDoc) {
			drainPendingDisplayMediaRequests('did-start-navigation');
			resetStreamingPriority();
		}
	});
	registerSpellcheck(webContents);
	session.setDevicePermissionHandler(({deviceType, origin}) => {
		if (!origin || !isTrustedOrigin(origin)) {
			return false;
		}
		return webAuthnDeviceTypes.has(deviceType);
	});
	session.on('select-hid-device', (event, details, callback) => {
		event.preventDefault();
		logger.warn('Cancelling WebHID device selection request', {origin: details.frame?.url});
		callback();
	});
	session.setPermissionRequestHandler((webContents, permission, callback, details) => {
		const origin = details.requestingUrl || webContents.getURL();
		const trusted = isTrustedOrigin(origin);
		const permissionName = String(permission);
		if (!trusted) {
			if (permissionName === 'fullscreen' && isTrustedOrigin(webContents.getURL())) {
				callback(true);
				return;
			}
			callback(false);
			return;
		}
		if (webAuthnPermissionTypes.has(permission)) {
			callback(true);
			return;
		}
		if (trustedRendererPermissionTypes.has(permissionName)) {
			callback(true);
			return;
		}
		callback(false);
	});
	session.setPermissionCheckHandler((webContents, permission, requestingOrigin, details) => {
		const origin = requestingOrigin || details?.requestingUrl || webContents?.getURL();
		const embeddingOrigin = details?.embeddingOrigin;
		const permissionName = String(permission);
		if (!webContents) return false;
		if (permissionName === 'fullscreen') {
			const topLevel = embeddingOrigin || webContents.getURL();
			if (isTrustedOrigin(topLevel)) {
				return true;
			}
		}
		if (!isTrustedOrigin(origin)) {
			return false;
		}
		if (embeddingOrigin && !isTrustedOrigin(embeddingOrigin)) {
			return false;
		}
		if (webAuthnPermissionTypes.has(permission)) {
			return true;
		}
		if (trustedRendererPermissionTypes.has(permissionName)) {
			return true;
		}
		return false;
	});
	registerDisplayMediaRequestHandler(session, webContents);
	getDesktopLocalAppAuthorization().authorize(webContents);
	logPhase('handlers');
	let appLoadFailurePrompt: AbortController | null = null;
	const dismissAppLoadFailurePrompt = () => {
		appLoadFailurePrompt?.abort();
		appLoadFailurePrompt = null;
	};
	const appLoadRetry = createAppLoadRetry({
		webContents,
		appUrl: resolveDesktopLandingUrl(app.getPath('userData')),
		logger,
		isTrustedUrl: isTrustedOrigin,
		onCommitted: dismissAppLoadFailurePrompt,
		onRepeatedFailure: (failure) => {
			const window = mainWindow;
			if (appLoadFailurePrompt || !isAliveWindow(window) || !window.isVisible()) return;
			const prompt = new AbortController();
			appLoadFailurePrompt = prompt;
			void dialog
				.showMessageBox(window, {
					type: 'warning',
					buttons: [t('desktop.appLoad.retry'), t('desktop.tray.quit')],
					defaultId: 0,
					cancelId: 0,
					title: t('desktop.appLoad.failedTitle'),
					message: t('desktop.appLoad.failedMessage'),
					detail: `${failure.errorDescription} (${failure.errorCode})\n${failure.url}`,
					noLink: true,
					signal: prompt.signal,
				})
				.then(({response}) => {
					if (appLoadFailurePrompt === prompt) appLoadFailurePrompt = null;
					if (prompt.signal.aborted || isQuitting) return;
					if (response === 1) {
						app.quit();
						return;
					}
					appLoadRetry.retryNow();
				});
		},
	});
	webContents.on('did-finish-load', () => {
		rendererGoneReloaded = false;
		mainWindowRendererGone = false;
	});
	appLoadRetry.start();
	logPhase('load-url-dispatched');
	webContents.on('will-navigate', (event, url) => {
		preventUntrustedNavigation(event, url, true);
	});
	webContents.on('will-frame-navigate', (event) => {
		preventUntrustedNavigation(event, event.url, event.isMainFrame);
	});
	webContents.on('did-create-window', (window, details) => {
		if (
			details.frameName === THEME_STUDIO_POPOUT_WINDOW_NAME &&
			getSanitizedPath(details.url) === THEME_STUDIO_POPOUT_PATHNAME
		) {
			trackThemeStudioPopoutWindow(window);
		}
		if (isVoicePopoutWindowName(details.frameName)) {
			trackVoicePopoutWindow(details.frameName, window);
		}
		attachChildWindowGuards(window.webContents);
	});
	webContents.setWindowOpenHandler(({url, frameName}) => {
		if (isVoicePopoutWindowName(frameName) && url === 'about:blank') {
			if (!hasVoicePopoutCapacity()) {
				logger.warn('Denied voice popout window: capacity reached', {frameName});
				return {action: 'deny'};
			}
			return {action: 'allow', overrideBrowserWindowOptions: getVoicePopoutWindowOptions()};
		}
		const pathname = getSanitizedPath(url);
		if (
			frameName?.startsWith(POPOUT_NAMESPACE) &&
			(pathname === '/popout' || pathname === '/quick-css-editor' || pathname === THEME_STUDIO_POPOUT_PATHNAME) &&
			isTrustedOrigin(url)
		) {
			const isThemeStudioPopout =
				frameName === THEME_STUDIO_POPOUT_WINDOW_NAME && pathname === THEME_STUDIO_POPOUT_PATHNAME;
			const isOpaqueChromePopout = pathname === '/quick-css-editor' || pathname === THEME_STUDIO_POPOUT_PATHNAME;
			const allowPopoutTransparency = allowTransparency && !isOpaqueChromePopout;
			const overrideBrowserWindowOptions: Electron.BrowserWindowConstructorOptions = {
				...getTitleBarWindowOptions(getActiveUseNativeTitleBar()),
				title: isThemeStudioPopout ? THEME_STUDIO_POPOUT_TITLE : undefined,
				minWidth: isThemeStudioPopout ? THEME_STUDIO_POPOUT_MIN_WIDTH : undefined,
				minHeight: isThemeStudioPopout ? THEME_STUDIO_POPOUT_MIN_HEIGHT : undefined,
				...(isMac ? {trafficLightPosition: CUSTOM_TITLEBAR_TRAFFIC_LIGHT_POSITION} : {}),
				backgroundColor: getWindowBackgroundColor(allowPopoutTransparency),
				transparent: allowPopoutTransparency,
				...getWindowShadowOptions(allowPopoutTransparency),
				show: true,
				webPreferences: getSharedWebPreferences(allowPopoutTransparency, getActiveUseNativeTitleBar()),
			};
			return {action: 'allow', overrideBrowserWindowOptions};
		}
		openExternalDeduped(url).catch((error) => {
			log.warn('Failed to open external URL from window-open:', error);
		});
		return {action: 'deny'};
	});
	return mainWindow;
}

export function showWindow(): void {
	if (mainWindowTakeover != null) {
		mainWindowTakeover();
		return;
	}
	if (mainWindow && pendingMainWindowReveal?.window === mainWindow && !mainWindow.isVisible()) {
		pendingMainWindowReveal.requestShow();
		return;
	}
	if (mainWindow) {
		cancelPendingFullScreenHide(mainWindow);
		if (mainWindow.isMinimized()) {
			mainWindow.restore();
		}
		ensureWindowOnScreen(mainWindow);
		recoverMainWindowRendererBeforeShow('show-window');
		if (process.platform === 'darwin') {
			try {
				app.dock?.show();
			} catch (error) {
				log.warn('[Window] Failed to show dock:', error);
			}
			try {
				app.focus({steal: true});
			} catch (error) {
				log.warn('[Window] Failed to focus app:', error);
			}
			try {
				mainWindow.setVisibleOnAllWorkspaces(true, {visibleOnFullScreen: true});
			} catch (error) {
				log.warn('[Window] Failed to set visible on all workspaces:', error);
			}
			mainWindow.show();
			mainWindow.focus();
			setTimeout(() => {
				if (!mainWindow || mainWindow.isDestroyed()) return;
				try {
					mainWindow.setVisibleOnAllWorkspaces(false);
				} catch (error) {
					log.warn('[Window] Failed to disable visible on all workspaces:', error);
				}
			}, 250);
		} else {
			mainWindow.show();
			mainWindow.focus();
		}
	}
}

export function hideWindow(): void {
	if (mainWindow) {
		hideWindowLeavingFullScreen(mainWindow);
	}
}

export function setQuitting(quitting: boolean): void {
	isQuitting = quitting;
}

export function beginMainWindowTakeover(focus: () => void): void {
	mainWindowTakeover = focus;
}

export function endMainWindowTakeover(): void {
	if (mainWindowTakeover == null) return;
	mainWindowTakeover = null;
	for (const listener of Array.from(takeoverEndedListeners)) {
		try {
			listener();
		} catch (error) {
			logger.error('A main window takeover listener threw', error);
		}
	}
}

export function onMainWindowTakeoverEnded(listener: () => void): () => void {
	takeoverEndedListeners.add(listener);
	return () => {
		takeoverEndedListeners.delete(listener);
	};
}

export function isMainWindowTakenOver(): boolean {
	return mainWindowTakeover != null;
}

export async function reloadMainWindowForUpdate(): Promise<boolean> {
	const window = mainWindow;
	if (!isAliveWindow(window) || window.webContents.isDestroyed()) {
		return false;
	}
	await closeAppWindowsForUpdate(window, {keepShellPages: true});
	if (window.isDestroyed() || window.webContents.isDestroyed()) {
		return false;
	}
	const committed = waitForReloadCommit(window.webContents);
	window.webContents.reloadIgnoringCache();
	if (await committed) {
		return true;
	}
	logger.warn('The main window did not start reloading for the update, replacing it', {
		timeoutMs: UPDATE_RELOAD_COMMIT_TIMEOUT_MS,
	});
	if (!window.isDestroyed()) {
		window.destroy();
	}
	return false;
}

function waitForReloadCommit(contents: Electron.WebContents): Promise<boolean> {
	return new Promise<boolean>((resolve) => {
		const finish = (committed: boolean): void => {
			clearTimeout(timer);
			contents.removeListener('did-navigate', onCommit);
			contents.removeListener('destroyed', onDestroyed);
			resolve(committed);
		};
		const onCommit = (): void => finish(true);
		const onDestroyed = (): void => finish(false);
		const timer = setTimeout(() => finish(false), UPDATE_RELOAD_COMMIT_TIMEOUT_MS);
		contents.once('did-navigate', onCommit);
		contents.once('destroyed', onDestroyed);
	});
}

export function hideAppWindowsForUpdate(keep: BrowserWindow): ReadonlyArray<BrowserWindow> {
	const hidden: Array<BrowserWindow> = [];
	for (const window of BrowserWindow.getAllWindows()) {
		if (window === keep || window.isDestroyed() || !window.isVisible()) continue;
		window.hide();
		hidden.push(window);
	}
	return hidden;
}

export function restoreAppWindowsAfterUpdate(hidden: ReadonlyArray<BrowserWindow>): void {
	for (const window of hidden) {
		if (window.isDestroyed()) continue;
		window.show();
	}
}

function showsShellPage(window: BrowserWindow): boolean {
	return !window.webContents.isDestroyed() && window.webContents.getURL().startsWith(SHELL_PAGE_URL_PREFIX);
}

export async function closeAppWindowsForUpdate(
	keep: BrowserWindow,
	{keepShellPages = false}: {readonly keepShellPages?: boolean} = {},
): Promise<void> {
	const closing = BrowserWindow.getAllWindows().filter(
		(window) => window !== keep && !window.isDestroyed() && !(keepShellPages && showsShellPage(window)),
	);
	closingMainWindowForUpdate = true;
	try {
		await Promise.all(
			closing.map(
				(window) =>
					new Promise<void>((resolve) => {
						let deadline: NodeJS.Timeout | null = null;
						window.once('closed', () => {
							if (deadline != null) clearTimeout(deadline);
							resolve();
						});
						if (!window.isClosable()) {
							window.destroy();
							return;
						}
						deadline = setTimeout(() => {
							logger.warn('A window did not close for the update in time, destroying it');
							if (!window.isDestroyed()) window.destroy();
						}, CLOSE_FOR_UPDATE_TIMEOUT_MS);
						window.webContents.once('will-prevent-unload', (event) => {
							event.preventDefault();
						});
						window.close();
					}),
			),
		);
	} finally {
		closingMainWindowForUpdate = false;
	}
}

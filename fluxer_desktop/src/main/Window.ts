// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {
	CANARY_APP_URL,
	DEFAULT_WINDOW_HEIGHT,
	DEFAULT_WINDOW_WIDTH,
	MIN_WINDOW_HEIGHT,
	MIN_WINDOW_WIDTH,
	STABLE_APP_URL,
} from '@electron/common/Constants';
import {getAppUrl, getCustomAppUrl, getDesktopWindowBehaviorSettings} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import type {DesktopWindowBehaviorSettings} from '@electron/common/Types';
import {
	shouldForwardRendererConsoleToMainLog,
	shouldIgnoreWindowStateForLaunch,
	shouldOpenDevToolsOnLaunch,
} from '@electron/main/DesktopDebugInfo';
import {hasActiveDesktopTray, refreshDesktopTrayMenu} from '@electron/main/DesktopTray';
import {drainPendingDisplayMediaRequests, registerDisplayMediaRequestHandler} from '@electron/main/DisplayMedia';
import {shouldDisableV8CodeCache} from '@electron/main/LaunchOptions';
import {openExternalDeduped} from '@electron/main/OpenExternal';
import {registerSpellcheck} from '@electron/main/Spellcheck';
import {resetStreamingPriority} from '@electron/main/StreamingPriority';
import {getMainWindowRendererGoneAction} from '@electron/main/WindowRendererLifecycle';
import {refreshWindowsBadgeOverlay} from '@electron/main/WindowsBadge';
import {app, BrowserWindow, screen} from 'electron';
import log from 'electron-log';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const logger = createChildLogger('Window');
const VISIBILITY_MARGIN = 32;
const INITIAL_APP_LOAD_RETRY_DELAY_MS = 1000;
const MAX_APP_LOAD_RETRY_DELAY_MS = 30000;
const RENDERER_GONE_REPEAT_WINDOW_MS = 30000;
const OPAQUE_WINDOW_BACKGROUND_COLOR = '#1a1a1a';
const TRANSPARENT_WINDOW_BACKGROUND_COLOR = '#00000000';
const MEDIA_DEVICE_BLINK_FEATURES = 'EnumerateDevices,AudioOutputDevices';
const ACTIVE_ALLOW_TRANSPARENCY_RENDERER_ARG = '--fluxer-active-allow-transparency=1';
const ACTIVE_USE_NATIVE_TITLEBAR_RENDERER_ARG = '--fluxer-active-use-native-titlebar=1';
const INSECURE_ORIGIN_RENDERER_ARG_PREFIX = '--unsafely-treat-insecure-origin-as-secure=';
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
const VOICE_POPOUT_TITLEBAR_HEIGHT_MAC = 28;
const VOICE_POPOUT_TRAFFIC_LIGHT_DIAMETER = 14;
const VOICE_POPOUT_TRAFFIC_LIGHT_POSITION = {
	x: 12,
	y: Math.round((VOICE_POPOUT_TITLEBAR_HEIGHT_MAC - VOICE_POPOUT_TRAFFIC_LIGHT_DIAMETER) / 2),
};
const trustedWebOrigins = new Set(
	[STABLE_APP_URL, CANARY_APP_URL]
		.map((url) => {
			try {
				return new URL(url).origin;
			} catch (error) {
				log.error('Invalid trusted origin URL', {url, error});
				return null;
			}
		})
		.filter(Boolean) as Array<string>,
);
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

function shouldRetryAppLoadFailure(errorCode: number): boolean {
	return errorCode < 0 && errorCode !== -3;
}

function getElectronLoadErrorCode(error: unknown): number | null {
	const message = error instanceof Error ? error.message : String(error);
	const match = /\(([-\d]+)\)/.exec(message);
	if (!match) return null;
	const value = Number.parseInt(match[1], 10);
	return Number.isFinite(value) ? value : null;
}

function getOrigin(url?: string): string | null {
	if (!url) return null;
	try {
		return new URL(url).origin;
	} catch (error) {
		log.warn('Invalid URL for origin check', {url, error});
		return null;
	}
}

function isTrustedOrigin(url?: string): boolean {
	const origin = getOrigin(url);
	if (!origin) return false;
	if (trustedWebOrigins.has(origin)) return true;
	const customUrl = getCustomAppUrl();
	if (customUrl) {
		try {
			return new URL(customUrl).origin === origin;
		} catch {
			return false;
		}
	}
	return false;
}

function getSanitizedPath(rawUrl: string): string | null {
	try {
		return new URL(rawUrl).pathname;
	} catch (error) {
		log.warn('Invalid URL for path check', {rawUrl, error});
		return null;
	}
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
let initialAcceptFirstMouseOnFocus: boolean | null = null;
let initialUseNativeTitleBar: boolean | null = null;
let initialAllowTransparency: boolean | null = null;
let themeStudioPopoutWindow: BrowserWindow | null = null;
let lastRestorableMainWindowMaximized = false;
let mainWindowRendererGone = false;

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

export function desktopFirstClickPassThroughPendingRestart(): boolean {
	if (process.platform !== 'darwin') return false;
	if (initialAcceptFirstMouseOnFocus === null) return false;
	return getDesktopWindowBehaviorSettings().firstClickPassThroughWhenUnfocused !== initialAcceptFirstMouseOnFocus;
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
	return allowTransparency ? TRANSPARENT_WINDOW_BACKGROUND_COLOR : OPAQUE_WINDOW_BACKGROUND_COLOR;
}

function getWindowHasShadow(allowTransparency: boolean): boolean | undefined {
	if (process.platform !== 'linux' || !allowTransparency) return undefined;
	return false;
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

function isLoopbackHostname(hostname: string): boolean {
	const normalized = hostname.toLowerCase();
	return normalized === 'localhost' || normalized === '127.0.0.1' || normalized === '::1' || normalized === '[::1]';
}

function getInsecureOriginRendererArgument(appUrl: string): string | null {
	try {
		const parsed = new URL(appUrl);
		if (parsed.protocol !== 'http:' || isLoopbackHostname(parsed.hostname)) {
			return null;
		}
		return `${INSECURE_ORIGIN_RENDERER_ARG_PREFIX}${parsed.origin}`;
	} catch {
		return null;
	}
}

function getRendererAdditionalArguments(
	allowTransparency: boolean,
	useNativeTitleBar: boolean,
	appUrl: string,
): Array<string> {
	const args: Array<string> = [];
	if (allowTransparency) args.push(ACTIVE_ALLOW_TRANSPARENCY_RENDERER_ARG);
	if (useNativeTitleBar) args.push(ACTIVE_USE_NATIVE_TITLEBAR_RENDERER_ARG);
	const insecureOriginArg = getInsecureOriginRendererArgument(appUrl);
	if (insecureOriginArg) args.push(insecureOriginArg);
	return args;
}

function getDevToolsOptions(options: {forceDetach?: boolean} = {}): Electron.OpenDevToolsOptions | undefined {
	return options.forceDetach || getActiveAllowTransparency() ? {mode: 'detach', activate: true} : undefined;
}

function openWindowDevTools(window: BrowserWindow, options?: {forceDetach?: boolean}): void {
	window.webContents.openDevTools(getDevToolsOptions(options));
}

async function clearStartupRenderingCaches(session: Electron.Session): Promise<void> {
	try {
		logger.info('Clearing Chromium startup rendering caches');
		await session.clearStorageData({storages: ['shadercache']});
		await session.clearCodeCaches({});
	} catch (error) {
		logger.warn('Failed to clear Chromium startup rendering caches', error);
	}
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
		trafficLightPosition: isMac ? VOICE_POPOUT_TRAFFIC_LIGHT_POSITION : undefined,
		backgroundColor: getWindowBackgroundColor(false),
		transparent: false,
		hasShadow: getWindowHasShadow(false),
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
		frame: !useCustomChrome,
	};
}

function getSharedWebPreferences(
	allowTransparency: boolean,
	useNativeTitleBar: boolean,
	appUrl: string,
): Electron.WebPreferences {
	return {
		preload: path.join(__dirname, '../preload/index.cjs'),
		enableBlinkFeatures: MEDIA_DEVICE_BLINK_FEATURES,
		contextIsolation: true,
		nodeIntegration: false,
		sandbox: false,
		webSecurity: true,
		allowRunningInsecureContent: false,
		spellcheck: process.platform !== 'linux',
		transparent: allowTransparency,
		additionalArguments: getRendererAdditionalArguments(allowTransparency, useNativeTitleBar, appUrl),
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
	const acceptFirstMouseOnFocus = isMac && desktopWindowBehavior.firstClickPassThroughWhenUnfocused;
	initialAcceptFirstMouseOnFocus = acceptFirstMouseOnFocus;
	initialUseNativeTitleBar = useNativeTitleBar;
	initialAllowTransparency = allowTransparency;
	const appUrl = getAppUrl();
	const windowOptions: Electron.BrowserWindowConstructorOptions = {
		width: windowWidth,
		height: windowHeight,
		minWidth: MIN_WINDOW_WIDTH,
		minHeight: MIN_WINDOW_HEIGHT,
		show: false,
		backgroundColor: getWindowBackgroundColor(allowTransparency),
		transparent: allowTransparency,
		hasShadow: getWindowHasShadow(allowTransparency),
		...getTitleBarWindowOptions(useNativeTitleBar),
		trafficLightPosition: isMac ? {x: 9, y: 9} : undefined,
		acceptFirstMouse: acceptFirstMouseOnFocus,
		webPreferences: getSharedWebPreferences(allowTransparency, useNativeTitleBar, appUrl),
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
	mainWindowRendererGone = false;
	installHtmlFullscreenChromeGuard(mainWindow);
	lastGoodWindowBounds = mainWindow.getNormalBounds();
	logPhase('browser-window');
	if (savedBounds?.isMaximized) {
		mainWindow.maximize();
	}
	let windowShown = Boolean(options.startHidden);
	const showWindowOnce = () => {
		if (!windowShown && mainWindow) {
			windowShown = true;
			mainWindow.show();
		}
	};
	if (!options.startHidden) {
		mainWindow.once('ready-to-show', showWindowOnce);
		setTimeout(() => {
			if (!windowShown) {
				log.warn('ready-to-show did not fire within 5 seconds, forcing window to show');
				showWindowOnce();
			}
		}, 5000);
	}
	let saveTimeout: NodeJS.Timeout | null = null;
	const debouncedSave = () => {
		if (saveTimeout) clearTimeout(saveTimeout);
		saveTimeout = setTimeout(() => {
			saveWindowBounds();
		}, 500);
	};
	mainWindow.on('resize', debouncedSave);
	mainWindow.on('move', debouncedSave);
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
		saveWindowBounds();
		if (!isQuitting && shouldHideMainWindowOnClose()) {
			event.preventDefault();
			logger.info('Window close hid the app to the tray; use Quit to terminate the process');
			mainWindow?.hide();
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
	logPhase('handlers');
	let appLoadRetryAttempt = 0;
	let appLoadRetryTimer: NodeJS.Timeout | null = null;
	const clearAppLoadRetry = () => {
		rendererGoneReloaded = false;
		mainWindowRendererGone = false;
		if (appLoadRetryTimer) {
			clearTimeout(appLoadRetryTimer);
			appLoadRetryTimer = null;
		}
		appLoadRetryAttempt = 0;
	};
	const scheduleAppLoadRetry = (reason: string, detail?: Record<string, unknown>) => {
		if (!mainWindow || mainWindow.isDestroyed()) return;
		if (appLoadRetryTimer) return;
		const delay = Math.min(INITIAL_APP_LOAD_RETRY_DELAY_MS * 2 ** appLoadRetryAttempt, MAX_APP_LOAD_RETRY_DELAY_MS);
		appLoadRetryAttempt += 1;
		logger.warn('Scheduling app load retry', {reason, delay, attempt: appLoadRetryAttempt, ...detail});
		appLoadRetryTimer = setTimeout(() => {
			appLoadRetryTimer = null;
			if (!mainWindow || mainWindow.isDestroyed()) return;
			if (mainWindow.webContents.isLoadingMainFrame()) {
				scheduleAppLoadRetry('main-frame-still-loading');
				return;
			}
			mainWindow.loadURL(appUrl).catch((error) => {
				scheduleAppLoadRetry('load-url-rejected', {error});
			});
		}, delay);
	};
	webContents.on('did-finish-load', clearAppLoadRetry);
	webContents.on('did-fail-load', (_event, errorCode, errorDescription, validatedURL, isMainFrame) => {
		if (isMainFrame) {
			logger.error('App main-frame load failed', {errorCode, errorDescription, validatedURL});
		}
		if (!isMainFrame || !isTrustedOrigin(validatedURL) || !shouldRetryAppLoadFailure(errorCode)) {
			return;
		}
		scheduleAppLoadRetry('did-fail-load', {errorCode, errorDescription, validatedURL});
	});
	const loadAppUrl = (): void => {
		if (!mainWindow || mainWindow.isDestroyed()) return;
		logger.info('Loading app URL', {appUrl});
		mainWindow.loadURL(appUrl).catch((error) => {
			const errorCode = getElectronLoadErrorCode(error);
			if (errorCode !== null && !shouldRetryAppLoadFailure(errorCode)) {
				logger.info('Ignoring non-retryable initial app load rejection', {errorCode});
				return;
			}
			logger.error('Failed to load app URL:', error);
			scheduleAppLoadRetry('initial-load-url-rejected', {error});
		});
		logPhase('load-url-dispatched');
	};
	void clearStartupRenderingCaches(session).then(loadAppUrl);
	webContents.on('will-navigate', (event, url) => {
		if (!isTrustedOrigin(url)) {
			event.preventDefault();
		}
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
				trafficLightPosition: isMac ? {x: 12, y: 5} : undefined,
				backgroundColor: getWindowBackgroundColor(allowPopoutTransparency),
				transparent: allowPopoutTransparency,
				hasShadow: getWindowHasShadow(allowPopoutTransparency),
				show: true,
				webPreferences: getSharedWebPreferences(allowPopoutTransparency, getActiveUseNativeTitleBar(), appUrl),
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
	if (mainWindow) {
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
		mainWindow.hide();
	}
}

export function setQuitting(quitting: boolean): void {
	isQuitting = quitting;
}

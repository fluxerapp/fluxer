/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

import {app, BrowserWindow, desktopCapturer, ipcMain, screen, shell} from 'electron';
import log from 'electron-log';
import {BUILD_CHANNEL} from '../common/build-channel.js';
import {
	CANARY_APP_URL,
	DEFAULT_WINDOW_HEIGHT,
	DEFAULT_WINDOW_WIDTH,
	MIN_WINDOW_HEIGHT,
	MIN_WINDOW_WIDTH,
	STABLE_APP_URL,
} from '../common/constants.js';
import {registerSpellcheck} from './spellcheck.js';
import {refreshWindowsBadgeOverlay} from './windows-badge.js';

const VISIBILITY_MARGIN = 32;

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
const POPOUT_NAMESPACE = 'fluxer_';

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
	return trustedWebOrigins.has(origin);
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

let mainWindow: BrowserWindow | null = null;
let windowStateFile: string;
let isQuitting = false;

interface PendingDisplayMediaRequest {
	callback: (streams: Electron.Streams | null) => void;
}

const pendingDisplayMediaRequests = new Map<string, PendingDisplayMediaRequest>();
let displayMediaRequestCounter = 0;

function setupDisplayMediaHandler(session: Electron.Session, webContents: Electron.WebContents): void {
	session.setDisplayMediaRequestHandler((request, callback) => {
		const requestId = `display-media-${++displayMediaRequestCounter}`;
		const requestCallback = (streams: Electron.Streams | null) => callback(streams as Electron.Streams);
		pendingDisplayMediaRequests.set(requestId, {callback: requestCallback});

		webContents.send('display-media-requested', requestId, {
			audioRequested: Boolean(request.audioRequested),
			videoRequested: Boolean(request.videoRequested),
		});

		setTimeout(() => {
			if (pendingDisplayMediaRequests.has(requestId)) {
				log.warn('[DisplayMedia] Request timed out:', requestId);
				pendingDisplayMediaRequests.delete(requestId);
				callback(null as unknown as Electron.Streams);
			}
		}, 60000);
	});
}

export function registerDisplayMediaHandlers(): void {
	ipcMain.handle(
		'get-desktop-sources',
		async (
			_event,
			types: Array<'screen' | 'window'>,
		): Promise<
			Array<{
				id: string;
				name: string;
				thumbnailDataUrl: string;
				appIconDataUrl?: string;
				display_id?: string;
			}>
		> => {
			try {
				const sources = await desktopCapturer.getSources({
					types,
					thumbnailSize: {width: 320, height: 180},
					fetchWindowIcons: true,
				});

				return sources.map((source) => ({
					id: source.id,
					name: source.name,
					thumbnailDataUrl: source.thumbnail.toDataURL(),
					appIconDataUrl: source.appIcon?.toDataURL(),
					display_id: source.display_id,
				}));
			} catch (error) {
				log.error('[getDesktopSources] Failed:', error);
				return [];
			}
		},
	);

	ipcMain.on(
		'select-display-media-source',
		async (_event, requestId: string, sourceId: string | null, withAudio: boolean) => {
			const pending = pendingDisplayMediaRequests.get(requestId);
			if (!pending) {
				log.warn('[selectDisplayMediaSource] No pending request for:', requestId);
				return;
			}

			pendingDisplayMediaRequests.delete(requestId);

			if (!sourceId) {
				log.info('[selectDisplayMediaSource] User cancelled');
				pending.callback(null);
				return;
			}

			try {
				const sources = await desktopCapturer.getSources({
					types: ['screen', 'window'],
				});

				const selectedSource = sources.find((s) => s.id === sourceId);
				if (!selectedSource) {
					log.error('[selectDisplayMediaSource] Source not found:', sourceId);
					pending.callback(null);
					return;
				}

				log.info('[selectDisplayMediaSource] Selected source:', {
					id: selectedSource.id,
					name: selectedSource.name,
					withAudio,
				});

				const audioSource = withAudio && process.platform === 'darwin' ? 'loopback' : undefined;

				pending.callback({
					video: selectedSource,
					audio: audioSource,
				});
			} catch (error) {
				log.error('[selectDisplayMediaSource] Failed:', error);
				pending.callback(null);
			}
		},
	);
}

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
	try {
		const filePath = getWindowStateFile();
		if (fs.existsSync(filePath)) {
			const data = fs.readFileSync(filePath, 'utf-8');
			const bounds = JSON.parse(data) as WindowBounds;

			const displays = screen.getAllDisplays();
			const display = findVisibleDisplay(displays, bounds);

			if (display != null) {
				log.info('Restored window bounds:', bounds);
				return bounds;
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

	try {
		const bounds = mainWindow.getBounds();
		const windowState: WindowBounds = {
			x: bounds.x,
			y: bounds.y,
			width: bounds.width,
			height: bounds.height,
			isMaximized: mainWindow.isMaximized(),
		};

		const filePath = getWindowStateFile();
		fs.writeFileSync(filePath, JSON.stringify(windowState, null, 2), 'utf-8');
		log.debug('Saved window bounds:', windowState);
	} catch (error) {
		log.error('Failed to save window bounds:', error);
	}
}

export function getMainWindow(): BrowserWindow | null {
	return mainWindow;
}

export function createWindow(): BrowserWindow {
	const isCanary = BUILD_CHANNEL === 'canary';

	const primaryDisplay = screen.getPrimaryDisplay();
	const {width: screenWidth, height: screenHeight} = primaryDisplay.workAreaSize;

	const savedBounds = loadWindowBounds();
	const windowWidth = savedBounds?.width ?? Math.min(DEFAULT_WINDOW_WIDTH, screenWidth);
	const windowHeight = savedBounds?.height ?? Math.min(DEFAULT_WINDOW_HEIGHT, screenHeight);

	const isMac = process.platform === 'darwin';
	const isWindows = process.platform === 'win32';
	const isLinux = process.platform === 'linux';

	const windowOptions: Electron.BrowserWindowConstructorOptions = {
		width: windowWidth,
		height: windowHeight,
		minWidth: MIN_WINDOW_WIDTH,
		minHeight: MIN_WINDOW_HEIGHT,
		show: false,
		backgroundColor: '#1a1a1a',
		titleBarStyle: isMac ? 'hidden' : 'hidden',
		trafficLightPosition: isMac ? {x: 9, y: 9} : undefined,
		titleBarOverlay: isWindows ? false : undefined,
		frame: false,
		resizable: true,

		webPreferences: {
			preload: path.join(__dirname, '../preload/index.js'),
			contextIsolation: true,
			nodeIntegration: false,
			sandbox: true,
			webSecurity: true,
			allowRunningInsecureContent: false,
			spellcheck: true,
		},
	};

	if (isLinux) {
		const baseIconName = '512x512.png';

		const resourceIconPath = path.join(process.resourcesPath, baseIconName);
		const exeDirIconPath = path.join(path.dirname(app.getPath('exe')), baseIconName);

		if (fs.existsSync(resourceIconPath)) {
			windowOptions.icon = resourceIconPath;
		} else if (fs.existsSync(exeDirIconPath)) {
			windowOptions.icon = exeDirIconPath;
		}
	}

	if (savedBounds?.x !== undefined && savedBounds?.y !== undefined) {
		windowOptions.x = savedBounds.x;
		windowOptions.y = savedBounds.y;
	} else {
		windowOptions.center = true;
	}

	mainWindow = new BrowserWindow(windowOptions);

	if (savedBounds?.isMaximized) {
		mainWindow.maximize();
	}

	let windowShown = false;
	const showWindowOnce = () => {
		if (!windowShown && mainWindow) {
			windowShown = true;
			mainWindow.show();
		}
	};

	mainWindow.once('ready-to-show', showWindowOnce);

	setTimeout(() => {
		if (!windowShown) {
			log.warn('ready-to-show did not fire within 5 seconds, forcing window to show');
			showWindowOnce();
		}
	}, 5000);

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
		saveWindowBounds();
		mainWindow?.webContents.send('window-maximize-change', true);
	});

	mainWindow.on('unmaximize', () => {
		saveWindowBounds();
		mainWindow?.webContents.send('window-maximize-change', false);
	});

	mainWindow.on('close', (event) => {
		if (saveTimeout) clearTimeout(saveTimeout);
		saveWindowBounds();

		if (process.platform === 'darwin' && !isQuitting) {
			event.preventDefault();
			mainWindow?.hide();
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

	registerSpellcheck(webContents);

	session.setDevicePermissionHandler(({deviceType, origin}) => {
		if (!origin || !isTrustedOrigin(origin)) {
			return false;
		}

		return webAuthnDeviceTypes.has(deviceType);
	});

	session.on('select-hid-device', (event, details, callback) => {
		const origin = details.frame?.url;
		if (!isTrustedOrigin(origin)) {
			return;
		}
		event.preventDefault();
		const firstDevice = details.deviceList?.[0];
		callback(firstDevice?.deviceId ?? '');
	});

	session.setPermissionRequestHandler((webContents, permission, callback, details) => {
		const origin = details.requestingUrl || webContents.getURL();
		const trusted = isTrustedOrigin(origin);

		if (!trusted) {
			callback(false);
			return;
		}

		if (webAuthnPermissionTypes.has(permission)) {
			callback(true);
			return;
		}

		if (
			permission === 'media' ||
			permission === 'notifications' ||
			permission === 'fullscreen' ||
			permission === 'pointerLock' ||
			permission === 'clipboard-sanitized-write'
		) {
			callback(true);
			return;
		}

		callback(false);
	});

	session.setPermissionCheckHandler((webContents, permission, requestingOrigin, details) => {
		const origin = requestingOrigin || details?.requestingUrl || webContents?.getURL();
		const embeddingOrigin = details?.embeddingOrigin;

		if (!webContents) return false;
		if (!isTrustedOrigin(origin)) {
			return false;
		}
		if (embeddingOrigin && !isTrustedOrigin(embeddingOrigin)) {
			return false;
		}

		if (webAuthnPermissionTypes.has(permission)) {
			return true;
		}

		if (
			permission === 'media' ||
			permission === 'notifications' ||
			permission === 'fullscreen' ||
			permission === 'pointerLock' ||
			permission === 'clipboard-sanitized-write'
		) {
			return true;
		}

		return false;
	});

	setupDisplayMediaHandler(session, webContents);

	const appUrl = isCanary ? CANARY_APP_URL : STABLE_APP_URL;

	mainWindow.loadURL(appUrl).catch((error) => {
		console.error('Failed to load app URL:', error);
	});

	webContents.on('will-navigate', (event, url) => {
		if (!isTrustedOrigin(url)) {
			event.preventDefault();
			shell.openExternal(url).catch((error) => {
				log.warn('Failed to open external URL from will-navigate:', error);
			});
		}
	});

	webContents.setWindowOpenHandler(({url, frameName}) => {
		const pathname = getSanitizedPath(url);

		if (frameName?.startsWith(POPOUT_NAMESPACE) && pathname === '/popout' && isTrustedOrigin(url)) {
			const overrideBrowserWindowOptions: Electron.BrowserWindowConstructorOptions = {
				titleBarStyle: isMac ? 'hidden' : undefined,
				trafficLightPosition: isMac ? {x: 12, y: 5} : undefined,
				frame: isLinux,
				resizable: true,
				backgroundColor: '#1a1a1a',
				show: true,
			};

			return {action: 'allow', overrideBrowserWindowOptions};
		}

		if (isTrustedOrigin(url)) {
			return {action: 'deny'};
		}

		shell.openExternal(url).catch((error) => {
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

		if (process.platform === 'darwin') {
			try {
				app.dock?.show();
			} catch {}

			try {
				app.focus({steal: true});
			} catch {}

			try {
				mainWindow.setVisibleOnAllWorkspaces(true, {visibleOnFullScreen: true});
			} catch {}

			mainWindow.show();
			mainWindow.focus();

			setTimeout(() => {
				if (!mainWindow || mainWindow.isDestroyed()) return;
				try {
					mainWindow.setVisibleOnAllWorkspaces(false);
				} catch {}
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

// SPDX-License-Identifier: AGPL-3.0-or-later

import os from 'node:os';
import {BrowserWindow, screen} from 'electron';
import log from 'electron-log';

const GUARD_SIZE_DIP = 2;
const WGC_SCREEN_CAPTURE_MIN_WINDOWS_BUILD = 26100;

interface ActiveMonitorCompositionGuard {
	window: BrowserWindow;
	sourceId: string;
	displayId?: string;
	owner: Electron.WebContents;
	onOwnerDestroyed: () => void;
}

let activeGuard: ActiveMonitorCompositionGuard | null = null;

function isScreenSource(source: Electron.DesktopCapturerSource): boolean {
	return source.id.startsWith('screen:');
}

function getWindowsBuildNumber(): number | null {
	const build = Number.parseInt(os.release().split('.')[2] ?? '', 10);
	return Number.isSafeInteger(build) && build > 0 ? build : null;
}

function monitorCaptureUsesDxgiDuplication(): boolean {
	const build = getWindowsBuildNumber();
	return build === null || build < WGC_SCREEN_CAPTURE_MIN_WINDOWS_BUILD;
}

function parseScreenSourceOrdinal(sourceId: string): number | null {
	const match = /^screen:([0-9]+):(?:0|1)$/.exec(sourceId);
	if (!match) return null;
	const parsed = Number.parseInt(match[1], 10);
	return Number.isSafeInteger(parsed) && parsed >= 0 ? parsed : null;
}

function resolveDisplayForSource(source: Electron.DesktopCapturerSource): Electron.Display | null {
	const displays = screen.getAllDisplays();
	if (source.display_id) {
		const byDisplayId = displays.find((display) => String(display.id) === String(source.display_id));
		if (byDisplayId) return byDisplayId;
	}
	const ordinal = parseScreenSourceOrdinal(source.id);
	if (ordinal != null && displays[ordinal]) {
		return displays[ordinal];
	}
	return screen.getPrimaryDisplay();
}

function detachOwnerListener(guard: ActiveMonitorCompositionGuard): void {
	try {
		guard.owner.removeListener('destroyed', guard.onOwnerDestroyed);
	} catch (error) {
		log.debug('[WindowsScreenCaptureGuard] Failed to detach capturer listener', {error});
	}
}

function createCompositionGuardWindow(display: Electron.Display): BrowserWindow {
	const {x, y, width, height} = display.bounds;
	const guardWindow = new BrowserWindow({
		x: x + Math.max(0, width - GUARD_SIZE_DIP),
		y: y + Math.max(0, height - GUARD_SIZE_DIP),
		width: GUARD_SIZE_DIP,
		height: GUARD_SIZE_DIP,
		frame: false,
		resizable: false,
		movable: false,
		minimizable: false,
		maximizable: false,
		closable: false,
		focusable: false,
		skipTaskbar: true,
		transparent: true,
		hasShadow: false,
		show: false,
		backgroundColor: '#00000000',
		webPreferences: {
			contextIsolation: true,
			nodeIntegration: false,
			sandbox: true,
		},
	});
	guardWindow.setIgnoreMouseEvents(true, {forward: true});
	try {
		guardWindow.setContentProtection(true);
	} catch (error) {
		log.debug('[WindowsScreenCaptureGuard] Failed to set content protection', {error});
	}
	try {
		guardWindow.setAlwaysOnTop(true, 'screen-saver', 1);
	} catch {
		guardWindow.setAlwaysOnTop(true);
	}
	try {
		guardWindow.setVisibleOnAllWorkspaces(true, {visibleOnFullScreen: true});
	} catch {}
	void guardWindow
		.loadURL(
			'data:text/html;charset=utf-8,' +
				encodeURIComponent(
					'<!doctype html><html><head><style>html,body{margin:0;width:100%;height:100%;overflow:hidden;background:rgba(0,0,0,0.01);}</style></head><body></body></html>',
				),
		)
		.catch((error) => {
			log.debug('[WindowsScreenCaptureGuard] Failed to load guard surface', {error});
		});
	guardWindow.showInactive();
	return guardWindow;
}

export function startWindowsScreenCaptureGuardForSource(
	source: Electron.DesktopCapturerSource,
	owner: Electron.WebContents,
): void {
	if (process.platform !== 'win32') return;
	if (!isScreenSource(source)) return;
	if (owner.isDestroyed()) return;
	if (!monitorCaptureUsesDxgiDuplication()) {
		log.info('[WindowsScreenCaptureGuard] Not forcing DWM composition; Chromium captures monitors with WGC here', {
			sourceId: source.id,
			windowsBuild: getWindowsBuildNumber(),
		});
		return;
	}
	if (activeGuard?.sourceId === source.id && activeGuard.owner === owner && !activeGuard.window.isDestroyed()) {
		return;
	}
	destroyActiveGuard('replace');
	const display = resolveDisplayForSource(source);
	if (!display) return;
	try {
		const guardWindow = createCompositionGuardWindow(display);
		const onOwnerDestroyed = (): void => {
			if (activeGuard?.window === guardWindow) {
				destroyActiveGuard('capturer-destroyed');
			}
		};
		const guard: ActiveMonitorCompositionGuard = {
			window: guardWindow,
			sourceId: source.id,
			displayId: source.display_id,
			owner,
			onOwnerDestroyed,
		};
		guardWindow.once('closed', () => {
			if (activeGuard === guard) {
				detachOwnerListener(guard);
				activeGuard = null;
			}
		});
		owner.once('destroyed', onOwnerDestroyed);
		activeGuard = guard;
		log.info('[WindowsScreenCaptureGuard] Forcing DWM composition on the captured monitor for DXGI duplication', {
			sourceId: source.id,
			displayId: source.display_id,
			windowsBuild: getWindowsBuildNumber(),
			bounds: display.bounds,
		});
	} catch (error) {
		log.warn('[WindowsScreenCaptureGuard] Failed to force DWM composition on the captured monitor', {
			sourceId: source.id,
			error,
		});
	}
}

function destroyActiveGuard(reason: string): void {
	const guard = activeGuard;
	if (!guard) return;
	activeGuard = null;
	detachOwnerListener(guard);
	if (!guard.window.isDestroyed()) {
		try {
			guard.window.destroy();
		} catch (error) {
			log.debug('[WindowsScreenCaptureGuard] Failed to destroy guard window', {reason, error});
		}
	}
	log.info('[WindowsScreenCaptureGuard] Stopped forcing DWM composition on the captured monitor', {
		reason,
		sourceId: guard.sourceId,
		displayId: guard.displayId,
	});
}

export function stopWindowsScreenCaptureGuard(reason: string): void {
	destroyActiveGuard(reason);
}

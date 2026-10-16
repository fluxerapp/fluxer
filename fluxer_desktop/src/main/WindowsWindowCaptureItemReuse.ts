// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {isSafeModeLaunch} from '@electron/main/LaunchOptions';
import {loadOptionalNativeAddon} from '@electron/main/NativeAddon';
import {getWindowsBuildNumber} from '@electron/main/WindowsScreenCaptureGuard';
import log from 'electron-log';

const WINDOWS_10_2004_BUILD = 19041;
const WINDOWS_10_22H2_BUILD = 19045;

const requireModule = createRequire(import.meta.url);

interface WindowCaptureItemCalls {
	forwarded: number;
	reused: number;
}

interface WindowCaptureItemReuseModule {
	reuseWindowCaptureItems: () => boolean;
	windowCaptureItemCalls: () => WindowCaptureItemCalls | null;
	loadError?: Error | null;
}

const loadWindowCaptureItemReuseModule = loadOptionalNativeAddon<WindowCaptureItemReuseModule>({
	platform: 'win32',
	disabled: () => isSafeModeLaunch(),
	load: () => requireModule('@fluxer/win-game-capture'),
	logger: log,
	level: 'warn',
	unavailableMessage:
		'[WindowsWindowCaptureItemReuse] Failed to load @fluxer/win-game-capture; window shares create a capture item per frame',
	disabledMessage:
		'[WindowsWindowCaptureItemReuse] Disabled by safe mode; window shares create a capture item per frame',
});

let reuseRequested = false;
let reusingModule: WindowCaptureItemReuseModule | null = null;
let reportedCalls: WindowCaptureItemCalls = {forwarded: 0, reused: 0};

function reusesCaptureItemsOnWindowsBuild(windowsBuild: number | null): windowsBuild is number {
	if (windowsBuild === null) return false;
	return windowsBuild >= WINDOWS_10_2004_BUILD && windowsBuild <= WINDOWS_10_22H2_BUILD;
}

export function reportWindowsCaptureItemReuse(reason: string): void {
	const calls = reusingModule?.windowCaptureItemCalls();
	if (!calls) return;
	const forwarded = calls.forwarded - reportedCalls.forwarded;
	const reused = calls.reused - reportedCalls.reused;
	if (forwarded === 0 && reused === 0) return;
	reportedCalls = calls;
	log.info('[WindowsWindowCaptureItemReuse] Capture item requests since the last report', {
		reason,
		forwardedToCaptureService: forwarded,
		reusedFromLiveShare: reused,
	});
}

export function reuseWindowsCaptureItemsForSource(source: Electron.DesktopCapturerSource): void {
	try {
		reuseCaptureItemsForSource(source);
	} catch (error) {
		log.warn('[WindowsWindowCaptureItemReuse] Window shares create a capture item per frame', {error});
	}
}

function reuseCaptureItemsForSource(source: Electron.DesktopCapturerSource): void {
	if (process.platform !== 'win32') return;
	if (!source.id.startsWith('window:')) return;
	if (reuseRequested) {
		reportWindowsCaptureItemReuse('window-share-start');
		return;
	}
	const windowsBuild = getWindowsBuildNumber();
	if (!reusesCaptureItemsOnWindowsBuild(windowsBuild)) return;
	reuseRequested = true;
	const addon = loadWindowCaptureItemReuseModule();
	if (!addon) return;
	if (!addon.reuseWindowCaptureItems()) {
		log.warn('[WindowsWindowCaptureItemReuse] Window shares create a capture item per frame', {windowsBuild});
		return;
	}
	reusingModule = addon;
	log.info('[WindowsWindowCaptureItemReuse] Window shares ask for their live capture item first', {windowsBuild});
}

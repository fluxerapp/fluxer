// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import type {ElectronAPI} from '@app/types/electron.d';
import Bowser from 'bowser';

const logger = new Logger('NativeUtils');

export function isElectron(): boolean {
	if (typeof window === 'undefined') return false;
	return (
		(
			window as {
				electron?: ElectronAPI;
			}
		).electron !== undefined
	);
}

export function hasUnavailableElectronNativeContext(): boolean {
	if (typeof window === 'undefined' || typeof navigator === 'undefined') return false;
	return (
		(
			window as {
				electron?: ElectronAPI | null;
			}
		).electron == null && /\bElectron\/\d+(?:\.\d+)*/.test(navigator.userAgent)
	);
}

export function getElectronAPI(): ElectronAPI | null {
	if (!isElectron()) return null;
	return (
		(
			window as {
				electron?: ElectronAPI;
			}
		).electron ?? null
	);
}

export function isDesktop(): boolean {
	return isElectron();
}

export function isCanaryDesktop(): boolean {
	return getElectronAPI()?.buildChannel === 'canary';
}

export type NativePlatform = 'macos' | 'windows' | 'linux' | 'unknown';

export type NativeTitlebarNavOptions = "back" | "forward"

const normalizePlatform = (platform: string | null | undefined): NativePlatform => {
	const value = platform?.toLowerCase() ?? '';
	if (value.startsWith('mac')) return 'macos';
	if (value.startsWith('darwin')) return 'macos';
	if (value.startsWith('win')) return 'windows';
	if (value.includes('linux')) return 'linux';
	return 'unknown';
};

export function guessPlatform(): NativePlatform {
	if (typeof navigator === 'undefined') return 'unknown';
	const uaDataPlatform = (
		navigator as {
			userAgentData?: {
				platform?: string;
			};
		}
	).userAgentData?.platform;
	if (uaDataPlatform) {
		return normalizePlatform(uaDataPlatform);
	}
	return normalizePlatform(navigator.platform);
}

export function getNativePlatformSync(): NativePlatform {
	const electronApi = getElectronAPI();
	if (electronApi) {
		switch (electronApi.platform) {
			case 'darwin':
				return 'macos';
			case 'win32':
				return 'windows';
			case 'linux':
				return 'linux';
			default:
				return 'unknown';
		}
	}
	return guessPlatform();
}

export async function getNativePlatform(): Promise<NativePlatform> {
	const electronApi = getElectronAPI();
	if (electronApi) {
		switch (electronApi.platform) {
			case 'darwin':
				return 'macos';
			case 'win32':
				return 'windows';
			case 'linux':
				return 'linux';
			default:
				return 'unknown';
		}
	}
	return guessPlatform();
}

export async function isLinuxWaylandDesktopSession(): Promise<boolean> {
	const electronApi = getElectronAPI();
	if (!electronApi || electronApi.platform !== 'linux') {
		return false;
	}
	try {
		return Boolean((await electronApi.getDesktopInfo())?.waylandSession);
	} catch (error) {
		logger.warn('Failed to read Linux desktop session type', error);
		return false;
	}
}

export function isNativeMacOS(platform?: NativePlatform) {
	return (platform ?? getNativePlatformSync()) === 'macos';
}

export function isNativeWindows(platform?: NativePlatform) {
	return (platform ?? getNativePlatformSync()) === 'windows';
}

export function isNativeLinux(platform?: NativePlatform) {
	return (platform ?? getNativePlatformSync()) === 'linux';
}

let _isChromium: boolean | null = null;

export function isChromiumBrowser(): boolean {
	if (_isChromium == null) {
		try {
			const parser = Bowser.getParser(navigator.userAgent);
			_isChromium = parser.getEngineName() === 'Blink';
		} catch {
			_isChromium = false;
		}
	}
	return _isChromium;
}

let _isFirefox: boolean | null = null;

export function isFirefoxBrowser(): boolean {
	if (_isFirefox == null) {
		try {
			const parser = Bowser.getParser(navigator.userAgent);
			_isFirefox = parser.getEngineName() === 'Gecko';
		} catch {
			_isFirefox = false;
		}
	}
	return _isFirefox;
}

export function supportsDesktopScreenShareAudioCapture(): boolean {
	const electronApi = getElectronAPI();
	if (!electronApi) {
		return !isFirefoxBrowser();
	}
	return electronApi.platform === 'win32' || electronApi.platform === 'linux' || electronApi.platform === 'darwin';
}

let externalLinkHandlerAttached = false;

const BLOCKED_EXTERNAL_URL_PROTOCOLS = new Set([
	'file:',
	'javascript:',
	'vbscript:',
	'data:',
	'about:',
	'chrome:',
	'ms-cxh:',
	'ms-cxh-full:',
	'ms-word:',
]);
const EXPLICIT_URL_PROTOCOL_PATTERN = /^[A-Za-z][A-Za-z0-9+.-]*:/;

const getSafeExternalUrl = (href: string | null): string | null => {
	if (!href) return null;
	const trimmed = href.trim();
	if (!EXPLICIT_URL_PROTOCOL_PATTERN.test(trimmed)) return null;
	try {
		const url = new URL(trimmed);
		const protocol = url.protocol.toLowerCase();
		return protocol && !BLOCKED_EXTERNAL_URL_PROTOCOLS.has(protocol) ? url.toString() : null;
	} catch {
		return null;
	}
};

export async function openExternalUrl(url: string, target: string = '_blank') {
	const safeUrl = getSafeExternalUrl(url);
	if (!safeUrl) return;
	const electronApi = getElectronAPI();
	if (electronApi) {
		try {
			await electronApi.openExternal(safeUrl);
			return;
		} catch (error) {
			logger.error(' Failed to open external URL via Electron', error);
			return;
		}
	}
	window.open(safeUrl, target, 'noopener,noreferrer');
}

interface ExternalLinkClickEvent {
	preventDefault(): void;
	stopPropagation(): void;
}

interface ExternalLinkClickOptions {
	stopPropagation?: boolean;
}

export function handleExternalLinkClick(
	event: ExternalLinkClickEvent,
	url: string,
	options?: ExternalLinkClickOptions,
): void {
	event.preventDefault();
	if (options?.stopPropagation) {
		event.stopPropagation();
	}
	void openExternalUrl(url);
}

export function attachExternalLinkInterceptor() {
	if (!isDesktop() || externalLinkHandlerAttached) return () => undefined;
	const handler = (event: MouseEvent) => {
		if (event.defaultPrevented) return;
		if (event.button !== 0) return;
		const target = event.target as HTMLElement | null;
		const anchor = target?.closest?.('a[target="_blank"]') as HTMLAnchorElement | null;
		if (!anchor) return;
		const href = anchor.getAttribute('href');
		if (!getSafeExternalUrl(href)) return;
		event.preventDefault();
		void openExternalUrl(href ?? '');
	};
	const auxHandler = (event: MouseEvent) => {
		if (event.defaultPrevented) return;
		if (event.button !== 1) return;
		const target = event.target as HTMLElement | null;
		const anchor = target?.closest?.('a[target="_blank"]') as HTMLAnchorElement | null;
		if (!anchor) return;
		const href = anchor.getAttribute('href');
		if (!getSafeExternalUrl(href)) return;
		event.preventDefault();
		void openExternalUrl(href ?? '');
	};
	document.addEventListener('click', handler);
	document.addEventListener('auxclick', auxHandler);
	externalLinkHandlerAttached = true;
	return () => {
		document.removeEventListener('click', handler);
		document.removeEventListener('auxclick', auxHandler);
		externalLinkHandlerAttached = false;
	};
}

export type NativeDownloadOutcome = 'success' | 'canceled' | 'failed' | 'unavailable';

export async function downloadWithNative(options: {
	url: string;
	suggestedName?: string;
	title?: string;
}): Promise<NativeDownloadOutcome> {
	const electronApi = getElectronAPI();
	if (!electronApi) {
		return 'unavailable';
	}
	try {
		const result = await electronApi.downloadFile(options.url, options.suggestedName ?? 'download');
		if (result.success) return 'success';
		if (result.canceled) return 'canceled';
		return 'failed';
	} catch (error) {
		logger.error(' Native download failed, falling back to browser', error);
		return 'failed';
	}
}

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

import type {ElectronAPI} from '../../src-electron/common/types';

export const isElectron = (): boolean => (window as {electron?: ElectronAPI}).electron !== undefined;

export const getElectronAPI = (): ElectronAPI | null => {
	if (!isElectron()) return null;
	return (window as {electron?: ElectronAPI}).electron ?? null;
};

export const isDesktop = (): boolean => isElectron();

export type NativePlatform = 'macos' | 'windows' | 'linux' | 'unknown';

const normalizePlatform = (platform: string | null | undefined): NativePlatform => {
	const value = platform?.toLowerCase() ?? '';
	if (value.startsWith('mac')) return 'macos';
	if (value.startsWith('darwin')) return 'macos';
	if (value.startsWith('win')) return 'windows';
	if (value.includes('linux')) return 'linux';
	return 'unknown';
};

export const guessPlatform = (): NativePlatform => {
	const uaDataPlatform = (navigator as {userAgentData?: {platform?: string}}).userAgentData?.platform;
	if (uaDataPlatform) {
		return normalizePlatform(uaDataPlatform);
	}
	return normalizePlatform(navigator.platform);
};

export const getNativePlatform = async (): Promise<NativePlatform> => {
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
};

export const isNativeMacOS = (platform?: NativePlatform) => (platform ?? guessPlatform()) === 'macos';
export const isNativeWindows = (platform?: NativePlatform) => (platform ?? guessPlatform()) === 'windows';
export const isNativeLinux = (platform?: NativePlatform) => (platform ?? guessPlatform()) === 'linux';

let externalLinkHandlerAttached = false;

const isLikelyExternal = (href: string | null): href is string => {
	if (!href) return false;
	if (href.startsWith('javascript:')) return false;
	try {
		const url = new URL(href, window.location.href);
		const allowedProtocols = ['http:', 'https:', 'mailto:', 'x-apple.systempreferences:'];
		return allowedProtocols.includes(url.protocol);
	} catch {
		return false;
	}
};

export const openExternalUrl = async (url: string, target: string = '_blank') => {
	const electronApi = getElectronAPI();
	if (electronApi) {
		try {
			await electronApi.openExternal(url);
			return;
		} catch (error) {
			console.error('[NativeUtils] Failed to open via Electron, falling back', error);
		}
	}

	window.open(url, target, 'noopener,noreferrer');
};

export const attachExternalLinkInterceptor = () => {
	if (!isDesktop() || externalLinkHandlerAttached) return () => undefined;

	const handler = (event: MouseEvent) => {
		const target = event.target as HTMLElement | null;
		const anchor = target?.closest?.('a[target="_blank"]') as HTMLAnchorElement | null;
		if (!anchor) return;

		const href = anchor.getAttribute('href');
		if (!isLikelyExternal(href)) return;

		event.preventDefault();
		void openExternalUrl(href ?? '');
	};

	document.addEventListener('click', handler);
	externalLinkHandlerAttached = true;

	return () => {
		document.removeEventListener('click', handler);
		externalLinkHandlerAttached = false;
	};
};

export const downloadWithNative = async (options: {
	url: string;
	suggestedName?: string;
	title?: string;
}): Promise<boolean> => {
	const electronApi = getElectronAPI();
	if (electronApi) {
		try {
			const result = await electronApi.downloadFile(options.url, options.suggestedName ?? 'download');
			return result.success;
		} catch (error) {
			console.error('[NativeUtils] Native download failed, falling back to browser', error);
			return false;
		}
	}

	return false;
};

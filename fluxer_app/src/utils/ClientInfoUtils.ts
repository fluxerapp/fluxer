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

import Bowser from 'bowser';
import Config from '~/Config';
import {getElectronAPI, isDesktop} from '~/utils/NativeUtils';

interface ClientInfo {
	browserName?: string;
	browserVersion?: string;
	osName?: string;
	osVersion?: string;
	arch?: string;
	desktopVersion?: string;
	desktopChannel?: string;
	desktopArch?: string;
	desktopOS?: string;
	desktopRunningUnderRosetta?: boolean;
}

type NavigatorHighEntropyHints = {
	architecture?: string;
	bitness?: string;
	platform?: string;
};

type NavigatorUADataLike = NavigatorHighEntropyHints & {
	getHighEntropyValues?: (hints: ReadonlyArray<keyof NavigatorHighEntropyHints>) => Promise<NavigatorHighEntropyHints>;
};

const normalize = <T>(value: T | null | undefined): T | undefined => value ?? undefined;

const ARCHITECTURE_PATTERNS: ReadonlyArray<{pattern: RegExp; label: string}> = [
	{pattern: /\barm64\b|\baarch64\b|\barmv8\b|\barm64e\b/i, label: 'arm64'},
	{pattern: /\barm\b|\barmv7\b|\barmv6\b/i, label: 'arm'},
	{pattern: /MacIntel/i, label: 'x64'},
	{pattern: /\bx86_64\b|\bx64\b|\bamd64\b|\bwin64\b|\bwow64\b/i, label: 'x64'},
	{pattern: /\bx86\b|\bi[3-6]86\b/i, label: 'x86'},
];

export const normalizeArchitectureValue = (value: string | null | undefined): string | undefined => {
	if (!value) {
		return undefined;
	}
	const trimmed = value.trim();
	for (const entry of ARCHITECTURE_PATTERNS) {
		if (entry.pattern.test(trimmed)) {
			return entry.label;
		}
	}
	return trimmed || undefined;
};

const getNavigatorObject = (): Navigator | undefined => {
	if (typeof navigator === 'undefined') {
		return undefined;
	}
	return navigator;
};

const detectAppleSiliconViaWebGL = (): string | undefined => {
	if (typeof document === 'undefined') {
		return undefined;
	}
	const canvas = document.createElement('canvas');
	const gl =
		(canvas.getContext('webgl') as WebGLRenderingContext | null) ??
		(canvas.getContext('experimental-webgl') as WebGLRenderingContext | null);
	if (!gl) {
		return undefined;
	}
	const ext = gl.getExtension('WEBGL_debug_renderer_info');
	if (!ext) {
		return undefined;
	}
	const renderer = gl.getParameter(ext.UNMASKED_RENDERER_WEBGL);
	if (typeof renderer !== 'string') {
		return undefined;
	}
	if (/apple m|apple gpu/i.test(renderer)) {
		return 'arm64';
	}
	if (/intel/i.test(renderer)) {
		return 'x64';
	}
	return undefined;
};

const isNavigatorPlatformMac = (nav: Navigator): boolean => {
	const platform = nav.platform ?? '';
	return /^(mac|darwin)/i.test(platform) || /Macintosh|Mac OS X/i.test(nav.userAgent ?? '');
};

const detectArchitectureFromNavigator = (): string | undefined => {
	const nav = getNavigatorObject();
	if (!nav) {
		return undefined;
	}

	const userAgentData = (nav as Navigator & {userAgentData?: NavigatorUADataLike}).userAgentData;
	if (userAgentData?.architecture) {
		return normalizeArchitectureValue(userAgentData.architecture);
	}

	const userAgent = nav.userAgent ?? '';
	const platform = nav.platform ?? '';
	const isMac = isNavigatorPlatformMac(nav);

	if (isMac) {
		const detected = detectAppleSiliconViaWebGL();
		if (detected) {
			return detected;
		}
	}

	for (const entry of ARCHITECTURE_PATTERNS) {
		if (entry.pattern.test(userAgent)) {
			if (isMac && entry.label === 'x64') {
				continue;
			}
			return entry.label;
		}
	}

	for (const entry of ARCHITECTURE_PATTERNS) {
		if (entry.pattern.test(platform)) {
			if (isMac && entry.label === 'x64') {
				continue;
			}
			return entry.label;
		}
	}

	return undefined;
};

let cachedClientInfo: ClientInfo | null = null;
let preloadPromise: Promise<ClientInfo> | null = null;

const parseUserAgent = (): ClientInfo => {
	const hasNavigator = typeof navigator !== 'undefined';
	const userAgent = hasNavigator ? navigator.userAgent : '';
	const parser = Bowser.getParser(userAgent);
	const result = parser.getResult();
	const isMac = hasNavigator && isNavigatorPlatformMac(navigator);
	const fallbackArch = hasNavigator && !isMac ? normalizeArchitectureValue(navigator.platform) : undefined;
	const arch = detectArchitectureFromNavigator() ?? fallbackArch;
	return {
		browserName: normalize(result.browser.name),
		browserVersion: normalize(result.browser.version),
		osName: normalize(result.os.name),
		osVersion: normalize(result.os.version),
		arch: arch,
	};
};

export const getClientInfoSync = (): ClientInfo => {
	if (cachedClientInfo) {
		return cachedClientInfo;
	}
	try {
		return parseUserAgent();
	} catch {
		return {};
	}
};

export const preloadClientInfo = (): Promise<ClientInfo> => {
	if (cachedClientInfo) {
		return Promise.resolve(cachedClientInfo);
	}
	if (preloadPromise) {
		return preloadPromise;
	}
	preloadPromise = getClientInfo().then((info) => {
		cachedClientInfo = info;
		return info;
	});
	return preloadPromise;
};

async function getDesktopContext(): Promise<Partial<ClientInfo>> {
	const electronApi = getElectronAPI();
	if (electronApi) {
		try {
			const desktopInfo = await electronApi.getDesktopInfo();
			return {
				desktopVersion: normalize(desktopInfo.version),
				desktopChannel: normalize(desktopInfo.channel),
				desktopArch: normalizeArchitectureValue(desktopInfo.hardwareArch ?? desktopInfo.arch),
				desktopOS: normalize(desktopInfo.os),
				desktopRunningUnderRosetta: desktopInfo.runningUnderRosetta,
			};
		} catch (error) {
			console.warn('[ClientInfo] Failed to load desktop context', error);
			return {};
		}
	}

	return {};
}

function getWindowsVersionName(osVersion: string): string {
	const parts = osVersion.split('.');
	const majorVersion = parseInt(parts[0], 10);
	const buildNumber = parseInt(parts[2], 10);

	if (majorVersion === 10) {
		if (buildNumber >= 22000) {
			return 'Windows 11';
		}
		return 'Windows 10';
	}

	return 'Windows';
}

const detectArchitectureFromClientHints = async (): Promise<string | undefined> => {
	const nav = getNavigatorObject();
	if (!nav) {
		return undefined;
	}
	const userAgentData = (nav as Navigator & {userAgentData?: NavigatorUADataLike}).userAgentData;
	if (!userAgentData?.getHighEntropyValues) {
		return undefined;
	}

	try {
		const hints = await userAgentData.getHighEntropyValues(['architecture', 'bitness']);
		const archHint = hints.architecture?.toLowerCase() ?? '';
		const bitness = hints.bitness?.toLowerCase() ?? '';
		const platform = (userAgentData.platform ?? '').toLowerCase();

		if (platform === 'windows') {
			if (archHint === 'arm') {
				return 'arm64';
			}
			if (archHint === 'x86' && bitness === '64') {
				return 'x64';
			}
		}

		if (archHint.includes('arm')) {
			return 'arm64';
		}
		if (archHint.includes('intel') || archHint.includes('x64')) {
			return 'x64';
		}

		return normalizeArchitectureValue(archHint);
	} catch (error) {
		console.warn('[ClientInfo] Failed to load architecture hints', error);
		return undefined;
	}
};

async function getOsContext(): Promise<Partial<ClientInfo>> {
	const electronApi = getElectronAPI();
	if (electronApi) {
		try {
			const desktopInfo = await electronApi.getDesktopInfo();
			let osName: string | undefined;

			switch (desktopInfo.os) {
				case 'darwin':
					osName = 'macOS';
					break;
				case 'win32':
					osName = getWindowsVersionName(desktopInfo.osVersion);
					break;
				case 'linux':
					osName = 'Linux';
					break;
				default:
					osName = desktopInfo.os;
			}

			const osVersion = normalize(desktopInfo.systemVersion ?? desktopInfo.osVersion);
			return {
				osName,
				osVersion,
				arch: normalizeArchitectureValue(desktopInfo.arch),
			};
		} catch (error) {
			console.warn('[ClientInfo] Failed to load OS context', error);
			return {};
		}
	}

	return {};
}

export const getClientInfo = async (): Promise<ClientInfo> => {
	const base = getClientInfoSync();
	if (!isDesktop()) {
		const hintsArch = await detectArchitectureFromClientHints();
		return {...base, arch: hintsArch ?? base.arch};
	}

	const [osContext, desktop] = await Promise.all([getOsContext(), getDesktopContext()]);
	return {...base, ...osContext, ...desktop};
};

export const getGatewayClientProperties = async (geo?: {latitude?: string | null; longitude?: string | null}) => {
	const info = await getClientInfo();
	return {
		os: info.osName ?? 'Unknown',
		os_version: info.osVersion ?? '',
		browser: info.browserName ?? 'Unknown',
		browser_version: info.browserVersion ?? '',
		device: info.arch ?? 'unknown',
		system_locale: navigator.language,
		locale: navigator.language,
		user_agent: navigator.userAgent,
		build_timestamp: Config.PUBLIC_BUILD_TIMESTAMP != null ? String(Config.PUBLIC_BUILD_TIMESTAMP) : '',
		build_sha: Config.PUBLIC_BUILD_SHA ?? '',
		build_number: Config.PUBLIC_BUILD_NUMBER != null ? Config.PUBLIC_BUILD_NUMBER : null,
		desktop_app_version: info.desktopVersion ?? null,
		desktop_app_channel: info.desktopChannel ?? null,
		desktop_arch: info.desktopArch ?? info.arch ?? null,
		desktop_os: info.desktopOS ?? info.osName ?? null,
		...(geo?.latitude ? {latitude: geo.latitude} : {}),
		...(geo?.longitude ? {longitude: geo.longitude} : {}),
	};
};

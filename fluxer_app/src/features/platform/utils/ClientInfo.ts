// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import Config from '@app/features/app/config/Config';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import type {DesktopInfo} from '@app/types/electron.d';
import Bowser from 'bowser';
import {isE2EESupported} from 'livekit-client';

const logger = new Logger('ClientInfoUtils');

export interface ClientInfo {
	browserName?: string;
	browserVersion?: string;
	osName?: string;
	osVersion?: string;
	systemVersion?: string;
	arch?: string;
	desktopVersion?: string;
	desktopChannel?: string;
	desktopArch?: string;
	desktopOS?: string;
	desktopRunningUnderRosetta?: boolean;
	desktopElectronVersion?: string;
	desktopChromeVersion?: string;
	desktopNodeVersion?: string;
}

interface NavigatorHighEntropyHints {
	architecture?: string;
	bitness?: string;
	platform?: string;
}

type NavigatorUADataLike = NavigatorHighEntropyHints & {
	getHighEntropyValues?: (hints: ReadonlyArray<keyof NavigatorHighEntropyHints>) => Promise<NavigatorHighEntropyHints>;
};

function normalize<T>(value: T | null | undefined): T | undefined {
	return value ?? undefined;
}

export function formatReleaseChannelLabel(value: string): string {
	const normalized = value.trim().toLowerCase();
	return normalized.charAt(0).toUpperCase() + normalized.slice(1);
}

const ARCHITECTURE_PATTERNS: ReadonlyArray<{
	pattern: RegExp;
	label: string;
}> = [
	{pattern: /\barm64\b|\baarch64\b|\barmv8\b|\barm64e\b/i, label: 'arm64'},
	{pattern: /\barm\b|\barmv7\b|\barmv6\b/i, label: 'arm'},
	{pattern: /MacIntel/i, label: 'x64'},
	{pattern: /\bx86_64\b|\bx64\b|\bamd64\b|\bwin64\b|\bwow64\b/i, label: 'x64'},
	{pattern: /\bx86\b|\bi[3-6]86\b/i, label: 'x86'},
];

export function normalizeArchitectureValue(value: string | null | undefined): string | undefined {
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
}

const detectAppleSiliconViaWebGL = (): string | undefined => {
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
	const userAgentData = (
		navigator as Navigator & {
			userAgentData?: NavigatorUADataLike;
		}
	).userAgentData;
	if (userAgentData?.architecture) {
		return normalizeArchitectureValue(userAgentData.architecture);
	}
	const userAgent = navigator.userAgent ?? '';
	const platform = navigator.platform ?? '';
	const isMac = isNavigatorPlatformMac(navigator);
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
	const userAgent = navigator.userAgent;
	const parser = Bowser.getParser(userAgent);
	const result = parser.getResult();
	const isMac = isNavigatorPlatformMac(navigator);
	const fallbackArch = !isMac ? normalizeArchitectureValue(navigator.platform) : undefined;
	const arch = detectArchitectureFromNavigator() ?? fallbackArch;
	return {
		browserName: normalize(result.browser.name),
		browserVersion: normalize(result.browser.version),
		osName: normalize(result.os.name),
		osVersion: normalize(result.os.version),
		arch: arch,
	};
};

export function getClientInfoSync(): ClientInfo {
	if (cachedClientInfo) {
		return cachedClientInfo;
	}
	try {
		return parseUserAgent();
	} catch {
		return {};
	}
}

export function preloadClientInfo(): Promise<ClientInfo> {
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
}

function getDesktopContextFromInfo(desktopInfo: DesktopInfo): Partial<ClientInfo> {
	return {
		desktopVersion: normalize(desktopInfo.version),
		desktopChannel: normalize(desktopInfo.channel),
		desktopArch: normalizeArchitectureValue(desktopInfo.hardwareArch ?? desktopInfo.arch),
		desktopOS: normalize(desktopInfo.os),
		desktopRunningUnderRosetta: desktopInfo.runningUnderRosetta,
		desktopElectronVersion: normalize(desktopInfo.electronVersion),
		desktopChromeVersion: normalize(desktopInfo.chromeVersion),
		desktopNodeVersion: normalize(desktopInfo.nodeVersion),
	};
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
	const userAgentData = (
		navigator as Navigator & {
			userAgentData?: NavigatorUADataLike;
		}
	).userAgentData;
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
		logger.warn(' Failed to load architecture hints', error);
		return undefined;
	}
};

function getOsContextFromInfo(desktopInfo: DesktopInfo): Partial<ClientInfo> {
	let osName: string | undefined;
	switch (desktopInfo.os) {
		case 'darwin':
			osName = 'macOS';
			break;
		case 'win32':
			osName = desktopInfo.osVersion ? getWindowsVersionName(desktopInfo.osVersion) : 'Windows';
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
}

async function getDesktopInfo(): Promise<DesktopInfo | null> {
	const electronApi = getElectronAPI();
	if (!electronApi) {
		return null;
	}
	try {
		return await electronApi.getDesktopInfo();
	} catch (error) {
		logger.warn(' Failed to load desktop info', error);
		return null;
	}
}

export async function getClientInfo(): Promise<ClientInfo> {
	const base = getClientInfoSync();
	if (!isDesktop()) {
		const hintsArch = await detectArchitectureFromClientHints();
		return {...base, arch: hintsArch ?? base.arch};
	}
	const desktopInfo = await getDesktopInfo();
	if (!desktopInfo) {
		return base;
	}
	return {...base, ...getOsContextFromInfo(desktopInfo), ...getDesktopContextFromInfo(desktopInfo)};
}

export function formatClientBuildInfo(info: ClientInfo, options: {unknownLabel?: string} = {}): string {
	const releaseChannel = formatReleaseChannelLabel(Config.PUBLIC_RELEASE_CHANNEL);
	const buildVersion = Config.PUBLIC_BUILD_VERSION || 'dev';
	const browserName = info.browserName || options.unknownLabel || '';
	const browserVersion = info.browserVersion || '';
	const browserInfo = `${browserName} ${browserVersion}`.trim();
	const osName = info.osName || options.unknownLabel || '';
	const osVersion = info.osVersion ?? '';
	const arch = info.desktopArch ?? info.arch;
	const osDescription = `${[osName, osVersion].filter(Boolean).join(' ')}${arch ? ` (${arch})` : ''}`.trim();
	const shouldShowBrowserInfo = !(info.desktopElectronVersion && browserName.toLowerCase() === 'electron');
	const desktopChannel = info.desktopChannel ? formatReleaseChannelLabel(info.desktopChannel) : null;
	const hasDesktopBuild = Boolean(info.desktopVersion);
	const primaryDesktopChannel = desktopChannel ?? releaseChannel;
	const webChannelPrefix = hasDesktopBuild && primaryDesktopChannel === releaseChannel ? '' : `${releaseChannel} `;
	const parts = [
		info.desktopVersion ? `${primaryDesktopChannel} Desktop ${info.desktopVersion}` : '',
		`${webChannelPrefix}Web ${buildVersion}`,
		osDescription,
		shouldShowBrowserInfo ? browserInfo : '',
		info.desktopElectronVersion ? `Electron ${info.desktopElectronVersion}` : '',
		info.desktopChromeVersion ? `Chrome ${info.desktopChromeVersion}` : '',
		info.desktopNodeVersion ? `Node ${info.desktopNodeVersion}` : '',
		i18n.locale ? `Locale ${i18n.locale}` : '',
	];
	return parts.filter(Boolean).join(', ');
}

export function getFormattedClientInfoSync(): string {
	return formatClientBuildInfo(getClientInfoSync());
}

export async function getFormattedClientInfo(): Promise<string> {
	return formatClientBuildInfo(await getClientInfo());
}

interface FluxerDebugApi {
	getClientInfo?: () => Promise<string>;
	getClientInfoSync?: () => string;
	getClientInfoObject?: () => Promise<ClientInfo>;
	getClientInfoObjectSync?: () => ClientInfo;
}

function getFluxerDebugObject(): (Record<string, unknown> & FluxerDebugApi) | null {
	if (typeof window === 'undefined') {
		return null;
	}
	const win = window as Window & {
		__FLUXER_DEBUG__?: Record<string, unknown> & FluxerDebugApi;
	};
	if (win.__FLUXER_DEBUG__ === undefined || win.__FLUXER_DEBUG__ === null) {
		win.__FLUXER_DEBUG__ = {};
	}
	if (typeof win.__FLUXER_DEBUG__ !== 'object' || Array.isArray(win.__FLUXER_DEBUG__)) {
		return null;
	}
	return win.__FLUXER_DEBUG__;
}

export function installFluxerConfigDebugApi(): void {
	const config = getFluxerDebugObject();
	if (!config) {
		return;
	}
	try {
		config.getClientInfo = getFormattedClientInfo;
		config.getClientInfoSync = getFormattedClientInfoSync;
		config.getClientInfoObject = getClientInfo;
		config.getClientInfoObjectSync = getClientInfoSync;
	} catch (error) {
		logger.warn(' Failed to install __FLUXER_DEBUG__ helpers', error);
	}
}

function isLiveKitE2EECapable(): boolean {
	if (typeof window === 'undefined' || typeof Worker === 'undefined') {
		return false;
	}
	if (!globalThis.crypto?.subtle) {
		return false;
	}
	try {
		return isE2EESupported();
	} catch (error) {
		logger.warn('Failed to detect LiveKit E2EE support', error);
		return false;
	}
}

export async function getGatewayClientProperties(geo?: {latitude?: string | null; longitude?: string | null}) {
	const info = await getClientInfo();
	return {
		os: info.osName ?? 'unknown',
		os_version: info.osVersion ?? '',
		browser: info.browserName ?? 'unknown',
		browser_version: info.browserVersion ?? '',
		device: info.arch ?? 'unknown',
		system_locale: navigator.language,
		locale: navigator.language,
		user_agent: navigator.userAgent,
		build_version: Config.PUBLIC_BUILD_VERSION ?? 'dev',
		desktop_app_version: info.desktopVersion ?? null,
		desktop_app_channel: info.desktopChannel ?? null,
		desktop_arch: info.desktopArch ?? info.arch ?? null,
		desktop_os: info.desktopOS ?? info.osName ?? null,
		e2ee_capable: isLiveKitE2EECapable(),
		...(geo?.latitude ? {latitude: geo.latitude} : {}),
		...(geo?.longitude ? {longitude: geo.longitude} : {}),
	};
}

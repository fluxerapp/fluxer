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
}

const normalize = <T>(value: T | null | undefined): T | undefined => value ?? undefined;

let cachedClientInfo: ClientInfo | null = null;
let preloadPromise: Promise<ClientInfo> | null = null;

const parseUserAgent = (): ClientInfo => {
	const hasNavigator = typeof navigator !== 'undefined';
	const userAgent = hasNavigator ? navigator.userAgent : '';
	const parser = Bowser.getParser(userAgent);
	const result = parser.getResult();
	return {
		browserName: normalize(result.browser.name),
		browserVersion: normalize(result.browser.version),
		osName: normalize(result.os.name),
		osVersion: normalize(result.os.version),
		arch: normalize(hasNavigator ? navigator.platform : undefined),
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
				desktopArch: normalize(desktopInfo.arch),
				desktopOS: normalize(desktopInfo.os),
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

async function getOsContext(): Promise<Partial<ClientInfo>> {
	const electronApi = getElectronAPI();
	if (electronApi) {
		try {
			const desktopInfo = await electronApi.getDesktopInfo();
			let osName: string | undefined;
			let osVersion: string | undefined;

			switch (desktopInfo.os) {
				case 'darwin':
					osName = 'macOS';
					break;
				case 'win32':
					osName = getWindowsVersionName(desktopInfo.osVersion);
					osVersion = desktopInfo.osVersion;
					break;
				case 'linux':
					osName = 'Linux';
					break;
				default:
					osName = desktopInfo.os;
			}
			return {
				osName,
				osVersion,
				arch: normalize(desktopInfo.arch),
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
	if (!isDesktop()) return base;

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

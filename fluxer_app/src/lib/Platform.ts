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

const userAgent = navigator.userAgent;
const hasNavigator = typeof navigator !== 'undefined';

const isIOSDevice = (() => {
	if (/iPhone|iPad|iPod/.test(userAgent)) return true;
	if (/Mac/.test(userAgent) && typeof navigator.maxTouchPoints === 'number' && navigator.maxTouchPoints > 1) {
		return true;
	}
	return false;
})();

const isAndroidDevice = /Android/.test(userAgent);
const isMobileBrowser = isIOSDevice || isAndroidDevice;
const isIOSWeb = isIOSDevice;
const isElectron = typeof (window as {electron?: unknown}).electron !== 'undefined';
const isPWA = typeof window.matchMedia === 'function' && window.matchMedia('(display-mode: standalone)').matches;

type PlatformSpecifics<T> = Partial<Record<string, T | undefined>> & {
	default?: T | undefined;
};

const selectValue = <T>(specifics: PlatformSpecifics<T>): T | undefined => {
	if (isElectron && specifics.electron !== undefined) {
		return specifics.electron;
	}
	if (specifics.web !== undefined) {
		return specifics.web;
	}
	if (specifics.default !== undefined) {
		return specifics.default;
	}
	return Object.values(specifics).find((value) => value !== undefined);
};

export const Platform = {
	OS: 'web' as const,
	isWeb: true,
	isIOS: isIOSDevice,
	isAndroid: isAndroidDevice,
	isElectron,
	isIOSWeb,
	isPWA,
	isAppleDevice: isIOSDevice,
	isMobileBrowser,
	select: selectValue,
};

export function isWebPlatform(): boolean {
	return Platform.isWeb;
}

export function isElectronPlatform(): boolean {
	return Platform.isElectron;
}

export function getNativeLocaleIdentifier(): string | null {
	if (!hasNavigator) {
		return null;
	}
	const languages = navigator.languages;
	if (Array.isArray(languages) && languages.length > 0) {
		return languages[0];
	}
	return navigator.language ?? null;
}

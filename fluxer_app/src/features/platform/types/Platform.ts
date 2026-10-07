// SPDX-License-Identifier: AGPL-3.0-or-later

const userAgent = navigator.userAgent;
const isIOSDevice = /iPad|iPhone|iPod/i.test(userAgent);
const isAndroidDevice = /Android/i.test(userAgent);
const isElectron =
	(
		window as {
			electron?: unknown;
		}
	).electron !== undefined;
const isIOSWeb = isIOSDevice && !isElectron;
const isPWA =
	window.matchMedia?.('(display-mode: standalone)').matches ||
	(
		navigator as {
			standalone?: boolean;
		}
	).standalone === true;
const isMobileBrowser = isIOSDevice || isAndroidDevice;

type PlatformSelector<T> = {
	web?: T;
	ios?: T;
	android?: T;
	electron?: T;
	default?: T;
};

function selectValue<T>(options: PlatformSelector<T>): T | undefined {
	if (isElectron && options.electron !== undefined) {
		return options.electron;
	}
	if (isIOSDevice && options.ios !== undefined) {
		return options.ios;
	}
	if (isAndroidDevice && options.android !== undefined) {
		return options.android;
	}
	if (options.web !== undefined) {
		return options.web;
	}
	return options.default;
}

export const Platform = {
	OS: 'web' as const,
	isWeb: true,
	isElectron,
	isIOSWeb,
	isPWA,
	isMobileBrowser,
	select: selectValue,
};

export function getNativeLocaleIdentifier(): string | null {
	const languages = navigator.languages;
	if (Array.isArray(languages) && languages.length > 0) {
		return languages[0];
	}
	return navigator.language ?? null;
}

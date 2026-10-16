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

export const Platform = {
	isElectron,
	isIOSWeb,
	isPWA,
	isMobileBrowser,
};

export function getNativeLocaleIdentifier(): string | null {
	const languages = navigator.languages;
	if (Array.isArray(languages) && languages.length > 0) {
		return languages[0];
	}
	return navigator.language ?? null;
}

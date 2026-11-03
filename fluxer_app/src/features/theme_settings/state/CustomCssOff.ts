// SPDX-License-Identifier: AGPL-3.0-or-later

import {getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';

const CUSTOM_CSS_OFF_STORAGE_KEY = 'QuickCss:customCssOff';
const CUSTOM_CSS_URL_PARAMETER = 'custom_css';

export function readCustomCssOff(): boolean {
	try {
		return getProtectedSessionStorage()?.getItem(CUSTOM_CSS_OFF_STORAGE_KEY) === '1';
	} catch {
		return false;
	}
}

export function writeCustomCssOff(off: boolean): void {
	try {
		const storage = getProtectedSessionStorage();
		if (off) storage?.setItem(CUSTOM_CSS_OFF_STORAGE_KEY, '1');
		else storage?.removeItem(CUSTOM_CSS_OFF_STORAGE_KEY);
	} catch {}
}

export function captureCustomCssOffFromLocation(): void {
	const url = new URL(window.location.href);
	const value = url.searchParams.get(CUSTOM_CSS_URL_PARAMETER);
	if (value === null) return;
	if (value === 'off' || value === 'on') writeCustomCssOff(value === 'off');
	url.searchParams.delete(CUSTOM_CSS_URL_PARAMETER);
	try {
		window.history.replaceState(window.history.state, '', url);
	} catch {}
}

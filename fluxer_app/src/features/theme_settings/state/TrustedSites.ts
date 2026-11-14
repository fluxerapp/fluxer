// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {trustableSiteOf} from '@app/features/theme/utils/ThemeCssUrlGate';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';

const TRUSTED_SITES_STORAGE_KEY = 'QuickCss:trustedSites';
const SCHEME_PATTERN = /^[a-z][a-z0-9+.-]*:/i;
export const MAX_TRUSTED_SITES = 50;

export function parseTrustedSite(input: string): string | null {
	const value = input.trim();
	if (value === '') return null;
	const address = SCHEME_PATTERN.test(value) ? value : `https://${value}`;
	const site = trustableSiteOf(address);
	if (site === null) return null;
	const url = new URL(address);
	return url.pathname === '/' && url.search === '' && url.hash === '' ? site : null;
}

export function readTrustedSites(): Array<string> {
	try {
		const raw = AppStorage.getItem(TRUSTED_SITES_STORAGE_KEY);
		if (raw === null) return [];
		const parsed: unknown = JSON.parse(raw);
		if (!Array.isArray(parsed)) return [];
		const sites = parsed.map((value) => (typeof value === 'string' ? parseTrustedSite(value) : null));
		return [...new Set(sites.filter((site) => site !== null))].slice(0, MAX_TRUSTED_SITES);
	} catch {
		return [];
	}
}

export function writeTrustedSites(sites: ReadonlyArray<string>): void {
	try {
		if (sites.length === 0) AppStorage.removeItem(TRUSTED_SITES_STORAGE_KEY);
		else AppStorage.setItem(TRUSTED_SITES_STORAGE_KEY, JSON.stringify(sites));
	} catch {}
}

export function canTrustSites(): boolean {
	return isDesktop();
}

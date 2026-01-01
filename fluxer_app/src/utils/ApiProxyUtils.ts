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

const DEFAULT_FLUXER_HOSTS = new Set([
	'api.fluxer.app',
	'api.canary.fluxer.app',
	'fluxer.app',
	'canary.fluxer.app',
	'web.fluxer.app',
	'web.canary.fluxer.app',
]);

const normalizeProxyPath = (path: string): string => {
	const trimmed = path.replace(/\/+$/, '');
	return trimmed === '' ? '/' : trimmed;
};

export function isElectronApiProxyUrl(raw: string): boolean {
	const base = getElectronApiProxyBaseUrl();
	if (!base) return false;

	try {
		const parsed = new URL(raw);
		if (parsed.origin !== base.origin) {
			return false;
		}

		const rawPath = normalizeProxyPath(parsed.pathname);
		const basePath = normalizeProxyPath(base.pathname);
		return rawPath === basePath;
	} catch {
		return false;
	}
}

export function isCustomInstanceUrl(url: string): boolean {
	try {
		const parsed = new URL(url);
		return !DEFAULT_FLUXER_HOSTS.has(parsed.hostname);
	} catch {
		return false;
	}
}

export function getElectronApiProxyBaseUrl(): URL | null {
	if (typeof window === 'undefined') {
		return null;
	}

	const getter = window.electron?.getApiProxyUrl;
	if (typeof getter !== 'function') {
		return null;
	}

	const raw = getter();
	if (!raw) return null;

	try {
		return new URL(raw);
	} catch {
		return null;
	}
}

export function wrapUrlWithElectronApiProxy(raw: string): string {
	const base = getElectronApiProxyBaseUrl();
	if (!base) return raw;
	if (isElectronApiProxyUrl(raw)) return raw;
	if (!isCustomInstanceUrl(raw)) return raw;

	const proxyUrl = new URL(base.toString());
	proxyUrl.searchParams.set('target', raw);
	return proxyUrl.toString();
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DESKTOP_LOCAL_APP_HOST,
	DESKTOP_LOCAL_APP_PROTOCOL,
	LOCAL_APP_API_PATH_PREFIX,
} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';

export function isDesktopLocalAppOrigin(protocol: string | null, hostname: string | null): boolean {
	return protocol === DESKTOP_LOCAL_APP_PROTOCOL && hostname === DESKTOP_LOCAL_APP_HOST;
}

export function isDesktopLocalAppDocument(): boolean {
	return 'window' in globalThis && isDesktopLocalAppOrigin(window.location.protocol, window.location.hostname);
}

export function parseDesktopLocalRouteKey(pathname: string, basePath: string): string | null {
	const prefix = `${basePath}/`;
	if (!pathname.startsWith(prefix)) return null;
	const start = prefix.length;
	const end = pathname.indexOf('/', start);
	const encodedRuntimeKey = end === -1 ? pathname.slice(start) : pathname.slice(start, end);
	if (encodedRuntimeKey.length === 0) return null;
	try {
		const runtimeKey = decodeURIComponent(encodedRuntimeKey);
		return runtimeKey.trim().length > 0 ? runtimeKey : null;
	} catch {
		return null;
	}
}

export function desktopLocalRuntimeKeyFromEndpoint(apiEndpoint: string): string | null {
	let endpointUrl: URL;
	try {
		endpointUrl = new URL(apiEndpoint);
	} catch {
		return null;
	}
	if (!isDesktopLocalAppOrigin(endpointUrl.protocol, endpointUrl.hostname)) return null;
	return parseDesktopLocalRouteKey(endpointUrl.pathname, LOCAL_APP_API_PATH_PREFIX);
}

export function desktopLocalApiEndpoint(instanceKey: string): string {
	const encodedInstanceKey = encodeURIComponent(instanceKey);
	return `${DESKTOP_LOCAL_APP_PROTOCOL}//${DESKTOP_LOCAL_APP_HOST}${LOCAL_APP_API_PATH_PREFIX}/${encodedInstanceKey}`;
}

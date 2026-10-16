// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_APP_HOST} from '@electron/common/Constants';
import {
	DESKTOP_LOCAL_APP_PROTOCOL,
	LOCAL_APP_API_PATH_PREFIX,
	LOCAL_APP_REMOTE_PROXY_PATH_PREFIX,
} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';

function parseLocalAppURL(value: string | null): URL | null {
	if (value == null || value.length === 0) {
		return null;
	}
	let parsed: URL;
	try {
		parsed = new URL(value);
	} catch {
		return null;
	}
	if (parsed.protocol !== DESKTOP_LOCAL_APP_PROTOCOL) {
		return null;
	}
	if (parsed.hostname !== DESKTOP_APP_HOST) {
		return null;
	}
	if (parsed.port !== '' || parsed.username !== '' || parsed.password !== '') {
		return null;
	}
	return parsed;
}

export function isLocalAppURL(value: string | null): boolean {
	return parseLocalAppURL(value) != null;
}

export function isReservedLocalAppProxyPath(pathname: string): boolean {
	if (pathname === LOCAL_APP_API_PATH_PREFIX || pathname.startsWith(`${LOCAL_APP_API_PATH_PREFIX}/`)) {
		return true;
	}
	return (
		pathname === LOCAL_APP_REMOTE_PROXY_PATH_PREFIX || pathname.startsWith(`${LOCAL_APP_REMOTE_PROXY_PATH_PREFIX}/`)
	);
}

export function isLocalAppRendererDocumentURL(value: string | null): boolean {
	const parsed = parseLocalAppURL(value);
	if (parsed == null) {
		return false;
	}
	return !isReservedLocalAppProxyPath(parsed.pathname);
}

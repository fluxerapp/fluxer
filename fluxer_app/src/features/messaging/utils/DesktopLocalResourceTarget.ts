// SPDX-License-Identifier: AGPL-3.0-or-later

import {isDesktopLocalAppOrigin, parseDesktopLocalRouteKey} from '@app/features/platform/DesktopLocalAppRuntime';
import {
	LOCAL_APP_REMOTE_PROXY_PATH_PREFIX,
	LOCAL_APP_REMOTE_PROXY_URL_PARAMETER,
} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';

export function parseDesktopLocalResourceProxyTarget(value: string): string {
	if (value.length === 0) return value;
	let parsedUrl: URL;
	try {
		parsedUrl = new URL(value);
	} catch {
		return value;
	}
	if (!isDesktopLocalAppOrigin(parsedUrl.protocol, parsedUrl.hostname)) return value;
	const runtimeKey = parseDesktopLocalRouteKey(parsedUrl.pathname, LOCAL_APP_REMOTE_PROXY_PATH_PREFIX);
	if (parsedUrl.pathname !== LOCAL_APP_REMOTE_PROXY_PATH_PREFIX && runtimeKey == null) return value;
	return parsedUrl.searchParams.get(LOCAL_APP_REMOTE_PROXY_URL_PARAMETER) ?? value;
}

export function unwrapDesktopLocalResourceURL(value: string): string {
	return parseDesktopLocalResourceProxyTarget(value);
}

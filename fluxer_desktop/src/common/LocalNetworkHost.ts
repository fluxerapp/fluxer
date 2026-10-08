// SPDX-License-Identifier: AGPL-3.0-or-later

import {isPublicIpAddress, parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';

const LOCAL_HOST_NAMES: ReadonlySet<string> = new Set(['localhost']);
const LOCAL_HOST_SUFFIXES: ReadonlyArray<string> = ['.localhost', '.local'];

export function isLocalNetworkHost(hostname: string): boolean {
	const host = hostname.trim().toLowerCase().replace(/^\[/u, '').replace(/\]$/u, '');
	if (host.length === 0) {
		return false;
	}
	if (parseIpAddress(host) != null) {
		return !isPublicIpAddress(host);
	}
	if (LOCAL_HOST_NAMES.has(host)) {
		return true;
	}
	return LOCAL_HOST_SUFFIXES.some((suffix) => host.endsWith(suffix));
}

export function isLocalNetworkUrl(value: string): boolean {
	try {
		return isLocalNetworkHost(new URL(value).hostname);
	} catch {
		return false;
	}
}

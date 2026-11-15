// SPDX-License-Identifier: AGPL-3.0-or-later

function collapsePort(url: URL): string {
	return url.port.length > 0 ? `:${url.port}` : '';
}

export function websocketOrigin(value: string): string | null {
	let url: URL;
	try {
		url = new URL(value);
	} catch {
		return null;
	}
	if (url.protocol === 'ws:' || url.protocol === 'http:') {
		return `ws://${url.hostname}${collapsePort(url)}`;
	}
	if (url.protocol === 'wss:' || url.protocol === 'https:') {
		return `wss://${url.hostname}${collapsePort(url)}`;
	}
	return null;
}

export function websocketHTTPOrigin(value: string): string | null {
	let url: URL;
	try {
		url = new URL(value);
	} catch {
		return null;
	}
	if (url.protocol === 'ws:' || url.protocol === 'http:') {
		return `http://${url.hostname}${collapsePort(url)}`;
	}
	if (url.protocol === 'wss:' || url.protocol === 'https:') {
		return `https://${url.hostname}${collapsePort(url)}`;
	}
	return null;
}

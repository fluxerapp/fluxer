// SPDX-License-Identifier: AGPL-3.0-or-later

export function buildExternalMediaProxyPath(inputUrl: string): string {
	const parsedUrl = new URL(inputUrl);
	const protocol = parsedUrl.protocol.replace(/:$/u, '');
	const host = parsedUrl.port ? `${parsedUrl.hostname}:${parsedUrl.port}` : parsedUrl.hostname;
	const path = parsedUrl.pathname
		.replace(/^\//u, '')
		.split('/')
		.map((segment) => encodeURIComponent(segment))
		.join('/');
	const segments = parsedUrl.search ? [encodeURIComponent(parsedUrl.search)] : [];
	segments.push(protocol, host);
	if (path) {
		segments.push(path);
	}
	return segments.join('/');
}

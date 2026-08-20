// SPDX-License-Identifier: AGPL-3.0-or-later

const BASE64_URL_PADDING_REGEX = /=*$/;
const LEGACY_PROTOCOL_REGEX = /^[A-Za-z][A-Za-z0-9+.-]*$/;
const V2_PATH_PREFIX = 'v2/';

function encodeV2PathComponent(value: string): string {
	return Buffer.from(value, 'utf8').toString('base64url').replace(BASE64_URL_PADDING_REGEX, '');
}

function decodeV2PathComponent(value: string): string {
	return Buffer.from(value, 'base64url').toString('utf8');
}

function decodeLegacyComponent(component: string): string {
	return decodeURIComponent(component);
}

function getLegacyProtocolIndex(parts: Array<string>): number {
	const firstPart = parts[0];
	if (firstPart && LEGACY_PROTOCOL_REGEX.test(firstPart)) {
		return 0;
	}
	if (firstPart?.includes('%3D')) {
		return 1;
	}
	for (let index = 1; index < parts.length - 1; index += 1) {
		const part = parts[index];
		if (part && LEGACY_PROTOCOL_REGEX.test(part)) {
			return index;
		}
	}
	throw new Error('Protocol is missing in the proxy URL path.');
}

interface LegacyHostAndPort {
	hostname: string;
	port: string;
}

function decodeLegacyHostAndPort(hostPart: string): LegacyHostAndPort {
	const separatorIndex = hostPart.lastIndexOf(':');
	if (separatorIndex === -1) {
		const hostname = decodeLegacyComponent(hostPart);
		if (!hostname) {
			throw new Error('Hostname is invalid in the proxy URL path.');
		}
		return {hostname, port: ''};
	}
	const encodedHostname = hostPart.slice(0, separatorIndex);
	const encodedPort = hostPart.slice(separatorIndex + 1);
	if (!encodedHostname) {
		throw new Error('Hostname is invalid in the proxy URL path.');
	}
	return {
		hostname: decodeLegacyComponent(encodedHostname),
		port: encodedPort ? decodeLegacyComponent(encodedPort) : '',
	};
}

function reconstructLegacyOriginalUrl(proxyUrlPath: string): string {
	const parts = proxyUrlPath.split('/');
	const protocolIndex = getLegacyProtocolIndex(parts);
	const protocol = parts[protocolIndex];
	if (!protocol) {
		throw new Error('Protocol is missing in the proxy URL path.');
	}
	const hostPart = parts[protocolIndex + 1];
	if (!hostPart) {
		throw new Error('Hostname is missing in the proxy URL path.');
	}
	const encodedQuery = parts.slice(0, protocolIndex).join('/');
	const encodedPath = parts.slice(protocolIndex + 2).join('/');
	const query = encodedQuery ? decodeLegacyComponent(encodedQuery) : '';
	const path = decodeLegacyComponent(encodedPath);
	const {hostname, port} = decodeLegacyHostAndPort(hostPart);
	const normalizedQuery = query.startsWith('?') ? query.slice(1) : query;
	return `${protocol}://${hostname}${port ? `:${port}` : ''}/${path}${normalizedQuery ? `?${normalizedQuery}` : ''}`;
}

function reconstructV2OriginalUrl(proxyUrlPath: string): string {
	const encodedOriginalUrl = proxyUrlPath.slice(V2_PATH_PREFIX.length);
	if (!encodedOriginalUrl) {
		throw new Error('Encoded URL is missing in the proxy URL path.');
	}
	return decodeV2PathComponent(encodedOriginalUrl);
}

export function buildV2ExternalMediaProxyPath(inputUrl: string): string {
	const parsedUrl = new URL(inputUrl);
	return `${V2_PATH_PREFIX}${encodeV2PathComponent(parsedUrl.toString())}`;
}

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

export function reconstructOriginalUrl(proxyUrlPath: string): string {
	const reconstructedUrl = proxyUrlPath.startsWith(V2_PATH_PREFIX)
		? reconstructV2OriginalUrl(proxyUrlPath)
		: reconstructLegacyOriginalUrl(proxyUrlPath);
	new URL(reconstructedUrl);
	return reconstructedUrl;
}

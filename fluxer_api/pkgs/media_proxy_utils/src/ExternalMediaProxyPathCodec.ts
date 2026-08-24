// SPDX-License-Identifier: AGPL-3.0-or-later

const BASE64_URL_PADDING_REGEX = /=*$/;
const LEGACY_PROTOCOL_REGEX = /^[A-Za-z][A-Za-z0-9+.-]*$/;
const OPAQUE_PATH_PREFIX = 'v2/';

function encodeOpaquePathComponent(value: string): string {
	return Buffer.from(value, 'utf8').toString('base64url').replace(BASE64_URL_PADDING_REGEX, '');
}

function decodeOpaquePathComponent(value: string): string {
	return Buffer.from(value, 'base64url').toString('utf8');
}

function decodeSegmentedComponent(component: string): string {
	return decodeURIComponent(component);
}

function getSegmentedProtocolIndex(parts: Array<string>): number {
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

interface SegmentedHostAndPort {
	hostname: string;
	port: string;
}

function decodeSegmentedHostAndPort(hostPart: string): SegmentedHostAndPort {
	const separatorIndex = hostPart.lastIndexOf(':');
	if (separatorIndex === -1) {
		const hostname = decodeSegmentedComponent(hostPart);
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
		hostname: decodeSegmentedComponent(encodedHostname),
		port: encodedPort ? decodeSegmentedComponent(encodedPort) : '',
	};
}

function reconstructSegmentedOriginalUrl(proxyUrlPath: string): string {
	const parts = proxyUrlPath.split('/');
	const protocolIndex = getSegmentedProtocolIndex(parts);
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
	const query = encodedQuery ? decodeSegmentedComponent(encodedQuery) : '';
	const path = decodeSegmentedComponent(encodedPath);
	const {hostname, port} = decodeSegmentedHostAndPort(hostPart);
	const normalizedQuery = query.startsWith('?') ? query.slice(1) : query;
	return `${protocol}://${hostname}${port ? `:${port}` : ''}/${path}${normalizedQuery ? `?${normalizedQuery}` : ''}`;
}

function reconstructOpaqueOriginalUrl(proxyUrlPath: string): string {
	const encodedOriginalUrl = proxyUrlPath.slice(OPAQUE_PATH_PREFIX.length);
	if (!encodedOriginalUrl) {
		throw new Error('Encoded URL is missing in the proxy URL path.');
	}
	return decodeOpaquePathComponent(encodedOriginalUrl);
}

export function buildOpaqueExternalMediaProxyPath(inputUrl: string): string {
	const parsedUrl = new URL(inputUrl);
	return `${OPAQUE_PATH_PREFIX}${encodeOpaquePathComponent(parsedUrl.toString())}`;
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
	const reconstructedUrl = proxyUrlPath.startsWith(OPAQUE_PATH_PREFIX)
		? reconstructOpaqueOriginalUrl(proxyUrlPath)
		: reconstructSegmentedOriginalUrl(proxyUrlPath);
	new URL(reconstructedUrl);
	return reconstructedUrl;
}

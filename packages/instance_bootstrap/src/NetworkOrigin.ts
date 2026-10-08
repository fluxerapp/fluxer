// SPDX-License-Identifier: AGPL-3.0-or-later

export const CanonicalNetworkProtocol = Object.freeze({
	HTTP: 'http:',
	HTTPS: 'https:',
	WS: 'ws:',
	WSS: 'wss:',
} as const);

export type CanonicalNetworkProtocol = (typeof CanonicalNetworkProtocol)[keyof typeof CanonicalNetworkProtocol];

export const HTTP_NETWORK_PROTOCOLS: ReadonlyArray<CanonicalNetworkProtocol> = Object.freeze([
	CanonicalNetworkProtocol.HTTP,
	CanonicalNetworkProtocol.HTTPS,
]);

export const WEBSOCKET_NETWORK_PROTOCOLS: ReadonlyArray<CanonicalNetworkProtocol> = Object.freeze([
	CanonicalNetworkProtocol.WS,
	CanonicalNetworkProtocol.WSS,
]);

export const NETWORK_ENDPOINT_INPUT_MAX_BYTES = 8192;

export interface CanonicalNetworkEndpointRule {
	readonly protocols: ReadonlyArray<CanonicalNetworkProtocol>;
	readonly allowPath: boolean;
	readonly allowRelative: boolean;
}

const NETWORK_PROTOCOL_PREFIX = /^[a-zA-Z][a-zA-Z0-9+.-]*:\/\//u;
const AUTHORITY_TERMINATOR = /[/?#]/u;

const NETWORK_ORIGIN_ONLY_RULE: CanonicalNetworkEndpointRule = Object.freeze({
	protocols: HTTP_NETWORK_PROTOCOLS,
	allowPath: false,
	allowRelative: false,
});

function exceedsInputByteCap(value: string): boolean {
	return new TextEncoder().encode(value).length > NETWORK_ENDPOINT_INPUT_MAX_BYTES;
}

function stripTrailingSlashes(pathname: string): string {
	let end = pathname.length;
	while (end > 0 && pathname[end - 1] === '/') {
		end -= 1;
	}
	return pathname.slice(0, end);
}

function networkAuthority(candidate: string): string {
	const schemeSeparator = candidate.indexOf('://');
	if (schemeSeparator < 0) {
		return '';
	}
	const authorityStart = schemeSeparator + 3;
	const suffixOffset = candidate.slice(authorityStart).search(AUTHORITY_TERMINATOR);
	if (suffixOffset < 0) {
		return candidate.slice(authorityStart);
	}
	return candidate.slice(authorityStart, authorityStart + suffixOffset);
}

function normalizeRelativeNetworkEndpoint(trimmed: string): string | null {
	if (trimmed.startsWith('//')) {
		return null;
	}
	if (trimmed.includes('?') || trimmed.includes('#')) {
		return null;
	}
	const path = stripTrailingSlashes(trimmed);
	if (path.length === 0) {
		return null;
	}
	return path;
}

function networkOriginCandidate(trimmed: string): string {
	if (NETWORK_PROTOCOL_PREFIX.test(trimmed)) {
		return trimmed;
	}
	return `${CanonicalNetworkProtocol.HTTPS}//${trimmed}`;
}

export function normalizeCanonicalNetworkEndpoint(value: unknown, rule: CanonicalNetworkEndpointRule): string | null {
	if (typeof value !== 'string' || exceedsInputByteCap(value)) {
		return null;
	}
	const trimmed = value.trim();
	if (trimmed.length === 0) {
		return null;
	}
	if (trimmed.startsWith('/')) {
		if (!rule.allowRelative) {
			return null;
		}
		return normalizeRelativeNetworkEndpoint(trimmed);
	}
	const candidate = networkOriginCandidate(trimmed);
	const authority = networkAuthority(candidate);
	if (authority.length === 0) {
		return null;
	}
	let url: URL;
	try {
		url = new URL(candidate);
	} catch {
		return null;
	}
	if (!rule.protocols.some((protocol) => protocol === url.protocol)) {
		return null;
	}
	if (url.hostname.length === 0 || url.username.length > 0 || url.password.length > 0) {
		return null;
	}
	if (url.port === '0' || url.search.length > 0 || url.hash.length > 0) {
		return null;
	}
	const path = stripTrailingSlashes(url.pathname);
	if (path.length > 0 && !rule.allowPath) {
		return null;
	}
	let origin = `${url.protocol}//${url.hostname}`;
	if (url.port.length > 0) {
		origin = `${origin}:${url.port}`;
	}
	return `${origin}${path}`;
}

export function normalizeHTTPNetworkOrigin(value: unknown): string | null {
	return normalizeCanonicalNetworkEndpoint(value, NETWORK_ORIGIN_ONLY_RULE);
}

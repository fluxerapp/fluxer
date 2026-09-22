// SPDX-License-Identifier: AGPL-3.0-or-later

export const HTTPMethod = Object.freeze({
	GET: 'GET',
	HEAD: 'HEAD',
} as const);

export type HTTPMethod = (typeof HTTPMethod)[keyof typeof HTTPMethod];

export const HTTPStatusCode = Object.freeze({
	OK: 200,
	MOVED_PERMANENTLY: 301,
	BAD_REQUEST: 400,
	NOT_FOUND: 404,
	METHOD_NOT_ALLOWED: 405,
	PAYLOAD_TOO_LARGE: 413,
	INTERNAL_SERVER_ERROR: 500,
	SERVICE_UNAVAILABLE: 503,
} as const);

export type HTTPStatusCode = (typeof HTTPStatusCode)[keyof typeof HTTPStatusCode];

export const MIMEType = Object.freeze({
	JSON: 'application/json',
	PLAIN: 'text/plain',
} as const);

export const HTTPHeader = Object.freeze({
	ALLOW: 'Allow',
	CACHE_CONTROL: 'Cache-Control',
	CONNECTION: 'Connection',
	CONTENT_LENGTH: 'Content-Length',
	CONTENT_TYPE: 'Content-Type',
	EXPECT: 'Expect',
	LOCATION: 'Location',
	PERMISSIONS_POLICY: 'Permissions-Policy',
	REFERRER_POLICY: 'Referrer-Policy',
	STRICT_TRANSPORT_SECURITY: 'Strict-Transport-Security',
	TRANSFER_ENCODING: 'Transfer-Encoding',
	USER_AGENT: 'User-Agent',
	X_CONTENT_TYPE_OPTIONS: 'X-Content-Type-Options',
	X_FRAME_OPTIONS: 'X-Frame-Options',
} as const);

export const OUTBOUND_USER_AGENT = 'Fluxer (https://fluxer.app)';

export const CanonicalNetworkProtocol = Object.freeze({
	HTTP: 'http:',
	HTTPS: 'https:',
} as const);

export type CanonicalNetworkProtocol = (typeof CanonicalNetworkProtocol)[keyof typeof CanonicalNetworkProtocol];

export function isCanonicalHTTPNetworkProtocol(protocol: string): boolean {
	if (protocol === CanonicalNetworkProtocol.HTTP) {
		return true;
	}
	return protocol === CanonicalNetworkProtocol.HTTPS;
}

export const NodeErrorCode = Object.freeze({
	NOT_FOUND: 'ENOENT',
	NOT_DIRECTORY: 'ENOTDIR',
} as const);

export function getNodeErrorCode(error: unknown): string | null {
	if (typeof error !== 'object') {
		return null;
	}
	if (error == null) {
		return null;
	}
	if (!('code' in error)) {
		return null;
	}
	const code = (error as {code: unknown}).code;
	if (typeof code !== 'string') {
		return null;
	}
	return code;
}

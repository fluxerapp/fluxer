// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_PROTOCOL_AUTHORIZATION_HEADER} from '@electron/main/LocalAppProtocolAuthorization';
import {httpOriginSource, type LocalAppRuntimePlan} from '@electron/main/LocalAppRuntimePlans';
import {HttpStatus, MimeType} from '@fluxer/constants/src/HttpConstants';
import {
	LOCAL_APP_REMOTE_PROXY_URL_PARAMETER,
	LOCAL_APP_UPLOAD_ID_HEADER,
	LOCAL_APP_UPLOAD_RELAY_PATH_SUFFIX,
	LOCAL_APP_UPLOAD_RELAY_TOKEN_PARAMETER,
} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';

const LOCAL_APP_PROXY_GET_METHOD = 'GET';
export const LOCAL_APP_PROXY_HEAD_METHOD = 'HEAD';
const LOCAL_APP_PROXY_PUT_METHOD = 'PUT';

const TEXT_CONTENT_TYPE = `${MimeType.PLAIN}; charset=utf-8`;
const NO_STORE_CACHE_CONTROL = 'no-store';
const PROXY_RESPONSE_DOCUMENT_POLICY = "sandbox; default-src 'none'; frame-ancestors 'none'";
const RELAY_PATH_SEGMENT_PATTERN = /(?:^|\/)relay(?:\/|$)/u;
const UPLOAD_RELAY_BASE_SUFFIX_PATTERN = /\/v1\/relay$/u;
const UPLOAD_RELAY_PATH_PREFIX = `/v1${LOCAL_APP_UPLOAD_RELAY_PATH_SUFFIX}`;
const UPLOAD_RELAY_MULTIPART_PARAMETERS: ReadonlySet<string> = new Set(['uploadId', 'partNumber']);
const UPLOAD_ID_PATTERN = /^[A-Za-z0-9_-]{1,128}$/u;
const CONTENT_LENGTH_PATTERN = /^\d+$/u;

const HTTP_HOP_BY_HOP_HEADER_NAMES: ReadonlyArray<string> = Object.freeze([
	'connection',
	'keep-alive',
	'proxy-authenticate',
	'proxy-authorization',
	'te',
	'trailer',
	'transfer-encoding',
	'upgrade',
]);

export const LOCAL_APP_PROXY_TEXT_RESPONSE_HEADERS: Readonly<Record<string, string>> = Object.freeze({
	'Content-Type': TEXT_CONTENT_TYPE,
});

export const LOCAL_APP_PROXY_FAILURE_HEADERS: Readonly<Record<string, string>> = Object.freeze({
	'Content-Type': TEXT_CONTENT_TYPE,
	'Cache-Control': NO_STORE_CACHE_CONTROL,
});

export const LOCAL_APP_REMOTE_PROXY_METHOD_NOT_ALLOWED_HEADERS: Readonly<Record<string, string>> = Object.freeze({
	Allow: [LOCAL_APP_PROXY_GET_METHOD, LOCAL_APP_PROXY_HEAD_METHOD, LOCAL_APP_PROXY_PUT_METHOD].join(', '),
	'Content-Type': TEXT_CONTENT_TYPE,
});

const API_REQUEST_HEADER_BLOCKLIST: ReadonlySet<string> = new Set([
	...HTTP_HOP_BY_HOP_HEADER_NAMES,
	DESKTOP_PROTOCOL_AUTHORIZATION_HEADER.toLowerCase(),
	LOCAL_APP_UPLOAD_ID_HEADER.toLowerCase(),
	'accept-encoding',
	'content-length',
	'cookie',
	'expect',
	'host',
	'origin',
	'proxy-connection',
	'referer',
]);

const REMOTE_RESOURCE_REQUEST_HEADER_BLOCKLIST: ReadonlySet<string> = new Set([
	...API_REQUEST_HEADER_BLOCKLIST,
	'authorization',
	'x-fluxer-sudo-mode-jwt',
]);

const PROXY_RESPONSE_HEADER_BLOCKLIST: ReadonlySet<string> = new Set([
	...HTTP_HOP_BY_HOP_HEADER_NAMES,
	'access-control-allow-credentials',
	'access-control-allow-headers',
	'access-control-allow-methods',
	'access-control-allow-origin',
	'access-control-expose-headers',
	'access-control-max-age',
	'content-encoding',
	'content-length',
	'content-security-policy',
	'content-security-policy-report-only',
	'set-cookie',
	'set-cookie2',
]);

export const LocalAppProxyCacheDefault = Object.freeze({
	NO_STORE: 'no-store',
	UPSTREAM: 'upstream',
} as const);

export type LocalAppProxyCacheDefault = (typeof LocalAppProxyCacheDefault)[keyof typeof LocalAppProxyCacheDefault];

interface BuildProxyResponseHeadersRequest {
	readonly headers: Headers;
	readonly cacheDefault: LocalAppProxyCacheDefault;
}

interface BuildAPIRequestHeadersRequest {
	readonly headers: Headers;
	readonly method: string;
	readonly origin: string | null;
}

interface BuildAPITargetURLRequest {
	readonly apiEndpoint: string;
	readonly requestURL: string;
	readonly localPathPrefix: string;
}

interface RemoteProxyTargetAdmissionRequest {
	readonly method: string;
	readonly targetURL: string;
	readonly plan: LocalAppRuntimePlan;
}

interface UploadRelayEndpoints {
	readonly target: URL;
	readonly endpoint: URL;
}

export function localAppProxyNotFoundResponse(message: string): Response {
	return new Response(message, {status: HttpStatus.NOT_FOUND, headers: LOCAL_APP_PROXY_TEXT_RESPONSE_HEADERS});
}

export function readLocalAppUploadId(headers: Headers): string | null {
	const value = headers.get(LOCAL_APP_UPLOAD_ID_HEADER);
	if (value == null || !UPLOAD_ID_PATTERN.test(value)) {
		return null;
	}
	return value;
}

export function readRequestContentLength(headers: Headers): number | null {
	const value = headers.get('Content-Length');
	if (value == null) {
		return null;
	}
	const normalized = value.trim();
	if (!CONTENT_LENGTH_PATTERN.test(normalized)) {
		return null;
	}
	const parsed = Number.parseInt(normalized, 10);
	return Number.isSafeInteger(parsed) ? parsed : null;
}

export function requestHasBody(method: string): boolean {
	return method !== LOCAL_APP_PROXY_GET_METHOD && method !== LOCAL_APP_PROXY_HEAD_METHOD;
}

export function buildAPIRequestHeaders({headers, method, origin}: BuildAPIRequestHeadersRequest): Headers {
	const next = copyAllowedHeaders(headers, API_REQUEST_HEADER_BLOCKLIST);
	if (origin != null && requestHasBody(method)) {
		next.set('Origin', origin);
	}
	return next;
}

export function buildRemoteResourceRequestHeaders(headers: Headers): Headers {
	return copyAllowedHeaders(headers, REMOTE_RESOURCE_REQUEST_HEADER_BLOCKLIST);
}

export function buildProxyResponseHeaders({headers, cacheDefault}: BuildProxyResponseHeadersRequest): Headers {
	const next = copyAllowedHeaders(headers, PROXY_RESPONSE_HEADER_BLOCKLIST);
	if (cacheDefault === LocalAppProxyCacheDefault.NO_STORE && next.get('Cache-Control') == null) {
		next.set('Cache-Control', NO_STORE_CACHE_CONTROL);
	}
	next.set('Content-Security-Policy', PROXY_RESPONSE_DOCUMENT_POLICY);
	next.set('X-Content-Type-Options', 'nosniff');
	return next;
}

export function buildAPITargetURL({apiEndpoint, requestURL, localPathPrefix}: BuildAPITargetURLRequest): string {
	const request = new URL(requestURL);
	const target = new URL(withTrailingSlash(apiEndpoint));
	const basePath = normalizeAPIEndpointPath(target.pathname);
	target.pathname = `${basePath}${apiTargetSuffixPath(request.pathname, localPathPrefix)}`;
	target.search = request.search;
	return target.toString();
}

export function remoteProxyTargetURL(requestURL: string): string | null {
	let rawURL: string | null;
	try {
		rawURL = new URL(requestURL).searchParams.get(LOCAL_APP_REMOTE_PROXY_URL_PARAMETER);
	} catch {
		return null;
	}
	if (rawURL == null || rawURL.length === 0) {
		return null;
	}
	let target: URL;
	try {
		target = new URL(rawURL);
	} catch {
		return null;
	}
	if (!isHTTPProtocol(target.protocol)) {
		return null;
	}
	if (target.username.length > 0 || target.password.length > 0) {
		return null;
	}
	return target.toString();
}

export function isSupportedRemoteResourceMethod(method: string): boolean {
	if (method === LOCAL_APP_PROXY_GET_METHOD || method === LOCAL_APP_PROXY_HEAD_METHOD) {
		return true;
	}
	return method === LOCAL_APP_PROXY_PUT_METHOD;
}

export function isAllowedRemoteProxyTarget({method, targetURL, plan}: RemoteProxyTargetAdmissionRequest): boolean {
	if (method === LOCAL_APP_PROXY_PUT_METHOD) {
		return isAllowedUploadRelayTarget(targetURL, plan);
	}
	return isAllowedRemoteResourceTarget(targetURL, plan);
}

function isAllowedRemoteResourceTarget(targetURL: string, plan: LocalAppRuntimePlan): boolean {
	const targetOrigin = httpOriginSource(targetURL);
	if (targetOrigin == null) {
		return false;
	}
	const allowedSources: ReadonlyArray<string | null> = [
		plan.endpoints.apiEndpoint,
		plan.endpoints.apiPublicEndpoint,
		plan.endpoints.webAppEndpoint,
		plan.endpoints.mediaEndpoint,
		plan.endpoints.staticCdnEndpoint,
	];
	for (const source of allowedSources) {
		if (httpOriginSource(source) === targetOrigin) {
			return true;
		}
	}
	return false;
}

function isAllowedUploadRelayTarget(targetURL: string, plan: LocalAppRuntimePlan): boolean {
	const endpoints = parseUploadRelayEndpoints(targetURL, plan.endpoints.uploadRelayEndpoint);
	if (endpoints == null) {
		return false;
	}
	if (!isAllowedUploadRelayEndpoint(endpoints.endpoint)) {
		return false;
	}
	if (!matchesUploadRelayEndpointIdentity(endpoints)) {
		return false;
	}
	return hasAllowedUploadRelayPathAndToken(endpoints);
}

function copyAllowedHeaders(headers: Headers, blocklist: ReadonlySet<string>): Headers {
	const next = new Headers();
	headers.forEach((value, key) => {
		if (!blocklist.has(key.toLowerCase())) {
			next.append(key, value);
		}
	});
	return next;
}

function parseUploadRelayEndpoints(targetURL: string, uploadRelayEndpoint: string | null): UploadRelayEndpoints | null {
	if (uploadRelayEndpoint == null || uploadRelayEndpoint.length === 0) {
		return null;
	}
	try {
		return {target: new URL(targetURL), endpoint: new URL(uploadRelayEndpoint)};
	} catch {
		return null;
	}
}

function isAllowedUploadRelayEndpoint(endpoint: URL): boolean {
	if (!isHTTPProtocol(endpoint.protocol)) {
		return false;
	}
	if (endpoint.username.length > 0 || endpoint.password.length > 0) {
		return false;
	}
	if (endpoint.search.length > 0 || endpoint.hash.length > 0) {
		return false;
	}
	if (endpoint.pathname !== '/' && endpoint.pathname.endsWith('/')) {
		return false;
	}
	return !RELAY_PATH_SEGMENT_PATTERN.test(uploadRelayBasePath(endpoint));
}

function matchesUploadRelayEndpointIdentity({target, endpoint}: UploadRelayEndpoints): boolean {
	if (target.protocol !== endpoint.protocol) {
		return false;
	}
	if (target.username.length > 0 || target.password.length > 0 || target.hash.length > 0) {
		return false;
	}
	return target.origin === endpoint.origin;
}

function hasAllowedUploadRelayPathAndToken({target, endpoint}: UploadRelayEndpoints): boolean {
	const relayKeyPrefix = uploadRelayKeyPrefix(endpoint);
	if (!target.pathname.startsWith(relayKeyPrefix) || target.pathname.length === relayKeyPrefix.length) {
		return false;
	}
	return hasAllowedUploadRelayQuery(target.searchParams);
}

function hasAllowedUploadRelayQuery(searchParams: URLSearchParams): boolean {
	let token: string | null = null;
	for (const [parameterName, parameterValue] of searchParams.entries()) {
		if (parameterName === LOCAL_APP_UPLOAD_RELAY_TOKEN_PARAMETER) {
			if (token !== null) {
				return false;
			}
			token = parameterValue;
			continue;
		}
		if (UPLOAD_RELAY_MULTIPART_PARAMETERS.has(parameterName)) {
			continue;
		}
		return false;
	}
	return token !== null && token.length > 0;
}

function uploadRelayKeyPrefix(endpoint: URL): string {
	const basePath = uploadRelayBasePath(endpoint);
	return `${basePath}${UPLOAD_RELAY_PATH_PREFIX}/`;
}

function uploadRelayBasePath(endpoint: URL): string {
	if (endpoint.pathname === '/') {
		return '';
	}
	return endpoint.pathname.replace(UPLOAD_RELAY_BASE_SUFFIX_PATTERN, '');
}

function apiTargetSuffixPath(pathname: string, localPathPrefix: string): string {
	const suffix = pathname.slice(localPathPrefix.length);
	if (suffix.length === 0) {
		return '/';
	}
	if (suffix.startsWith('/')) {
		return suffix;
	}
	return `/${suffix}`;
}

function normalizeAPIEndpointPath(pathname: string): string {
	if (pathname === '/' || pathname === '') {
		return '';
	}
	return pathname.replace(/\/+$/u, '');
}

function withTrailingSlash(value: string): string {
	return value.endsWith('/') ? value : `${value}/`;
}

function isHTTPProtocol(protocol: string): boolean {
	return protocol === 'http:' || protocol === 'https:';
}

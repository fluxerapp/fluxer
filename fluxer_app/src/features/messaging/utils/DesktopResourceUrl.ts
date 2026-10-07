// SPDX-License-Identifier: AGPL-3.0-or-later

import {runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	parseDesktopLocalResourceProxyTarget,
	unwrapDesktopLocalResourceURL,
} from '@app/features/messaging/utils/DesktopLocalResourceTarget';
import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import {
	LOCAL_APP_REMOTE_PROXY_PATH_PREFIX,
	LOCAL_APP_REMOTE_PROXY_URL_PARAMETER,
	LOCAL_APP_UPLOAD_RELAY_PATH_SUFFIX,
	LOCAL_APP_UPLOAD_RELAY_TOKEN_PARAMETER,
} from '@fluxer/desktop_ipc/src/LocalAppRouteContract';

export {unwrapDesktopLocalResourceURL};

const UPLOAD_RELAY_PATH_PREFIX = `/v1${LOCAL_APP_UPLOAD_RELAY_PATH_SUFFIX}`;
const UPLOAD_RELAY_MULTIPART_PARAMETERS: ReadonlySet<string> = new Set(['uploadId', 'partNumber']);
const UPLOAD_RELAY_BASE_PATH_PATTERN = /(?:^|\/)relay(?:\/|$)/u;
const UPLOAD_RELAY_BASE_SUFFIX_PATTERN = /\/v1\/relay$/u;
const TRAILING_SLASHES_PATTERN = /\/+$/u;
const LOOPBACK_IPV4_PATTERN = /^127(?:\.\d{1,3}){3}$/u;

class DesktopUploadRelayUrlError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'DesktopUploadRelayUrlError';
	}
}

class DesktopResourceRuntimeKeyError extends Error {
	constructor() {
		super('Desktop resource proxy requires a valid instance runtime key');
		this.name = 'DesktopResourceRuntimeKeyError';
	}
}

export function wrapDesktopLocalResourceURL(value: string): string {
	return wrapResolvedDesktopLocalResourceURL(parseDesktopLocalResourceProxyTarget(value), false);
}

export function wrapDesktopLocalResourceURLForInstance(value: string, instanceKey: string): string {
	if (!isDesktopLocalAppDocument()) return value;
	if (instanceKey.trim().length === 0) {
		throw new DesktopResourceRuntimeKeyError();
	}
	const targetValue = parseDesktopLocalResourceProxyTarget(value);
	let target: URL;
	try {
		target = new URL(targetValue);
	} catch {
		return value;
	}
	if (target.protocol !== 'http:' && target.protocol !== 'https:') return target.toString();
	return buildDesktopLocalResourceProxyURL(target, instanceKey);
}

export function resolveDesktopDisplayResourceURL(value: string): string {
	if (!isDesktopLocalAppDocument()) return value;
	const target = parseDisplayTarget(value);
	if (target == null) return value;
	if (isDirectlyDisplayableTarget(target)) return target.toString();
	return wrapResolvedDesktopLocalResourceURL(target.toString(), false);
}

export function resolveDesktopDisplayResourceURLForInstance(value: string, instanceKey: string): string {
	if (!isDesktopLocalAppDocument()) return value;
	const target = parseDisplayTarget(value);
	if (target != null && isDirectlyDisplayableTarget(target)) return target.toString();
	return wrapDesktopLocalResourceURLForInstance(value, instanceKey);
}

export function resolveDesktopCrossOriginMediaURL(value: string | undefined): string | undefined {
	if (value == null || value.length === 0) return value;
	return wrapDesktopLocalResourceURL(value);
}

function parseDisplayTarget(value: string): URL | null {
	try {
		return new URL(parseDesktopLocalResourceProxyTarget(value));
	} catch {
		return null;
	}
}

function isDirectlyDisplayableTarget(target: URL): boolean {
	if (target.protocol === 'https:') return true;
	return target.protocol === 'http:' && isLoopbackHostname(target.hostname);
}

function isLoopbackHostname(hostname: string): boolean {
	if (hostname === 'localhost' || hostname.endsWith('.localhost') || hostname === '[::1]') return true;
	return LOOPBACK_IPV4_PATTERN.test(hostname);
}

export function updateDesktopLocalResourceURLTarget(value: string, updateTarget: (target: URL) => boolean): string {
	const targetValue = parseDesktopLocalResourceProxyTarget(value);
	let target: URL;
	try {
		target = new URL(targetValue);
	} catch {
		return value;
	}
	if (!updateTarget(target)) return value;
	return wrapDesktopLocalResourceURL(target.toString());
}

export function wrapDesktopLocalUploadURL(value: string): string {
	if (!isDesktopLocalAppDocument()) return value;
	const targetValue = parseDesktopLocalResourceProxyTarget(value);
	let target: URL;
	try {
		target = new URL(targetValue);
	} catch {
		return value;
	}
	const relayBase = parseUploadRelayBase(RuntimeConfig.uploadRelayEndpoint);
	if (relayBase == null || target.origin !== relayBase.origin) return value;
	if (!isUploadRelayTarget(target, relayBase)) {
		throw new DesktopUploadRelayUrlError('Upload relay URL does not match the configured relay endpoint');
	}
	return wrapResolvedDesktopLocalResourceURL(target.toString(), true);
}

function wrapResolvedDesktopLocalResourceURL(value: string, validatedUploadRelayTarget: boolean): string {
	if (!isDesktopLocalAppDocument()) return value;
	let target: URL;
	try {
		target = new URL(value);
	} catch {
		return value;
	}
	if (target.protocol !== 'http:' && target.protocol !== 'https:') return target.toString();
	const runtimeKey = currentDesktopLocalRuntimeKey();
	if (runtimeKey == null) return target.toString();
	if (!validatedUploadRelayTarget && !isCurrentRuntimeRemoteResourceTarget(target)) return target.toString();
	return buildDesktopLocalResourceProxyURL(target, runtimeKey);
}

function buildDesktopLocalResourceProxyURL(target: URL, runtimeKey: string): string {
	const proxy = new URL(
		`${LOCAL_APP_REMOTE_PROXY_PATH_PREFIX}/${encodeURIComponent(runtimeKey)}`,
		window.location.href,
	);
	proxy.searchParams.set(LOCAL_APP_REMOTE_PROXY_URL_PARAMETER, target.toString());
	return proxy.toString();
}

function currentDesktopLocalRuntimeKey(): string | null {
	return runtimeInstanceKey(RuntimeConfig.getSnapshot());
}

function isCurrentRuntimeRemoteResourceTarget(target: URL): boolean {
	const targetOrigin = originSource(target.toString());
	if (targetOrigin == null) return false;
	for (const source of [RuntimeConfig.mediaEndpoint, RuntimeConfig.staticCdnEndpoint]) {
		const sourceOrigin = originSource(source);
		if (sourceOrigin != null && sourceOrigin === targetOrigin) return true;
	}
	return false;
}

function originSource(value: string | null): string | null {
	if (value == null || value.length === 0) return null;
	try {
		const parsedUrl = new URL(value);
		if (parsedUrl.protocol !== 'http:' && parsedUrl.protocol !== 'https:') return null;
		return parsedUrl.origin;
	} catch {
		return null;
	}
}

function parseUploadRelayBase(value: string | null): URL | null {
	if (value == null || value.length === 0) return null;
	let relayUrl: URL;
	try {
		relayUrl = new URL(value);
	} catch {
		return null;
	}
	if (relayUrl.protocol !== 'http:' && relayUrl.protocol !== 'https:') return null;
	if (relayUrl.username.length > 0) return null;
	if (relayUrl.password.length > 0) return null;
	if (relayUrl.search.length > 0) return null;
	if (relayUrl.hash.length > 0) return null;
	const basePath = relayUrl.pathname
		.replace(TRAILING_SLASHES_PATTERN, '')
		.replace(UPLOAD_RELAY_BASE_SUFFIX_PATTERN, '');
	if (UPLOAD_RELAY_BASE_PATH_PATTERN.test(basePath)) return null;
	relayUrl.pathname = basePath.length === 0 ? '/' : basePath;
	return relayUrl;
}

function isUploadRelayTarget(target: URL, relayBase: URL): boolean {
	if (target.protocol !== relayBase.protocol) return false;
	if (target.origin !== relayBase.origin) return false;
	if (target.username.length > 0) return false;
	if (target.password.length > 0) return false;
	if (target.hash.length > 0) return false;
	const basePath = relayBase.pathname === '/' ? '' : relayBase.pathname;
	const relayKeyPrefix = `${basePath}${UPLOAD_RELAY_PATH_PREFIX}/`;
	if (!target.pathname.startsWith(relayKeyPrefix)) return false;
	if (target.pathname.length === relayKeyPrefix.length) return false;
	return hasUploadRelayToken(target.searchParams);
}

function hasUploadRelayToken(searchParams: URLSearchParams): boolean {
	let token: string | null = null;
	for (const [name, value] of searchParams.entries()) {
		if (name === LOCAL_APP_UPLOAD_RELAY_TOKEN_PARAMETER) {
			if (token !== null) return false;
			token = value;
			continue;
		}
		if (UPLOAD_RELAY_MULTIPART_PARAMETERS.has(name)) continue;
		return false;
	}
	return token !== null && token.length > 0;
}

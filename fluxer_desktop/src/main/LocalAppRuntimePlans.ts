// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_APP_URL} from '@electron/common/Constants';
import {websocketHTTPOrigin} from '@electron/main/WebSocketOrigin';
import {
	type ClientRuntimeDiscovery,
	projectClientRuntimeDiscovery,
} from '@fluxer/instance_bootstrap/src/ClientRuntimeDiscovery';
import {parseInstanceDiscoveryDocument} from '@fluxer/instance_bootstrap/src/Discovery';

const RUNTIME_PLAN_CACHE_MAX_ENTRIES = 256;
const ROUTE_CACHE_KEY_PREFIX = 'route:';

export interface LocalAppRuntimeEndpoints {
	readonly apiEndpoint: string;
	readonly apiPublicEndpoint: string | null;
	readonly webAppEndpoint: string | null;
	readonly mediaEndpoint: string | null;
	readonly staticCdnEndpoint: string | null;
	readonly uploadRelayEndpoint: string | null;
	readonly gatewayEndpoint: string | null;
	readonly inviteEndpoint: string | null;
	readonly giftEndpoint: string | null;
}

export interface LocalAppRuntimePlan {
	readonly instanceKey: string;
	readonly document: unknown;
	readonly endpoints: LocalAppRuntimeEndpoints;
	readonly selfHosted: boolean;
	readonly desktopModulesEnabled: boolean | null;
}

interface LocalAppRuntimeRoute {
	readonly runtimeKey: string;
	readonly localPathPrefix: string;
}

interface LocalAppRuntimeRouteSegments {
	readonly encodedRuntimeKey: string;
	readonly localPathPrefix: string;
}

class InvalidLocalAppRuntimeAPIEndpointError extends Error {
	public constructor(apiEndpoint: string) {
		super(`Instance discovery advertised an unusable API endpoint: ${apiEndpoint}`);
		this.name = 'InvalidLocalAppRuntimeAPIEndpointError';
	}
}

class LocalAppRuntimePlanCacheInvariantError extends Error {
	public constructor() {
		super('Local app runtime plan cache reached its limit without an oldest entry');
		this.name = 'LocalAppRuntimePlanCacheInvariantError';
	}
}

export function localAppRuntimeInstanceKey(apiEndpoint: string): string | null {
	let url: URL;
	try {
		url = new URL(apiEndpoint.trim());
	} catch {
		return null;
	}
	if (url.protocol !== 'https:' && url.protocol !== 'http:') {
		return null;
	}
	if (url.username.length > 0 || url.password.length > 0 || url.search.length > 0 || url.hash.length > 0) {
		return null;
	}
	const path = url.pathname.replace(/\/+$/u, '');
	return `${url.origin.toLowerCase()}${path}`;
}

export function httpOriginSource(value: string | null): string | null {
	if (value == null || value.length === 0) {
		return null;
	}
	let url: URL;
	try {
		url = new URL(value);
	} catch {
		return null;
	}
	if (url.protocol !== 'https:' && url.protocol !== 'http:') {
		return null;
	}
	return url.origin;
}

export function localAppRuntimePlanFromDiscovery(document: unknown): LocalAppRuntimePlan {
	const discovery = projectClientRuntimeDiscovery(parseInstanceDiscoveryDocument(document));
	return {
		instanceKey: requireInstanceKey(discovery),
		document,
		endpoints: runtimeEndpointsFromDiscovery(discovery),
		selfHosted: discovery.features.self_hosted,
		desktopModulesEnabled: discovery.features.desktop_modules_enabled ?? null,
	};
}

export function buildLocalAppRuntimeURL(basePath: string, instanceKey: string): string {
	const url = new URL(DESKTOP_APP_URL);
	url.pathname = `/${basePath.replace(/^\/+|\/+$/gu, '')}/${encodeURIComponent(instanceKey)}`;
	return url.toString();
}

export function parseLocalAppRuntimeRoute(requestURL: string, basePath: string): LocalAppRuntimeRoute | null {
	let pathname: string;
	try {
		pathname = new URL(requestURL).pathname;
	} catch {
		return null;
	}
	const basePathWithSlash = `${basePath}/`;
	if (!pathname.startsWith(basePathWithSlash)) {
		return null;
	}
	const segments = splitLocalAppRuntimeRoute(pathname, basePathWithSlash);
	if (segments.encodedRuntimeKey.length === 0) {
		return null;
	}
	const runtimeKey = decodeLocalAppRuntimeKey(segments.encodedRuntimeKey);
	if (runtimeKey == null) {
		return null;
	}
	return {runtimeKey, localPathPrefix: segments.localPathPrefix};
}

export function runtimePlanTrustedHTTPOrigins(plan: LocalAppRuntimePlan): Array<string> {
	const origins = new Set<string>();
	const endpoints = plan.endpoints;
	const httpEndpoints: ReadonlyArray<string | null> = [
		endpoints.apiEndpoint,
		endpoints.apiPublicEndpoint,
		endpoints.mediaEndpoint,
		endpoints.uploadRelayEndpoint,
		endpoints.staticCdnEndpoint,
		endpoints.webAppEndpoint,
	];
	for (const endpoint of httpEndpoints) {
		const origin = httpOriginSource(endpoint);
		if (origin != null) {
			origins.add(origin);
		}
	}
	const gatewayOrigin = endpoints.gatewayEndpoint == null ? null : websocketHTTPOrigin(endpoints.gatewayEndpoint);
	if (gatewayOrigin != null) {
		origins.add(gatewayOrigin);
	}
	return [...origins].sort();
}

export class DesktopLocalAppRuntimePlans {
	private activePlan: LocalAppRuntimePlan | null = null;
	private readonly cachedPlans = new Map<string, LocalAppRuntimePlan>();

	public activate(plan: LocalAppRuntimePlan): void {
		this.cache(plan);
		this.activePlan = plan;
	}

	public deactivate(): void {
		this.activePlan = null;
	}

	public cache(plan: LocalAppRuntimePlan): void {
		this.setCachedPlan(routeCacheKey(plan.instanceKey), plan);
	}

	public getActivePlan(): LocalAppRuntimePlan | null {
		return this.activePlan;
	}

	public findPlanForRoute(runtimeKey: string): LocalAppRuntimePlan | null {
		const active = this.activePlan;
		if (active != null && active.instanceKey === runtimeKey) {
			return active;
		}
		const key = routeCacheKey(runtimeKey);
		const plan = this.cachedPlans.get(key);
		if (plan != null) {
			this.cachedPlans.delete(key);
			this.cachedPlans.set(key, plan);
		}
		return plan ?? null;
	}

	private setCachedPlan(key: string, plan: LocalAppRuntimePlan): void {
		this.cachedPlans.delete(key);
		if (this.cachedPlans.size >= RUNTIME_PLAN_CACHE_MAX_ENTRIES) {
			const oldestKey = this.cachedPlans.keys().next().value;
			if (oldestKey == null) {
				throw new LocalAppRuntimePlanCacheInvariantError();
			}
			this.cachedPlans.delete(oldestKey);
		}
		this.cachedPlans.set(key, plan);
	}
}

function requireInstanceKey(discovery: ClientRuntimeDiscovery): string {
	const instanceKey = localAppRuntimeInstanceKey(discovery.endpoints.apiEndpoint);
	if (instanceKey == null) {
		throw new InvalidLocalAppRuntimeAPIEndpointError(discovery.endpoints.apiEndpoint);
	}
	return instanceKey;
}

function runtimeEndpointsFromDiscovery(discovery: ClientRuntimeDiscovery): LocalAppRuntimeEndpoints {
	const endpoints = discovery.endpoints;
	return {
		apiEndpoint: endpoints.apiEndpoint,
		apiPublicEndpoint: endpoints.apiPublicEndpoint,
		webAppEndpoint: endpoints.webAppEndpoint,
		mediaEndpoint: endpoints.mediaEndpoint,
		staticCdnEndpoint: endpoints.staticCdnEndpoint,
		uploadRelayEndpoint: endpoints.uploadRelayEndpoint,
		gatewayEndpoint: endpoints.gatewayEndpoint,
		inviteEndpoint: endpoints.inviteEndpoint,
		giftEndpoint: endpoints.giftEndpoint,
	};
}

function routeCacheKey(runtimeKey: string): string {
	return `${ROUTE_CACHE_KEY_PREFIX}${runtimeKey}`;
}

function splitLocalAppRuntimeRoute(pathname: string, basePathWithSlash: string): LocalAppRuntimeRouteSegments {
	const runtimeKeyStart = basePathWithSlash.length;
	const runtimeKeyEnd = pathname.indexOf('/', runtimeKeyStart);
	if (runtimeKeyEnd === -1) {
		const encodedRuntimeKey = pathname.slice(runtimeKeyStart);
		return {encodedRuntimeKey, localPathPrefix: `${basePathWithSlash}${encodedRuntimeKey}`};
	}
	return {
		encodedRuntimeKey: pathname.slice(runtimeKeyStart, runtimeKeyEnd),
		localPathPrefix: pathname.slice(0, runtimeKeyEnd),
	};
}

function decodeLocalAppRuntimeKey(encodedRuntimeKey: string): string | null {
	let runtimeKey: string;
	try {
		runtimeKey = decodeURIComponent(encodedRuntimeKey);
	} catch {
		return null;
	}
	return runtimeKey.trim().length === 0 ? null : runtimeKey;
}

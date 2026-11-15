// SPDX-License-Identifier: AGPL-3.0-or-later

import {sendProviderRequest} from '@app/api/auth/services/SsoUtils';
import {Logger} from '@app/api/Logger';
import {EXTERNAL_RESPONSE_LIMITS} from '@app/api/utils/ExternalResponseLimits';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {isJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import {SsoProviderUnavailableError} from '@fluxer/errors/src/domains/auth/SsoProviderUnavailableError';
import {formatUrlForDiagnostics} from '@pkgs/http_client/src/HttpClientDiagnostics';
import {ms} from 'itty-time';
import {
	type CryptoKey,
	createLocalJWKSet,
	type FlattenedJWSInput,
	errors as JoseErrors,
	type JSONWebKeySet,
	type JWSHeaderParameters,
} from 'jose';

interface RemoteJwkSetResolver {
	(protectedHeader?: JWSHeaderParameters, token?: FlattenedJWSInput): Promise<CryptoKey>;
	coolingDown: boolean;
	fresh: boolean;
	reloading: boolean;
	reload: () => Promise<void>;
	jwks: () => JSONWebKeySet | undefined;
}

interface JwksCacheEntry {
	jwks: RemoteJwkSetResolver;
	cachedAt: number;
}

const JWKS_CACHE_TTL_MS = ms('1 hour');
const JWKS_CACHE_SWEEP_SIZE = 10;

const jwksCache = new Map<string, JwksCacheEntry>();
const jwksCreations = new Map<string, Promise<RemoteJwkSetResolver>>();

export function resetOidcJwksCachesForTests(): void {
	jwksCache.clear();
	jwksCreations.clear();
}

function isJsonWebKeySet(value: unknown): value is JSONWebKeySet {
	if (!isJsonRecord(value)) return false;
	const keys = value['keys'];
	return Array.isArray(keys) && keys.every(isJsonRecord);
}

export async function getOidcJwks(jwksUrl: string): Promise<RemoteJwkSetResolver> {
	const cached = jwksCache.get(jwksUrl);
	if (cached && Date.now() - cached.cachedAt < JWKS_CACHE_TTL_MS) {
		return cached.jwks;
	}
	let creation = jwksCreations.get(jwksUrl);
	if (!creation) {
		creation = createJwks(jwksUrl).finally(() => jwksCreations.delete(jwksUrl));
		jwksCreations.set(jwksUrl, creation);
	}
	return creation;
}

async function fetchJwks(jwksUrl: string): Promise<JSONWebKeySet> {
	const response = await sendProviderRequest({
		url: jwksUrl,
		method: 'GET',
		headers: {Accept: 'application/json'},
		timeout: ms('15 seconds'),
		serviceName: 'sso_jwks',
	});
	if (response.status < 200 || response.status >= 300) {
		FetchUtils.discardResponseBody(response.stream, response.status);
		if (response.status >= 500) {
			Logger.warn({status: response.status, url: formatUrlForDiagnostics(jwksUrl)}, 'SSO keys request failed');
			throw new SsoProviderUnavailableError();
		}
		throw new Error(`Failed to fetch JWKS: HTTP ${response.status}`);
	}
	const rawBody = await FetchUtils.streamToStringWithLimit(response.stream, {
		maxBytes: EXTERNAL_RESPONSE_LIMITS.ssoJwksBytes,
		headers: response.headers,
		url: jwksUrl,
		description: 'SSO JWKS response',
	});
	const parsed = parseJsonWithGuard(rawBody, isJsonWebKeySet);
	if (!parsed) {
		throw new Error('SSO JWKS response was not a JSON Web Key Set');
	}
	return parsed;
}

async function createJwks(jwksUrl: string): Promise<RemoteJwkSetResolver> {
	const now = Date.now();
	let currentJwks = await fetchJwks(jwksUrl);
	let currentResolver = createLocalJWKSet(currentJwks);
	let fetchedAt = now;
	let pendingReload: Promise<void> | null = null;
	let jwks!: RemoteJwkSetResolver;
	const reload = async (): Promise<void> => {
		pendingReload ??= (async () => {
			currentJwks = await fetchJwks(jwksUrl);
			currentResolver = createLocalJWKSet(currentJwks);
			fetchedAt = Date.now();
			jwksCache.set(jwksUrl, {jwks, cachedAt: fetchedAt});
		})().finally(() => {
			pendingReload = null;
		});
		await pendingReload;
	};
	jwks = (async (protectedHeader?: JWSHeaderParameters, token?: FlattenedJWSInput) => {
		if (Date.now() - fetchedAt >= JWKS_CACHE_TTL_MS) {
			await reload();
		}
		try {
			return await currentResolver(protectedHeader, token);
		} catch (error) {
			if (error instanceof JoseErrors.JWKSNoMatchingKey) {
				await reload();
				return currentResolver(protectedHeader, token);
			}
			throw error;
		}
	}) as RemoteJwkSetResolver;
	Object.defineProperties(jwks, {
		coolingDown: {
			get: () => false,
			enumerable: true,
			configurable: false,
		},
		fresh: {
			get: () => Date.now() - fetchedAt < JWKS_CACHE_TTL_MS,
			enumerable: true,
			configurable: false,
		},
		reloading: {
			get: () => pendingReload != null,
			enumerable: true,
			configurable: false,
		},
		reload: {
			value: reload,
			enumerable: true,
			configurable: false,
			writable: false,
		},
		jwks: {
			value: () => currentJwks,
			enumerable: true,
			configurable: false,
			writable: false,
		},
	});
	jwksCache.set(jwksUrl, {jwks, cachedAt: now});
	if (jwksCache.size > JWKS_CACHE_SWEEP_SIZE) {
		for (const [url, entry] of jwksCache.entries()) {
			if (now - entry.cachedAt >= JWKS_CACHE_TTL_MS) {
				jwksCache.delete(url);
			}
		}
	}
	return jwks;
}

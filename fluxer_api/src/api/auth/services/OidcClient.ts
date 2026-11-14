// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {getOidcJwks, resetOidcJwksCachesForTests} from '@app/api/auth/services/OidcJwks';
import {
	acceptedIssuers,
	type DiscoveredOidcProviderMetadata,
	parseTokenEndpointResponse,
	sendProviderRequest,
	tryDiscoverOidcProviderMetadata,
} from '@app/api/auth/services/SsoUtils';
import {isHttpsSsoUrl} from '@app/api/instance/SsoConfigValidation';
import {Logger} from '@app/api/Logger';
import {EXTERNAL_RESPONSE_LIMITS} from '@app/api/utils/ExternalResponseLimits';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {SsoProviderUnavailableError} from '@fluxer/errors/src/domains/auth/SsoProviderUnavailableError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {formatUrlForDiagnostics} from '@pkgs/http_client/src/HttpClientDiagnostics';
import type {StreamResponse} from '@pkgs/http_client/src/HttpClientTypes';
import {ms, seconds} from 'itty-time';
import {type JWTPayload, jwtVerify} from 'jose';

type ProviderExchangeStage =
	| 'token_unavailable'
	| 'token_client_refused'
	| 'token_rejected'
	| 'id_token_invalid'
	| 'userinfo_failed'
	| 'userinfo_signed'
	| 'userinfo_unparseable';

export class ProviderExchangeError extends Error {
	constructor(readonly stage: ProviderExchangeStage) {
		super(`OIDC provider exchange failed at ${stage}`);
		this.name = 'ProviderExchangeError';
	}
}

export interface ProviderTokens {
	id_token?: string;
	access_token?: string;
}

export interface TokenClient {
	tokenUrl: string;
	clientId: string;
	clientSecret: string | null;
	authMethods: Array<string> | null;
}

interface DiscoveryCacheEntry {
	metadata: DiscoveredOidcProviderMetadata | null;
	fetchedAt: number;
	failed: boolean;
}

type ClientAuthMethod = 'client_secret_basic' | 'client_secret_post';

interface TokenEndpointError {
	error: string | null;
	description: string | null;
}

type TokenAttempt = {ok: true; tokens: ProviderTokens} | {ok: false; status: number; error: TokenEndpointError};

const DISCOVERY_CACHE_TTL_SECONDS = seconds('30 days');
const DISCOVERY_FRESH_MS = ms('1 hour');
const DISCOVERY_FAILURE_FRESH_MS = ms('1 minute');
const TOKEN_ERROR_BODY_MAX_BYTES = 4 * 1024;
const PROVIDER_ERROR_TEXT_MAX_LENGTH = 300;
const MISCONFIGURED_CLIENT_ERRORS = new Set(['invalid_client', 'unauthorized_client']);
const ID_TOKEN_ALGORITHMS = [
	'RS256',
	'RS384',
	'RS512',
	'PS256',
	'PS384',
	'PS512',
	'ES256',
	'ES384',
	'ES512',
	'EdDSA',
	'Ed25519',
];

const discoveryRefreshes = new Map<string, Promise<DiscoveredOidcProviderMetadata | null>>();
const workingClientAuthMethods = new Map<string, ClientAuthMethod>();

export function resetOidcClientCachesForTests(): void {
	resetOidcJwksCachesForTests();
	workingClientAuthMethods.clear();
}

function issuerCacheId(issuer: string): string {
	return createHash('sha256').update(issuer).digest('hex').slice(0, 32);
}

export function ssoDiscoveryCacheKey(issuer: string): string {
	return `sso:oidc-discovery:v2:${issuerCacheId(issuer)}`;
}

export function legacySsoDiscoveryCacheKey(issuer: string): string {
	return `sso:oidc-discovery:${issuerCacheId(issuer)}`;
}

function isDiscoveryFresh(entry: DiscoveryCacheEntry): boolean {
	return Date.now() - entry.fetchedAt < (entry.failed ? DISCOVERY_FAILURE_FRESH_MS : DISCOVERY_FRESH_MS);
}

export function discoveredHttpsUrl(url: string | undefined): string | null {
	return url && isHttpsSsoUrl(url) ? url : null;
}

export async function resolveOidcDiscovery(
	cache: ICacheService,
	issuer: string,
): Promise<DiscoveredOidcProviderMetadata | null> {
	const cacheKey = ssoDiscoveryCacheKey(issuer);
	const cached = await cache.get<DiscoveryCacheEntry>(cacheKey);
	if (cached && isDiscoveryFresh(cached)) {
		return cached.metadata;
	}
	let refresh = discoveryRefreshes.get(cacheKey);
	if (!refresh) {
		refresh = refreshDiscovery(cache, issuer, cacheKey, cached?.metadata ?? null).finally(() =>
			discoveryRefreshes.delete(cacheKey),
		);
		discoveryRefreshes.set(cacheKey, refresh);
	}
	return refresh;
}

async function refreshDiscovery(
	cache: ICacheService,
	issuer: string,
	cacheKey: string,
	lastGood: DiscoveredOidcProviderMetadata | null,
): Promise<DiscoveredOidcProviderMetadata | null> {
	const metadata = await tryDiscoverOidcProviderMetadata(issuer);
	if (!metadata && lastGood) {
		Logger.warn(
			{issuer: formatUrlForDiagnostics(issuer)},
			'OIDC provider discovery failed and the last discovered endpoints stay in use',
		);
	}
	const entry: DiscoveryCacheEntry = {metadata: metadata ?? lastGood, fetchedAt: Date.now(), failed: !metadata};
	await cache.set(cacheKey, entry, DISCOVERY_CACHE_TTL_SECONDS);
	return entry.metadata;
}

function clientAuthMethodsToTry(tokenUrl: string, supported: Array<string> | null): Array<ClientAuthMethod> {
	const basicFirst =
		!supported?.length || (supported.includes('client_secret_basic') && !supported.includes('client_secret_post'));
	const first = workingClientAuthMethods.get(tokenUrl) ?? (basicFirst ? 'client_secret_basic' : 'client_secret_post');
	return [first, first === 'client_secret_post' ? 'client_secret_basic' : 'client_secret_post'];
}

function formEncode(value: string): string {
	return new URLSearchParams({v: value}).toString().slice(2);
}

function basicAuthorization(clientId: string, clientSecret: string): string {
	return `Basic ${Buffer.from(`${formEncode(clientId)}:${formEncode(clientSecret)}`).toString('base64')}`;
}

function providerErrorText(value: unknown): string | null {
	return typeof value === 'string' ? value.slice(0, PROVIDER_ERROR_TEXT_MAX_LENGTH) : null;
}

function wwwAuthenticateError(header: string | null): string | null {
	return providerErrorText(header?.match(/error="([^"]*)"/)?.[1]);
}

function isRefusedClient(attempt: TokenAttempt): boolean {
	return !attempt.ok && (attempt.status === 400 || attempt.status === 401) && attempt.error.error !== 'invalid_grant';
}

async function readTokenError(resp: StreamResponse, url: string): Promise<TokenEndpointError> {
	try {
		const rawBody = await FetchUtils.streamToStringWithLimit(resp.stream, {
			maxBytes: TOKEN_ERROR_BODY_MAX_BYTES,
			headers: resp.headers,
			url,
			description: 'OIDC token error response',
		});
		const parsed = parseJsonRecord(rawBody);
		return {error: providerErrorText(parsed?.['error']), description: providerErrorText(parsed?.['error_description'])};
	} catch {
		return {error: null, description: null};
	}
}

async function requestTokens(
	client: TokenClient,
	params: Record<string, string>,
	method: ClientAuthMethod | null,
): Promise<TokenAttempt> {
	const body = new URLSearchParams(params);
	const headers: Record<string, string> = {
		Accept: 'application/json',
		'Content-Type': 'application/x-www-form-urlencoded',
	};
	if (method === 'client_secret_basic') {
		headers['Authorization'] = basicAuthorization(client.clientId, client.clientSecret ?? '');
	} else if (method === 'client_secret_post') {
		body.set('client_secret', client.clientSecret ?? '');
	}
	const resp = await sendProviderRequest({
		url: client.tokenUrl,
		method: 'POST',
		headers,
		body,
		timeout: ms('15 seconds'),
		serviceName: 'sso_token_exchange',
	});
	if (resp.status < 200 || resp.status >= 300) {
		return {ok: false, status: resp.status, error: await readTokenError(resp, client.tokenUrl)};
	}
	const rawBody = await FetchUtils.streamToStringWithLimit(resp.stream, {
		maxBytes: EXTERNAL_RESPONSE_LIMITS.ssoTokenBytes,
		headers: resp.headers,
		url: client.tokenUrl,
		description: 'OIDC token response',
	});
	return {ok: true, tokens: parseTokenEndpointResponse(resp.headers.get('content-type'), rawBody)};
}

export async function exchangeAuthorizationCode(
	client: TokenClient,
	{code, codeVerifier, redirectUri}: {code: string; codeVerifier: string; redirectUri: string},
): Promise<ProviderTokens> {
	const params = {
		grant_type: 'authorization_code',
		code,
		redirect_uri: redirectUri,
		client_id: client.clientId,
		code_verifier: codeVerifier,
	};
	const methods: Array<ClientAuthMethod | null> = client.clientSecret
		? clientAuthMethodsToTry(client.tokenUrl, client.authMethods)
		: [null];
	let method = methods[0] ?? null;
	let attempt = await requestTokens(client, params, method);
	if (methods.length > 1 && isRefusedClient(attempt)) {
		method = methods[1] ?? null;
		attempt = await requestTokens(client, params, method);
	}
	if (!attempt.ok) {
		Logger.warn(
			{
				status: attempt.status,
				token_url: formatUrlForDiagnostics(client.tokenUrl),
				error: attempt.error.error,
				error_description: attempt.error.description,
			},
			'OIDC token request failed',
		);
		if (attempt.status >= 500) {
			throw new ProviderExchangeError('token_unavailable');
		}
		if (attempt.status === 401 || (attempt.error.error && MISCONFIGURED_CLIENT_ERRORS.has(attempt.error.error))) {
			throw new ProviderExchangeError('token_client_refused');
		}
		throw new ProviderExchangeError('token_rejected');
	}
	if (method && methods.length > 1) {
		workingClientAuthMethods.set(client.tokenUrl, method);
	}
	return attempt.tokens;
}

export async function verifyIdToken({
	idToken,
	jwksUrl,
	issuer,
	clientId,
	nonce,
}: {
	idToken: string;
	jwksUrl: string;
	issuer: string;
	clientId: string;
	nonce: string;
}): Promise<JWTPayload> {
	try {
		const jwks = await getOidcJwks(jwksUrl);
		const {payload} = await jwtVerify(idToken, jwks, {
			issuer: acceptedIssuers(issuer),
			audience: clientId,
			algorithms: ID_TOKEN_ALGORITHMS,
			requiredClaims: ['exp', 'iat'],
			clockTolerance: 10,
		});
		const audiences = Array.isArray(payload.aud) ? payload.aud : [payload.aud];
		const azp = payload['azp'];
		if ((audiences.length > 1 || azp !== undefined) && azp !== clientId) {
			throw new Error('azp does not match the client');
		}
		const claimedNonce = payload['nonce'];
		if (typeof claimedNonce !== 'string' || claimedNonce.length === 0 || claimedNonce !== nonce) {
			throw new Error('nonce missing or mismatched');
		}
		return payload;
	} catch (error) {
		if (error instanceof SsoProviderUnavailableError) throw error;
		Logger.warn({error}, 'Failed to verify an OIDC id_token');
		throw new ProviderExchangeError('id_token_invalid');
	}
}

export async function fetchUserInfo(
	userInfoUrl: string,
	accessToken: string,
	{
		headers = {},
		maxBytes = EXTERNAL_RESPONSE_LIMITS.ssoUserInfoBytes,
	}: {headers?: Record<string, string>; maxBytes?: number} = {},
): Promise<Record<string, unknown>> {
	const resp = await sendProviderRequest({
		url: userInfoUrl,
		method: 'GET',
		headers: {
			...headers,
			Authorization: `Bearer ${accessToken}`,
			Accept: 'application/json',
		},
		timeout: ms('15 seconds'),
		serviceName: 'sso_user_info',
	});
	if (resp.status < 200 || resp.status >= 300) {
		FetchUtils.discardResponseBody(resp.stream, resp.status);
		Logger.warn(
			{
				status: resp.status,
				userinfo_url: formatUrlForDiagnostics(userInfoUrl),
				error: wwwAuthenticateError(resp.headers.get('www-authenticate')),
			},
			'OIDC userinfo request failed',
		);
		throw new ProviderExchangeError('userinfo_failed');
	}
	if (resp.headers.get('content-type')?.toLowerCase().includes('application/jwt')) {
		FetchUtils.discardResponseBody(resp.stream, resp.status);
		Logger.warn(
			{userinfo_url: formatUrlForDiagnostics(userInfoUrl)},
			'The provider sent a signed userinfo response, which Fluxer does not support. Turn off userinfo signing for this client at the provider.',
		);
		throw new ProviderExchangeError('userinfo_signed');
	}
	try {
		const rawBody = await FetchUtils.streamToStringWithLimit(resp.stream, {
			maxBytes,
			headers: resp.headers,
			url: userInfoUrl,
			description: 'OIDC user info response',
		});
		const parsed = parseJsonRecord(rawBody);
		if (!parsed) {
			throw new Error('User info response was not a JSON object');
		}
		return parsed;
	} catch {
		throw new ProviderExchangeError('userinfo_unparseable');
	}
}

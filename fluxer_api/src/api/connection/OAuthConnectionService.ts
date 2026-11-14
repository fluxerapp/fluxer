// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomBytes} from 'node:crypto';
import type {ApiContext} from '@app/api/ApiContext';
import {
	discoveredHttpsUrl,
	exchangeAuthorizationCode,
	fetchUserInfo,
	type ProviderTokens,
	resolveOidcDiscovery,
	verifyIdToken,
} from '@app/api/auth/services/OidcClient';
import {acceptedIssuers} from '@app/api/auth/services/SsoUtils';
import type {UserID} from '@app/api/BrandedTypes';
import {
	buildOAuthConnectionIdentifier,
	type ConnectionProviderEntry,
	connectionCallbackUrl,
	normalizeConnectionAccountName,
	normalizeConnectionSubject,
	readJsonPath,
	type StoredConnectionProvider,
} from '@app/api/connection/ConnectionProviders';
import type {ConnectionService} from '@app/api/connection/ConnectionService';
import {ConnectionAlreadyExistsError} from '@app/api/connection/errors/ConnectionAlreadyExistsError';
import {ConnectionLimitReachedError} from '@app/api/connection/errors/ConnectionLimitReachedError';
import {UnknownConnectionProviderError} from '@app/api/connection/errors/UnknownConnectionProviderError';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {Logger} from '@app/api/Logger';
import {isSignInRefused} from '@app/api/user/UserHelpers';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {seconds} from 'itty-time';

export type OAuthConnectionCallbackReason =
	| 'not_enabled'
	| 'state_invalid'
	| 'denied'
	| 'callback_failed'
	| 'already_connected'
	| 'limit_reached'
	| 'unknown';

export class OAuthConnectionCallbackError extends Error {
	constructor(readonly reason: OAuthConnectionCallbackReason) {
		super(`OAuth connection callback failed: ${reason}`);
		this.name = 'OAuthConnectionCallbackError';
	}
}

interface OAuthConnectionState {
	userId: string;
	sessionIdHash: string;
	providerId: string;
	fingerprint: string;
	codeVerifier: string;
	nonce: string | null;
}

interface ProviderEndpoints {
	authorizationUrl: string;
	tokenUrl: string;
	userInfoUrl: string | null;
	jwksUrl: string | null;
	issuer: string | null;
	authMethods: Array<string> | null;
}

const STATE_TTL_SECONDS = seconds('10 minutes');
const STATE_BYTE_LENGTH = 32;
const CODE_VERIFIER_BYTE_LENGTH = 32;
const NONCE_BYTE_LENGTH = 16;
const OAUTH2_USER_MAX_BYTES = 64 * 1024;
const DEFAULT_NAME_CLAIM = 'preferred_username';
const RESERVED_AUTHORIZATION_PARAMS = [
	'response_type',
	'client_id',
	'redirect_uri',
	'scope',
	'state',
	'code_challenge',
	'code_challenge_method',
	'nonce',
];

function stateCacheKey(state: string): string {
	return `connection-oauth-state:${state}`;
}

function randomToken(byteLength: number): string {
	return randomBytes(byteLength).toString('base64url');
}

function fail(reason: OAuthConnectionCallbackReason): never {
	throw new OAuthConnectionCallbackError(reason);
}

function readName(sources: ReadonlyArray<unknown>, paths: ReadonlyArray<string>): string | undefined {
	for (const path of paths) {
		for (const source of sources) {
			const value = readJsonPath(source, path);
			if (typeof value === 'string' && value.trim().length > 0) return value;
		}
	}
	return undefined;
}

export class OAuthConnectionService {
	constructor(
		private readonly apiContext: ApiContext,
		private readonly instanceConfigRepository: InstanceConfigRepository,
		private readonly connectionService: ConnectionService,
	) {}

	private async findProvider(providerId: string): Promise<ConnectionProviderEntry | null> {
		const entry = (await this.instanceConfigRepository.getConnectionProviderIndex()).get(providerId);
		return entry?.provider.enabled ? entry : null;
	}

	private async resolveEndpoints(provider: StoredConnectionProvider): Promise<ProviderEndpoints | null> {
		if (provider.kind === 'oauth2') {
			if (!provider.authorization_url || !provider.token_url || !provider.userinfo_url) return null;
			return {
				authorizationUrl: provider.authorization_url,
				tokenUrl: provider.token_url,
				userInfoUrl: provider.userinfo_url,
				jwksUrl: null,
				issuer: null,
				authMethods: null,
			};
		}
		if (!provider.issuer) return null;
		const discovered = await resolveOidcDiscovery(this.apiContext.services.cache, provider.issuer);
		const authorizationUrl = provider.authorization_url ?? discoveredHttpsUrl(discovered?.authorization_endpoint);
		const tokenUrl = provider.token_url ?? discoveredHttpsUrl(discovered?.token_endpoint);
		const jwksUrl = discoveredHttpsUrl(discovered?.jwks_uri);
		if (!discovered || !authorizationUrl || !tokenUrl || !jwksUrl) return null;
		return {
			authorizationUrl,
			tokenUrl,
			userInfoUrl: provider.userinfo_url ?? discoveredHttpsUrl(discovered.userinfo_endpoint),
			jwksUrl,
			issuer: discovered.issuer,
			authMethods: discovered.token_endpoint_auth_methods_supported ?? null,
		};
	}

	async authorize(userId: UserID, sessionIdHash: Buffer, providerId: string): Promise<string> {
		const entry = await this.findProvider(providerId);
		if (!entry) throw new UnknownConnectionProviderError();
		await this.connectionService.requireAvailableConnectionSlot(userId);
		const {provider} = entry;
		const endpoints = await this.resolveEndpoints(provider);
		if (!endpoints) throw new ServiceUnavailableError();
		const state = randomToken(STATE_BYTE_LENGTH);
		const codeVerifier = randomToken(CODE_VERIFIER_BYTE_LENGTH);
		const nonce = provider.kind === 'oidc' ? randomToken(NONCE_BYTE_LENGTH) : null;
		const payload: OAuthConnectionState = {
			userId: userId.toString(),
			sessionIdHash: sessionIdHash.toString('base64url'),
			providerId: provider.id,
			fingerprint: entry.fingerprint,
			codeVerifier,
			nonce,
		};
		await this.apiContext.services.cache.set(stateCacheKey(state), payload, STATE_TTL_SECONDS);
		const url = new URL(endpoints.authorizationUrl);
		for (const key of RESERVED_AUTHORIZATION_PARAMS) url.searchParams.delete(key);
		url.searchParams.set('response_type', 'code');
		url.searchParams.set('client_id', provider.client_id);
		url.searchParams.set('redirect_uri', connectionCallbackUrl(provider.id));
		url.searchParams.set('scope', provider.scope);
		url.searchParams.set('state', state);
		url.searchParams.set('code_challenge', createHash('sha256').update(codeVerifier).digest('base64url'));
		url.searchParams.set('code_challenge_method', 'S256');
		if (nonce) url.searchParams.set('nonce', nonce);
		return url.toString();
	}

	async callback(providerId: string, params: URLSearchParams): Promise<void> {
		const entry = await this.findProvider(providerId);
		if (!entry) fail('not_enabled');
		const stateParam = params.get('state');
		const state = stateParam
			? await this.apiContext.services.cache.getAndDelete<OAuthConnectionState>(stateCacheKey(stateParam))
			: null;
		if (params.has('error')) fail('denied');
		if (!state || state.providerId !== providerId || state.fingerprint !== entry.fingerprint) fail('state_invalid');
		const userId = await this.requireLinkingUser(state);
		const endpoints = await this.resolveEndpoints(entry.provider);
		if (!endpoints) fail('callback_failed');
		const issuerParam = params.get('iss');
		if (issuerParam !== null && endpoints.issuer !== null && !acceptedIssuers(endpoints.issuer).includes(issuerParam)) {
			fail('state_invalid');
		}
		const code = params.get('code');
		if (!code) fail('callback_failed');
		const {subject, name} = await this.resolveAccount(entry.provider, endpoints, code, state).catch(
			(error: unknown) => {
				if (error instanceof OAuthConnectionCallbackError) throw error;
				Logger.warn({error, provider_id: providerId}, 'Connection provider exchange failed');
				return fail('callback_failed');
			},
		);
		try {
			await this.connectionService.createOrUpdateOAuthConnection(
				userId,
				buildOAuthConnectionIdentifier(entry, subject),
				name,
			);
		} catch (error) {
			if (error instanceof ConnectionAlreadyExistsError) fail('already_connected');
			if (error instanceof ConnectionLimitReachedError) fail('limit_reached');
			throw error;
		}
	}

	private async requireLinkingUser(state: OAuthConnectionState): Promise<UserID> {
		const {users} = this.apiContext.services;
		const session = await users.getAuthSessionByToken(Buffer.from(state.sessionIdHash, 'base64url'));
		const user = session?.userId.toString() === state.userId ? await users.findUnique(session.userId) : null;
		if (!user || isSignInRefused(user)) fail('state_invalid');
		return user.id;
	}

	private async resolveAccount(
		provider: StoredConnectionProvider,
		endpoints: ProviderEndpoints,
		code: string,
		state: OAuthConnectionState,
	): Promise<{subject: string; name: string}> {
		const tokens = await exchangeAuthorizationCode(
			{
				tokenUrl: endpoints.tokenUrl,
				clientId: provider.client_id,
				clientSecret: provider.client_secret,
				authMethods: endpoints.authMethods,
			},
			{code, codeVerifier: state.codeVerifier, redirectUri: connectionCallbackUrl(provider.id)},
		);
		return provider.kind === 'oidc'
			? this.resolveOidcAccount(provider, endpoints, tokens, state)
			: this.resolveOAuth2Account(provider, endpoints, tokens);
	}

	private async resolveOidcAccount(
		provider: StoredConnectionProvider,
		endpoints: ProviderEndpoints,
		tokens: ProviderTokens,
		state: OAuthConnectionState,
	): Promise<{subject: string; name: string}> {
		if (!tokens.id_token || !endpoints.jwksUrl || !endpoints.issuer || !state.nonce) fail('callback_failed');
		const claims = await verifyIdToken({
			idToken: tokens.id_token,
			jwksUrl: endpoints.jwksUrl,
			issuer: endpoints.issuer,
			clientId: provider.client_id,
			nonce: state.nonce,
		});
		const subject = normalizeConnectionSubject(claims.sub);
		if (!subject) fail('callback_failed');
		const namePaths = [...new Set([provider.name_claim ?? DEFAULT_NAME_CLAIM, 'name'])];
		let name = readName([claims], namePaths);
		if (name === undefined && endpoints.userInfoUrl && tokens.access_token) {
			const userInfo = await fetchUserInfo(endpoints.userInfoUrl, tokens.access_token);
			if (userInfo['sub'] !== claims.sub) fail('callback_failed');
			name = readName([userInfo], namePaths);
		}
		return {subject, name: normalizeConnectionAccountName(name, subject)};
	}

	private async resolveOAuth2Account(
		provider: StoredConnectionProvider,
		endpoints: ProviderEndpoints,
		tokens: ProviderTokens,
	): Promise<{subject: string; name: string}> {
		if (!tokens.access_token || !endpoints.userInfoUrl || !provider.id_path) fail('callback_failed');
		const user = await fetchUserInfo(endpoints.userInfoUrl, tokens.access_token, {
			headers: {'Client-Id': provider.client_id},
			maxBytes: OAUTH2_USER_MAX_BYTES,
		});
		const subject = normalizeConnectionSubject(readJsonPath(user, provider.id_path));
		if (!subject) fail('callback_failed');
		const name = provider.name_path ? readJsonPath(user, provider.name_path) : undefined;
		return {subject, name: normalizeConnectionAccountName(name, subject)};
	}
}

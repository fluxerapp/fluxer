// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {readFile} from 'node:fs/promises';
import {isAbsolute} from 'node:path';
import type {UserID} from '@app/api/BrandedTypes';
import {BlueskyOAuthStateInvalidError, BlueskyOAuthStoreError} from '@app/api/bluesky/BlueskyOAuthErrors';
import {createSessionStore, createStateStore, validateBlueskyGrantOwner} from '@app/api/bluesky/BlueskyOAuthStores';
import type {
	BlueskyAuthorizeResult,
	BlueskyCallbackResult,
	BlueskyOAuthGrantOwner,
	IBlueskyOAuthService,
} from '@app/api/bluesky/IBlueskyOAuthService';
import type {BlueskyOAuthConfig, BlueskyOAuthKeyConfig} from '@app/api/config/APIConfig';
import {ConnectionCredentialRepository} from '@app/api/connection/ConnectionCredentialRepository';
import type {ExternalFetchResponse, IUnfurlerService} from '@app/api/infrastructure/IUnfurlerService';
import {JoseKey} from '@atproto/jwk-jose';
import {
	AtprotoHandleResolver,
	type Fetch,
	FetchError,
	type HandleResolver,
	NodeOAuthClient,
	type NodeSavedSessionStore,
	type OAuthClientMetadataInput,
	OAuthResponseError,
	requestLocalLock,
} from '@atproto/oauth-client-node';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

interface BlueskyClientConfiguration {
	clientMetadata: OAuthClientMetadataInput;
	keyset: Array<JoseKey>;
	fetch: Fetch;
	handleResolver: HandleResolver;
}

const NULL_BODY_STATUSES = new Set([204, 205, 304]);

const HANDLE_LOOKUP_TIMEOUT_MS = 3000;
const HANDLE_INVALID = 'handle.invalid';

function createUnfurlerFetch(unfurler: IUnfurlerService): Fetch {
	return async (input, init) => {
		const request = new Request(input, init);
		request.signal.throwIfAborted();
		const body = request.method === 'GET' || request.method === 'HEAD' ? null : await request.text();
		const response = await new Promise<ExternalFetchResponse>((resolve, reject) => {
			const onAbort = () => reject(request.signal.reason);
			request.signal.addEventListener('abort', onAbort, {once: true});
			unfurler
				.fetchExternal({method: request.method, url: request.url, headers: [...request.headers], body})
				.then(resolve, (error: unknown) => reject(new TypeError('fetch failed', {cause: error})))
				.finally(() => request.signal.removeEventListener('abort', onAbort));
		});
		return new Response(NULL_BODY_STATUSES.has(response.status) ? null : response.body, {
			status: response.status,
			headers: response.headers,
		});
	};
}

function createHandleResolver(unfurler: IUnfurlerService, fetch: Fetch): HandleResolver {
	return new AtprotoHandleResolver({
		fetch: (input, init) => {
			const timeout = AbortSignal.timeout(HANDLE_LOOKUP_TIMEOUT_MS);
			return fetch(input, {...init, signal: init?.signal ? AbortSignal.any([init.signal, timeout]) : timeout});
		},
		resolveTxt: (name) => unfurler.resolveTxt(name),
	});
}

interface BlueskyClientSession {
	client: NodeOAuthClient;
	sessionStore: NodeSavedSessionStore;
}

export async function loadSigningKey(config: BlueskyOAuthKeyConfig, index: number): Promise<JoseKey> {
	const field = `Bluesky OAuth keys[${index}]`;
	let keyData = config.private_key;
	if (keyData == null) {
		const keyPath = config.private_key_path;
		if (!keyPath) {
			throw new Error(`${field} requires private_key or private_key_path`);
		}
		if (typeof keyPath !== 'string' || !isAbsolute(keyPath)) {
			throw new Error(`${field}.private_key_path must be absolute`);
		}
		try {
			keyData = await readFile(keyPath, 'utf-8');
		} catch {
			throw new Error(`${field}.private_key_path could not be read`);
		}
	}
	let key: JoseKey;
	try {
		key = await JoseKey.fromImportable(keyData, config.kid);
	} catch {
		throw new Error(`${field} must contain valid PEM or JWK key material`);
	}
	try {
		if (!key.matches({usage: 'sign', alg: 'ES256'})) return key;
		const publicJwk = key.publicJwk;
		if (!publicJwk) {
			throw new Error(`${field} has no advertised public key`);
		}
		const publicKey = await JoseKey.fromJWK(publicJwk);
		const jwt = await key.createJwt({alg: 'ES256'}, {});
		await publicKey.verifyJwt(jwt);
	} catch {
		throw new Error(`${field} must support ES256 signing with its advertised public key`);
	}
	return key;
}

export class BlueskyOAuthService implements IBlueskyOAuthService {
	readonly clientMetadata: Record<string, unknown>;
	readonly jwks: Record<string, unknown>;
	private readonly credentials = new ConnectionCredentialRepository();

	private constructor(
		private readonly configuration: BlueskyClientConfiguration,
		private readonly kvClient: IKVProvider,
	) {
		const {client} = this.createClient(() => {
			throw new Error('Bluesky public client documents have no grant owner');
		});
		this.clientMetadata = client.clientMetadata as Record<string, unknown>;
		this.jwks = client.jwks as Record<string, unknown>;
	}

	static async create(
		config: BlueskyOAuthConfig,
		kvClient: IKVProvider,
		unfurler: IUnfurlerService,
		apiPublicEndpoint: string,
	): Promise<BlueskyOAuthService> {
		const baseUrl = apiPublicEndpoint.replace(/\/$/, '');
		const keyset: Array<JoseKey> = [];
		for (const [index, key] of config.keys.entries()) {
			keyset.push(await loadSigningKey(key, index));
		}
		const fetch = createUnfurlerFetch(unfurler);
		const handleResolver = createHandleResolver(unfurler, fetch);
		return new BlueskyOAuthService(
			{
				clientMetadata: {
					client_id: `${baseUrl}/connections/bluesky/client-metadata.json`,
					client_name: config.client_name,
					client_uri: baseUrl,
					logo_uri: config.logo_uri || undefined,
					tos_uri: config.tos_uri || undefined,
					policy_uri: config.policy_uri || undefined,
					redirect_uris: [`${baseUrl}/connections/bluesky/callback`],
					grant_types: ['authorization_code', 'refresh_token'],
					scope: 'atproto',
					response_types: ['code'],
					application_type: 'web',
					token_endpoint_auth_method: 'private_key_jwt',
					token_endpoint_auth_signing_alg: 'ES256',
					dpop_bound_access_tokens: true,
					jwks_uri: `${baseUrl}/connections/bluesky/jwks.json`,
				},
				keyset,
				fetch,
				handleResolver,
			},
			kvClient,
		);
	}

	async authorize(handle: string, userId: UserID): Promise<BlueskyAuthorizeResult> {
		try {
			const owner = validateBlueskyGrantOwner({userId, grantId: randomUUID()});
			const {client} = this.createClient(() => owner);
			const state = JSON.stringify({userId: String(owner.userId), grantId: owner.grantId});
			const url = await client.authorize(handle, {state});
			return {authorizeUrl: url.toString()};
		} catch (error) {
			throw createOAuthOperationError('authorize account', error);
		}
	}

	async callback(params: URLSearchParams): Promise<BlueskyCallbackResult> {
		if (!params.has('response') && !params.get('state')) {
			throw new BlueskyOAuthStateInvalidError();
		}
		let owner: BlueskyOAuthGrantOwner | undefined;
		const requireOwner = (): BlueskyOAuthGrantOwner => {
			if (!owner) throw new BlueskyOAuthStateInvalidError();
			return owner;
		};
		const {client} = this.createClient(requireOwner, (consumedOwner) => {
			if (owner) throw new BlueskyOAuthStateInvalidError();
			owner = consumedOwner;
		});
		const {session} = await client.callback(params);
		const grantOwner = requireOwner();
		const handle = await this.resolveHandle(client, session.did);
		return {
			...grantOwner,
			did: session.did,
			handle,
		};
	}

	private createClient(
		resolveOwner: () => BlueskyOAuthGrantOwner,
		onConsume?: (owner: BlueskyOAuthGrantOwner) => void,
	): BlueskyClientSession {
		const sessionStore = createSessionStore(this.credentials, 86400, resolveOwner);
		const client = new NodeOAuthClient({
			...this.configuration,
			stateStore: createStateStore(this.kvClient, this.credentials, 3600, resolveOwner, onConsume),
			sessionStore,
			requestLock: (name, operation) => {
				const owner = resolveOwner();
				return requestLocalLock(`bsky:${owner.userId}:${owner.grantId}:${name}`, operation);
			},
		});
		return {client, sessionStore};
	}

	private async resolveHandle(client: NodeOAuthClient, did: string): Promise<string> {
		try {
			for (const noCache of [false, true]) {
				const {handle} = await client.identityResolver.resolve(did, {noCache});
				if (handle !== HANDLE_INVALID) return handle;
			}
		} catch (error) {
			throw createOAuthOperationError('resolve handle', error);
		}
		throw createOAuthOperationError('resolve handle', new Error('The handle does not point back to the account'));
	}
}

function createOAuthOperationError(operation: 'authorize account' | 'resolve handle', error: unknown): Error {
	if (error instanceof BlueskyOAuthStoreError) return error;
	let status: number | undefined;
	if (error instanceof OAuthResponseError) status = error.status;
	else if (error instanceof FetchError) status = error.statusCode;
	const detail =
		status !== undefined && Number.isInteger(status) && status >= 100 && status <= 599 ? ` (HTTP ${status})` : '';
	return new Error(`Bluesky OAuth could not ${operation}${detail}`);
}

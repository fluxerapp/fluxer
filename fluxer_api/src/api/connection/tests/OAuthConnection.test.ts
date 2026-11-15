// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {createAuthHarness, createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {ConnectionResponse} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import {type CryptoKey, exportJWK, generateKeyPair, type JWK, type JWTPayload, SignJWT} from 'jose';
import {HttpResponse, http} from 'msw';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const ISSUER = 'https://1.1.1.1/realms/connections';
const OAUTH2_HOST = 'https://8.8.8.8';
const CLIENT_ID = 'connections-client';
const SECRET = 'connections-secret';
const KEY_ID = 'connections-key';

interface ProviderInput {
	id: string;
	name: string;
	enabled: boolean;
	kind: 'oidc' | 'oauth2';
	client_id: string;
	scope: string;
	client_secret?: string;
	[key: string]: unknown;
}

interface AdminProvider {
	id: string;
	client_secret_set: boolean;
	callback_url: string;
	client_secret?: string;
}

interface TokenRequest {
	body: URLSearchParams;
	authorization: string | null;
}

interface ProfileResponse {
	connected_accounts?: Array<ConnectionResponse>;
}

function oidcProvider(overrides: Partial<ProviderInput> = {}): ProviderInput {
	return {
		id: 'idp',
		name: 'Test IdP',
		enabled: true,
		kind: 'oidc',
		issuer: ISSUER,
		client_id: CLIENT_ID,
		client_secret: SECRET,
		scope: 'openid profile',
		profile_url_template: 'https://1.1.1.1/u/{name}',
		icon_url: 'https://1.1.1.1/icon.png',
		...overrides,
	};
}

function oauth2Provider(overrides: Partial<ProviderInput> = {}): ProviderInput {
	return {
		id: 'stream',
		name: 'Stream',
		enabled: true,
		kind: 'oauth2',
		authorization_url: `${OAUTH2_HOST}/oauth2/authorize?force_verify=true`,
		token_url: `${OAUTH2_HOST}/oauth2/token`,
		userinfo_url: `${OAUTH2_HOST}/helix/users`,
		client_id: CLIENT_ID,
		client_secret: SECRET,
		scope: 'user:read',
		id_path: 'data.0.id',
		name_path: 'data.0.display_name',
		...overrides,
	};
}

describe('OAuth and OpenID Connect profile connections', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let account: TestAccount;
	let signingKey: CryptoKey;
	let jwk: JWK;
	let tokenRequests: Array<TokenRequest>;
	let claimOverrides: JWTPayload;
	let originalSelfHosted: boolean;

	beforeAll(async () => {
		harness = await createAuthHarness();
		originalSelfHosted = getConfig().instance.selfHosted;
		const {publicKey, privateKey} = await generateKeyPair('RS256');
		signingKey = privateKey;
		jwk = {...(await exportJWK(publicKey)), kid: KEY_ID, alg: 'RS256'};
	});

	beforeEach(async () => {
		getConfig().instance.selfHosted = originalSelfHosted;
		await harness.reset();
		tokenRequests = [];
		claimOverrides = {};
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'instance:config:update',
			'instance:config:view',
		]);
		account = await createTestAccount(harness);
		getConfig().instance.selfHosted = true;
		server.use(
			http.get(`${ISSUER}/certs`, () => HttpResponse.json({keys: [jwk]})),
			http.get(`${ISSUER}/.well-known/openid-configuration`, () =>
				HttpResponse.json({
					issuer: ISSUER,
					authorization_endpoint: `${ISSUER}/auth`,
					token_endpoint: `${ISSUER}/token`,
					userinfo_endpoint: `${ISSUER}/userinfo`,
					jwks_uri: `${ISSUER}/certs`,
				}),
			),
			http.post(`${ISSUER}/token`, async ({request}) => {
				const body = new URLSearchParams(await request.text());
				tokenRequests.push({body, authorization: request.headers.get('authorization')});
				const [sub, nonce] = (body.get('code') ?? '').split('|');
				const idToken = await new SignJWT({nonce, preferred_username: 'Ada Lovelace', ...claimOverrides})
					.setProtectedHeader({alg: 'RS256', kid: KEY_ID})
					.setIssuer((claimOverrides.iss as string | undefined) ?? ISSUER)
					.setAudience(claimOverrides.aud ?? CLIENT_ID)
					.setSubject(sub ?? '')
					.setIssuedAt()
					.setExpirationTime('5m')
					.sign(signingKey);
				return HttpResponse.json({id_token: idToken, access_token: 'access', token_type: 'Bearer'});
			}),
		);
	});

	afterAll(async () => {
		getConfig().instance.selfHosted = originalSelfHosted;
		await harness?.shutdown();
	});

	async function saveProviders(
		providers: Array<ProviderInput>,
		status = 200,
	): Promise<{
		integrations: {connection_providers: Array<AdminProvider>};
		errors?: Array<{path: string}>;
	}> {
		return createBuilder<{integrations: {connection_providers: Array<AdminProvider>}; errors?: Array<{path: string}>}>(
			harness,
			admin.token,
		)
			.patch('/admin/instance/config')
			.body({integrations: {connection_providers: providers}})
			.expect(status)
			.execute();
	}

	async function authorize(providerId = 'idp', user = account): Promise<URL> {
		const {authorize_url} = await createBuilder<{authorize_url: string}>(harness, user.token)
			.post(`/users/@me/connections/oauth/${providerId}/authorize`)
			.execute();
		return new URL(authorize_url);
	}

	async function callback(providerId: string, params: Record<string, string>): Promise<URL> {
		const {response} = await createBuilderWithoutAuth(harness)
			.get(`/connections/oauth/${providerId}/callback?${new URLSearchParams(params)}`)
			.executeRaw();
		expect(response.status).toBe(302);
		return new URL(response.headers.get('location') ?? '');
	}

	async function connect(sub: string, providerId = 'idp'): Promise<URL> {
		const url = await authorize(providerId);
		const state = url.searchParams.get('state') ?? '';
		return callback(providerId, {code: `${sub}|${url.searchParams.get('nonce') ?? ''}`, state});
	}

	async function listConnections(user = account): Promise<Array<ConnectionResponse>> {
		return createBuilder<Array<ConnectionResponse>>(harness, user.token).get('/users/@me/connections').execute();
	}

	async function profileConnections(user = account): Promise<Array<ConnectionResponse>> {
		const profile = await createBuilder<ProfileResponse>(harness, user.token)
			.get(`/users/${user.userId}/profile`)
			.execute();
		return profile.connected_accounts ?? [];
	}

	function reason(location: URL): string | null {
		return location.searchParams.get('status') === 'connected' ? 'connected' : location.searchParams.get('reason');
	}

	it('links an OpenID Connect account with PKCE, state and nonce', async () => {
		await saveProviders([oidcProvider()]);
		const url = await authorize();
		expect(`${url.origin}${url.pathname}`).toBe(`${ISSUER}/auth`);
		expect(url.searchParams.get('code_challenge_method')).toBe('S256');
		expect(url.searchParams.get('redirect_uri')).toMatch(/\/connections\/oauth\/idp\/callback$/);
		const location = await callback('idp', {
			code: `subject-1|${url.searchParams.get('nonce')}`,
			state: url.searchParams.get('state') ?? '',
			iss: ISSUER,
		});
		expect(reason(location)).toBe('connected');
		const verifier = tokenRequests[0]?.body.get('code_verifier') ?? '';
		expect(createHash('sha256').update(verifier).digest('base64url')).toBe(url.searchParams.get('code_challenge'));
		const [connection] = await listConnections();
		expect(connection).toMatchObject({
			type: 'oauth',
			provider_id: 'idp',
			name: 'Ada Lovelace',
			verified: true,
			url: 'https://1.1.1.1/u/Ada%20Lovelace',
		});
	});

	it('refuses a state that was already used', async () => {
		await saveProviders([oidcProvider()]);
		const url = await authorize();
		const params = {code: `subject-1|${url.searchParams.get('nonce')}`, state: url.searchParams.get('state') ?? ''};
		expect(reason(await callback('idp', params))).toBe('connected');
		expect(reason(await callback('idp', params))).toBe('state_invalid');
	});

	it('refuses a state started for another provider', async () => {
		await saveProviders([oidcProvider(), oidcProvider({id: 'other', name: 'Other'})]);
		const url = await authorize('idp');
		const location = await callback('other', {
			code: `subject-1|${url.searchParams.get('nonce')}`,
			state: url.searchParams.get('state') ?? '',
		});
		expect(reason(location)).toBe('state_invalid');
	});

	it('refuses a callback that names another issuer', async () => {
		await saveProviders([oidcProvider()]);
		const url = await authorize();
		const location = await callback('idp', {
			code: `subject-1|${url.searchParams.get('nonce')}`,
			state: url.searchParams.get('state') ?? '',
			iss: 'https://1.1.1.1/realms/other',
		});
		expect(reason(location)).toBe('state_invalid');
	});

	it.each([
		['another issuer', {iss: 'https://1.1.1.1/realms/other'}],
		['another audience', {aud: 'other-client'}],
		['another authorized party', {aud: [CLIENT_ID, 'other-client'], azp: 'other-client'}],
		['another nonce', {nonce: 'replayed-nonce'}],
	])('refuses an id_token with %s', async (_label, overrides) => {
		await saveProviders([oidcProvider()]);
		claimOverrides = overrides;
		expect(reason(await connect('subject-1'))).toBe('callback_failed');
		expect(await listConnections()).toEqual([]);
	});

	it('consumes the state when the person declines at the provider', async () => {
		await saveProviders([oidcProvider()]);
		const url = await authorize();
		const state = url.searchParams.get('state') ?? '';
		expect(reason(await callback('idp', {error: 'access_denied', state}))).toBe('denied');
		const nonce = url.searchParams.get('nonce') ?? '';
		expect(reason(await callback('idp', {code: `subject-1|${nonce}`, state}))).toBe('state_invalid');
	});

	it('refuses the link after the session that started it signs out', async () => {
		await saveProviders([oidcProvider()]);
		const url = await authorize();
		await createBuilder(harness, account.token).post('/auth/logout').expect(204).execute();
		const location = await callback('idp', {
			code: `subject-1|${url.searchParams.get('nonce')}`,
			state: url.searchParams.get('state') ?? '',
		});
		expect(reason(location)).toBe('state_invalid');
		expect(tokenRequests).toEqual([]);
	});

	it('refreshes the existing connection when the same account links again', async () => {
		await saveProviders([oidcProvider()]);
		await connect('subject-1');
		claimOverrides = {preferred_username: 'Countess'};
		expect(reason(await connect('subject-1'))).toBe('connected');
		const connections = await listConnections();
		expect(connections).toHaveLength(1);
		expect(connections[0]?.name).toBe('Countess');
	});

	it('removes a connection', async () => {
		await saveProviders([oidcProvider()]);
		await connect('subject-1');
		const [connection] = await listConnections();
		await createBuilder(harness, account.token)
			.delete(`/users/@me/connections/oauth/${connection?.id}`)
			.expect(204)
			.execute();
		expect(await listConnections()).toEqual([]);
	});

	it('hides connections while their provider is removed or points at another issuer', async () => {
		await saveProviders([oidcProvider()]);
		await connect('subject-1');
		expect(await profileConnections()).toHaveLength(1);

		await saveProviders([]);
		expect(await profileConnections()).toEqual([]);
		expect(await listConnections()).toMatchObject([{type: 'oauth', provider_id: null, url: null}]);

		const otherIssuer = 'https://1.1.1.1/realms/elsewhere';
		server.use(
			http.get(`${otherIssuer}/.well-known/openid-configuration`, () =>
				HttpResponse.json({issuer: otherIssuer, authorization_endpoint: `${otherIssuer}/auth`}),
			),
		);
		await saveProviders([oidcProvider({issuer: otherIssuer})]);
		expect(await profileConnections()).toEqual([]);

		await saveProviders([oidcProvider()]);
		expect(await profileConnections()).toMatchObject([{provider_id: 'idp', name: 'Ada Lovelace'}]);
	});

	it('links a plain OAuth 2.0 account through its user endpoint', async () => {
		const userRequests: Array<Headers> = [];
		server.use(
			http.post(`${OAUTH2_HOST}/oauth2/token`, async ({request}) => {
				tokenRequests.push({
					body: new URLSearchParams(await request.text()),
					authorization: request.headers.get('authorization'),
				});
				return HttpResponse.json({access_token: 'stream-access', token_type: 'bearer'});
			}),
			http.get(`${OAUTH2_HOST}/helix/users`, ({request}) => {
				userRequests.push(request.headers);
				return HttpResponse.json({data: [{id: 4242, display_name: 'Streamer'}]});
			}),
		);
		await saveProviders([oauth2Provider()]);
		const url = await authorize('stream');
		expect(url.searchParams.get('force_verify')).toBe('true');
		expect(url.searchParams.has('nonce')).toBe(false);
		const location = await callback('stream', {code: 'stream-code', state: url.searchParams.get('state') ?? ''});
		expect(reason(location)).toBe('connected');
		expect(userRequests[0]?.get('authorization')).toBe('Bearer stream-access');
		expect(userRequests[0]?.get('client-id')).toBe(CLIENT_ID);
		expect(await listConnections()).toMatchObject([{type: 'oauth', provider_id: 'stream', name: 'Streamer'}]);
	});

	it('keeps client secrets out of responses and asks for one when the identity provider changes', async () => {
		const saved = await saveProviders([oidcProvider()]);
		expect(saved.integrations.connection_providers[0]).toMatchObject({id: 'idp', client_secret_set: true});
		expect(saved.integrations.connection_providers[0]).not.toHaveProperty('client_secret');
		const {client_secret: _secret, ...withoutSecret} = oidcProvider();
		await saveProviders([{...withoutSecret, name: 'Renamed'}]);
		await saveProviders([{...withoutSecret, issuer: 'https://1.1.1.1/realms/elsewhere'}], 400);
		const moved = await saveProviders([{...withoutSecret, token_url: 'https://8.8.8.8/token'}], 400);
		expect(moved.errors?.map((error) => error.path)).toEqual(['integrations.connection_providers.0.client_secret']);
		await saveProviders([{...withoutSecret, id: 'fresh'}], 400);
	});

	it('lists enabled providers in the discovery document', async () => {
		await saveProviders([oidcProvider(), oauth2Provider({enabled: false})]);
		const discovery = await createBuilderWithoutAuth<{
			services: {connection_providers: Array<{id: string; name: string; icon_url: string | null}>};
		}>(harness)
			.get('/.well-known/fluxer')
			.execute();
		expect(discovery.services.connection_providers).toHaveLength(1);
		expect(discovery.services.connection_providers[0]).toMatchObject({id: 'idp', name: 'Test IdP'});
		expect(discovery.services.connection_providers[0]?.icon_url).not.toBe('https://1.1.1.1/icon.png');
	});

	it('offers no providers outside self-hosted instances', async () => {
		await saveProviders([oidcProvider()]);
		getConfig().instance.selfHosted = false;
		try {
			await saveProviders([oidcProvider()], 400);
			await createBuilder(harness, account.token)
				.post('/users/@me/connections/oauth/idp/authorize')
				.expect(404)
				.execute();
		} finally {
			getConfig().instance.selfHosted = true;
		}
	});
});

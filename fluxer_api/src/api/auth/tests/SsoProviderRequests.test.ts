// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	disableSso,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {exportJWK, generateKeyPair, SignJWT} from 'jose';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const ISSUER = 'https://1.1.1.1/realms/requests';
const CLIENT_ID = 'requests-client';
const SECRET = 'requests-secret';

interface SsoStartResponse {
	authorization_url: string;
	state: string;
	client_binding: string;
}

interface ErrorBody {
	code: string;
	errors?: Array<{path: string; code: string}>;
}

interface TokenRequest {
	authorization: string | null;
	body: URLSearchParams;
}

interface ProviderOptions {
	authMethods?: Array<string>;
	jwks?: boolean;
	clientSecret?: string;
}

describe('SSO requests to the provider', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let tokenRequests: Array<TokenRequest>;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		tokenRequests = [];
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			'admin:authenticate',
			'instance:config:update',
			'instance:config:view',
		]);
	});
	afterEach(async () => {
		await disableSso(harness, admin.token);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function useProvider({authMethods, jwks = false, clientSecret = SECRET}: ProviderOptions = {}): Promise<void> {
		server.use(
			http.get(`${ISSUER}/.well-known/openid-configuration`, () =>
				HttpResponse.json({
					issuer: ISSUER,
					authorization_endpoint: `${ISSUER}/auth`,
					token_endpoint: `${ISSUER}/token`,
					userinfo_endpoint: `${ISSUER}/userinfo`,
					...(jwks ? {jwks_uri: `${ISSUER}/certs`} : {}),
					...(authMethods ? {token_endpoint_auth_methods_supported: authMethods} : {}),
				}),
			),
			http.get(`${ISSUER}/userinfo`, () =>
				HttpResponse.json({
					sub: `itest-requests-${randomUUID()}`,
					email: createUniqueEmail('sso-requests'),
					email_verified: true,
				}),
			),
		);
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({
				sso: {
					enabled: true,
					enforced: false,
					issuer: ISSUER,
					authorization_url: null,
					token_url: null,
					userinfo_url: null,
					jwks_url: null,
					client_id: CLIENT_ID,
					client_secret: clientSecret,
					allowed_domains: [],
					auto_provision: true,
				},
			})
			.execute();
	}

	function tokenEndpoint(respond: (request: TokenRequest) => Response | undefined = () => undefined): void {
		server.use(
			http.post(`${ISSUER}/token`, async ({request}) => {
				const tokenRequest = {
					authorization: request.headers.get('authorization'),
					body: new URLSearchParams(await request.text()),
				};
				tokenRequests.push(tokenRequest);
				return (
					respond(tokenRequest) ??
					HttpResponse.json({access_token: `access-${tokenRequests.length}`, token_type: 'Bearer'})
				);
			}),
		);
	}

	async function start(): Promise<SsoStartResponse> {
		return createBuilderWithoutAuth<SsoStartResponse>(harness).post('/auth/sso/start').body({}).execute();
	}

	async function complete(status: number, code = 'provider-code'): Promise<ErrorBody> {
		const {state, client_binding} = await start();
		return createBuilderWithoutAuth<ErrorBody>(harness)
			.post('/auth/sso/complete')
			.body({code, state, client_binding})
			.expect(status)
			.execute();
	}

	function decodeBasic(header: string | null): Array<string> {
		const decoded = Buffer.from((header ?? '').replace(/^Basic /, ''), 'base64').toString();
		return decoded.split(':').map((part) => decodeURIComponent(part.replace(/\+/g, ' ')));
	}

	function errorCodes(body: ErrorBody): Array<string> {
		return (body.errors ?? []).map((error) => error.code);
	}

	describe('client authentication', () => {
		it('sends the secret in the body when the provider lists both methods', async () => {
			await useProvider({authMethods: ['client_secret_basic', 'client_secret_post']});
			tokenEndpoint();
			await complete(200);
			expect(tokenRequests.map((request) => (request.authorization ? 'basic' : 'post'))).toEqual(['post']);
			expect(tokenRequests[0]!.body.get('client_secret')).toBe(SECRET);
		});

		it('uses Basic auth when the provider lists only that', async () => {
			await useProvider({authMethods: ['client_secret_basic']});
			tokenEndpoint();
			await complete(200);
			expect(decodeBasic(tokenRequests[0]!.authorization)).toEqual([CLIENT_ID, SECRET]);
			expect(tokenRequests[0]!.body.has('client_secret')).toBe(false);
		});

		it('sends the secret in the body when the provider only lists that', async () => {
			await useProvider({authMethods: ['client_secret_post']});
			tokenEndpoint();
			await complete(200);
			expect(tokenRequests[0]!.authorization).toBeNull();
			expect(tokenRequests[0]!.body.get('client_secret')).toBe(SECRET);
		});

		it('form-encodes a secret with a colon and a percent sign for Basic auth', async () => {
			await useProvider({authMethods: ['client_secret_basic'], clientSecret: 'a:b%c d'});
			tokenEndpoint();
			await complete(200);
			const expected = Buffer.from(`${CLIENT_ID}:a%3Ab%25c+d`).toString('base64');
			expect(tokenRequests[0]!.authorization).toBe(`Basic ${expected}`);
		});

		it('uses Basic auth first when the provider lists no methods', async () => {
			await useProvider();
			tokenEndpoint();
			await complete(200);
			expect(tokenRequests.map((request) => (request.authorization ? 'basic' : 'post'))).toEqual(['basic']);
			expect(decodeBasic(tokenRequests[0]!.authorization)).toEqual([CLIENT_ID, SECRET]);
		});

		it('switches to the body once Basic auth is refused, and remembers it', async () => {
			await useProvider();
			tokenEndpoint(({authorization}) =>
				authorization ? HttpResponse.json({error: 'invalid_client'}, {status: 401}) : undefined,
			);
			await complete(200);
			await complete(200);
			expect(tokenRequests.map((request) => (request.authorization ? 'basic' : 'post'))).toEqual([
				'basic',
				'post',
				'post',
			]);
		});

		it('switches to Basic auth when the provider lists both and turns the body down with another error', async () => {
			await useProvider({authMethods: ['client_secret_post', 'client_secret_basic']});
			tokenEndpoint(({authorization}) =>
				authorization ? undefined : HttpResponse.json({error: 'unauthorized_client'}, {status: 400}),
			);
			await complete(200);
			expect(tokenRequests.map((request) => (request.authorization ? 'basic' : 'post'))).toEqual(['post', 'basic']);
		});

		it('switches to the body when the provider lists only Basic auth and turns it down', async () => {
			await useProvider({authMethods: ['client_secret_basic']});
			tokenEndpoint(({authorization}) =>
				authorization ? HttpResponse.json({error: 'invalid_request'}, {status: 400}) : undefined,
			);
			await complete(200);
			expect(tokenRequests.map((request) => (request.authorization ? 'basic' : 'post'))).toEqual(['basic', 'post']);
		});

		it('keeps a public client id in the body', async () => {
			await useProvider({authMethods: ['client_secret_basic'], clientSecret: ''});
			tokenEndpoint();
			await complete(200);
			expect(tokenRequests[0]!.authorization).toBeNull();
			expect(tokenRequests[0]!.body.get('client_id')).toBe(CLIENT_ID);
			expect(tokenRequests[0]!.body.has('client_secret')).toBe(false);
		});
	});

	describe('token errors', () => {
		it('reports a refused client as misconfigured', async () => {
			await useProvider({authMethods: ['client_secret_basic']});
			tokenEndpoint(() => HttpResponse.json({error: 'invalid_client'}, {status: 401}));
			expect(errorCodes(await complete(400))).toEqual([ValidationErrorCodes.SSO_MISCONFIGURED]);
		});

		it('reports a client the provider turns away with any 401 as misconfigured', async () => {
			await useProvider();
			tokenEndpoint(() => HttpResponse.json({error: 'access_denied'}, {status: 401}));
			expect(errorCodes(await complete(400))).toEqual([ValidationErrorCodes.SSO_MISCONFIGURED]);
			expect(tokenRequests).toHaveLength(2);
		});

		it('reports a server error from the token endpoint as unavailable', async () => {
			await useProvider();
			tokenEndpoint(() => HttpResponse.json({error: 'server_error'}, {status: 502}));
			const {state, client_binding} = await start();
			await createBuilderWithoutAuth(harness)
				.post('/auth/sso/complete')
				.body({code: 'provider-code', state, client_binding})
				.expect(503, 'SSO_PROVIDER_UNAVAILABLE')
				.execute();
		});

		it('reports a refused code as an invalid code', async () => {
			await useProvider({authMethods: ['client_secret_basic']});
			tokenEndpoint(() => HttpResponse.json({error: 'invalid_grant'}, {status: 400}));
			expect(errorCodes(await complete(400))).toEqual([ValidationErrorCodes.INVALID_SSO_AUTHORIZATION_CODE]);
			expect(tokenRequests).toHaveLength(1);
		});
	});

	it('does not follow a redirect from the token endpoint', async () => {
		await useProvider({authMethods: ['client_secret_post']});
		let redirectedRequests = 0;
		server.use(
			http.post(`${ISSUER}/token`, () => new HttpResponse(null, {status: 307, headers: {location: `${ISSUER}/moved`}})),
			http.post(`${ISSUER}/moved`, () => {
				redirectedRequests++;
				return HttpResponse.json({access_token: 'moved-access', token_type: 'Bearer'});
			}),
		);
		expect(errorCodes(await complete(400))).toEqual([ValidationErrorCodes.SSO_MISCONFIGURED]);
		expect(redirectedRequests).toBe(0);
	});

	it('reports a redirecting userinfo URL as misconfigured', async () => {
		await useProvider({authMethods: ['client_secret_basic']});
		tokenEndpoint();
		server.use(
			http.get(
				`${ISSUER}/userinfo`,
				() => new HttpResponse(null, {status: 302, headers: {location: `${ISSUER}/userinfo/`}}),
			),
		);
		expect(errorCodes(await complete(400))).toEqual([ValidationErrorCodes.SSO_MISCONFIGURED]);
	});

	it('reports a token endpoint that does not answer as unavailable', async () => {
		await useProvider({authMethods: ['client_secret_post']});
		server.use(http.post(`${ISSUER}/token`, () => HttpResponse.error()));
		const {state, client_binding} = await start();
		await createBuilderWithoutAuth(harness)
			.post('/auth/sso/complete')
			.body({code: 'provider-code', state, client_binding})
			.expect(503, 'SSO_PROVIDER_UNAVAILABLE')
			.execute();
	});

	it('reports a userinfo endpoint that does not answer as unavailable', async () => {
		await useProvider({authMethods: ['client_secret_post']});
		tokenEndpoint();
		server.use(http.get(`${ISSUER}/userinfo`, () => HttpResponse.error()));
		const {state, client_binding} = await start();
		await createBuilderWithoutAuth(harness)
			.post('/auth/sso/complete')
			.body({code: 'provider-code', state, client_binding})
			.expect(503, 'SSO_PROVIDER_UNAVAILABLE')
			.execute();
	});

	it('refuses a signed userinfo response', async () => {
		await useProvider({authMethods: ['client_secret_basic']});
		tokenEndpoint();
		server.use(
			http.get(
				`${ISSUER}/userinfo`,
				() => new HttpResponse('eyJhbGciOiJSUzI1NiJ9.e30.sig', {headers: {'content-type': 'application/jwt'}}),
			),
		);
		expect(errorCodes(await complete(400))).toEqual([ValidationErrorCodes.SSO_USER_INFO_SIGNED_RESPONSE_UNSUPPORTED]);
	});

	describe('provider keys', () => {
		let jwksRequests: number;

		async function useIdTokenProvider(): Promise<CryptoKey> {
			jwksRequests = 0;
			const {publicKey, privateKey} = await generateKeyPair('RS256');
			const jwk = {...(await exportJWK(publicKey)), kid: 'requests-key', alg: 'RS256'};
			server.use(
				http.get(`${ISSUER}/certs`, () => {
					jwksRequests++;
					return HttpResponse.json({keys: [jwk]});
				}),
			);
			await useProvider({authMethods: ['client_secret_basic'], jwks: true});
			tokenEndpoint(({body}) => HttpResponse.json({id_token: body.get('code'), token_type: 'Bearer'}));
			return privateKey;
		}

		async function signIn(privateKey: CryptoKey, status = 200): Promise<ErrorBody> {
			const {authorization_url, state, client_binding} = await start();
			const nonce = new URL(authorization_url).searchParams.get('nonce');
			const idToken = await new SignJWT({nonce, email: createUniqueEmail('sso-keys'), email_verified: true})
				.setProtectedHeader({alg: 'RS256', kid: 'requests-key'})
				.setIssuer(ISSUER)
				.setAudience(CLIENT_ID)
				.setSubject(`itest-keys-${state}`)
				.setIssuedAt()
				.setExpirationTime('5m')
				.sign(privateKey);
			return createBuilderWithoutAuth<ErrorBody>(harness)
				.post('/auth/sso/complete')
				.body({code: idToken, state, client_binding})
				.expect(status)
				.execute();
		}

		it('reports keys that cannot be fetched as unavailable', async () => {
			const privateKey = await useIdTokenProvider();
			server.use(http.get(`${ISSUER}/certs`, () => HttpResponse.error()));
			expect((await signIn(privateKey, 503)).code).toBe('SSO_PROVIDER_UNAVAILABLE');
		});

		it('reports a keys URL that answers with a server error as unavailable', async () => {
			const privateKey = await useIdTokenProvider();
			server.use(http.get(`${ISSUER}/certs`, () => new HttpResponse(null, {status: 503})));
			expect((await signIn(privateKey, 503)).code).toBe('SSO_PROVIDER_UNAVAILABLE');
		});

		it('still reports an id_token signed with another key as invalid', async () => {
			await useIdTokenProvider();
			const {privateKey: otherKey} = await generateKeyPair('RS256');
			expect(errorCodes(await signIn(otherKey, 400))).toEqual([ValidationErrorCodes.INVALID_SSO_TOKEN]);
		});

		it('fetches the keys once across sign-ins', async () => {
			const privateKey = await useIdTokenProvider();
			await signIn(privateKey);
			await signIn(privateKey);
			expect(jwksRequests).toBe(1);
		});

		it('fetches the keys once for sign-ins that arrive together', async () => {
			const privateKey = await useIdTokenProvider();
			await Promise.all([signIn(privateKey), signIn(privateKey)]);
			expect(jwksRequests).toBe(1);
		});
	});
});

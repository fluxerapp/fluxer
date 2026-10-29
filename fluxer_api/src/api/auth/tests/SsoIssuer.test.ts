// SPDX-License-Identifier: AGPL-3.0-or-later

import {SsoIdentityRepository} from '@app/api/auth/services/SsoIdentityRepository';
import {
	createAuthHarness,
	createTestAccount,
	createUniqueEmail,
	disableSso,
	enableSso,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {exportJWK, generateKeyPair, SignJWT} from 'jose';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const ISSUER = 'https://1.1.1.1/realms/issuer';
const CLIENT_ID = 'issuer-client';

interface SsoStartResponse {
	authorization_url: string;
	state: string;
	client_binding: string;
}

interface ErrorBody {
	code: string;
	errors?: Array<{path: string; code: string}>;
}

describe('SSO issuer', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
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

	function saveSso(sso: Record<string, unknown>) {
		return createBuilder<ErrorBody>(harness, admin.token).patch('/admin/instance/config').body({sso});
	}

	async function signInWithTestProvider(sub: string): Promise<string> {
		const start = await createBuilderWithoutAuth<SsoStartResponse>(harness).post('/auth/sso/start').body({}).execute();
		const login = await createBuilderWithoutAuth<{user_id: string}>(harness)
			.post('/auth/sso/complete')
			.body({
				code: JSON.stringify({sub, email: createUniqueEmail('sso-issuer')}),
				state: start.state,
				client_binding: start.client_binding,
			})
			.execute();
		return login.user_id;
	}

	async function holderOf(providerId: string, sub: string): Promise<string | undefined> {
		return (await new SsoIdentityRepository().findUserId(providerId, sub))?.toString();
	}

	function subject(label: string): string {
		return `itest-issuer-${label}-${Date.now()}-${Math.random().toString(36).slice(2)}`;
	}

	describe('at runtime', () => {
		async function useIdTokenProvider(): Promise<CryptoKey> {
			const {publicKey, privateKey} = await generateKeyPair('RS256');
			const jwk = {...(await exportJWK(publicKey)), kid: 'issuer-key', alg: 'RS256'};
			server.use(
				http.get(`${ISSUER}/.well-known/openid-configuration`, () =>
					HttpResponse.json({
						issuer: ISSUER,
						authorization_endpoint: `${ISSUER}/auth`,
						token_endpoint: `${ISSUER}/token`,
						jwks_uri: `${ISSUER}/certs`,
					}),
				),
				http.get(`${ISSUER}/certs`, () => HttpResponse.json({keys: [jwk]})),
			);
			await saveSso({
				enabled: true,
				enforced: false,
				issuer: ISSUER,
				authorization_url: null,
				token_url: null,
				userinfo_url: null,
				jwks_url: null,
				client_id: CLIENT_ID,
				allowed_domains: [],
				auto_provision: true,
			}).execute();
			return privateKey;
		}

		function completeWithIdTokenFrom(privateKey: CryptoKey, iss: string) {
			return {
				async expect(status: number) {
					const start = await createBuilderWithoutAuth<SsoStartResponse>(harness)
						.post('/auth/sso/start')
						.body({})
						.execute();
					const nonce = new URL(start.authorization_url).searchParams.get('nonce');
					const idToken = await new SignJWT({nonce, email: createUniqueEmail('sso-iss'), email_verified: true})
						.setProtectedHeader({alg: 'RS256', kid: 'issuer-key'})
						.setIssuer(iss)
						.setAudience(CLIENT_ID)
						.setSubject(subject('iss'))
						.setIssuedAt()
						.setExpirationTime('5m')
						.sign(privateKey);
					server.use(
						http.post(`${ISSUER}/token`, () =>
							HttpResponse.json({id_token: idToken, access_token: 'issuer-access', token_type: 'Bearer'}),
						),
					);
					return createBuilderWithoutAuth<ErrorBody>(harness)
						.post('/auth/sso/complete')
						.body({code: 'issuer-code', state: start.state, client_binding: start.client_binding})
						.expect(status)
						.execute();
				},
			};
		}

		it('signs in with an id_token from the configured issuer', async () => {
			const privateKey = await useIdTokenProvider();
			await completeWithIdTokenFrom(privateKey, ISSUER).expect(200);
		});

		it('rejects an id_token from another issuer', async () => {
			const privateKey = await useIdTokenProvider();
			const body = await completeWithIdTokenFrom(privateKey, 'https://1.1.1.1/realms/other').expect(400);
			expect(body.errors).toEqual([expect.objectContaining({code: ValidationErrorCodes.INVALID_SSO_TOKEN})]);
		});

		describe('with every endpoint filled in', () => {
			const MANUAL_ISSUER = 'https://1.1.1.1/application/o/fluxer';

			async function useManualProvider(savedIssuer: string): Promise<CryptoKey> {
				const {publicKey, privateKey} = await generateKeyPair('RS256');
				const jwk = {...(await exportJWK(publicKey)), kid: 'issuer-key', alg: 'RS256'};
				server.use(http.get(`${MANUAL_ISSUER}/certs`, () => HttpResponse.json({keys: [jwk]})));
				await saveSso({
					enabled: true,
					enforced: false,
					issuer: savedIssuer,
					authorization_url: `${MANUAL_ISSUER}/authorize`,
					token_url: `${ISSUER}/token`,
					userinfo_url: null,
					jwks_url: `${MANUAL_ISSUER}/certs`,
					client_id: CLIENT_ID,
					allowed_domains: [],
					auto_provision: true,
				})
					.expect(200)
					.execute();
				return privateKey;
			}

			it('accepts an id_token whose issuer ends in a slash the saved issuer lacks', async () => {
				const privateKey = await useManualProvider(MANUAL_ISSUER);
				await completeWithIdTokenFrom(privateKey, `${MANUAL_ISSUER}/`).expect(200);
			});

			it('accepts an id_token whose issuer lacks the slash the saved issuer ends in', async () => {
				const privateKey = await useManualProvider(`${MANUAL_ISSUER}/`);
				await completeWithIdTokenFrom(privateKey, MANUAL_ISSUER).expect(200);
			});

			it('still rejects an id_token from another issuer', async () => {
				const privateKey = await useManualProvider(MANUAL_ISSUER);
				await completeWithIdTokenFrom(privateKey, `${MANUAL_ISSUER}-other`).expect(400);
			});
		});

		it('reports a stored config without an issuer as not ready', async () => {
			await getInstanceConfigRepository().setSsoConfig({
				enabled: true,
				issuer: null,
				authorizationUrl: 'https://1.1.1.1/auth',
				tokenUrl: 'https://1.1.1.1/token',
				userInfoUrl: 'https://1.1.1.1/userinfo',
				jwksUrl: 'https://1.1.1.1/certs',
				clientId: CLIENT_ID,
			});
			const status = await createBuilderWithoutAuth<{enabled: boolean}>(harness).get('/auth/sso/status').execute();
			expect(status.enabled).toBe(false);
		});
	});

	describe('when it changes', () => {
		const OLD_AUTHORIZATION_URL = 'https://1.1.1.1/legacy/auth';
		const NEW_ISSUER = 'https://1.1.1.1/realms/added';

		async function linkWithoutIssuer(label: string): Promise<{sub: string; userId: string}> {
			await getInstanceConfigRepository().setSsoConfig({
				enabled: true,
				enforced: false,
				issuer: null,
				authorizationUrl: OLD_AUTHORIZATION_URL,
				tokenUrl: 'https://1.1.1.1/legacy/token',
				userInfoUrl: 'https://1.1.1.1/legacy/userinfo',
				jwksUrl: null,
				clientId: CLIENT_ID,
			});
			const account = await createTestAccount(harness);
			const sub = subject(label);
			await new SsoIdentityRepository().tryClaimIdentity({
				providerId: OLD_AUTHORIZATION_URL,
				subject: sub,
				userId: createUserID(BigInt(account.userId)),
				claimedAt: new Date(),
			});
			return {sub, userId: account.userId};
		}

		function discoverNewIssuerWith(authorizationEndpoint: string) {
			server.use(
				http.get(`${NEW_ISSUER}/.well-known/openid-configuration`, () =>
					HttpResponse.json({
						issuer: NEW_ISSUER,
						authorization_endpoint: authorizationEndpoint,
						token_endpoint: `${NEW_ISSUER}/token`,
						jwks_uri: `${NEW_ISSUER}/certs`,
					}),
				),
			);
		}

		it('moves the links when the new issuer discovers the same authorization endpoint', async () => {
			const {sub, userId} = await linkWithoutIssuer('added');
			discoverNewIssuerWith(OLD_AUTHORIZATION_URL);
			await saveSso({issuer: NEW_ISSUER}).expect(200).execute();
			expect(await holderOf(NEW_ISSUER, sub)).toBe(userId);
			expect(await holderOf(OLD_AUTHORIZATION_URL, sub)).toBeUndefined();
		});

		it('refuses an added issuer that discovers another authorization endpoint', async () => {
			const {sub, userId} = await linkWithoutIssuer('other-endpoint');
			discoverNewIssuerWith(`${NEW_ISSUER}/auth`);
			const body = await saveSso({issuer: NEW_ISSUER}).expect(400).execute();
			expect(body.errors).toEqual([
				expect.objectContaining({path: 'issuer', code: ValidationErrorCodes.SSO_ISSUER_CHANGE_HAS_IDENTITIES}),
			]);
			expect(await holderOf(OLD_AUTHORIZATION_URL, sub)).toBe(userId);
		});

		it('refuses an added issuer whose discovery fails', async () => {
			const {sub, userId} = await linkWithoutIssuer('no-discovery');
			server.use(
				http.get(`${NEW_ISSUER}/.well-known/openid-configuration`, () => new HttpResponse(null, {status: 503})),
			);
			const body = await saveSso({issuer: NEW_ISSUER}).expect(400).execute();
			expect(body.errors).toEqual([
				expect.objectContaining({path: 'issuer', code: ValidationErrorCodes.SSO_ISSUER_CHANGE_HAS_IDENTITIES}),
			]);
			expect(await holderOf(OLD_AUTHORIZATION_URL, sub)).toBe(userId);
		});

		it('moves the links when only a trailing slash changes', async () => {
			await enableSso(harness, admin.token, {enforced: false, issuer: 'test-slash'});
			const sub = subject('slash');
			const userId = await signInWithTestProvider(sub);
			await saveSso({issuer: 'test-slash/'}).expect(200).execute();
			expect(await holderOf('test-slash/', sub)).toBe(userId);
		});

		it('refuses a new issuer while accounts are linked to the old one', async () => {
			await enableSso(harness, admin.token, {enforced: false, issuer: 'test-old'});
			await signInWithTestProvider(subject('refused'));
			const body = await saveSso({issuer: 'test-new'}).expect(400).execute();
			expect(body.errors).toEqual([
				expect.objectContaining({path: 'issuer', code: ValidationErrorCodes.SSO_ISSUER_CHANGE_HAS_IDENTITIES}),
			]);
		});

		it('moves the links to the new issuer when asked', async () => {
			await enableSso(harness, admin.token, {enforced: false, issuer: 'test-old'});
			const sub = subject('moved');
			const userId = await signInWithTestProvider(sub);
			await saveSso({issuer: 'test-new', migrate_identities: true}).expect(200).execute();
			expect(await holderOf('test-new', sub)).toBe(userId);
		});

		it('keeps the links when SSO is turned off and the issuer cleared', async () => {
			await enableSso(harness, admin.token, {enforced: false, issuer: 'test-cleared'});
			const sub = subject('cleared');
			const userId = await signInWithTestProvider(sub);
			await saveSso({enabled: false, issuer: null}).expect(200).execute();
			expect(await holderOf('test-cleared', sub)).toBe(userId);
		});

		async function signInWithEmail(sub: string, email: string, status: number) {
			const start = await createBuilderWithoutAuth<SsoStartResponse>(harness)
				.post('/auth/sso/start')
				.body({})
				.execute();
			return createBuilderWithoutAuth<{user_id?: string} & ErrorBody>(harness)
				.post('/auth/sso/complete')
				.body({code: JSON.stringify({sub, email}), state: start.state, client_binding: start.client_binding})
				.expect(status)
				.execute();
		}

		it('links an account to a different provider by email once the old links are left behind', async () => {
			await enableSso(harness, admin.token, {enforced: false, issuer: 'test-first-provider'});
			const email = createUniqueEmail('sso-switch');
			const first = await signInWithEmail(subject('first'), email, 200);
			await saveSso({issuer: 'test-second-provider', migrate_identities: false}).expect(200).execute();
			const second = await signInWithEmail(subject('second'), email, 200);
			expect(second.user_id).toBe(first.user_id);
		});

		it('refuses a sign-in from a different provider after its links were moved there by mistake', async () => {
			await enableSso(harness, admin.token, {enforced: false, issuer: 'test-first-provider'});
			const email = createUniqueEmail('sso-switch-moved');
			await signInWithEmail(subject('first'), email, 200);
			await saveSso({issuer: 'test-second-provider', migrate_identities: true}).expect(200).execute();
			const body = await signInWithEmail(subject('second'), email, 400);
			expect(body.errors).toEqual([
				expect.objectContaining({code: ValidationErrorCodes.SSO_ACCOUNT_LINKED_TO_ANOTHER_IDENTITY}),
			]);
		});

		it('leaves the links on the old issuer when asked', async () => {
			await enableSso(harness, admin.token, {enforced: false, issuer: 'test-old'});
			const sub = subject('left');
			const userId = await signInWithTestProvider(sub);
			await saveSso({issuer: 'test-new', migrate_identities: false}).expect(200).execute();
			expect(await holderOf('test-old', sub)).toBe(userId);
			expect(await holderOf('test-new', sub)).toBeUndefined();
		});
	});
});

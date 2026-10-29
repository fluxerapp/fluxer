// SPDX-License-Identifier: AGPL-3.0-or-later

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

const ISSUER = 'https://1.1.1.1/entra/v2.0';
const CLIENT_ID = 'entra-client';
const KEY_ID = 'entra-key';

interface SsoStartResponse {
	authorization_url: string;
	state: string;
	client_binding: string;
}

interface ErrorBody {
	errors?: Array<{code: string}>;
}

interface Provider {
	idToken: Record<string, unknown>;
	userInfo?: Record<string, unknown>;
}

describe('SSO email verification from Entra', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
	let privateKey: CryptoKey;
	let jwk: Record<string, unknown>;

	beforeAll(async () => {
		harness = await createAuthHarness();
		const keys = await generateKeyPair('RS256');
		privateKey = keys.privateKey;
		jwk = {...(await exportJWK(keys.publicKey)), kid: KEY_ID, alg: 'RS256'};
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

	async function signIn({idToken, userInfo}: Provider, status: number): Promise<ErrorBody> {
		server.use(
			http.get(`${ISSUER}/.well-known/openid-configuration`, () =>
				HttpResponse.json({
					issuer: ISSUER,
					authorization_endpoint: `${ISSUER}/authorize`,
					token_endpoint: `${ISSUER}/token`,
					jwks_uri: `${ISSUER}/keys`,
					...(userInfo ? {userinfo_endpoint: `${ISSUER}/userinfo`} : {}),
				}),
			),
			http.get(`${ISSUER}/keys`, () => HttpResponse.json({keys: [jwk]})),
			http.get(`${ISSUER}/userinfo`, () => HttpResponse.json(userInfo ?? {})),
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
					allowed_domains: [],
					auto_provision: true,
				},
			})
			.execute();
		const start = await createBuilderWithoutAuth<SsoStartResponse>(harness).post('/auth/sso/start').body({}).execute();
		const nonce = new URL(start.authorization_url).searchParams.get('nonce');
		const signed = await new SignJWT({nonce, ...idToken})
			.setProtectedHeader({alg: 'RS256', kid: KEY_ID})
			.setIssuer(ISSUER)
			.setAudience(CLIENT_ID)
			.setSubject(String(idToken['sub']))
			.setIssuedAt()
			.setExpirationTime('5m')
			.sign(privateKey);
		server.use(
			http.post(`${ISSUER}/token`, () =>
				HttpResponse.json({id_token: signed, access_token: 'entra-access', token_type: 'Bearer'}),
			),
		);
		return createBuilderWithoutAuth<ErrorBody>(harness)
			.post('/auth/sso/complete')
			.body({code: 'entra-code', state: start.state, client_binding: start.client_binding})
			.expect(status)
			.execute();
	}

	function subject(): string {
		return `itest-entra-${Date.now()}-${Math.random().toString(36).slice(2)}`;
	}

	function expectUnverified(body: ErrorBody): void {
		expect(body.errors?.map((error) => error.code)).toEqual([ValidationErrorCodes.SSO_EMAIL_NOT_VERIFIED]);
	}

	it('accepts xms_edov in the id_token as a verified email', async () => {
		await signIn({idToken: {sub: subject(), email: createUniqueEmail('entra'), xms_edov: true}}, 200);
	});

	it('lets email_verified false win over xms_edov', async () => {
		const body = await signIn(
			{idToken: {sub: subject(), email: createUniqueEmail('entra'), email_verified: false, xms_edov: true}},
			400,
		);
		expectUnverified(body);
	});

	it('ignores xms_edov from userinfo', async () => {
		const sub = subject();
		const email = createUniqueEmail('entra');
		const body = await signIn({idToken: {sub, email}, userInfo: {sub, email, xms_edov: true}}, 400);
		expectUnverified(body);
	});

	it('ignores xms_edov when the id_token has no email that matches the one in use', async () => {
		const sub = subject();
		const body = await signIn(
			{idToken: {sub, xms_edov: true}, userInfo: {sub, email: createUniqueEmail('entra')}},
			400,
		);
		expectUnverified(body);
	});
});

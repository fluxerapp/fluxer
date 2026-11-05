// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ADMIN_CLIENT_BASIC_AUTH,
	ADMIN_SSO_ISSUER,
	ADMIN_SSO_KID,
	ADMIN_SSO_NONCE,
	type AdminSsoProvider,
	adminSsoSignInRequest,
	idTokenClaimsJson,
	useAdminSsoProvider,
} from '@app/api/admin/tests/AdminSsoTestUtils';
import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import {generateKeyPair} from 'jose';
import {HttpResponse, http} from 'msw';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface SignInBody {
	user_id: string;
	access_token: string;
	token_type: string;
	expires_in: number;
	scope: string;
	code?: string;
	error?: string;
}

function isDoubleExact(id: string): boolean {
	return Number(BigInt(id)).toString() === id;
}

describe('Admin SSO sign-in', () => {
	let harness: ApiTestHarness;
	let provider: AdminSsoProvider;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		provider = await useAdminSsoProvider();
		admin = await createTestAccount(harness);
		while (isDoubleExact(admin.userId)) {
			admin = await createTestAccount(harness);
		}
		admin = await setUserACLs(harness, admin, ['admin:authenticate']);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function signIn(claimsJson: string, options: {nonce?: string; alg?: string; key?: CryptoKey} = {}) {
		const idToken = await provider.signRawClaims(claimsJson, {alg: options.alg, key: options.key});
		const response = await adminSsoSignInRequest(harness, {idToken, nonce: options.nonce});
		return {status: response.status, body: (await response.json()) as SignInBody, idToken};
	}

	it('signs in an admin and returns a working admin bearer token', async () => {
		const {status, body} = await signIn(idTokenClaimsJson(admin.userId));
		expect(status).toBe(200);
		expect(body.user_id).toBe(admin.userId);
		expect(body.token_type).toBe('Bearer');
		expect(body.scope).toBe('identify email');
		const me = await harness.requestJson({
			path: '/admin/users/@me',
			headers: {Authorization: `Bearer ${body.access_token}`},
		});
		expect(me.status).toBe(200);
	});

	it('keeps every digit of a fluxer_id sent as a bare JSON number', async () => {
		expect(isDoubleExact(admin.userId)).toBe(false);
		const {status, body} = await signIn(idTokenClaimsJson(admin.userId));
		expect(status).toBe(200);
		expect(body.user_id).toBe(admin.userId);
	});

	it('accepts fluxer_id as a decimal string', async () => {
		const {status, body} = await signIn(idTokenClaimsJson(`"${admin.userId}"`));
		expect(status).toBe(200);
		expect(body.user_id).toBe(admin.userId);
	});

	it.each([
		{aud: 'admin-sso-test-client'},
		{aud: ['admin-sso-test-client']},
		{aud: ['admin-sso-test-client', 'other-client']},
	])('accepts the admin client as the authorized party with audience %j', async ({aud}) => {
		const {status} = await signIn(idTokenClaimsJson(admin.userId, {aud, azp: 'admin-sso-test-client'}));
		expect(status).toBe(200);
	});

	it.each(['1.5', '-1', '1e21', '"abc"', '"0"', 'null'])('refuses fluxer_id %s', async (fluxerId) => {
		const {status, body} = await signIn(idTokenClaimsJson(fluxerId));
		expect(status).toBe(400);
		expect(body.code).toBe('INVALID_FORM_BODY');
	});

	it('refuses a token signed by another key with the same kid', async () => {
		const other = await generateKeyPair('RS256');
		const {status} = await signIn(idTokenClaimsJson(admin.userId), {key: other.privateKey});
		expect(status).toBe(400);
	});

	it.each([
		['a wrong issuer', {iss: 'https://1.1.1.1/other'}],
		['a wrong audience', {aud: 'someone-else'}],
		['an expired token', {exp: Math.floor(Date.now() / 1000) - 60}],
		['a future nbf', {nbf: Math.floor(Date.now() / 1000) + 600}],
		['a missing nonce', {nonce: undefined}],
		['an access token', {type: 'access-token'}],
		['a second audience without azp', {aud: ['admin-sso-test-client', 'other-client']}],
		['another authorized party with a string audience', {azp: 'other-client'}],
		['another authorized party with a single audience array', {aud: ['admin-sso-test-client'], azp: 'other-client'}],
		[
			'another authorized party with multiple audiences',
			{aud: ['admin-sso-test-client', 'other-client'], azp: 'other-client'},
		],
	])('refuses %s', async (_label, overrides) => {
		const {status} = await signIn(idTokenClaimsJson(admin.userId, overrides));
		expect(status).toBe(400);
	});

	it('refuses a nonce that does not match', async () => {
		const {status} = await signIn(idTokenClaimsJson(admin.userId), {nonce: 'admin-sso-other-nonce-0001'});
		expect(status).toBe(400);
	});

	it('refuses an unsigned token', async () => {
		const header = Buffer.from(JSON.stringify({alg: 'none', typ: 'JWT'})).toString('base64url');
		const payload = Buffer.from(idTokenClaimsJson(admin.userId)).toString('base64url');
		const response = await adminSsoSignInRequest(harness, {idToken: `${header}.${payload}.`});
		expect(response.status).toBe(400);
	});

	it('refuses an algorithm the provider does not advertise', async () => {
		const rs384 = await generateKeyPair('RS384');
		const {status} = await signIn(idTokenClaimsJson(admin.userId), {alg: 'RS384', key: rs384.privateKey});
		expect(status).toBe(400);
	});

	it('loads the key set again when the provider rotates its key', async () => {
		expect((await signIn(idTokenClaimsJson(admin.userId))).status).toBe(200);
		const before = provider.jwksRequests();
		const rotatedKey = await provider.rotate();
		const nonce = 'admin-sso-test-nonce-0002';
		const idToken = await provider.signRawClaims(idTokenClaimsJson(admin.userId, {nonce}), {
			key: rotatedKey,
			kid: `${ADMIN_SSO_KID}-rotated`,
		});
		const response = await adminSsoSignInRequest(harness, {idToken, nonce});
		expect(response.status).toBe(200);
		expect(provider.jwksRequests()).toBeGreaterThan(before);
	});

	it('refuses the same token twice', async () => {
		const {status, idToken} = await signIn(idTokenClaimsJson(admin.userId));
		expect(status).toBe(200);
		const replay = await adminSsoSignInRequest(harness, {idToken});
		expect(replay.status).toBe(400);
	});

	it('refuses the same token again with its signature encoded another way', async () => {
		const {status, idToken} = await signIn(idTokenClaimsJson(admin.userId));
		expect(status).toBe(200);
		const alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';
		const last = idToken.at(-1) ?? '';
		const variant = `${idToken.slice(0, -1)}${alphabet[(alphabet.indexOf(last) ^ 1) % 64]}`;
		const replay = await adminSsoSignInRequest(harness, {idToken: variant});
		expect(replay.status).toBe(400);
	});

	it('refuses an account without admin ACLs', async () => {
		const member = await createTestAccount(harness);
		const {status, body} = await signIn(idTokenClaimsJson(member.userId));
		expect(status).toBe(403);
		expect(body.code).toBe('MISSING_PERMISSIONS');
	});

	it('refuses an unknown fluxer_id', async () => {
		const {status, body} = await signIn(idTokenClaimsJson('1130650140672000001'));
		expect(status).toBe(403);
		expect(body.code).toBe('MISSING_PERMISSIONS');
	});

	it('refuses a wrong client secret before it looks at the token', async () => {
		const idToken = await provider.signRawClaims(idTokenClaimsJson(admin.userId));
		const auth = `Basic ${Buffer.from(`${ADMIN_OAUTH2_APPLICATION_ID}:wrong-secret`).toString('base64')}`;
		const response = await adminSsoSignInRequest(harness, {idToken, auth});
		expect(response.status).toBe(400);
		expect(((await response.json()) as SignInBody).error).toBe('invalid_client');
		expect(provider.jwksRequests()).toBe(0);
		const retry = await adminSsoSignInRequest(harness, {idToken, auth: ADMIN_CLIENT_BASIC_AUTH});
		expect(retry.status).toBe(200);
	});

	it('is off when the API has no admin SSO issuer', async () => {
		const previous = Config.admin.sso.issuer;
		Config.admin.sso.issuer = '';
		try {
			const {status} = await signIn(idTokenClaimsJson(admin.userId), {nonce: ADMIN_SSO_NONCE});
			expect(status).toBe(403);
		} finally {
			Config.admin.sso.issuer = previous;
		}
	});

	it('refuses signing keys supplied through unauthenticated HTTP discovery', async () => {
		const previous = Config.admin.sso.issuer;
		const issuer = ADMIN_SSO_ISSUER.replace('https:', 'http:');
		let discoveryRequests = 0;
		server.use(
			http.get(`${issuer}/.well-known/openid-configuration`, () => {
				discoveryRequests += 1;
				return HttpResponse.json({
					issuer,
					jwks_uri: `${ADMIN_SSO_ISSUER}/jwks.json`,
					id_token_signing_alg_values_supported: ['RS256'],
				});
			}),
		);
		Config.admin.sso.issuer = issuer;
		try {
			const {status} = await signIn(idTokenClaimsJson(admin.userId, {iss: issuer}));
			expect(status).toBe(400);
			expect(discoveryRequests).toBe(0);
			expect(provider.jwksRequests()).toBe(0);
		} finally {
			Config.admin.sso.issuer = previous;
		}
	});

	it.each(['http://1.1.1.1/admin-sso', 'https://1.1.1.1/admin-sso?tenant=admin'])(
		'refuses insecure or invalid issuer %s',
		async (issuer) => {
			const previous = Config.admin.sso.issuer;
			Config.admin.sso.issuer = issuer;
			try {
				const {status, body} = await signIn(idTokenClaimsJson(admin.userId));
				expect(status).toBe(400);
				expect(body.code).toBe('INVALID_FORM_BODY');
				expect(provider.jwksRequests()).toBe(0);
			} finally {
				Config.admin.sso.issuer = previous;
			}
		},
	);
});

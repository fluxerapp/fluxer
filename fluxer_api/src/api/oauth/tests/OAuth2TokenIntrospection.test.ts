// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	authorizeOAuth2,
	createOAuth2TestSetup,
	exchangeOAuth2AuthorizationCode,
	introspectOAuth2Token,
	revokeOAuth2Token,
} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Token Introspection', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should introspect active access token', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify email',
		});
		const tokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const introspection = await introspectOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			token: tokens.access_token,
		});
		expect(introspection.active).toBe(true);
		expect(introspection.client_id).toBe(application.id);
		expect(introspection.sub).toBe(endUser.userId);
		expect(introspection.scope).toContain('identify');
		expect(introspection.scope).toContain('email');
		expect(introspection.token_type).toBe('Bearer');
		expect(introspection.exp).toBeGreaterThan(0);
		expect(introspection.iat).toBeGreaterThan(0);
	});
	test('should show revoked token as inactive', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const tokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		await revokeOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			token: tokens.access_token,
			token_type_hint: 'access_token',
		});
		const introspection = await introspectOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			token: tokens.access_token,
		});
		expect(introspection.active).toBe(false);
	});
	test('should show invalid token as inactive', async () => {
		const {application} = await createOAuth2TestSetup(harness);
		const introspection = await introspectOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			token: 'invalid_token_12345',
		});
		expect(introspection.active).toBe(false);
	});
	test('should fail introspection with wrong client credentials', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const tokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		await createBuilder(harness, '')
			.post('/oauth2/introspect')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.header('Authorization', `Basic ${Buffer.from(`999999999999999999:wrong_secret`).toString('base64')}`)
			.body(
				new URLSearchParams({
					token: tokens.access_token,
				}).toString(),
			)
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});

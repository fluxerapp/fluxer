// SPDX-License-Identifier: AGPL-3.0-or-later

import {createApplicationID, createUserID} from '@app/api/BrandedTypes';
import {ACCESS_TOKEN_TTL_SECONDS} from '@app/api/oauth/OAuth2TokenConstants';
import {generateOAuthTokenSecret} from '@app/api/oauth/OAuthTokenSecret';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {
	authorizeOAuth2,
	createOAuth2TestSetup,
	exchangeOAuth2AuthorizationCode,
	getOAuth2UserInfo,
	introspectOAuth2Token,
	refreshOAuth2Token,
} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Token Expiration', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should include expires_in in token response', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify email',
		});
		const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		expect(tokenResponse.expires_in).toBeGreaterThan(0);
		expect(tokenResponse.expires_in).toBe(604800);
	});
	test('should have consistent expiration across multiple token exchanges', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCode1 = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const token1 = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCode1.code,
			redirect_uri: redirectURI,
		});
		const authCode2 = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify email',
		});
		const token2 = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCode2.code,
			redirect_uri: redirectURI,
		});
		expect(token1.expires_in).toBe(token2.expires_in);
	});
	test('should return expiration metadata in introspection response', async () => {
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
		const introspection = await introspectOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			token: tokens.access_token,
		});
		expect(introspection.active).toBe(true);
		expect(introspection.exp).toBeGreaterThan(0);
		expect(introspection.iat).toBeGreaterThan(0);
		expect(introspection.exp).toBeGreaterThan(introspection.iat!);
	});
	test('should maintain consistent expiration with multiple scopes', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify email guilds',
		});
		const tokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		expect(tokens.expires_in).toBe(604800);
		const introspection = await introspectOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			token: tokens.access_token,
		});
		expect(introspection.active).toBe(true);
		expect(introspection.scope).toContain('identify');
		expect(introspection.scope).toContain('email');
		expect(introspection.scope).toContain('guilds');
	});
	test('should maintain expiration after token refresh', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify email',
		});
		const initialTokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const refreshedTokens = await refreshOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			refresh_token: initialTokens.refresh_token!,
		});
		expect(refreshedTokens.expires_in).toBe(initialTokens.expires_in);
	});
	test('should allow userinfo access with valid token', async () => {
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
		const userInfo = await getOAuth2UserInfo(harness, tokens.access_token);
		expect(userInfo.sub).toBe(endUser.userId);
	});
	test('should reject userinfo access with invalid token', async () => {
		await createBuilder(harness, 'Bearer invalid_token_12345')
			.get('/oauth2/userinfo')
			.expect(HTTP_STATUS.UNAUTHORIZED)
			.execute();
	});
	test('should reject userinfo access with an expired access token', async () => {
		const {endUser, application} = await createOAuth2TestSetup(harness);
		const repository = new OAuth2TokenRepository();
		const token = generateOAuthTokenSecret();
		const expiredCreatedAt = new Date(Date.now() - (ACCESS_TOKEN_TTL_SECONDS + 60) * 1000);
		await repository.createAccessToken({
			token_: token,
			application_id: createApplicationID(BigInt(application.id)),
			user_id: createUserID(BigInt(endUser.userId)),
			scope: new Set(['identify']),
			created_at: expiredCreatedAt,
		});
		expect(await repository.getAccessToken(token)).toBeNull();
		await createBuilder(harness, `Bearer ${token}`).get('/oauth2/userinfo').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
	test('should track token active state in introspection', async () => {
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
		const introspection = await introspectOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			token: tokens.access_token,
		});
		expect(introspection.active).toBe(true);
		expect(introspection.token_type).toBe('Bearer');
		expect(introspection.client_id).toBe(application.id);
	});
	test('should provide expires timestamp through oauth2/@me endpoint', async () => {
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
		const json = await createBuilder<{
			application: {
				id: string;
			};
			scopes: Array<string>;
			expires: string;
		}>(harness, `Bearer ${tokens.access_token}`)
			.get('/oauth2/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.application.id).toBe(application.id);
		expect(json.scopes).toContain('identify');
		expect(json.expires).toBeTruthy();
		const expiresDate = new Date(json.expires);
		expect(expiresDate.getTime()).toBeGreaterThan(Date.now());
	});
});

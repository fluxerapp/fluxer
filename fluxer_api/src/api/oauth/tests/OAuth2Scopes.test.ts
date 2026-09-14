// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {
	authorizeOAuth2,
	createOAuth2Application,
	createOAuth2TestSetup,
	exchangeOAuth2AuthorizationCode,
} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface OAuth2MeResponse {
	application: {
		id: string;
		name: string;
	};
	scopes: Array<string>;
	expires: string;
	user?: {
		id: string;
		username: string;
		email?: string;
	};
}

interface UserGuildsResponse {
	id: string;
	name: string;
}

describe('OAuth2 Scopes', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	test('token with "identify" scope can access /oauth2/@me user info', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const json = await createBuilder<OAuth2MeResponse>(harness, `Bearer ${tokenResponse.access_token}`)
			.get('/oauth2/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.application).toBeDefined();
		expect(json.application.id).toBe(application.id);
		expect(json.scopes).toContain('identify');
		expect(json.user).toBeDefined();
		expect(json.user?.id).toBe(endUser.userId);
		expect(json.user?.username).toBeDefined();
	});
	test('token with "email" scope returns email in user info', async () => {
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
		const json = await createBuilder<OAuth2MeResponse>(harness, `Bearer ${tokenResponse.access_token}`)
			.get('/oauth2/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.user).toBeDefined();
		expect(json.user?.email).toBe(endUser.email);
		expect(json.scopes).toContain('email');
	});
	test('token without "email" scope omits email from user info', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const json = await createBuilder<OAuth2MeResponse>(harness, `Bearer ${tokenResponse.access_token}`)
			.get('/oauth2/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.user).toBeDefined();
		expect(json.user?.email).toBeNull();
		expect(json.scopes).not.toContain('email');
	});
	test('token with "guilds" scope can access /users/@me/guilds', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		await createGuild(harness, endUser.token, 'Test Guild');
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify guilds',
		});
		const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const json = await createBuilder<Array<UserGuildsResponse>>(harness, `Bearer ${tokenResponse.access_token}`)
			.get('/users/@me/guilds')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(Array.isArray(json)).toBe(true);
		expect(json.length).toBeGreaterThan(0);
		expect(json[0]?.name).toBe('Test Guild');
	});
	test('token without "guilds" scope cannot access /users/@me/guilds', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		await createGuild(harness, endUser.token, 'Test Guild');
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		await createBuilder(harness, `Bearer ${tokenResponse.access_token}`)
			.get('/users/@me/guilds')
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	test('invalid scope in authorization request returns error', async () => {
		const {appOwner, endUser} = await createOAuth2TestSetup(harness);
		const application = await createOAuth2Application(harness, appOwner, {
			name: 'Invalid Scope Test',
			redirect_uris: ['https://example.com/callback'],
		});
		await createBuilder(harness, endUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				response_type: 'code',
				client_id: application.id,
				redirect_uri: 'https://example.com/callback',
				scope: 'invalid_scope_xyz',
				state: 'test-state',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('token without "identify" scope cannot access /oauth2/@me user info', async () => {
		const appOwner = await createTestAccount(harness);
		const endUser = await createTestAccount(harness);
		const application = await createOAuth2Application(harness, appOwner, {
			name: 'No Identify Scope Test',
			redirect_uris: [],
		});
		const json = await createBuilder<{
			redirect_to: string;
		}>(harness, endUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				response_type: 'code',
				client_id: application.id,
				scope: 'bot',
				state: 'test-state',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.redirect_to).toBeTruthy();
		const redirectUrl = new URL(json.redirect_to);
		const code = redirectUrl.searchParams.get('code');
		if (code) {
			const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
				client_id: application.id,
				client_secret: application.client_secret,
				code,
				redirect_uri: json.redirect_to.split('?')[0] ?? '',
			});
			const meJson = await createBuilder<OAuth2MeResponse>(harness, `Bearer ${tokenResponse.access_token}`)
				.get('/oauth2/@me')
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(meJson.user).toBeUndefined();
		}
	});
	test('scopes are correctly returned in token response', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify email guilds',
		});
		const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		expect(tokenResponse.scope).toContain('identify');
		expect(tokenResponse.scope).toContain('email');
		expect(tokenResponse.scope).toContain('guilds');
	});
	test('scope is preserved through token refresh', async () => {
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
		expect(tokenResponse.refresh_token).toBeTruthy();
		const formData = new URLSearchParams({
			grant_type: 'refresh_token',
			refresh_token: tokenResponse.refresh_token!,
			client_id: application.id,
		});
		const refreshJson = await createBuilder<{
			scope: string;
			access_token: string;
		}>(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.header(
				'Authorization',
				`Basic ${Buffer.from(`${application.id}:${application.client_secret}`).toString('base64')}`,
			)
			.body(formData.toString())
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(refreshJson.scope).toContain('identify');
		expect(refreshJson.scope).toContain('email');
		const meJson = await createBuilder<OAuth2MeResponse>(harness, `Bearer ${refreshJson.access_token}`)
			.get('/oauth2/@me')
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(meJson.user?.email).toBe(endUser.email);
	});
});

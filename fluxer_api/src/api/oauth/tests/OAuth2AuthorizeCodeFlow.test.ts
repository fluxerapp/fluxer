// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	authorizeOAuth2,
	createOAuth2Application,
	exchangeOAuth2AuthorizationCode,
	getOAuth2UserInfo,
	introspectOAuth2Token,
} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('OAuth2 authorization code flow', () => {
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
	it('verifies the basic authorization code flow works end-to-end', async () => {
		const appOwner = await createTestAccount(harness);
		const endUser = await createTestAccount(harness);
		const redirectURI = 'https://example.com/oauth/callback';
		const app = await createOAuth2Application(harness, appOwner, {
			name: 'OAuth2 Basic Flow',
			redirect_uris: [redirectURI],
		});
		const {code: authCode, state: returnedState} = await authorizeOAuth2(harness, endUser.token, {
			client_id: app.id,
			redirect_uri: redirectURI,
			scope: 'identify email',
		});
		expect(authCode).toBeTruthy();
		expect(returnedState).toBeTruthy();
		const tokenResp = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: app.id,
			client_secret: app.client_secret,
			code: authCode,
			redirect_uri: redirectURI,
		});
		expect(tokenResp.access_token).toBeTruthy();
		expect(tokenResp.refresh_token).toBeTruthy();
		expect(tokenResp.token_type).toBe('Bearer');
		expect(tokenResp.expires_in).toBeGreaterThan(0);
		expect(tokenResp.scope).toBe('identify email');
		const userInfo = await getOAuth2UserInfo(harness, tokenResp.access_token);
		expect(userInfo.sub).toBe(endUser.userId);
		const introspection = await introspectOAuth2Token(harness, {
			client_id: app.id,
			client_secret: app.client_secret,
			token: tokenResp.access_token,
		});
		expect(introspection.active).toBe(true);
		expect(introspection.client_id).toBe(app.id);
		expect(introspection.scope).toBe('identify email');
	});
});

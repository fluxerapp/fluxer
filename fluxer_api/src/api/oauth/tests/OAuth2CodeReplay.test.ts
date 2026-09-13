// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	authorizeOAuth2,
	createOAuth2TestSetup,
	exchangeOAuth2AuthorizationCode,
} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Authorization Code Replay Prevention', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('should prevent authorization code reuse', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const firstTokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		expect(firstTokenResponse.access_token).toBeTruthy();
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
			client_id: application.id,
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.header(
				'Authorization',
				`Basic ${Buffer.from(`${application.id}:${application.client_secret}`).toString('base64')}`,
			)
			.body(formData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should issue different access tokens for same authorization', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse1 = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const tokenResponse1 = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse1.code,
			redirect_uri: redirectURI,
		});
		const authCodeResponse2 = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const tokenResponse2 = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse2.code,
			redirect_uri: redirectURI,
		});
		expect(tokenResponse1.access_token).not.toBe(tokenResponse2.access_token);
		expect(tokenResponse1.refresh_token).not.toBe(tokenResponse2.refresh_token);
	});
	test('should consume authorization code after first use', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const code = authCodeResponse.code;
		await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code,
			redirect_uri: redirectURI,
		});
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code,
			redirect_uri: redirectURI,
			client_id: application.id,
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.header(
				'Authorization',
				`Basic ${Buffer.from(`${application.id}:${application.client_secret}`).toString('base64')}`,
			)
			.body(formData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});

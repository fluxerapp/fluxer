// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	authorizeOAuth2,
	createOAuth2TestSetup,
	exchangeOAuth2AuthorizationCode,
	getOAuth2UserInfo,
	refreshOAuth2Token,
} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Token Refresh', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('should refresh access token using refresh token', async () => {
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
		expect(initialTokens.refresh_token).toBeTruthy();
		const refreshedTokens = await refreshOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			refresh_token: initialTokens.refresh_token!,
		});
		expect(refreshedTokens.access_token).toBeTruthy();
		expect(refreshedTokens.access_token).toHaveLength(43);
		expect(refreshedTokens.token_type).toBe('Bearer');
		expect(refreshedTokens.expires_in).toBeGreaterThan(0);
		expect(refreshedTokens.refresh_token).toBeTruthy();
		expect(refreshedTokens.scope).toContain('identify');
		expect(refreshedTokens.scope).toContain('email');
		const userInfo = await getOAuth2UserInfo(harness, refreshedTokens.access_token);
		expect(userInfo.sub).toBe(endUser.userId);
	});
	test('should return new access_token and refresh_token on valid refresh', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
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
		expect(refreshedTokens.access_token).toBeTruthy();
		expect(refreshedTokens.access_token).not.toBe(initialTokens.access_token);
		expect(refreshedTokens.refresh_token).toBeTruthy();
		expect(refreshedTokens.refresh_token).not.toBe(initialTokens.refresh_token);
		expect(refreshedTokens.token_type).toBe('Bearer');
		expect(refreshedTokens.expires_in).toBeGreaterThan(0);
	});
	test('should allow multiple sequential refreshes', async () => {
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
		const firstRefresh = await refreshOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			refresh_token: initialTokens.refresh_token!,
		});
		expect(firstRefresh.access_token).toBeTruthy();
		const secondRefresh = await refreshOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			refresh_token: firstRefresh.refresh_token!,
		});
		expect(secondRefresh.access_token).toBeTruthy();
	});
	test('should preserve scopes during refresh', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify email guilds',
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
		expect(refreshedTokens.scope).toContain('identify');
		expect(refreshedTokens.scope).toContain('email');
		expect(refreshedTokens.scope).toContain('guilds');
	});
	test('should fail with invalid refresh token', async () => {
		const {application} = await createOAuth2TestSetup(harness);
		const formData = new URLSearchParams({
			grant_type: 'refresh_token',
			refresh_token: 'invalid_refresh_token_12345',
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
	test('should fail with revoked refresh token', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const initialTokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const revokeFormData = new URLSearchParams({
			token: initialTokens.refresh_token!,
			token_type_hint: 'refresh_token',
		});
		await createBuilder(harness, '')
			.post('/oauth2/token/revoke')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.header(
				'Authorization',
				`Basic ${Buffer.from(`${application.id}:${application.client_secret}`).toString('base64')}`,
			)
			.body(revokeFormData.toString())
			.expect(HTTP_STATUS.OK)
			.execute();
		const refreshFormData = new URLSearchParams({
			grant_type: 'refresh_token',
			refresh_token: initialTokens.refresh_token!,
			client_id: application.id,
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.header(
				'Authorization',
				`Basic ${Buffer.from(`${application.id}:${application.client_secret}`).toString('base64')}`,
			)
			.body(refreshFormData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should fail with wrong client_id', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const initialTokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const formData = new URLSearchParams({
			grant_type: 'refresh_token',
			refresh_token: initialTokens.refresh_token!,
			client_id: '999999999999999999',
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.header(
				'Authorization',
				`Basic ${Buffer.from(`999999999999999999:${application.client_secret}`).toString('base64')}`,
			)
			.body(formData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should fail with wrong client_secret', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const initialTokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const formData = new URLSearchParams({
			grant_type: 'refresh_token',
			refresh_token: initialTokens.refresh_token!,
			client_id: application.id,
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.header('Authorization', `Basic ${Buffer.from(`${application.id}:wrong_client_secret_12345`).toString('base64')}`)
			.body(formData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should fail with missing client_secret', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const initialTokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const formData = new URLSearchParams({
			grant_type: 'refresh_token',
			refresh_token: initialTokens.refresh_token!,
			client_id: application.id,
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.body(formData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should fail when using refresh token from different application', async () => {
		const setup1 = await createOAuth2TestSetup(harness);
		const setup2 = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, setup1.endUser.token, {
			client_id: setup1.application.id,
			redirect_uri: setup1.redirectURI,
			scope: 'identify',
		});
		const tokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: setup1.application.id,
			client_secret: setup1.application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: setup1.redirectURI,
		});
		const formData = new URLSearchParams({
			grant_type: 'refresh_token',
			refresh_token: tokens.refresh_token!,
			client_id: setup2.application.id,
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.header(
				'Authorization',
				`Basic ${Buffer.from(`${setup2.application.id}:${setup2.application.client_secret}`).toString('base64')}`,
			)
			.body(formData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should issue tokens that rotate on each refresh', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const initialTokens = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		const firstRefresh = await refreshOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			refresh_token: initialTokens.refresh_token!,
		});
		expect(firstRefresh.refresh_token).not.toBe(initialTokens.refresh_token);
		const secondRefresh = await refreshOAuth2Token(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			refresh_token: firstRefresh.refresh_token!,
		});
		expect(secondRefresh.refresh_token).not.toBe(firstRefresh.refresh_token);
		expect(secondRefresh.refresh_token).not.toBe(initialTokens.refresh_token);
	});
});

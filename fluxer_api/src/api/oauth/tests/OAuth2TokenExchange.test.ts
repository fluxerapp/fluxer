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

describe('OAuth2 Token Exchange', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('should exchange authorization code for tokens', async () => {
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
		expect(tokenResponse.access_token).toBeTruthy();
		expect(tokenResponse.access_token).toHaveLength(43);
		expect(tokenResponse.token_type).toBe('Bearer');
		expect(tokenResponse.expires_in).toBeGreaterThan(0);
		expect(tokenResponse.refresh_token).toBeTruthy();
		expect(tokenResponse.scope).toBe('identify email');
	});
	test('should fail without authorization code', async () => {
		const {application, redirectURI} = await createOAuth2TestSetup(harness);
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: '',
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
	test('should fail with invalid authorization code', async () => {
		const {application, redirectURI} = await createOAuth2TestSetup(harness);
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: 'invalid_code_12345',
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
	test('should fail without redirect_uri in token exchange', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: authCodeResponse.code,
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
	test('should fail with mismatched redirect_uri in token exchange', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: authCodeResponse.code,
			redirect_uri: 'https://evil.com/callback',
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
	test('should fail without client authentication', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
			client_id: application.id,
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.body(formData.toString())
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
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
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
	test('should support scope subset during exchange', async () => {
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
		expect(tokenResponse.access_token).toBeTruthy();
	});
	test('should fail without authentication', async () => {
		const {redirectURI, application} = await createOAuth2TestSetup(harness);
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: 'some_code',
			redirect_uri: redirectURI,
			client_id: application.id,
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.body(formData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should support basic auth client authentication', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
			client_id: application.id,
		});
		const json = await createBuilder<{
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
		expect(json.access_token).toBeTruthy();
		expect(json.access_token).toHaveLength(43);
	});
	test('should fail authorization code reuse', async () => {
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
		expect(tokenResponse.access_token).toBeTruthy();
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
	test('should fail with wrong client_secret', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
			client_id: application.id,
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.header('Authorization', `Basic ${Buffer.from(`${application.id}:wrong_secret_value`).toString('base64')}`)
			.body(formData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should fail with missing grant_type', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const formData = new URLSearchParams({
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
	test('should fail with missing client_id', async () => {
		const {endUser, redirectURI, application} = await createOAuth2TestSetup(harness);
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: redirectURI,
			scope: 'identify',
		});
		const formData = new URLSearchParams({
			grant_type: 'authorization_code',
			code: authCodeResponse.code,
			redirect_uri: redirectURI,
		});
		await createBuilder(harness, '')
			.post('/oauth2/token')
			.header('Content-Type', 'application/x-www-form-urlencoded')
			.body(formData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
});

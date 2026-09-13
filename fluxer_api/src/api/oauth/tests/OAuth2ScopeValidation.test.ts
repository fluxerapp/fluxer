// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {
	authorizeOAuth2,
	createOAuth2Application,
	createOAuth2TestSetup,
	exchangeOAuth2AuthorizationCode,
} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ADMIN_OAUTH2_APPLICATION_ID} from '@fluxer/constants/src/Core';
import {beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Scope Validation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('should accept valid platform scopes', async () => {
		const {appOwner, endUser} = await createOAuth2TestSetup(harness);
		const validScopes = ['identify', 'email', 'guilds', 'connections'];
		for (const scope of validScopes) {
			const application = await createOAuth2Application(harness, appOwner, {
				name: `Scope Test ${scope}`,
				redirect_uris: ['https://example.com/callback'],
			});
			const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
				client_id: application.id,
				redirect_uri: 'https://example.com/callback',
				scope,
			});
			expect(authCodeResponse.code).toBeTruthy();
			const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
				client_id: application.id,
				client_secret: application.client_secret,
				code: authCodeResponse.code,
				redirect_uri: 'https://example.com/callback',
			});
			expect(tokenResponse.scope).toContain(scope);
		}
	}, 10000);
	test('should accept multiple valid scopes', async () => {
		const {appOwner, endUser} = await createOAuth2TestSetup(harness);
		const application = await createOAuth2Application(harness, appOwner, {
			name: 'Multi Scope Test',
			redirect_uris: ['https://example.com/callback'],
		});
		const authCodeResponse = await authorizeOAuth2(harness, endUser.token, {
			client_id: application.id,
			redirect_uri: 'https://example.com/callback',
			scope: 'identify email guilds',
		});
		expect(authCodeResponse.code).toBeTruthy();
		const tokenResponse = await exchangeOAuth2AuthorizationCode(harness, {
			client_id: application.id,
			client_secret: application.client_secret,
			code: authCodeResponse.code,
			redirect_uri: 'https://example.com/callback',
		});
		expect(tokenResponse.scope).toContain('identify');
		expect(tokenResponse.scope).toContain('email');
		expect(tokenResponse.scope).toContain('guilds');
	}, 10000);
	test('should reject unknown scope', async () => {
		const {appOwner, endUser} = await createOAuth2TestSetup(harness);
		const application = await createOAuth2Application(harness, appOwner, {
			name: 'Unknown Scope Test',
			redirect_uris: ['https://example.com/callback'],
		});
		await createBuilder(harness, endUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				response_type: 'code',
				client_id: application.id,
				redirect_uri: 'https://example.com/callback',
				scope: 'unknown_scope_invalid',
				state: 'test-state',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should reject mix of valid and invalid scopes', async () => {
		const {appOwner, endUser} = await createOAuth2TestSetup(harness);
		const application = await createOAuth2Application(harness, appOwner, {
			name: 'Mixed Scope Test',
			redirect_uris: ['https://example.com/callback'],
		});
		await createBuilder(harness, endUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				response_type: 'code',
				client_id: application.id,
				redirect_uri: 'https://example.com/callback',
				scope: 'identify invalid_scope_xyz',
				state: 'test-state',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should allow bot scope on applications with bots', async () => {
		const {appOwner, endUser} = await createOAuth2TestSetup(harness);
		const application = await createOAuth2Application(harness, appOwner, {
			name: 'Bot Scope Test',
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
	});
	test('should reject bot scope for non-bot applications', async () => {
		const appOwner = await createTestAccount(harness);
		const endUser = await createTestAccount(harness);
		const application = await createOAuth2Application(harness, appOwner, {
			name: 'Bot Scope Test App',
			redirect_uris: ['https://example.com/callback'],
		});
		await createBuilder(harness, endUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				response_type: 'code',
				client_id: application.id,
				redirect_uri: 'https://example.com/callback',
				scope: 'bot',
				state: 'test-state',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
	});
	test('should reject the removed admin scope for third-party applications', async () => {
		const {appOwner, endUser} = await createOAuth2TestSetup(harness);
		const application = await createOAuth2Application(harness, appOwner, {
			name: 'Removed Admin Scope Test',
			redirect_uris: ['https://example.com/callback'],
		});
		const json = await createBuilder<{
			error: string;
		}>(harness, endUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				response_type: 'code',
				client_id: application.id,
				redirect_uri: 'https://example.com/callback',
				scope: 'identify admin',
				state: 'test-state',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(json.error).toBe('invalid_scope');
	});
	test('should reject the removed admin scope for the built-in admin OAuth2 application', async () => {
		const endUser = await createTestAccount(harness);
		const adminRedirectUri = `${Config.endpoints.admin}/oauth2_callback`;
		const json = await createBuilder<{
			error: string;
		}>(harness, endUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				response_type: 'code',
				client_id: ADMIN_OAUTH2_APPLICATION_ID.toString(),
				redirect_uri: adminRedirectUri,
				scope: 'identify email admin',
				state: 'test-state',
			})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(json.error).toBe('invalid_scope');
	});
	test('should allow the built-in admin OAuth2 application without an admin scope', async () => {
		const endUser = await createTestAccount(harness);
		const adminRedirectUri = `${Config.endpoints.admin}/oauth2_callback`;
		const json = await createBuilder<{
			redirect_to: string;
		}>(harness, endUser.token)
			.post('/oauth2/authorize/consent')
			.body({
				response_type: 'code',
				client_id: ADMIN_OAUTH2_APPLICATION_ID.toString(),
				redirect_uri: adminRedirectUri,
				scope: 'identify email',
				state: 'test-state',
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.redirect_to).toBeTruthy();
		expect(json.redirect_to).toContain('code=');
	});
});

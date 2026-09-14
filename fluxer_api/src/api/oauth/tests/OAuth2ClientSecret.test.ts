// SPDX-License-Identifier: AGPL-3.0-or-later

import {authorizeOAuth2, createOAuth2Application, createOAuth2TestSetup} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {beforeEach, describe, expect, test} from 'vitest';

describe('OAuth2 Client Secret', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	test('should require client secret for confidential applications', async () => {
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
	test('should validate client secret', async () => {
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
			.header('Authorization', `Basic ${Buffer.from(`${application.id}:wrong_secret`).toString('base64')}`)
			.body(formData.toString())
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
	});
	test('should accept correct client secret via basic auth', async () => {
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
	test('should not leak client secret in application response', async () => {
		const {appOwner} = await createOAuth2TestSetup(harness);
		const application = await createOAuth2Application(harness, appOwner, {
			name: 'Secret Test App',
			redirect_uris: ['https://example.com/callback'],
		});
		const json = await createBuilderWithoutAuth<Record<string, unknown>>(harness)
			.get(`/oauth2/applications/${application.id}/public`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.client_secret).toBeUndefined();
		expect(json.id).toBe(application.id);
	});
});

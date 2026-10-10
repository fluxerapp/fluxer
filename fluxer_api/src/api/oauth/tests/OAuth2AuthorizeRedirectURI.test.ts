// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createOAuth2Application} from '@app/api/oauth/tests/OAuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('OAuth2 authorize redirect URI validation', () => {
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
	it('verifies that redirect URIs must match exactly (no partial matches)', async () => {
		const appOwner = await createTestAccount(harness);
		const endUser = await createTestAccount(harness);
		const registeredURI = 'https://example.com/callback';
		const app = await createOAuth2Application(harness, appOwner, {
			name: 'Exact Match',
			redirect_uris: [registeredURI],
		});
		const testCases = [
			'https://example.com/callback/extra',
			'https://example.com/callback?foo=bar',
			'https://example.com/callback#fragment',
			'http://example.com/callback',
			'https://other.com/callback',
			'https://example.com:8080/callback',
			'https://example.com/callback/',
		];
		for (const invalidURI of testCases) {
			await createBuilder(harness, endUser.token)
				.post('/oauth2/authorize/consent')
				.body({
					response_type: 'code',
					client_id: app.id,
					redirect_uri: invalidURI,
					scope: 'identify',
					state: 'test-state',
				})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		}
	});
});

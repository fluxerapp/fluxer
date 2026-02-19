/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {createUserID} from '@fluxer/api/src/BrandedTypes';
import {
	createBlueskyConnectionViaOAuth,
	createBlueskyDid,
	createBlueskyHandle,
	listConnections,
} from '@fluxer/api/src/connection/tests/ConnectionTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@fluxer/api/src/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConnectionTypes} from '@fluxer/constants/src/ConnectionConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Bluesky OAuth', () => {
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

	describe('GET /connections/bluesky/client-metadata.json', () => {
		it('returns client metadata', async () => {
			const response = await harness.requestJson({
				path: '/connections/bluesky/client-metadata.json',
				method: 'GET',
			});

			expect(response.status).toBe(200);

			const body = (await response.json()) as {client_id: string};
			expect(body).toHaveProperty('client_id');
			expect(body.client_id).toBe('https://test/metadata.json');
		});
	});

	describe('GET /connections/bluesky/jwks.json', () => {
		it('returns JWKS', async () => {
			const response = await harness.requestJson({
				path: '/connections/bluesky/jwks.json',
				method: 'GET',
			});

			expect(response.status).toBe(200);

			const body = (await response.json()) as {keys: Array<unknown>};
			expect(body).toHaveProperty('keys');
			expect(body.keys).toEqual([]);
		});
	});

	describe('POST /users/@me/connections/bluesky/authorize', () => {
		it('returns authorize_url for valid handle', async () => {
			const account = await createTestAccount(harness);

			const result = await createBuilder<{authorize_url: string}>(harness, account.token)
				.post('/users/@me/connections/bluesky/authorize')
				.body({handle: 'alice.bsky.social'})
				.expect(200)
				.execute();

			expect(result.authorize_url).toBeTruthy();
			expect(result.authorize_url).toContain('https://');
		});

		it('requires authentication', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/users/@me/connections/bluesky/authorize')
				.body({handle: 'alice.bsky.social'})
				.expect(HTTP_STATUS.UNAUTHORIZED)
				.execute();
		});

		it('returns error for empty handle', async () => {
			const account = await createTestAccount(harness);

			await createBuilder(harness, account.token)
				.post('/users/@me/connections/bluesky/authorize')
				.body({handle: ''})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});

		it('calls authorize with correct handle and userId', async () => {
			const account = await createTestAccount(harness);
			const handle = 'alice.bsky.social';

			await createBuilder(harness, account.token)
				.post('/users/@me/connections/bluesky/authorize')
				.body({handle})
				.expect(200)
				.execute();

			expect(harness.mockBlueskyOAuthService.authorizeSpy).toHaveBeenCalledTimes(1);
			const callArgs = harness.mockBlueskyOAuthService.authorizeSpy.mock.calls[0];
			expect(callArgs[0]).toBe(handle);
		});

		it('normalises a bsky.app profile URL to a handle', async () => {
			const account = await createTestAccount(harness);

			await createBuilder(harness, account.token)
				.post('/users/@me/connections/bluesky/authorize')
				.body({handle: 'https://bsky.app/profile/alice.bsky.social'})
				.expect(200)
				.execute();

			expect(harness.mockBlueskyOAuthService.authorizeSpy).toHaveBeenCalledTimes(1);
			expect(harness.mockBlueskyOAuthService.authorizeSpy.mock.calls[0][0]).toBe('alice.bsky.social');
		});

		it('normalises an http bsky.app profile URL to a handle', async () => {
			const account = await createTestAccount(harness);

			await createBuilder(harness, account.token)
				.post('/users/@me/connections/bluesky/authorize')
				.body({handle: 'http://bsky.app/profile/someone.bsky.social'})
				.expect(200)
				.execute();

			expect(harness.mockBlueskyOAuthService.authorizeSpy.mock.calls[0][0]).toBe('someone.bsky.social');
		});

		it('strips leading @ from handle', async () => {
			const account = await createTestAccount(harness);

			await createBuilder(harness, account.token)
				.post('/users/@me/connections/bluesky/authorize')
				.body({handle: '@alice.bsky.social'})
				.expect(200)
				.execute();

			expect(harness.mockBlueskyOAuthService.authorizeSpy.mock.calls[0][0]).toBe('alice.bsky.social');
		});

		it('detects duplicate after normalising profile URL', async () => {
			const account = await createTestAccount(harness);
			const handle = createBlueskyHandle('testuser');
			const did = createBlueskyDid('testuser');
			const userId = createUserID(BigInt(account.userId));

			await createBlueskyConnectionViaOAuth(harness, account.token, handle, did, userId);

			await createBuilder(harness, account.token)
				.post('/users/@me/connections/bluesky/authorize')
				.body({handle: `https://bsky.app/profile/${handle}`})
				.expect(HTTP_STATUS.CONFLICT, APIErrorCodes.CONNECTION_ALREADY_EXISTS)
				.execute();
		});

		it('returns BLUESKY_OAUTH_AUTHORIZATION_FAILED when authorize throws', async () => {
			const account = await createTestAccount(harness);
			harness.mockBlueskyOAuthService.configure({shouldFailAuthorize: true});

			await createBuilder(harness, account.token)
				.post('/users/@me/connections/bluesky/authorize')
				.body({handle: 'invalid-handle'})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.BLUESKY_OAUTH_AUTHORIZATION_FAILED)
				.execute();
		});
	});

	describe('GET /connections/bluesky/callback', () => {
		it('redirects to app on successful callback', async () => {
			const account = await createTestAccount(harness);
			const handle = createBlueskyHandle('testuser');
			const did = createBlueskyDid('testuser');
			const userId = createUserID(BigInt(account.userId));

			harness.mockBlueskyOAuthService.configure({
				callbackResult: {userId, did, handle},
			});

			const response = await harness.requestJson({
				path: '/connections/bluesky/callback?code=mock_code&state=mock_state&iss=mock_iss',
				method: 'GET',
				headers: {Authorization: account.token},
			});

			expect(response.status).toBe(302);
			const location = response.headers.get('location') ?? '';
			expect(location).toContain('status=connected');
		});

		it('redirects to app with error on failed callback', async () => {
			harness.mockBlueskyOAuthService.configure({
				shouldFailCallback: true,
			});

			const response = await harness.requestJson({
				path: '/connections/bluesky/callback?code=bad&state=bad',
				method: 'GET',
			});

			expect(response.status).toBe(302);
			const location = response.headers.get('location') ?? '';
			expect(location).toContain('status=error');
		});

		it('creates connection on successful callback', async () => {
			const account = await createTestAccount(harness);
			const handle = createBlueskyHandle('testuser');
			const did = createBlueskyDid('testuser');
			const userId = createUserID(BigInt(account.userId));

			harness.mockBlueskyOAuthService.configure({
				callbackResult: {userId, did, handle},
			});

			await harness.requestJson({
				path: '/connections/bluesky/callback?code=mock_code&state=mock_state&iss=mock_iss',
				method: 'GET',
				headers: {Authorization: account.token},
			});

			const connections = await listConnections(harness, account.token);
			expect(connections).toHaveLength(1);
			expect(connections[0].type).toBe(ConnectionTypes.BLUESKY);
			expect(connections[0].name).toBe(handle);
			expect(connections[0].verified).toBe(true);
		});

		it('updates existing connection on re-authorisation', async () => {
			const account = await createTestAccount(harness);
			const handle = createBlueskyHandle('testuser');
			const newHandle = 'newhandle.bsky.social';
			const did = createBlueskyDid('testuser');
			const userId = createUserID(BigInt(account.userId));

			harness.mockBlueskyOAuthService.configure({
				callbackResult: {userId, did, handle},
			});

			await harness.requestJson({
				path: '/connections/bluesky/callback?code=mock_code&state=mock_state&iss=mock_iss',
				method: 'GET',
				headers: {Authorization: account.token},
			});

			const connectionsBefore = await listConnections(harness, account.token);
			expect(connectionsBefore).toHaveLength(1);
			expect(connectionsBefore[0].name).toBe(handle);

			harness.mockBlueskyOAuthService.configure({
				callbackResult: {userId, did, handle: newHandle},
			});

			await harness.requestJson({
				path: '/connections/bluesky/callback?code=new_code&state=new_state&iss=new_iss',
				method: 'GET',
				headers: {Authorization: account.token},
			});

			const connectionsAfter = await listConnections(harness, account.token);
			expect(connectionsAfter).toHaveLength(1);
			expect(connectionsAfter[0].name).toBe(newHandle);
			expect(connectionsAfter[0].verified).toBe(true);
		});
	});

	describe('Old initiate endpoint rejects Bluesky', () => {
		it('returns BLUESKY_OAUTH_NOT_ENABLED for POST /users/@me/connections with bsky type', async () => {
			const account = await createTestAccount(harness);

			await createBuilder(harness, account.token)
				.post('/users/@me/connections')
				.body({
					type: ConnectionTypes.BLUESKY,
					identifier: 'test.bsky.social',
				})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.BLUESKY_OAUTH_NOT_ENABLED)
				.execute();
		});
	});
});

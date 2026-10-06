// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createFakeAuthToken,
	createTestAccount,
	type UserMeResponse,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth token validation', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('malformed token returns unauthorized', async () => {
		const malformedTokens = ['', 'not-a-token', 'Bearer invalid', 'invalid.token.format', 'ey123.ey456.sig789'];
		for (const token of malformedTokens) {
			await createBuilder(harness, token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
		}
	});
	it('non-existent token returns unauthorized', async () => {
		const fakeToken = createFakeAuthToken();
		await createBuilder(harness, fakeToken).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
	it('revoked token returns unauthorized', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.OK).execute();
		await createBuilder(harness, account.token).post('/auth/logout').expect(HTTP_STATUS.NO_CONTENT).execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
	it('valid token allows access', async () => {
		const account = await createTestAccount(harness);
		const user = await createBuilder<UserMeResponse>(harness, account.token).get('/users/@me').execute();
		expect(user.id).toBe(account.userId);
	});
	it('valid session token with Bearer prefix allows access', async () => {
		const account = await createTestAccount(harness);
		const user = await createBuilder<UserMeResponse>(harness, `Bearer ${account.token}`).get('/users/@me').execute();
		expect(user.id).toBe(account.userId);
	});
	it('token with wrong signature returns unauthorized', async () => {
		const account = await createTestAccount(harness);
		const tamperedToken = `${account.token.slice(0, Math.max(0, account.token.length - 10))}0123456789`;
		await createBuilder(harness, tamperedToken).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
	it('missing authorization header returns unauthorized', async () => {
		await createBuilderWithoutAuth(harness).get('/users/@me').expect(HTTP_STATUS.UNAUTHORIZED).execute();
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createFakeAuthToken,
	createTestAccount,
	type UserMeResponse,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth login and sessions', () => {
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
	it('rejects invalid login credentials', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: account.email, password: 'WrongPassword123!'})
			.expect(400)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: 'nonexistent@example.com', password: 'SomePassword123!'})
			.expect(400)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: 'not-an-email', password: 'SomePassword123!'})
			.expect(400)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: 'test@example.com', password: ''})
			.expect(400)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email: '', password: 'SomePassword123!'})
			.expect(400)
			.execute();
	});
	it('enforces token validity for /users/@me', async () => {
		const malformedTokens = ['', 'not-a-token', 'Bearer invalid', 'invalid.token.format', 'ey123.ey456.sig789'];
		for (const token of malformedTokens) {
			await createBuilder(harness, token).get('/users/@me').expect(401).execute();
		}
		const fakeToken = createFakeAuthToken();
		await createBuilder(harness, fakeToken).get('/users/@me').expect(401).execute();
		const account = await createTestAccount(harness);
		const meOkPayload = await createBuilder<
			UserMeResponse & {
				id?: string;
			}
		>(harness, account.token)
			.get('/users/@me')
			.execute();
		expect(
			(
				meOkPayload as {
					id: string;
				}
			).id,
		).toBe(account.userId);
		await createBuilder(harness, account.token).post('/auth/logout').expect(204).execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
		const fresh = await createTestAccount(harness);
		const tamperedToken = `${fresh.token.slice(0, Math.max(0, fresh.token.length - 10))}0123456789`;
		await createBuilder(harness, tamperedToken).get('/users/@me').expect(401).execute();
		await createBuilderWithoutAuth(harness).get('/users/@me').expect(401).execute();
	});
	it('logout requires a live session', async () => {
		await createBuilderWithoutAuth(harness).post('/auth/logout').expect(401).execute();
		await createBuilder(harness, createFakeAuthToken()).post('/auth/logout').expect(401).execute();
		const account = await createTestAccount(harness);
		await createBuilder(harness, account.token).post('/auth/logout').expect(204).execute();
		await createBuilder(harness, account.token).post('/auth/logout').expect(401).execute();
	});
	it('logout with a Bearer-prefixed session token revokes it', async () => {
		const account = await createTestAccount(harness);
		await createBuilder(harness, `Bearer ${account.token}`).post('/auth/logout').expect(204).execute();
		await createBuilder(harness, account.token).get('/users/@me').expect(401).execute();
	});
});

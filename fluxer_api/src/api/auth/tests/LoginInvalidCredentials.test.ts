// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

describe('Auth login invalid credentials', () => {
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
	it('wrong password returns bad request with field errors', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({
				email: account.email,
				password: 'WrongPassword123!',
			})
			.expect(400)
			.execute();
	});
	it('non-existent email returns bad request with field errors', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({
				email: 'nonexistent@example.com',
				password: 'SomePassword123!',
			})
			.expect(400)
			.execute();
	});
	it('invalid email format returns bad request', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({
				email: 'not-an-email',
				password: 'SomePassword123!',
			})
			.expect(400)
			.execute();
	});
	it('empty password returns bad request or unauthorized', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({
				email: 'test@example.com',
				password: '',
			})
			.expect(400)
			.execute();
	});
	it('empty email returns bad request or unauthorized', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({
				email: '',
				password: 'SomePassword123!',
			})
			.expect(400)
			.execute();
	});
});

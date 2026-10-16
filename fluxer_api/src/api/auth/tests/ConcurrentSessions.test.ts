// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, loginAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {AuthSessionResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth concurrent sessions', () => {
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
	it('logging out one session does not affect other sessions', async () => {
		const account = await createTestAccount(harness);
		const session1Token = account.token;
		const account2 = await loginAccount(harness, account);
		const session2Token = account2.token;
		const account3 = await loginAccount(harness, account);
		const session3Token = account3.token;
		const sessions = await createBuilder<Array<AuthSessionResponse>>(harness, session1Token)
			.get('/auth/sessions')
			.execute();
		expect(sessions.length).toBeGreaterThanOrEqual(3);
		await createBuilder(harness, session2Token).post('/auth/logout').expect(204).execute();
		await createBuilder(harness, session1Token).get('/users/@me').expect(200).execute();
		await createBuilder(harness, session3Token).get('/users/@me').expect(200).execute();
		await createBuilder(harness, session2Token).get('/users/@me').expect(401).execute();
	});
});

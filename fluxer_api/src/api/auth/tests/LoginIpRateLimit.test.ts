// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getRateLimitService} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const CLIENT_IP = '127.0.0.1';
const IP_LIMIT = {
	identifier: `login:ip:${getSameIpDecisionKey(CLIENT_IP) ?? CLIENT_IP}`,
	maxAttempts: 10,
	windowMs: 30 * 60 * 1000,
};

describe('per-IP sign-in limit', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		await getRateLimitService().resetLimit(IP_LIMIT.identifier);
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	const signIn = (email: string, password: string) =>
		createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.header('x-forwarded-for', CLIENT_IP)
			.body({email, password})
			.expect(200);

	it('does not spend the IP budget on correct passwords', async () => {
		for (let i = 0; i < 3; i++) {
			const account = await createTestAccount(harness);
			await signIn(account.email, account.password).execute();
		}
		expect((await getRateLimitService().peekLimit(IP_LIMIT)).remaining).toBe(IP_LIMIT.maxAttempts);
	});

	it('spends the IP budget on wrong passwords', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.header('x-forwarded-for', CLIENT_IP)
			.body({email: account.email, password: 'WrongPassword123!'})
			.expect(400)
			.execute();
		expect((await getRateLimitService().peekLimit(IP_LIMIT)).remaining).toBe(IP_LIMIT.maxAttempts - 1);
	});
});

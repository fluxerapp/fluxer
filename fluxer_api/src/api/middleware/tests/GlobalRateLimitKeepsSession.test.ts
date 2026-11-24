// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const BURST_SIZE = 120;

interface RateLimitErrorResponse {
	code?: string;
	global?: boolean;
}

describe('Global API rate limit', () => {
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

	it('returns 429 with the global flag and keeps the session token valid', async () => {
		const account = await createTestAccount(harness);

		const results = await Promise.all(
			Array.from({length: BURST_SIZE}, () =>
				createBuilder<RateLimitErrorResponse>(harness, account.token)
					.get('/users/@me')
					.header('x-fluxer-test-enable-rate-limits', 'true')
					.executeRaw(),
			),
		);

		const globalDenials = results.filter(({response, json}) => response.status === 429 && json.global === true);
		expect(globalDenials.length).toBeGreaterThan(0);

		const after = await createBuilder(harness, account.token).get('/users/@me').executeRaw();
		expect(after.response.status).toBe(HTTP_STATUS.OK);
	});
});

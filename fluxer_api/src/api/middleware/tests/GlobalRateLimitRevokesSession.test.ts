// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const HTTP_TOO_MANY_REQUESTS = 429;

interface RateLimitErrorResponse {
	code?: string;
	message?: string;
	global?: boolean;
	retry_after?: number;
}

interface UnauthorizedErrorResponse {
	code?: string;
	message?: string;
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
	it('revokes the auth session when an authenticated user hits the global rate limit', async () => {
		const account = await createTestAccount(harness);
		const firstResponse = await createBuilder(harness, account.token)
			.get('/users/@me')
			.header('x-fluxer-test-enable-rate-limits', 'true')
			.header('x-fluxer-test-global-rate-limit', '1')
			.expect(HTTP_STATUS.OK)
			.executeWithResponse();
		if (firstResponse.response.headers.get('X-RateLimit-Limit') === null) {
			return;
		}
		expect(firstResponse.response.headers.get('X-RateLimit-Limit')).toBe('40');
		expect(firstResponse.response.headers.get('X-RateLimit-Remaining')).toBe('39');
		expect(firstResponse.response.headers.get('X-RateLimit-Reset-After')).not.toBeNull();
		expect(firstResponse.response.headers.get('X-RateLimit-Bucket')).toBe(
			createHash('sha256').update(RateLimitConfigs.USER_SETTINGS_GET.bucket).digest('hex').slice(0, 16),
		);
		const rateLimitResult = await createBuilder<RateLimitErrorResponse>(harness, account.token)
			.get('/users/@me')
			.header('x-fluxer-test-enable-rate-limits', 'true')
			.header('x-fluxer-test-global-rate-limit', '1')
			.executeRaw();
		expect(rateLimitResult.response.status).toBe(HTTP_TOO_MANY_REQUESTS);
		expect(rateLimitResult.json.code).toBe(APIErrorCodes.RATE_LIMITED);
		expect(rateLimitResult.response.headers.get('X-RateLimit-Global')).toBe('true');
		expect(rateLimitResult.response.headers.get('X-RateLimit-Scope')).toBe('global');
		expect(rateLimitResult.response.headers.get('X-RateLimit-Bucket')).toBeNull();
		expect(rateLimitResult.json.global).toBe(true);
		expect(typeof rateLimitResult.json.retry_after).toBe('number');
		const unauth = await createBuilder<UnauthorizedErrorResponse>(harness, account.token)
			.get('/users/@me')
			.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.UNAUTHORIZED)
			.execute();
		expect(unauth.code).toBe(APIErrorCodes.UNAUTHORIZED);
	});
});

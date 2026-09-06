// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createAuthHarness} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {UserRepository} from '../../user/repositories/UserRepository';

const MISSING_USER_ID = createUserID(999999999999999998n);
const SESSION_TOKEN = 'flx_Zmlzc2lvbmVkc2Vzc2lvbnRva2VuMDAwMDAx';

interface UnauthorizedErrorResponse {
	code?: string;
	message?: string;
}

describe('Session token whose account row is gone', () => {
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
	it('answers 401 on an authenticated route instead of 500', async () => {
		const now = new Date();
		await new UserRepository().createAuthSession({
			user_id: MISSING_USER_ID,
			session_id_hash: Buffer.from(createHash('sha256').update(SESSION_TOKEN).digest()),
			created_at: now,
			approx_last_used_at: now,
			client_ip: '127.0.0.1',
			client_user_agent: null,
			client_os: null,
			client_country: null,
			version: 1,
		});
		const result = await createBuilder<UnauthorizedErrorResponse>(harness, SESSION_TOKEN)
			.get('/users/@me')
			.executeRaw();
		expect(result.response.status).toBe(HTTP_STATUS.UNAUTHORIZED);
		expect(result.json.code).toBe(APIErrorCodes.UNAUTHORIZED);
	});
});

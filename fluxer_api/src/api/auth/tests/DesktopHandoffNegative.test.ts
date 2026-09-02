// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {createAuthHarness, createTestAccount, loginAccount} from './AuthTestUtils';

interface HandoffInitiateResponse {
	code: string;
}

describe('Auth desktop handoff negative paths', () => {
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
	it('rejects unknown handoff code on status endpoint', async () => {
		await createBuilderWithoutAuth(harness)
			.get('/auth/handoff/unknown-code/status')
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_HANDOFF_CODE)
			.execute();
	});
	it('rejects unknown handoff code on info endpoint', async () => {
		await createBuilderWithoutAuth(harness)
			.get('/auth/handoff/unknown-code/info')
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_HANDOFF_CODE)
			.execute();
	});
	it('rejects handoff complete with bad token', async () => {
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const initResp = await createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.body(null)
			.execute();
		await createBuilderWithoutAuth(harness).get(`/auth/handoff/${initResp.code}/info`).execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.body({
				code: initResp.code,
				token: 'bad-token',
				user_id: login.userId,
			})
			.expect(HTTP_STATUS.UNAUTHORIZED, APIErrorCodes.INVALID_TOKEN)
			.execute();
	});
	it('rejects handoff complete before the code is approved through info lookup', async () => {
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const initResp = await createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.body(null)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.header('Authorization', login.token)
			.body({
				code: initResp.code,
				user_id: login.userId,
			})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_HANDOFF_CODE)
			.execute();
	});
	it('handles cancel for unknown handoff code gracefully', async () => {
		await createBuilderWithoutAuth(harness)
			.delete('/auth/handoff/unknown-code')
			.body({poll_secret: 'not-the-secret'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_HANDOFF_CODE)
			.execute();
	});
});

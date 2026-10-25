// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, loginAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface HandoffInitiateResponse {
	code: string;
	poll_secret: string;
}

interface HandoffInfoResponse {
	status: 'pending' | 'expired';
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
	it('locks an address out with a retry time after repeated failed lookups', async () => {
		const ip = '198.51.100.40';
		const initResp = await createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.body(null)
			.execute();
		await createBuilderWithoutAuth(harness)
			.delete(`/auth/handoff/${initResp.code}`)
			.body({poll_secret: initResp.poll_secret})
			.expect(204)
			.execute();
		for (let i = 0; i < 5; i++) {
			const info = await createBuilderWithoutAuth<HandoffInfoResponse>(harness)
				.get(`/auth/handoff/${initResp.code}/info`)
				.header('x-forwarded-for', ip)
				.execute();
			expect(info.status).toBe('expired');
		}
		await createBuilderWithoutAuth(harness)
			.get(`/auth/handoff/${initResp.code}/info`)
			.header('x-forwarded-for', ip)
			.expect(429, APIErrorCodes.HANDOFF_ATTEMPTS_EXCEEDED)
			.execute();
	});
	it('says a code was already used only to the network that approved it', async () => {
		const approverIp = '198.51.100.41';
		const strangerIp = '198.51.100.42';
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const initResp = await createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.body(null)
			.execute();
		await createBuilderWithoutAuth(harness)
			.get(`/auth/handoff/${initResp.code}/info`)
			.header('x-forwarded-for', approverIp)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.header('x-forwarded-for', approverIp)
			.header('Authorization', login.token)
			.body({code: initResp.code, user_id: login.userId})
			.expect(204)
			.execute();
		const strangerInfo = await createBuilderWithoutAuth<HandoffInfoResponse>(harness)
			.get(`/auth/handoff/${initResp.code}/info`)
			.header('x-forwarded-for', strangerIp)
			.execute();
		expect(strangerInfo.status).toBe('expired');
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.header('x-forwarded-for', strangerIp)
			.header('Authorization', login.token)
			.body({code: initResp.code, user_id: login.userId})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_HANDOFF_CODE)
			.execute();
		for (let i = 0; i < 6; i++) {
			await createBuilderWithoutAuth(harness)
				.get(`/auth/handoff/${initResp.code}/info`)
				.header('x-forwarded-for', approverIp)
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.HANDOFF_CODE_ALREADY_USED)
				.execute();
		}
	});
});

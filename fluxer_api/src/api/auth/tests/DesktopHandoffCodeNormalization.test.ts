// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {createAuthHarness, createTestAccount, loginAccount} from './AuthTestUtils';

interface HandoffInitiateResponse {
	code: string;
	poll_secret: string;
}

interface HandoffInfoResponse {
	status: 'pending' | 'expired';
}

interface HandoffStatusResponse {
	status: 'pending' | 'completed' | 'expired';
	token?: string;
	user_id?: string;
}

function validateHandoffCodeFormat(code: string): boolean {
	return /^[A-Z0-9]{6}-[A-Z0-9]{6}$/.test(code);
}

describe('Auth desktop handoff code normalization', () => {
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
	it('accepts codes without dashes and is case-insensitive', async () => {
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const initResp = await createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.body(null)
			.execute();
		expect(validateHandoffCodeFormat(initResp.code)).toBe(true);
		const codeWithoutDash = initResp.code.replace(/-/g, '');
		const status1 = await createBuilderWithoutAuth<HandoffStatusResponse>(harness)
			.get(`/auth/handoff/${codeWithoutDash}/status`)
			.execute();
		expect(status1.status).toBe('pending');
		const lowercaseCode = initResp.code.toLowerCase();
		await createBuilderWithoutAuth<HandoffInfoResponse>(harness).get(`/auth/handoff/${lowercaseCode}/info`).execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.header('Authorization', login.token)
			.body({
				code: lowercaseCode,
				user_id: login.userId,
			})
			.expect(204)
			.execute();
		const status2 = await createBuilderWithoutAuth<HandoffStatusResponse>(harness)
			.post(`/auth/handoff/${initResp.code}/status`)
			.body({poll_secret: initResp.poll_secret})
			.execute();
		expect(status2.status).toBe('completed');
		expect(status2.token).toBeTruthy();
		expect(status2.token).not.toBe(login.token);
	});
	it('normalises code on info endpoint', async () => {
		const initResp = await createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.body(null)
			.execute();
		const lowercaseCode = initResp.code.toLowerCase().replace(/-/g, '');
		const info = await createBuilderWithoutAuth<HandoffInfoResponse>(harness)
			.get(`/auth/handoff/${lowercaseCode}/info`)
			.execute();
		expect(info.status).toBe('pending');
	});
});

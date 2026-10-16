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
	return_method: 'deep_link' | 'code';
}

interface HandoffInfoResponse {
	status: 'pending' | 'expired';
	return_method?: 'deep_link' | 'code';
}

interface HandoffCompleteResponse {
	return_url: string;
}

interface HandoffStatusResponse {
	status: 'pending' | 'completed' | 'denied' | 'expired';
	token?: string | null;
	user_id?: string | null;
}

const RETURN_URI = 'fluxer-canary://handoff';

describe('Auth desktop handoff deep link return', () => {
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

	async function initiate(body: Record<string, unknown> | null): Promise<HandoffInitiateResponse> {
		return createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.body(body)
			.execute();
	}

	async function readStatus(code: string, body: Record<string, unknown>): Promise<HandoffStatusResponse> {
		return createBuilderWithoutAuth<HandoffStatusResponse>(harness)
			.post(`/auth/handoff/${code}/status`)
			.body(body)
			.execute();
	}

	it('releases the token only to the app that presents both the poll secret and the grant', async () => {
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const initResp = await initiate({return_uri: RETURN_URI});
		expect(initResp.return_method).toBe('deep_link');
		const info = await createBuilderWithoutAuth<HandoffInfoResponse>(harness)
			.get(`/auth/handoff/${initResp.code}/info`)
			.execute();
		expect(info.return_method).toBe('deep_link');
		const completed = await createBuilderWithoutAuth<HandoffCompleteResponse>(harness)
			.post('/auth/handoff/complete')
			.body({code: initResp.code, token: login.token, user_id: login.userId, return_method: 'deep_link'})
			.expect(200)
			.execute();
		const returnUrl = new URL(completed.return_url);
		expect(`${returnUrl.protocol}//${returnUrl.host}`).toBe(RETURN_URI);
		expect(returnUrl.searchParams.get('code')).toBe(initResp.code);
		const grant = returnUrl.searchParams.get('grant');
		expect(grant).toBeTruthy();
		const withoutGrant = await readStatus(initResp.code, {poll_secret: initResp.poll_secret});
		expect(withoutGrant.status).toBe('pending');
		expect(withoutGrant.token ?? null).toBeNull();
		const wrongGrant = await readStatus(initResp.code, {poll_secret: initResp.poll_secret, grant: 'not-the-grant'});
		expect(wrongGrant.status).toBe('pending');
		const grantWithoutSecret = await readStatus(initResp.code, {poll_secret: 'not-the-secret', grant: grant!});
		expect(grantWithoutSecret.status).toBe('pending');
		const released = await readStatus(initResp.code, {poll_secret: initResp.poll_secret, grant: grant!});
		expect(released.status).toBe('completed');
		expect(released.token).toBeTruthy();
		expect(released.user_id).toBe(login.userId);
	});

	it('keeps polling without a grant from counting as a failed attempt', async () => {
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const initResp = await initiate({return_uri: RETURN_URI});
		await createBuilderWithoutAuth(harness).get(`/auth/handoff/${initResp.code}/info`).execute();
		const completed = await createBuilderWithoutAuth<HandoffCompleteResponse>(harness)
			.post('/auth/handoff/complete')
			.body({code: initResp.code, token: login.token, user_id: login.userId, return_method: 'deep_link'})
			.execute();
		for (let i = 0; i < 8; i++) {
			const pending = await readStatus(initResp.code, {poll_secret: initResp.poll_secret});
			expect(pending.status).toBe('pending');
		}
		const grant = new URL(completed.return_url).searchParams.get('grant')!;
		const released = await readStatus(initResp.code, {poll_secret: initResp.poll_secret, grant});
		expect(released.status).toBe('completed');
	});

	it('still completes through the typed code when the app registered a deep link', async () => {
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const initResp = await initiate({return_uri: RETURN_URI});
		await createBuilderWithoutAuth(harness).get(`/auth/handoff/${initResp.code}/info`).execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.body({code: initResp.code, token: login.token, user_id: login.userId, return_method: 'code'})
			.expect(204)
			.execute();
		const released = await readStatus(initResp.code, {poll_secret: initResp.poll_secret});
		expect(released.status).toBe('completed');
	});

	it('reports the code return method when the app did not register a deep link', async () => {
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const initResp = await initiate(null);
		expect(initResp.return_method).toBe('code');
		const info = await createBuilderWithoutAuth<HandoffInfoResponse>(harness)
			.get(`/auth/handoff/${initResp.code}/info`)
			.execute();
		expect(info.return_method).toBe('code');
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.body({code: initResp.code, token: login.token, user_id: login.userId, return_method: 'deep_link'})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_HANDOFF_CODE)
			.execute();
	});

	it('rejects a return deep link outside the desktop app schemes', async () => {
		for (const returnUri of ['https://evil.example/handoff', 'fluxer://handoff/extra', 'javascript://handoff']) {
			await createBuilderWithoutAuth(harness)
				.post('/auth/handoff/initiate')
				.body({return_uri: returnUri})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		}
	});

	it('tells the app when the browser declines the request and blocks later approval', async () => {
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const initResp = await initiate({return_uri: RETURN_URI});
		await createBuilderWithoutAuth(harness)
			.post(`/auth/handoff/${initResp.code}/deny`)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_HANDOFF_CODE)
			.execute();
		await createBuilderWithoutAuth(harness).get(`/auth/handoff/${initResp.code}/info`).execute();
		await createBuilderWithoutAuth(harness).post(`/auth/handoff/${initResp.code}/deny`).expect(204).execute();
		const denied = await readStatus(initResp.code, {poll_secret: initResp.poll_secret});
		expect(denied.status).toBe('denied');
		const info = await createBuilderWithoutAuth<HandoffInfoResponse>(harness)
			.get(`/auth/handoff/${initResp.code}/info`)
			.execute();
		expect(info.status).toBe('expired');
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.body({code: initResp.code, token: login.token, user_id: login.userId})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_HANDOFF_CODE)
			.execute();
	});
});

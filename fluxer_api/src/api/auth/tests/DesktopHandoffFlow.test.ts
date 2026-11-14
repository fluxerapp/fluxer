// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, loginAccount} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {maskIpForDisplay} from '@fluxer/ip_utils/src/IpAddress';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface HandoffInitiateResponse {
	code: string;
	expires_at: string;
	poll_secret: string;
}

interface HandoffInfoResponse {
	status: 'pending' | 'expired';
	client_info?: {
		platform?: string | null;
		os?: string | null;
		device?: 'mobile' | 'desktop';
		location?: {
			city?: string | null;
			region?: string | null;
			country?: string | null;
		} | null;
	} | null;
}

interface AuthSessionsResponseItem {
	masked_ip?: string | null;
	client_info?: {
		platform?: string | null;
		os?: string | null;
		browser?: string | null;
		device?: 'mobile' | 'desktop';
	} | null;
}

interface HandoffStatusResponse {
	status: 'pending' | 'completed' | 'expired';
	token?: string;
	user_id?: string;
}

describe('Auth desktop handoff flow', () => {
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
	it('attributes the handed-off session to the initiating desktop, not the approving browser', async () => {
		const DESKTOP_IP = '203.0.113.77';
		const desktopUserAgent =
			'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) FluxerStable/1.4.0 Chrome/128.0.0.0 Electron/32.0.0 Safari/537.36';
		const browserUserAgent =
			'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36';
		const account = await createTestAccount(harness);
		const login = await loginAccount(harness, account);
		const initResp = await createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.header('User-Agent', desktopUserAgent)
			.header('x-forwarded-for', DESKTOP_IP)
			.body(null)
			.execute();
		const info = await createBuilderWithoutAuth<HandoffInfoResponse>(harness)
			.get(`/auth/handoff/${initResp.code}/info`)
			.header('User-Agent', browserUserAgent)
			.execute();
		expect(info.client_info?.platform).toBe('Fluxer macOS');
		expect(info.client_info?.device).toBe('desktop');
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.header('User-Agent', browserUserAgent)
			.body({code: initResp.code, token: login.token, user_id: login.userId})
			.expect(204)
			.execute();
		const completed = await createBuilderWithoutAuth<HandoffStatusResponse>(harness)
			.post(`/auth/handoff/${initResp.code}/status`)
			.body({poll_secret: initResp.poll_secret})
			.execute();
		const sessions = await createBuilder<Array<AuthSessionsResponseItem>>(harness, completed.token!)
			.get('/auth/sessions')
			.execute();
		const handedOff = sessions.filter((session) => session.client_info?.platform === 'Fluxer macOS');
		expect(handedOff).toHaveLength(1);
		expect(handedOff[0]?.client_info?.os).toBe('macOS');
		expect(handedOff[0]?.client_info?.browser).toBeNull();
		expect(handedOff[0]?.client_info?.device).toBe('desktop');
		expect(sessions.some((session) => session.client_info?.browser === 'Chrome')).toBe(false);
		expect(handedOff[0]?.masked_ip).toBe(maskIpForDisplay(DESKTOP_IP));
	});
	it('rejects info lookup after 3 attempts on same code', async () => {
		const initResp = await createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.body(null)
			.execute();
		for (let i = 0; i < 3; i++) {
			const info = await createBuilderWithoutAuth<HandoffInfoResponse>(harness)
				.get(`/auth/handoff/${initResp.code}/info`)
				.execute();
			expect(info.status).toBe('pending');
		}
		await createBuilderWithoutAuth(harness)
			.get(`/auth/handoff/${initResp.code}/info`)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_HANDOFF_CODE)
			.execute();
	});
	it('allows handoff completion across rotated IPv6 privacy addresses on the same /64', async () => {
		const originIp = '2a01:e0a:d10:95b0:8f54:410e:f290:1c66';
		const approverInfoIp = '2a01:e0a:d10:95b0:1e4:53a8:d0dd:7733';
		const approverCompleteIp = '2a01:e0a:d10:95b0:b53f:16d3:aff2:9b0f';
		const pollerIp = '2a01:e0a:d10:95b0:5e4e:f862:62d3:d9cc';
		const account = await createTestAccount(harness, {ipAddress: originIp});
		const login = await loginAccount(harness, account);
		const initResp = await createBuilderWithoutAuth<HandoffInitiateResponse>(harness)
			.post('/auth/handoff/initiate')
			.header('x-forwarded-for', originIp)
			.body(null)
			.execute();
		const info = await createBuilderWithoutAuth<HandoffInfoResponse>(harness)
			.get(`/auth/handoff/${initResp.code}/info`)
			.header('x-forwarded-for', approverInfoIp)
			.execute();
		expect(info.status).toBe('pending');
		await createBuilderWithoutAuth(harness)
			.post('/auth/handoff/complete')
			.header('x-forwarded-for', approverCompleteIp)
			.header('Authorization', login.token)
			.body({
				code: initResp.code,
				user_id: login.userId,
			})
			.expect(204)
			.execute();
		const completed = await createBuilderWithoutAuth<HandoffStatusResponse>(harness)
			.post(`/auth/handoff/${initResp.code}/status`)
			.header('x-forwarded-for', pollerIp)
			.body({poll_secret: initResp.poll_secret})
			.execute();
		expect(completed.status).toBe('completed');
		expect(completed.user_id).toBe(login.userId);
	});
});

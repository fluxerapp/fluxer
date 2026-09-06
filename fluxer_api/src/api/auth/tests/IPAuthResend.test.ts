// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {
	clearTestEmails,
	createAuthHarness,
	createUniqueEmail,
	createUniqueUsername,
	listTestEmails,
	registerUser,
} from './AuthTestUtils';

const HTTP_TOO_MANY_REQUESTS = 429;

interface IpAuthorizationResponse {
	ticket?: string;
	ip_authorization_required?: boolean;
	email?: string;
	resend_available_in?: number;
	code?: string;
	message?: string;
}

interface ResendErrorResponse {
	code?: string;
	message?: string;
	resend_available_in?: number;
	retry_after?: number;
}

describe('Auth IP Authorization Resend', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await clearTestEmails(harness);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	async function triggerIpAuthorization(email: string, password: string, ip: string): Promise<IpAuthorizationResponse> {
		const ipAuthResp = await createBuilderWithoutAuth<IpAuthorizationResponse>(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', ip)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		expect(ipAuthResp.ip_authorization_required).toBe(true);
		expect(ipAuthResp.ticket).toBeTruthy();
		return ipAuthResp;
	}
	describe('Resend rate limit enforcement', () => {
		it('returns 429 when resend is attempted immediately after ticket creation', async () => {
			const email = createUniqueEmail('ip-resend-immediate');
			const password = 'a-strong-password';
			await registerUser(harness, {
				email,
				username: createUniqueUsername('resendimm'),
				global_name: 'Resend Immediate User',
				password,
				date_of_birth: '2000-01-01',
				consent: true,
			});
			const newIP = '10.200.210.220';
			const ipAuthResp = await triggerIpAuthorization(email, password, newIP);
			const errorResp = await createBuilderWithoutAuth<ResendErrorResponse>(harness)
				.post('/auth/ip-authorization/resend')
				.body({ticket: ipAuthResp.ticket})
				.header('x-forwarded-for', newIP)
				.expect(HTTP_TOO_MANY_REQUESTS)
				.execute();
			expect(errorResp.code).toBe(APIErrorCodes.IP_AUTHORIZATION_RESEND_COOLDOWN);
		});
		it('returns resend_available_in and a matching Retry-After header when rate limited', async () => {
			const email = createUniqueEmail('ip-resend-cooldown');
			const password = 'a-strong-password';
			await registerUser(harness, {
				email,
				username: createUniqueUsername('resendcool'),
				global_name: 'Resend Cooldown User',
				password,
				date_of_birth: '2000-01-01',
				consent: true,
			});
			const newIP = '10.230.240.250';
			const ipAuthResp = await triggerIpAuthorization(email, password, newIP);
			const {response, json: errorResp} = await createBuilderWithoutAuth<ResendErrorResponse>(harness)
				.post('/auth/ip-authorization/resend')
				.body({ticket: ipAuthResp.ticket})
				.header('x-forwarded-for', newIP)
				.expect(HTTP_TOO_MANY_REQUESTS)
				.executeWithResponse();
			expect(errorResp.code).toBe(APIErrorCodes.IP_AUTHORIZATION_RESEND_COOLDOWN);
			expect(typeof errorResp.resend_available_in).toBe('number');
			expect(errorResp.resend_available_in).toBeGreaterThan(0);
			expect(response.headers.get('Retry-After')).toBe(String(errorResp.resend_available_in));
			expect(errorResp.retry_after).toBe(errorResp.resend_available_in);
		});
	});
	describe('Multiple resend attempts handling', () => {
		it('returns 429 for multiple consecutive resend attempts', async () => {
			const email = createUniqueEmail('ip-multi-resend');
			const password = 'a-strong-password';
			await registerUser(harness, {
				email,
				username: createUniqueUsername('multiresend'),
				global_name: 'Multi Resend User',
				password,
				date_of_birth: '2000-01-01',
				consent: true,
			});
			const newIP = '10.30.31.32';
			const ipAuthResp = await triggerIpAuthorization(email, password, newIP);
			for (let i = 0; i < 3; i++) {
				await createBuilderWithoutAuth(harness)
					.post('/auth/ip-authorization/resend')
					.body({ticket: ipAuthResp.ticket})
					.header('x-forwarded-for', newIP)
					.expect(HTTP_TOO_MANY_REQUESTS)
					.execute();
			}
		});
	});
	describe('Resend with already-used ticket', () => {
		it('returns 400 when resend flag is already set on the ticket', async () => {
			const email = createUniqueEmail('ip-resend-used');
			const password = 'a-strong-password';
			const reg = await registerUser(harness, {
				email,
				username: createUniqueUsername('resendused'),
				global_name: 'Resend Used User',
				password,
				date_of_birth: '2000-01-01',
				consent: true,
			});
			const ticket = `ticket-used-${Date.now()}`;
			const token = `token-used-${Date.now()}`;
			await createBuilderWithoutAuth(harness)
				.post('/test/auth/ip-authorization')
				.body({
					ticket,
					token,
					user_id: reg.user_id,
					email,
					username: 'resend-used-user',
					client_ip: '203.0.113.10',
					user_agent: 'IntegrationTest/1.0',
					client_location: 'Testland',
					resend_used: true,
					created_at: Date.now() - 2 * 60 * 1000,
					ttl_seconds: 900,
				})
				.execute();
			const {response, json: errorResp} = await createBuilderWithoutAuth<ResendErrorResponse>(harness)
				.post('/auth/ip-authorization/resend')
				.body({ticket})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.executeWithResponse();
			expect(errorResp.code).toBe(APIErrorCodes.IP_AUTHORIZATION_RESEND_LIMIT_EXCEEDED);
			expect(response.headers.get('Retry-After')).toBeNull();
		});
		it('returns 400 after IP has already been authorized', async () => {
			const email = createUniqueEmail('ip-resend-after-auth');
			const password = 'a-strong-password';
			await registerUser(harness, {
				email,
				username: createUniqueUsername('resendafterauth'),
				global_name: 'Resend After Auth User',
				password,
				date_of_birth: '2000-01-01',
				consent: true,
			});
			await clearTestEmails(harness);
			const newIP = '10.26.27.28';
			const ipAuthResp = await triggerIpAuthorization(email, password, newIP);
			const emails = await listTestEmails(harness);
			const ipAuthEmail = emails.find((e) => e.type === 'ip_authorization' && e.to === email);
			expect(ipAuthEmail).toBeDefined();
			const authToken = ipAuthEmail!.metadata['token'];
			expect(authToken).toBeTruthy();
			await createBuilderWithoutAuth(harness)
				.post('/auth/authorize-ip')
				.body({token: authToken})
				.header('x-forwarded-for', newIP)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/auth/ip-authorization/resend')
				.body({ticket: ipAuthResp.ticket})
				.header('x-forwarded-for', newIP)
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
	describe('Resend with empty ticket', () => {
		it('returns 400 when ticket is empty string', async () => {
			await createBuilderWithoutAuth(harness)
				.post('/auth/ip-authorization/resend')
				.body({ticket: ''})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
	describe('Resend with invalid ticket', () => {
		it('returns 400 when ticket does not exist', async () => {
			const errorResp = await createBuilderWithoutAuth<ResendErrorResponse>(harness)
				.post('/auth/ip-authorization/resend')
				.body({ticket: 'invalid-ticket-xyz-12345'})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(errorResp.code).toBeDefined();
			expect(errorResp.message).toBeDefined();
		});
		it('returns 400 when ticket has expired', async () => {
			const email = createUniqueEmail('ip-ticket-expired');
			const password = 'a-strong-password';
			await registerUser(harness, {
				email,
				username: createUniqueUsername('ticketexp'),
				global_name: 'Ticket Expired User',
				password,
				date_of_birth: '2000-01-01',
				consent: true,
			});
			const newIP = '10.70.80.90';
			const ipAuthResp = await triggerIpAuthorization(email, password, newIP);
			await createBuilderWithoutAuth(harness)
				.post('/test/auth/ip-authorization/expire')
				.body({ticket: ipAuthResp.ticket})
				.execute();
			await createBuilderWithoutAuth(harness)
				.post('/auth/ip-authorization/resend')
				.body({ticket: ipAuthResp.ticket})
				.header('x-forwarded-for', newIP)
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
	});
	describe('Resend response format validation', () => {
		it('returns proper error structure with code and message on rate limit', async () => {
			const email = createUniqueEmail('ip-resend-format');
			const password = 'a-strong-password';
			await registerUser(harness, {
				email,
				username: createUniqueUsername('resendformat'),
				global_name: 'Resend Format User',
				password,
				date_of_birth: '2000-01-01',
				consent: true,
			});
			const newIP = '10.231.241.251';
			const ipAuthResp = await triggerIpAuthorization(email, password, newIP);
			const errorResp = await createBuilderWithoutAuth<ResendErrorResponse>(harness)
				.post('/auth/ip-authorization/resend')
				.body({ticket: ipAuthResp.ticket})
				.header('x-forwarded-for', newIP)
				.expect(HTTP_TOO_MANY_REQUESTS)
				.execute();
			expect(errorResp.code).toBe(APIErrorCodes.IP_AUTHORIZATION_RESEND_COOLDOWN);
			expect(typeof errorResp.message).toBe('string');
			expect(errorResp.message!.length).toBeGreaterThan(0);
		});
		it('returns proper error structure when ticket is invalid', async () => {
			const errorResp = await createBuilderWithoutAuth<ResendErrorResponse>(harness)
				.post('/auth/ip-authorization/resend')
				.body({ticket: 'nonexistent-ticket-abc'})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(typeof errorResp.code).toBe('string');
			expect(typeof errorResp.message).toBe('string');
		});
		it('returns proper error structure when resend limit exceeded', async () => {
			const email = createUniqueEmail('ip-limit-format');
			const password = 'a-strong-password';
			const reg = await registerUser(harness, {
				email,
				username: createUniqueUsername('limitformat'),
				global_name: 'Limit Format User',
				password,
				date_of_birth: '2000-01-01',
				consent: true,
			});
			const ticket = `ticket-limit-${Date.now()}`;
			const token = `token-limit-${Date.now()}`;
			await createBuilderWithoutAuth(harness)
				.post('/test/auth/ip-authorization')
				.body({
					ticket,
					token,
					user_id: reg.user_id,
					email,
					username: 'limit-format-user',
					client_ip: '203.0.113.20',
					user_agent: 'IntegrationTest/1.0',
					client_location: 'Testland',
					resend_used: true,
					created_at: Date.now() - 2 * 60 * 1000,
					ttl_seconds: 900,
				})
				.execute();
			const errorResp = await createBuilderWithoutAuth<ResendErrorResponse>(harness)
				.post('/auth/ip-authorization/resend')
				.body({ticket})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(errorResp.code).toBe(APIErrorCodes.IP_AUTHORIZATION_RESEND_LIMIT_EXCEEDED);
			expect(typeof errorResp.message).toBe('string');
		});
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createUniqueEmail,
	createUniqueUsername,
	registerUser,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth IP Authorization Ticket', () => {
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
	it('validates the new IP authorization ticket error response and the resend rate limit behavior', async () => {
		const email = createUniqueEmail('ip-ticket');
		const password = 'a-strong-password';
		await registerUser(harness, {
			email,
			username: createUniqueUsername('ticket'),
			global_name: 'Ticket User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const otherIP = '10.55.44.33';
		const body = await createBuilderWithoutAuth<{
			code?: string;
			ticket?: string;
			ip_authorization_required?: boolean;
			email?: string;
			resend_available_in?: number;
			message?: string;
		}>(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', otherIP)
			.expect(403)
			.execute();
		expect(body.ip_authorization_required).toBe(true);
		expect(body.ticket).toBeTruthy();
		expect(body.email).toBeTruthy();
		await createBuilderWithoutAuth(harness)
			.post('/auth/ip-authorization/resend')
			.body({ticket: body.ticket})
			.header('x-forwarded-for', otherIP)
			.expect(429)
			.execute();
	});
	it('validates that the ticket returned in the login response expires and cannot be used for resending after expiration', async () => {
		const email = createUniqueEmail('ip-ticket-expire');
		const password = 'a-strong-password';
		await registerUser(harness, {
			email,
			username: createUniqueUsername('ticketexpire'),
			global_name: 'Ticket Expire User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const newIP = '10.70.80.90';
		const ipAuthResp = await createBuilderWithoutAuth<{
			ticket?: string;
		}>(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', newIP)
			.expect(403)
			.execute();
		expect(ipAuthResp.ticket).toBeTruthy();
		await createBuilderWithoutAuth(harness)
			.post('/test/auth/ip-authorization/expire')
			.body({ticket: ipAuthResp.ticket})
			.expect(200)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post('/auth/ip-authorization/resend')
			.body({ticket: ipAuthResp.ticket})
			.header('x-forwarded-for', newIP)
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
});

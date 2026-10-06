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

describe('Auth IP Authorization Bypass Flags', () => {
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
	const testCases = [
		{
			name: 'APP_STORE_REVIEWER only',
			flags: ['APP_STORE_REVIEWER'],
		},
		{
			name: 'APP_STORE_REVIEWER with STAFF',
			flags: ['APP_STORE_REVIEWER', 'STAFF'],
		},
		{
			name: 'APP_STORE_REVIEWER with BUG_HUNTER and STAFF',
			flags: ['APP_STORE_REVIEWER', 'BUG_HUNTER', 'STAFF'],
		},
		{
			name: 'APP_STORE_REVIEWER with BUG_HUNTER',
			flags: ['APP_STORE_REVIEWER', 'BUG_HUNTER'],
		},
	];
	for (const tc of testCases) {
		it(`validates that users with ${tc.name} can login from any IP without authorization`, async () => {
			const email = createUniqueEmail(`bypass-flags-${tc.flags.join('-')}`);
			const password = 'a-strong-password';
			const reg = await registerUser(harness, {
				email,
				username: createUniqueUsername('bypass'),
				global_name: 'Bypass User',
				password,
				date_of_birth: '2000-01-01',
				consent: true,
			});
			await createBuilderWithoutAuth(harness)
				.post(`/test/users/${reg.user_id}/security-flags`)
				.body({
					set_flags: tc.flags,
				})
				.execute();
			const newIP = '10.50.60.70';
			const loginResp = await createBuilderWithoutAuth<{
				token?: string;
				user_id?: string;
			}>(harness)
				.post('/auth/login')
				.body({email, password})
				.header('x-forwarded-for', newIP)
				.execute();
			expect(loginResp.token).toBeTruthy();
			expect(loginResp.user_id).toBe(reg.user_id);
		});
	}
	it('validates that users without bypass flags still require IP authorization for new IPs', async () => {
		const email = createUniqueEmail('regular-with-flags');
		const password = 'a-strong-password';
		const reg = await registerUser(harness, {
			email,
			username: createUniqueUsername('regularflags'),
			global_name: 'Regular Flags User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${reg.user_id}/security-flags`)
			.body({
				set_flags: ['PARTNER', 'BUG_HUNTER'],
			})
			.execute();
		const newIP = '10.160.170.180';
		const ipAuthResp = await createBuilderWithoutAuth<{
			ip_authorization_required?: boolean;
		}>(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', newIP)
			.expect(403)
			.execute();
		expect(ipAuthResp.ip_authorization_required).toBe(true);
	});
	it('validates that when a bypass flag is removed from a user, they once again require IP authorization for new IPs', async () => {
		const email = createUniqueEmail('bypass-removed');
		const password = 'a-strong-password';
		const reg = await registerUser(harness, {
			email,
			username: createUniqueUsername('bypassremoved'),
			global_name: 'Bypass Removed User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${reg.user_id}/security-flags`)
			.body({
				set_flags: ['APP_STORE_REVIEWER'],
			})
			.execute();
		const newIP = '10.100.110.120';
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', newIP)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${reg.user_id}/security-flags`)
			.body({
				clear_flags: ['APP_STORE_REVIEWER'],
			})
			.execute();
		const anotherNewIP = '10.130.140.150';
		const ipAuthResp = await createBuilderWithoutAuth<{
			ip_authorization_required?: boolean;
		}>(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', anotherNewIP)
			.expect(403)
			.execute();
		expect(ipAuthResp.ip_authorization_required).toBe(true);
	});
	it('validates that when a bypass flag is added to an existing user, they can immediately login from new IPs without requiring authorization', async () => {
		const email = createUniqueEmail('bypass-added-later');
		const password = 'a-strong-password';
		const reg = await registerUser(harness, {
			email,
			username: createUniqueUsername('bypassadded'),
			global_name: 'Bypass Added User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const newIP = '10.80.90.100';
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', newIP)
			.expect(403)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${reg.user_id}/security-flags`)
			.body({
				set_flags: ['APP_STORE_REVIEWER'],
			})
			.execute();
		const loginResp = await createBuilderWithoutAuth<{
			token?: string;
		}>(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', newIP)
			.execute();
		expect(loginResp.token).toBeTruthy();
	});
	it('verifies that regular users still require IP authorization when logging in from a new location', async () => {
		const email = createUniqueEmail('regular-user-ip-check');
		const password = 'a-strong-password';
		await registerUser(harness, {
			email,
			username: createUniqueUsername('regularip'),
			global_name: 'Regular IP User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const differentIP = '10.88.77.66';
		await createBuilderWithoutAuth(harness)
			.post('/auth/login')
			.body({email, password})
			.header('x-forwarded-for', differentIP)
			.expect(403)
			.execute();
	});
});

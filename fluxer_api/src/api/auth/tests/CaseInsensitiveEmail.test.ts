// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createAuthHarness,
	createUniqueEmail,
	createUniqueUsername,
	fetchMe,
	type LoginSuccessResponse,
	loginUser,
	registerUser,
} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Auth case-insensitive email', () => {
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
	describe('login with different case variations succeeds', () => {
		const baseEmail = createUniqueEmail('login-case');
		const password = 'Xk9#mP2$vL5@nQ8';
		beforeEach(async () => {
			await registerUser(harness, {
				email: baseEmail,
				username: createUniqueUsername('login'),
				global_name: 'Login Test User',
				password,
				date_of_birth: '2000-01-01',
				consent: true,
			});
		});
		it('allows login with lowercase email', async () => {
			const login = await loginUser(harness, {
				email: baseEmail.toLowerCase(),
				password,
			});
			expect('mfa' in login).toBe(false);
			expect((login as LoginSuccessResponse).token).toBeTruthy();
		});
		it('allows login with uppercase email', async () => {
			const login = await loginUser(harness, {
				email: baseEmail.toUpperCase(),
				password,
			});
			expect('mfa' in login).toBe(false);
			expect((login as LoginSuccessResponse).token).toBeTruthy();
		});
		it('allows login with mixed case email', async () => {
			const mixedCaseEmail = baseEmail
				.split('')
				.map((char, index) => (index % 2 === 0 ? char.toUpperCase() : char.toLowerCase()))
				.join('');
			const login = await loginUser(harness, {
				email: mixedCaseEmail,
				password,
			});
			expect('mfa' in login).toBe(false);
			expect((login as LoginSuccessResponse).token).toBeTruthy();
		});
		it('allows login with title case email', async () => {
			const titleCaseEmail = baseEmail
				.toLowerCase()
				.replace(/(^|[.@])([a-z])/g, (_match, prefix, char) => `${prefix}${char.toUpperCase()}`);
			const login = await loginUser(harness, {
				email: titleCaseEmail,
				password,
			});
			expect('mfa' in login).toBe(false);
			expect((login as LoginSuccessResponse).token).toBeTruthy();
		});
	});
	it('rejects registration with different case as duplicate', async () => {
		const baseEmail = createUniqueEmail('duplicate-case');
		const password = 'Rt7&kW3!qL9@mP2';
		await registerUser(harness, {
			email: baseEmail,
			username: createUniqueUsername('duplicate1'),
			global_name: 'Duplicate Test User 1',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({
				email: baseEmail.toUpperCase(),
				username: createUniqueUsername('duplicate2'),
				global_name: 'Duplicate Test User 2',
				password: 'different-password-456',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.expect(400)
			.execute();
	});
	it('allows forgot password with different case', async () => {
		const baseEmail = createUniqueEmail('forgot-case');
		const password = 'Mn8$jX4&vB6@pL1';
		await registerUser(harness, {
			email: baseEmail,
			username: createUniqueUsername('forgot'),
			global_name: 'Forgot Test User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		await createBuilderWithoutAuth(harness)
			.post('/auth/forgot')
			.body({
				email: baseEmail.toUpperCase(),
			})
			.expect(204)
			.execute();
	});
	it('preserves original email case in user record', async () => {
		const mixedEmail = `${createUniqueEmail('normalized').split('@')[0]}@Example.COM`;
		const password = 'Df5&gH9@kW3!qL2';
		const reg = await registerUser(harness, {
			email: mixedEmail,
			username: createUniqueUsername('normalize'),
			global_name: 'Normalize Test User',
			password,
			date_of_birth: '2000-01-01',
			consent: true,
		});
		const {response, json} = await fetchMe(harness, reg.token);
		expect(response.status).toBe(200);
		const user = json as {
			email: string | null;
			username: string;
			global_name: string | null;
		};
		expect(user.email).toBe(mixedEmail);
		const login = await loginUser(harness, {
			email: mixedEmail.toLowerCase(),
			password,
		});
		expect('mfa' in login).toBe(false);
		const nonMfaLogin = login as {
			user_id: string;
			token: string;
		};
		expect(nonMfaLogin.token).toBeTruthy();
	});
});

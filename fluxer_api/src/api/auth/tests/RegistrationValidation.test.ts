// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';
import {createAuthHarness, createTestAccount, createUniqueEmail, createUniqueUsername} from './AuthTestUtils';

interface ValidationErrorResponse {
	code: string;
	message: string;
	errors?: Array<{
		path: string;
		code: string;
		message: string;
	}>;
}

describe('Registration validation', () => {
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
	it('rejects invalid email format', async () => {
		const json = await createBuilderWithoutAuth<ValidationErrorResponse>(harness)
			.post('/auth/register')
			.header('accept-language', 'fr')
			.body({
				email: 'not-an-email',
				username: createUniqueUsername(),
				global_name: 'Test User',
				password: 'a-strong-password',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		const emailError = json.errors?.find((e) => e.path === 'email');
		expect(emailError?.code).toBe('INVALID_EMAIL_FORMAT');
		expect(emailError?.message).toBe("Format d'adresse e-mail invalide.");
	});
	it('rejects blocked registration email TLDs as invalid email', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({
				email: 'holdermichael_6023@example.blocked',
				username: createUniqueUsername(),
				global_name: 'Test User',
				password: 'a-strong-password',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.expect(400)
			.execute();
	});
	it('rejects weak password', async () => {
		const json = await createBuilderWithoutAuth<ValidationErrorResponse>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail(),
				username: createUniqueUsername(),
				global_name: 'Test User',
				password: 'weak',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		const passwordError = json.errors?.find((e) => e.path === 'password');
		expect(passwordError?.code).toBe('PASSWORD_LENGTH_INVALID');
		expect(passwordError?.message).toBe('String length must be between 8 and 256 characters.');
	});
	it('includes bounds in username length validation message', async () => {
		const json = await createBuilderWithoutAuth<ValidationErrorResponse>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail(),
				username: 'a'.repeat(33),
				global_name: 'Test User',
				password: 'a-strong-password',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		const usernameError = json.errors?.find((e) => e.path === 'username');
		expect(usernameError?.code).toBe('USERNAME_LENGTH_INVALID');
		expect(usernameError?.message).toBe('Username must be between 1 and 32 characters.');
	});
	it('rejects duplicate email', async () => {
		const account = await createTestAccount(harness);
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({
				email: account.email,
				username: createUniqueUsername(),
				global_name: 'Test User',
				password: 'a-strong-password',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
	});
	it('rejects missing date of birth', async () => {
		await createBuilderWithoutAuth(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail(),
				username: createUniqueUsername(),
				global_name: 'Test User',
				password: 'a-strong-password',
				consent: true,
			})
			.expect(400)
			.execute();
	});
	it('rejects an impossible calendar date with INVALID_DATE_OF_BIRTH_FORMAT', async () => {
		for (const dateOfBirth of ['2000-02-30', '2000-13-45']) {
			const json = await createBuilderWithoutAuth<ValidationErrorResponse>(harness)
				.post('/auth/register')
				.body({
					email: createUniqueEmail('impossible-dob'),
					username: createUniqueUsername('impossible'),
					global_name: 'Test User',
					password: 'a-strong-password',
					date_of_birth: dateOfBirth,
					consent: true,
				})
				.expect(400, 'INVALID_FORM_BODY')
				.execute();
			const dateOfBirthError = json.errors?.find((e) => e.path === 'date_of_birth');
			expect(dateOfBirthError?.code).toBe('INVALID_DATE_OF_BIRTH_FORMAT');
		}
	});
	it('rejects a real calendar date below the minimum age with MUST_BE_MINIMUM_AGE', async () => {
		const json = await createBuilderWithoutAuth<ValidationErrorResponse>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('underage'),
				username: createUniqueUsername('underage'),
				global_name: 'Test User',
				password: 'a-strong-password',
				date_of_birth: '2020-01-01',
				consent: true,
			})
			.expect(400, 'INVALID_FORM_BODY')
			.execute();
		const dateOfBirthError = json.errors?.find((e) => e.path === 'date_of_birth');
		expect(dateOfBirthError?.code).toBe('MUST_BE_MINIMUM_AGE');
	});
	it('accepts a real calendar date above the minimum age', async () => {
		const reg = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('valid-dob'),
				username: createUniqueUsername('validdob'),
				global_name: 'Test User',
				password: 'a-strong-password',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.execute();
		expect(reg.token).toBeTruthy();
	});
	it('allows emoji in global name', async () => {
		const globalName = '🌻 Sunflower';
		const reg = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('emoji'),
				username: createUniqueUsername('emoji'),
				global_name: globalName,
				password: 'a-strong-password',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.execute();
		const me = await createBuilder<{
			global_name: string;
		}>(harness, reg.token)
			.get('/users/@me')
			.execute();
		expect(me.global_name).toBe(globalName);
	});
	it('derives username from global name when username is not provided', async () => {
		const reg = await createBuilderWithoutAuth<{
			token: string;
		}>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('derived'),
				global_name: 'Magic Tester',
				password: 'a-strong-password',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.execute();
		const me = await createBuilder<{
			username: string;
		}>(harness, reg.token)
			.get('/users/@me')
			.execute();
		expect(me.username).toBe('Magic_Tester');
	});
});

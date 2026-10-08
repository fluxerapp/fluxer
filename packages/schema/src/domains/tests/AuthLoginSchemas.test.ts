// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	LoginRequest,
	RecoverAccountResponse,
	UsernameInstanceLoginRequest,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {createStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {EmailType, PasswordType} from '@fluxer/schema/src/primitives/UserValidators';
import {describe, expect, it} from 'vitest';
import {z} from 'zod';

const password = 'correct horse battery';

const EmailOnlyLoginRequest = z.object({
	email: EmailType,
	password: PasswordType,
	invite_code: createStringType(0, 256).nullish(),
});

describe('LoginRequest', () => {
	it('accepts an email on its own, as old clients send it', () => {
		expect(LoginRequest.safeParse({email: 'person@example.com', password}).success).toBe(true);
	});

	it('accepts a login on its own', () => {
		expect(LoginRequest.parse({login: 'person#1234', password})).toMatchObject({login: 'person#1234'});
	});

	it.each([
		{},
		{password},
		{password: 'x'},
		{password, invite_code: 'abc'},
		{login: '', password},
		{login: null, password},
		{login: null, password: 'x'},
	])('reports the same issues as the email-only schema did for %j', (body) => {
		const before = EmailOnlyLoginRequest.safeParse(body);
		const after = LoginRequest.safeParse(body);
		expect(after.success).toBe(false);
		expect(after.error?.issues.map(({path, code}) => ({path, code}))).toEqual(
			before.error?.issues.map(({path, code}) => ({path, code})),
		);
	});

	it('accepts a body with both email and login, as it did before login existed', () => {
		expect(LoginRequest.parse({email: 'person@example.com', login: 'person', password})).toMatchObject({
			email: 'person@example.com',
		});
	});

	it.each([{login: ''}, {login: null}, {login: 5}, {login: 'x'.repeat(400)}])(
		'ignores a stray login %j next to an email, as the old schema stripped it',
		(extra) => {
			const body = {email: 'person@example.com', ...extra, password};
			expect(EmailOnlyLoginRequest.safeParse(body).success).toBe(true);
			expect(LoginRequest.parse(body)).toEqual({email: 'person@example.com', password});
		},
	);

	it.each([{login: ''}, {login: null}, {login: 5}, {login: 'x'.repeat(400)}])(
		'reports the same email issues as before for a bad email next to %j',
		(extra) => {
			const body = {email: 'nope', ...extra, password: 'x', invite_code: 'y'.repeat(300)};
			expect(LoginRequest.safeParse(body).error?.issues.map(({path, code}) => ({path, code}))).toEqual(
				EmailOnlyLoginRequest.safeParse(body).error?.issues.map(({path, code}) => ({path, code})),
			);
		},
	);

	it('reports only the type error for a body that is not an object', () => {
		const result = LoginRequest.safeParse('nope');
		expect(result.error?.issues).toEqual([expect.objectContaining({path: [], code: 'invalid_type'})]);
	});
});

describe('UsernameInstanceLoginRequest', () => {
	it('keeps email and drops login when an older app sends both', () => {
		expect(UsernameInstanceLoginRequest.parse({email: 'alex', login: 'someone', password})).toEqual({
			email: 'alex',
			password,
		});
	});

	it('reads a null login as no identifier', () => {
		const result = UsernameInstanceLoginRequest.safeParse({login: null, password});
		expect(result.error?.issues.map(({path}) => path)).toEqual([['email']]);
	});
});

describe('RecoverAccountResponse', () => {
	const kit = {
		recovery_key: 'ABCD-EFGH-JKMN-PQRS-TVWX-YZ01-2345-6789',
		recovery_kit_created_at: '2026-10-01T12:00:00.000Z',
	};

	it('adds the new kit to every response variant', () => {
		for (const option of RecoverAccountResponse.options) {
			expect(Object.keys(option.shape)).toEqual(expect.arrayContaining(Object.keys(kit)));
		}
	});

	it('carries the new kit next to an MFA ticket', () => {
		const parsed = RecoverAccountResponse.safeParse({
			mfa: true,
			ticket: 'ticket',
			allowed_methods: ['totp'],
			totp: true,
			webauthn: false,
			backup_codes: false,
			...kit,
		});
		expect(parsed.success ? parsed.data : parsed.error.issues).toMatchObject(kit);
	});
});

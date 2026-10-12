// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {GlobalNameType, PasswordType, UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {describe, expect, it} from 'vitest';

describe('UsernameType', () => {
	it('accepts valid alphanumeric usernames', () => {
		const result = UsernameType.parse('testuser');
		expect(result).toBe('testuser');
	});
	it('accepts usernames with underscores', () => {
		const result = UsernameType.safeParse('test_user');
		expect(result.success).toBe(true);
	});
	it('accepts usernames with numbers', () => {
		const result = UsernameType.safeParse('user123');
		expect(result.success).toBe(true);
	});
	it('trims whitespace', () => {
		const result = UsernameType.parse('  testuser  ');
		expect(result).toBe('testuser');
	});
	it('rejects empty usernames', () => {
		const result = UsernameType.safeParse('');
		expect(result.success).toBe(false);
	});
	it('rejects usernames exceeding max length', () => {
		const result = UsernameType.safeParse('a'.repeat(33));
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.USERNAME_LENGTH_INVALID);
		}
	});
	it('rejects usernames with invalid characters', () => {
		const result = UsernameType.safeParse('test-user');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.USERNAME_INVALID_CHARACTERS);
		}
	});
	it('rejects "everyone" as username', () => {
		const result = UsernameType.safeParse('everyone');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.USERNAME_RESERVED_VALUE);
		}
	});
	it('rejects "here" as username', () => {
		const result = UsernameType.safeParse('here');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.USERNAME_RESERVED_VALUE);
		}
	});
	it('rejects usernames containing "fluxer"', () => {
		const result = UsernameType.safeParse('fluxeruser');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.USERNAME_CANNOT_CONTAIN_RESERVED_TERMS);
		}
	});
	it('rejects reserved terms case-insensitively', () => {
		const result = UsernameType.safeParse('EVERYONE');
		expect(result.success).toBe(false);
	});
});

describe('GlobalNameType', () => {
	it('rejects "everyone" as global name', () => {
		const result = GlobalNameType.safeParse('everyone');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.GLOBAL_NAME_RESERVED_VALUE);
		}
	});
	it('rejects "here" as global name', () => {
		const result = GlobalNameType.safeParse('here');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.GLOBAL_NAME_RESERVED_VALUE);
		}
	});
	it('rejects names containing "system message"', () => {
		const result = GlobalNameType.safeParse('My System Message');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.GLOBAL_NAME_CANNOT_CONTAIN_RESERVED_TERMS);
		}
	});
});

describe('PasswordType', () => {
	it('accepts valid passwords', () => {
		const result = PasswordType.parse('securepassword123');
		expect(result).toBe('securepassword123');
	});
	it('accepts passwords at minimum length', () => {
		const result = PasswordType.safeParse('12345678');
		expect(result.success).toBe(true);
	});
	it('rejects passwords shorter than minimum', () => {
		const result = PasswordType.safeParse('1234567');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.PASSWORD_LENGTH_INVALID);
		}
	});
	it('rejects passwords exceeding maximum length', () => {
		const result = PasswordType.safeParse('a'.repeat(257));
		expect(result.success).toBe(false);
	});
	it('trims and normalizes password', () => {
		const result = PasswordType.parse('  password123  ');
		expect(result).toBe('password123');
	});
});

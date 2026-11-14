// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {GlobalNameType, PasswordType, UsernameType} from '@fluxer/schema/src/primitives/UserValidators';
import {describe, expect, it} from 'vitest';

describe('UsernameType', () => {
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

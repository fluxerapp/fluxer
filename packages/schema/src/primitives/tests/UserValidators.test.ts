// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	DiscriminatorType,
	EmailType,
	GlobalNameType,
	PasswordType,
	PhoneNumberType,
	UsernameType,
	WebhookNameType,
} from '@fluxer/schema/src/primitives/UserValidators';
import {describe, expect, it} from 'vitest';

describe('EmailType', () => {
	it('accepts valid email addresses', () => {
		const result = EmailType.parse('user@example.com');
		expect(result).toBe('user@example.com');
	});
	it('accepts emails with subdomains', () => {
		const result = EmailType.safeParse('user@mail.example.com');
		expect(result.success).toBe(true);
	});
	it('accepts emails with plus addressing', () => {
		const result = EmailType.safeParse('user+tag@example.com');
		expect(result.success).toBe(true);
	});
	it('rejects emails without @ symbol', () => {
		const result = EmailType.safeParse('userexample.com');
		expect(result.success).toBe(false);
	});
	it('rejects emails without domain', () => {
		const result = EmailType.safeParse('user@');
		expect(result.success).toBe(false);
	});
	it('rejects emails with invalid local part characters', () => {
		const result = EmailType.safeParse('user name@example.com');
		expect(result.success).toBe(false);
	});
	it('rejects emails exceeding max length', () => {
		const longLocal = 'a'.repeat(250);
		const result = EmailType.safeParse(`${longLocal}@example.com`);
		expect(result.success).toBe(false);
	});
	it('trims emails with leading/trailing whitespace', () => {
		const result = EmailType.parse('  user@example.com  ');
		expect(result).toBe('user@example.com');
	});
});

describe('DiscriminatorType', () => {
	it('accepts valid single digit discriminators', () => {
		const result = DiscriminatorType.parse('1');
		expect(result).toBe(1);
	});
	it('accepts valid four digit discriminators', () => {
		const result = DiscriminatorType.parse('1234');
		expect(result).toBe(1234);
	});
	it('accepts zero as discriminator', () => {
		const result = DiscriminatorType.parse('0');
		expect(result).toBe(0);
	});
	it('accepts discriminators with leading zeros', () => {
		const result = DiscriminatorType.parse('0001');
		expect(result).toBe(1);
	});
	it('rejects discriminators with more than 4 digits', () => {
		const result = DiscriminatorType.safeParse('12345');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.DISCRIMINATOR_INVALID_FORMAT);
		}
	});
	it('rejects non-numeric discriminators', () => {
		const result = DiscriminatorType.safeParse('abc');
		expect(result.success).toBe(false);
	});
	it('rejects negative discriminators', () => {
		const result = DiscriminatorType.safeParse('-1');
		expect(result.success).toBe(false);
	});
});

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
	it('accepts valid display names', () => {
		const result = GlobalNameType.parse('Test User');
		expect(result).toBe('Test User');
	});
	it('accepts names with unicode characters', () => {
		const result = GlobalNameType.safeParse('Test User');
		expect(result.success).toBe(true);
	});
	it('sanitizes and normalizes input', () => {
		const result = GlobalNameType.parse('  Test  User  ');
		expect(result).toBe('Test User');
	});
	it('rejects empty names', () => {
		const result = GlobalNameType.safeParse('');
		expect(result.success).toBe(false);
	});
	it('rejects names exceeding max length', () => {
		const result = GlobalNameType.safeParse('a'.repeat(33));
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.GLOBAL_NAME_LENGTH_INVALID);
		}
	});
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

describe('PhoneNumberType', () => {
	it('accepts valid E.164 phone numbers', () => {
		const result = PhoneNumberType.parse('+14155551234');
		expect(result).toBe('+14155551234');
	});
	it('accepts international phone numbers', () => {
		const result = PhoneNumberType.safeParse('+442071234567');
		expect(result.success).toBe(true);
	});
	it('rejects phone numbers without plus prefix', () => {
		const result = PhoneNumberType.safeParse('14155551234');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.PHONE_NUMBER_INVALID_FORMAT);
		}
	});
	it('rejects phone numbers starting with +0', () => {
		const result = PhoneNumberType.safeParse('+04155551234');
		expect(result.success).toBe(false);
	});
	it('rejects phone numbers with invalid characters', () => {
		const result = PhoneNumberType.safeParse('+1-415-555-1234');
		expect(result.success).toBe(false);
	});
	it('rejects phone numbers that are too short', () => {
		const result = PhoneNumberType.safeParse('+1');
		expect(result.success).toBe(false);
	});
	it('rejects phone numbers that are too long', () => {
		const result = PhoneNumberType.safeParse('+1234567890123456');
		expect(result.success).toBe(false);
	});
});

describe('WebhookNameType', () => {
	it('accepts valid webhook names', () => {
		const result = WebhookNameType.parse('My Webhook');
		expect(result).toBe('My Webhook');
	});
	it('accepts single character names', () => {
		const result = WebhookNameType.safeParse('A');
		expect(result.success).toBe(true);
	});
	it('accepts names at maximum length', () => {
		const result = WebhookNameType.safeParse('a'.repeat(80));
		expect(result.success).toBe(true);
	});
	it('rejects empty names', () => {
		const result = WebhookNameType.safeParse('');
		expect(result.success).toBe(false);
	});
	it('rejects names exceeding maximum length', () => {
		const result = WebhookNameType.safeParse('a'.repeat(81));
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.WEBHOOK_NAME_LENGTH_INVALID);
		}
	});
	it('trims and normalizes webhook names', () => {
		const result = WebhookNameType.parse('  My Webhook  ');
		expect(result).toBe('My Webhook');
	});
});

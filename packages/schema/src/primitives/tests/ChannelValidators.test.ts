// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	AuditLogReasonType,
	ChannelNameType,
	GeneralChannelNameType,
	VanityURLCodeType,
} from '@fluxer/schema/src/primitives/ChannelValidators';
import {MAX_STRING_PROCESSING_LENGTH} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {describe, expect, it} from 'vitest';

describe('ChannelNameType', () => {
	it.each([
		['general', 'general'],
		['GENERAL', 'general'],
		['my channel', 'my-channel'],
		['my#channel@name', 'mychannelname'],
		['日本語-ÉTÉ', '日本語-été'],
		['###', '-'],
		['', '-'],
		['  my-channel  ', 'my-channel'],
		['hello\u202Eworld', 'helloworld'],
	])('normalizes %j to %j', (input, expected) => {
		expect(ChannelNameType.parse(input)).toBe(expected);
	});

	it('accepts the normalized length boundary', () => {
		const name = 'a'.repeat(100);
		expect(ChannelNameType.parse(`  ${name}  `)).toBe(name);
	});

	it.each([101, MAX_STRING_PROCESSING_LENGTH + 1])('returns a validation issue for %i characters', (length) => {
		expect(ChannelNameType.safeParse('a'.repeat(length))).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.STRING_LENGTH_INVALID}]},
		});
	});
});

describe('GeneralChannelNameType', () => {
	it.each([
		['General Chat', 'General Chat'],
		['My Channel', 'My Channel'],
		['My   Channel', 'My Channel'],
		['  My Channel  ', 'My Channel'],
		['My\u00A0Channel', 'My Channel'],
		['My\u200DChannel', 'MyChannel'],
	])('preserves display text while normalizing %j', (input, expected) => {
		expect(GeneralChannelNameType.parse(input)).toBe(expected);
	});

	it.each(['', '   ', '\u200D'])('rejects empty display names %j', (input) => {
		expect(GeneralChannelNameType.safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.NAME_EMPTY_AFTER_NORMALIZATION}]},
		});
	});

	it.each([101, MAX_STRING_PROCESSING_LENGTH + 1])('rejects %i characters without throwing', (length) => {
		expect(GeneralChannelNameType.safeParse('a'.repeat(length))).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.STRING_LENGTH_INVALID}]},
		});
	});
});

describe('VanityURLCodeType', () => {
	it.each([
		['myserver', 'myserver'],
		['my-server', 'my-server'],
		['server123', 'server123'],
		['MyServer', 'myserver'],
		['my server', 'my-server'],
		['my--server', 'my-server'],
		['  My \u202EServer  ', 'my-server'],
		['ab', 'ab'],
		['a'.repeat(32), 'a'.repeat(32)],
	])('normalizes %j to %j', (input, expected) => {
		expect(VanityURLCodeType.parse(input)).toBe(expected);
	});

	it.each(['a', 'a'.repeat(33)])('rejects an out-of-range code %j', (input) => {
		expect(VanityURLCodeType.safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.VANITY_URL_CODE_LENGTH_INVALID}]},
		});
	});

	it.each(['', '-myserver', 'myserver-', 'my_server', '日本語'])('rejects invalid characters in %j', (input) => {
		expect(VanityURLCodeType.safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.VANITY_URL_INVALID_CHARACTERS}]},
		});
	});
});

describe('AuditLogReasonType', () => {
	it.each([
		['User was spamming', 'User was spamming'],
		['  Reason here  ', 'Reason here'],
		['a'.repeat(512), 'a'.repeat(512)],
		[null, null],
		[undefined, null],
		['', null],
		['   ', null],
		['a'.repeat(513), null],
	])('normalizes %j', (input, expected) => {
		expect(AuditLogReasonType.parse(input)).toBe(expected);
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ChannelTypeSchema, VanityURLCodeType} from '@fluxer/schema/src/primitives/ChannelValidators';
import {describe, expect, it} from 'vitest';

describe('ChannelTypeSchema', () => {
	it.each(Object.values(ChannelTypes))('accepts channel type %i', (value) => {
		expect(ChannelTypeSchema.parse(value)).toBe(value);
	});

	it.each([9, 13, 14, 17, -1, '0', null])('rejects unknown channel type %j', (value) => {
		expect(ChannelTypeSchema.safeParse(value).success).toBe(false);
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

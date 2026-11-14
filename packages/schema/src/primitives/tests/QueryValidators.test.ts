// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {createQueryIntegerType, QueryBooleanType} from '@fluxer/schema/src/primitives/QueryValidators';
import {describe, expect, it} from 'vitest';

describe('QueryBooleanType', () => {
	it.each([
		['true', true],
		['True', true],
		['1', true],
		['  true  ', true],
		['false', false],
		['0', false],
		['yes', false],
		['TRUE', false],
		['', false],
		[undefined, false],
	])('parses %j as %s', (input, expected) => {
		expect(QueryBooleanType.parse(input)).toBe(expected);
	});

	it.each([null, true, 1])('rejects non-string input %j', (input) => {
		expect(QueryBooleanType.safeParse(input).success).toBe(false);
	});
});

describe('createQueryIntegerType', () => {
	const schema = createQueryIntegerType();

	it.each([
		['42', 42],
		['  42  ', 42],
		['+42', 42],
		['0042', 42],
		['0', 0],
		['2147483647', 2147483647],
		[undefined, 0],
	])('parses the complete decimal integer %j', (input, expected) => {
		expect(schema.parse(input)).toBe(expected);
	});

	it.each(['3.14', '3.0', '42px', '1e2', '0x10', '', '   ', 'NaN', 'Infinity', '-1', '2147483648'])(
		'rejects %j without truncation or coercion',
		(input) => {
			expect(schema.safeParse(input)).toMatchObject({
				success: false,
				error: {issues: [{message: ValidationErrorCodes.VALUE_MUST_BE_INTEGER_IN_RANGE}]},
			});
		},
	);

	it.each(['-5', '5'])('accepts the configured range boundary %s', (input) => {
		expect(createQueryIntegerType({minValue: -5, maxValue: 5}).parse(input)).toBe(Number(input));
	});

	it.each(['-6', '6'])('reports configured bounds for %s', (input) => {
		expect(createQueryIntegerType({minValue: -5, maxValue: 5}).safeParse(input)).toMatchObject({
			success: false,
			error: {
				issues: [
					{
						message: ValidationErrorCodes.VALUE_MUST_BE_INTEGER_IN_RANGE,
						params: {minValue: -5, maxValue: 5},
					},
				],
			},
		});
	});

	it('validates defaults using the same range as supplied values', () => {
		expect(createQueryIntegerType({defaultValue: 10}).parse(undefined)).toBe(10);
		expect(createQueryIntegerType({defaultValue: 10, maxValue: 5}).safeParse(undefined).success).toBe(false);
	});

	it('rejects unsafe integers even when the configured range includes them', () => {
		expect(createQueryIntegerType({maxValue: Number.MAX_VALUE}).safeParse('9007199254740993').success).toBe(false);
	});
});

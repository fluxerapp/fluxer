// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {createQueryIntegerType, DateTimeType, QueryBooleanType} from '@fluxer/schema/src/primitives/QueryValidators';
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

	it.each([
		'3.14',
		'3.0',
		'42px',
		'1e2',
		'0x10',
		'',
		'   ',
		'NaN',
		'Infinity',
		'-1',
		'2147483648',
	])('rejects %j without truncation or coercion', (input) => {
		expect(schema.safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.VALUE_MUST_BE_INTEGER_IN_RANGE}]},
		});
	});

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

describe('DateTimeType', () => {
	it.each([
		['2024-01-15T12:30:00Z', '2024-01-15T12:30:00.000Z'],
		['2024-01-15T12:30:00.123Z', '2024-01-15T12:30:00.123Z'],
		['2024-01-15T12:30:00+05:00', '2024-01-15T07:30:00.000Z'],
		['2024-01-15T12:30:00-0800', '2024-01-15T20:30:00.000Z'],
		['2024-02-29T00:00:00Z', '2024-02-29T00:00:00.000Z'],
		['2000-02-29T00:00:00Z', '2000-02-29T00:00:00.000Z'],
		[1705323000000, '2024-01-15T12:50:00.000Z'],
		[0, '1970-01-01T00:00:00.000Z'],
	])('parses %j to the exact instant', (input, expected) => {
		expect(DateTimeType.parse(input).toISOString()).toBe(expected);
	});

	it('preserves timezone-free timestamps as local time', () => {
		expect(DateTimeType.parse('2024-01-15T12:30:00')).toEqual(new Date(2024, 0, 15, 12, 30));
	});

	it.each([
		'2024-01-15 12:30:00',
		'not-a-date',
		'2023-02-29T00:00:00Z',
		'1900-02-29T00:00:00Z',
		'2024-04-31T00:00:00Z',
		'2024-01-15T25:00:00Z',
		-1,
		8640000000000001,
		1705323000000.5,
		Number.NaN,
		Number.POSITIVE_INFINITY,
	])('rejects invalid calendar dates or timestamps: %j', (input) => {
		expect(DateTimeType.safeParse(input).success).toBe(false);
	});

	it('accepts the maximum JavaScript date', () => {
		expect(DateTimeType.parse(8640000000000000).getTime()).toBe(8640000000000000);
	});
});

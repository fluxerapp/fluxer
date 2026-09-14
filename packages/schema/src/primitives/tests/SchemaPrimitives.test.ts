// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	ColorType,
	coerceNumberFromString,
	createInt32EnumType,
	createStringType,
	createUnboundedStringType,
	Int32Type,
	Int64StringType,
	Int64Type,
	NonNegativeSafeIntegerType,
	normalizeString,
	normalizeWhitespace,
	removeStandaloneSurrogates,
	SnowflakeType,
	stripInvisibles,
	stripVariationSelectors,
	UnsignedInt64Type,
	withFieldDescription,
	withOpenApiType,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {schemaMetadata} from '@fluxer/schema/src/SchemaMetadata';
import {describe, expect, it} from 'vitest';
import {z} from 'zod';

describe('normalizeString', () => {
	it.each([
		['hello\u202Eworld', 'helloworld'],
		['hello\u000Cworld', 'helloworld'],
		['hello\x00\x01\x1B\x7F\u009Bworld', 'hello\x00\x01\x1B\x7F\u009Bworld'],
		['  hello world  ', 'hello world'],
		['', ''],
		['   ', ''],
		['Hello, World!', 'Hello, World!'],
	])('normalizes %j to %j', (input, expected) => {
		expect(normalizeString(input)).toBe(expected);
	});
});

describe('Int64Type', () => {
	it.each([
		['12345', 12345n],
		[12345, 12345n],
		['  +0012345  ', 12345n],
		['-9223372036854775808', -9223372036854775808n],
		['9223372036854775807', 9223372036854775807n],
	])('parses decimal input %j without precision loss', (input, expected) => {
		expect(Int64Type.parse(input)).toBe(expected);
	});

	it.each(['9223372036854775808', '-9223372036854775809'])('rejects out-of-range integer %s', (input) => {
		expect(Int64Type.safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.INTEGER_OUT_OF_INT64_RANGE}]},
		});
	});

	it.each(['not-a-number', '', '   ', '0x10', '0b10', '1e3', '3.14'])('rejects nondecimal input %j', (input) => {
		expect(Int64Type.safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.INVALID_INTEGER_FORMAT}]},
		});
	});

	it.each([
		Number.MAX_SAFE_INTEGER + 1,
		Number.NaN,
		Number.POSITIVE_INFINITY,
		1.5,
	])('rejects lossy or noninteger numeric input %j', (input) => {
		expect(Int64Type.safeParse(input).success).toBe(false);
	});
});

describe('Int64StringType', () => {
	it.each(['12345', '-12345'])('preserves integer string %j', (input) => {
		expect(Int64StringType.parse(input)).toBe(input);
	});
	it.each(['not-a-number', '1.5', ''])('rejects noninteger string %j', (input) => {
		expect(Int64StringType.safeParse(input).success).toBe(false);
	});
});

describe('UnsignedInt64Type', () => {
	it.each([
		['0', 0n],
		['12345', 12345n],
		['9223372036854775807', 9223372036854775807n],
	])('parses unsigned storage integer %j', (input, expected) => {
		expect(UnsignedInt64Type.parse(input)).toBe(expected);
	});

	it.each([
		'-1',
		'+1',
		'9223372036854775808',
		'18446744073709551615',
	])('rejects %s outside unsigned signed-64-bit storage', (input) => {
		expect(UnsignedInt64Type.safeParse(input).success).toBe(false);
	});
});

describe('SnowflakeType', () => {
	it.each([
		['0', 0n],
		[123, 123n],
		['  123  ', 123n],
		['9223372036854775807', 9223372036854775807n],
	])('parses snowflake %j', (input, expected) => {
		expect(SnowflakeType.parse(input)).toBe(expected);
	});

	it.each(['01', '+1', '-1', '0x10', '', '1.5'])('rejects noncanonical snowflake %j', (input) => {
		expect(SnowflakeType.safeParse(input)).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.INVALID_SNOWFLAKE_FORMAT}]},
		});
	});

	it('reports the snowflake-specific range error', () => {
		expect(SnowflakeType.safeParse('9223372036854775808')).toMatchObject({
			success: false,
			error: {issues: [{message: ValidationErrorCodes.SNOWFLAKE_OUT_OF_RANGE}]},
		});
	});
});

describe('ColorType', () => {
	it.each([0xff5500, 0x000000, 0xffffff])('preserves valid color %i', (value) => {
		expect(ColorType.parse(value)).toBe(value);
	});
	it.each([
		[-1, ValidationErrorCodes.COLOR_VALUE_TOO_LOW],
		[0x1000000, ValidationErrorCodes.COLOR_VALUE_TOO_HIGH],
	])('reports the color-specific boundary error for %i', (input, message) => {
		expect(ColorType.safeParse(input)).toMatchObject({success: false, error: {issues: [{message}]}});
	});
	it('rejects fractional colors', () => {
		expect(ColorType.safeParse(123.45).success).toBe(false);
	});
});

describe.each([
	{name: 'Int32Type', schema: Int32Type, maximum: 2147483647},
	{name: 'NonNegativeSafeIntegerType', schema: NonNegativeSafeIntegerType, maximum: Number.MAX_SAFE_INTEGER},
])('$name', ({schema, maximum}) => {
	it.each([0, 1000, maximum])('preserves integer %i', (value) => {
		expect(schema.parse(value)).toBe(value);
	});
	it.each([-1, 1.5, maximum + 1, Number.NaN, Number.POSITIVE_INFINITY])('rejects invalid number %j', (value) => {
		expect(schema.safeParse(value).success).toBe(false);
	});
});

it('preserves safe integers above int32', () => {
	expect(NonNegativeSafeIntegerType.parse(2147483648)).toBe(2147483648);
});

describe('coerceNumberFromString', () => {
	const schema = coerceNumberFromString(z.number().int());
	it.each([
		['50', 50],
		['-42', -42],
		[42, 42],
		['  123  ', 123],
		['+0012', 12],
	])('parses decimal input %j to %i', (input, expected) => {
		expect(schema.parse(input)).toBe(expected);
	});
	it.each(['not-a-number', '', ' ', '1.5', '0x10', '1e3', '9007199254740992'])('rejects invalid input %j', (value) => {
		expect(schema.safeParse(value).success).toBe(false);
	});
	it('leaves overflow rejection to the numeric schema', () => {
		expect(coerceNumberFromString(z.number()).safeParse('9'.repeat(400)).success).toBe(false);
	});
	it('preserves the supplied numeric bounds', () => {
		const bounded = coerceNumberFromString(z.number().int().min(0).max(100));
		expect(bounded.parse('50')).toBe(50);
		expect(bounded.safeParse('101').success).toBe(false);
	});
});

describe('createStringType', () => {
	it.each(['hello', '  hello  ', '  hel\u202Elo\u000C  '])('normalizes before checking length: %j', (input) => {
		expect(createStringType(5, 5).parse(input)).toBe('hello');
	});
	it.each([
		{min: 5, max: 10, input: 'hi', message: ValidationErrorCodes.STRING_LENGTH_INVALID, params: {min: 5, max: 10}},
		{
			min: 1,
			max: 5,
			input: 'hello world',
			message: ValidationErrorCodes.STRING_LENGTH_INVALID,
			params: {min: 1, max: 5},
		},
		{
			min: 5,
			max: 5,
			input: 'hi',
			message: ValidationErrorCodes.STRING_LENGTH_EXACT,
			params: {min: 5, max: 5, length: 5},
		},
	])('reports exact length issue for $input in $min..$max', ({min, max, input, message, params}) => {
		expect(createStringType(min, max).safeParse(input).error?.issues).toEqual([
			{code: 'custom', message, params, path: []},
		]);
	});
});

describe('createUnboundedStringType', () => {
	it('normalizes string without length validation', () => {
		const UnboundedStringType = createUnboundedStringType();
		const result = UnboundedStringType.parse('  hello\x00world  ');
		expect(result).toBe('hello\x00world');
	});
	it('accepts empty strings', () => {
		const UnboundedStringType = createUnboundedStringType();
		const result = UnboundedStringType.parse('');
		expect(result).toBe('');
	});
});

describe('removeStandaloneSurrogates', () => {
	it('preserves valid characters', () => {
		expect(removeStandaloneSurrogates('hello')).toBe('hello');
	});
	it('preserves valid emoji (surrogate pairs)', () => {
		expect(removeStandaloneSurrogates('hello\uD83D\uDE00world')).toBe('hello\uD83D\uDE00world');
	});
	it('removes standalone high surrogates', () => {
		expect(removeStandaloneSurrogates('hello\uD83Dworld')).toBe('helloworld');
	});
	it('removes standalone low surrogates', () => {
		expect(removeStandaloneSurrogates('hello\uDE00world')).toBe('helloworld');
	});
	it('handles empty strings', () => {
		expect(removeStandaloneSurrogates('')).toBe('');
	});
});

describe('normalizeWhitespace', () => {
	it('collapses multiple spaces to single space', () => {
		expect(normalizeWhitespace('hello    world')).toBe('hello world');
	});
	it('normalizes unicode spaces', () => {
		expect(normalizeWhitespace('hello\u00A0world')).toBe('hello world');
	});
	it('trims leading and trailing whitespace', () => {
		expect(normalizeWhitespace('  hello world  ')).toBe('hello world');
	});
	it('handles strings with only whitespace', () => {
		expect(normalizeWhitespace('     ')).toBe('');
	});
	it('throws on excessively long strings', () => {
		const longString = 'a'.repeat(10001);
		expect(() => normalizeWhitespace(longString)).toThrow(ValidationErrorCodes.STRING_LENGTH_INVALID);
	});
});

describe('stripInvisibles', () => {
	it('removes C0 and C1 control characters', () => {
		expect(stripInvisibles('hello\x00\x01\x02world')).toBe('helloworld');
	});
	it('removes zero-width joiner and non-joiner', () => {
		expect(stripInvisibles('hello\u200C\u200Dworld')).toBe('helloworld');
	});
	it('removes word joiner and BOM', () => {
		expect(stripInvisibles('hello\u2060\uFEFFworld')).toBe('helloworld');
	});
	it('removes bidirectional control characters', () => {
		expect(stripInvisibles('hello\u200E\u200F\u202Aworld')).toBe('helloworld');
	});
	it('preserves normal text', () => {
		expect(stripInvisibles('Hello, World!')).toBe('Hello, World!');
	});
	it('throws on excessively long strings', () => {
		const longString = 'a'.repeat(10001);
		expect(() => stripInvisibles(longString)).toThrow(ValidationErrorCodes.STRING_LENGTH_INVALID);
	});
});

describe('stripVariationSelectors', () => {
	it('removes basic variation selectors', () => {
		expect(stripVariationSelectors('hello\uFE0Fworld')).toBe('helloworld');
	});
	it('removes ideographic variation selectors', () => {
		expect(stripVariationSelectors('hello\u{E0100}world')).toBe('helloworld');
	});
	it('preserves normal text', () => {
		expect(stripVariationSelectors('Hello, World!')).toBe('Hello, World!');
	});
	it('throws on excessively long strings', () => {
		const longString = 'a'.repeat(10001);
		expect(() => stripVariationSelectors(longString)).toThrow(ValidationErrorCodes.STRING_LENGTH_INVALID);
	});
});

describe('named enum metadata', () => {
	const schema = createInt32EnumType(
		[
			[0, 'NONE', 'No selection'],
			[2, 'SECOND'],
		],
		'Selection',
		'OriginalSelection',
	);
	it.each([0, 2])('preserves enum member %i', (value) => {
		expect(schema.parse(value)).toBe(value);
	});
	it('retains the custom error for an unrecognized integer', () => {
		expect(schema.safeParse(1).error?.issues).toEqual([{code: 'custom', message: 'Expected one of [0, 2]', path: []}]);
	});
	it.each([-1, 0.5, '0'])('does not loosen integer validation for %j', (value) => {
		expect(schema.safeParse(value).success).toBe(false);
	});
	it('retains enum metadata when describing and renaming a clone', () => {
		const described = withFieldDescription(schema, 'Field selection');
		const renamed = withOpenApiType(described, 'FieldSelection');
		expect(schema.description).toBe('Selection');
		expect(schemaMetadata.get(schema)?.name).toBe('OriginalSelection');
		expect(renamed.description).toBe('Field selection');
		expect(schemaMetadata.get(renamed)).toEqual({
			name: 'FieldSelection',
			format: 'int32',
			enumEntries: [
				{value: 0, name: 'NONE', description: 'No selection'},
				{value: 2, name: 'SECOND'},
			],
		});
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	ColorType,
	coerceNumberFromString,
	createStringType,
	createUnboundedStringType,
	Int32Type,
	Int64StringType,
	Int64Type,
	NonNegativeSafeIntegerType,
	normalizeString,
	normalizeWhitespace,
	removeStandaloneSurrogates,
	stripInvisibles,
	stripVariationSelectors,
	UnsignedInt64Type,
} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {describe, expect, it} from 'vitest';
import {z} from 'zod';

describe('normalizeString', () => {
	it('removes stripped RTL override characters', () => {
		const input = 'hello\u202Eworld';
		expect(normalizeString(input)).toBe('helloworld');
	});
	it('removes stripped form feed characters', () => {
		const input = 'hello\u000Cworld';
		expect(normalizeString(input)).toBe('helloworld');
	});
	it('preserves allowed control characters', () => {
		const input = 'hello\x00\x01\x1B\x7F\u009Bworld';
		expect(normalizeString(input)).toBe(input);
	});
	it('trims whitespace', () => {
		const input = '  hello world  ';
		expect(normalizeString(input)).toBe('hello world');
	});
	it('handles empty strings', () => {
		expect(normalizeString('')).toBe('');
	});
	it('handles strings with only whitespace', () => {
		expect(normalizeString('   ')).toBe('');
	});
	it('preserves normal text', () => {
		const input = 'Hello, World!';
		expect(normalizeString(input)).toBe('Hello, World!');
	});
});

describe('Int64Type', () => {
	it('accepts valid integer strings', () => {
		const result = Int64Type.safeParse('12345');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(12345n);
		}
	});
	it('accepts valid integer numbers', () => {
		const result = Int64Type.safeParse(12345);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(12345n);
		}
	});
	it('accepts negative integers', () => {
		const result = Int64Type.safeParse('-9223372036854775808');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(-9223372036854775808n);
		}
	});
	it('accepts maximum int64 value', () => {
		const result = Int64Type.safeParse('9223372036854775807');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(9223372036854775807n);
		}
	});
	it('rejects values exceeding int64 range', () => {
		const result = Int64Type.safeParse('9223372036854775808');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.INTEGER_OUT_OF_INT64_RANGE);
		}
	});
	it('rejects values below int64 range', () => {
		const result = Int64Type.safeParse('-9223372036854775809');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.INTEGER_OUT_OF_INT64_RANGE);
		}
	});
	it('rejects invalid integer strings', () => {
		const result = Int64Type.safeParse('not-a-number');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.INVALID_INTEGER_FORMAT);
		}
	});
	it('rejects unsafe JavaScript integers', () => {
		const result = Int64Type.safeParse(Number.MAX_SAFE_INTEGER + 1);
		expect(result.success).toBe(false);
	});
	it('handles whitespace in string input', () => {
		const result = Int64Type.safeParse('  12345  ');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(12345n);
		}
	});
});

describe('Int64StringType', () => {
	it('accepts positive integer strings', () => {
		const result = Int64StringType.safeParse('12345');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('12345');
		}
	});
	it('accepts negative integer strings', () => {
		const result = Int64StringType.safeParse('-12345');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('-12345');
		}
	});
	it('rejects non-integer strings', () => {
		const result = Int64StringType.safeParse('not-a-number');
		expect(result.success).toBe(false);
	});
});

describe('UnsignedInt64Type', () => {
	it('accepts positive integer strings', () => {
		const result = UnsignedInt64Type.safeParse('12345');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(12345n);
		}
	});
	it('rejects negative integer strings', () => {
		const result = UnsignedInt64Type.safeParse('-12345');
		expect(result.success).toBe(false);
	});
});

describe('ColorType', () => {
	it('accepts valid color values', () => {
		const result = ColorType.safeParse(0xff5500);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(0xff5500);
		}
	});
	it('accepts minimum color value (black)', () => {
		const result = ColorType.safeParse(0x000000);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(0x000000);
		}
	});
	it('accepts maximum color value (white)', () => {
		const result = ColorType.safeParse(0xffffff);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(0xffffff);
		}
	});
	it('rejects negative color values', () => {
		const result = ColorType.safeParse(-1);
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.COLOR_VALUE_TOO_LOW);
		}
	});
	it('rejects color values exceeding max', () => {
		const result = ColorType.safeParse(0x1000000);
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.COLOR_VALUE_TOO_HIGH);
		}
	});
	it('rejects non-integer values', () => {
		const result = ColorType.safeParse(123.45);
		expect(result.success).toBe(false);
	});
});

describe('Int32Type', () => {
	it('accepts valid int32 values', () => {
		const result = Int32Type.safeParse(1000);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(1000);
		}
	});
	it('accepts zero', () => {
		const result = Int32Type.safeParse(0);
		expect(result.success).toBe(true);
	});
	it('accepts maximum int32 value', () => {
		const result = Int32Type.safeParse(2147483647);
		expect(result.success).toBe(true);
	});
	it('rejects negative values', () => {
		const result = Int32Type.safeParse(-1);
		expect(result.success).toBe(false);
	});
	it('rejects values exceeding int32 max', () => {
		const result = Int32Type.safeParse(2147483648);
		expect(result.success).toBe(false);
	});
});

describe('NonNegativeSafeIntegerType', () => {
	it('accepts maximum JavaScript safe integer', () => {
		const result = NonNegativeSafeIntegerType.safeParse(Number.MAX_SAFE_INTEGER);
		expect(result.success).toBe(true);
	});
	it('accepts values above int32', () => {
		const result = NonNegativeSafeIntegerType.safeParse(2147483648);
		expect(result.success).toBe(true);
	});
	it('rejects unsafe JavaScript integers', () => {
		const result = NonNegativeSafeIntegerType.safeParse(Number.MAX_SAFE_INTEGER + 1);
		expect(result.success).toBe(false);
	});
	it('rejects negative values', () => {
		const result = NonNegativeSafeIntegerType.safeParse(-1);
		expect(result.success).toBe(false);
	});
	it('rejects non-integer values', () => {
		const result = NonNegativeSafeIntegerType.safeParse(1.5);
		expect(result.success).toBe(false);
	});
});

describe('coerceNumberFromString', () => {
	it('coerces valid integer strings to numbers', () => {
		const schema = coerceNumberFromString(z.number().int().min(0).max(100));
		const result = schema.safeParse('50');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(50);
		}
	});
	it('coerces negative integer strings', () => {
		const schema = coerceNumberFromString(z.number().int());
		const result = schema.safeParse('-42');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(-42);
		}
	});
	it('passes through numbers unchanged', () => {
		const schema = coerceNumberFromString(z.number().int());
		const result = schema.safeParse(42);
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(42);
		}
	});
	it('does not coerce non-integer strings', () => {
		const schema = coerceNumberFromString(z.number().int());
		const result = schema.safeParse('not-a-number');
		expect(result.success).toBe(false);
	});
	it('handles empty strings', () => {
		const schema = coerceNumberFromString(z.number().int());
		const result = schema.safeParse('');
		expect(result.success).toBe(false);
	});
	it('handles whitespace trimming', () => {
		const schema = coerceNumberFromString(z.number().int());
		const result = schema.safeParse('  123  ');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe(123);
		}
	});
});

describe('createStringType', () => {
	it('validates string within length bounds', () => {
		const StringType = createStringType(1, 10);
		const result = StringType.safeParse('hello');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('hello');
		}
	});
	it('normalizes and trims input', () => {
		const StringType = createStringType(1, 10);
		const result = StringType.safeParse('  hello  ');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('hello');
		}
	});
	it('rejects strings shorter than minimum', () => {
		const StringType = createStringType(5, 10);
		const result = StringType.safeParse('hi');
		expect(result.success).toBe(false);
	});
	it('rejects strings longer than maximum', () => {
		const StringType = createStringType(1, 5);
		const result = StringType.safeParse('hello world');
		expect(result.success).toBe(false);
	});
	it('uses STRING_LENGTH_EXACT for exact length requirement', () => {
		const StringType = createStringType(5, 5);
		const result = StringType.safeParse('hi');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.STRING_LENGTH_EXACT);
		}
	});
	it('uses STRING_LENGTH_INVALID for range requirement', () => {
		const StringType = createStringType(5, 10);
		const result = StringType.safeParse('hi');
		expect(result.success).toBe(false);
		if (!result.success) {
			expect(result.error.issues[0].message).toBe(ValidationErrorCodes.STRING_LENGTH_INVALID);
		}
	});
});

describe('createUnboundedStringType', () => {
	it('normalizes string without length validation', () => {
		const UnboundedStringType = createUnboundedStringType();
		const result = UnboundedStringType.safeParse('  hello\x00world  ');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('hello\x00world');
		}
	});
	it('accepts empty strings', () => {
		const UnboundedStringType = createUnboundedStringType();
		const result = UnboundedStringType.safeParse('');
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data).toBe('');
		}
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

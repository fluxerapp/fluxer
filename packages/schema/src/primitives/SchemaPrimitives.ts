// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {ZodTypeAny} from 'zod';
import {z} from 'zod';

export function withOpenApiType<T extends ZodTypeAny>(schema: T, typeName: string): T {
	(schema as Record<string, unknown>).__fluxer_custom_type__ = typeName;
	return schema;
}

export function withFieldDescription<T extends z.ZodTypeAny>(schema: T, fieldDescription: string): T {
	const currentDesc = schema.description ?? '';
	const newDesc = currentDesc ? `${currentDesc}|fieldDesc:${fieldDescription}` : `|fieldDesc:${fieldDescription}`;
	return schema.describe(newDesc) as T;
}

const MESSAGE_REMOVED_FORMAT_REGEX = /\u202E/g;
// biome-ignore lint/complexity/useRegexLiterals: The literal form trips noControlCharactersInRegex for form feed.
const MESSAGE_REMOVED_CONTROL_REGEX = new RegExp('\\u000C', 'g');
export const MAX_STRING_PROCESSING_LENGTH = 10000;

export function normalizeString(value: string): string {
	return value.replace(MESSAGE_REMOVED_CONTROL_REGEX, '').replace(MESSAGE_REMOVED_FORMAT_REGEX, '').trim();
}

export const Int64Type = z
	.union([z.string(), z.number().int()])
	.transform((value, ctx) => {
		if (typeof value === 'number' && !Number.isSafeInteger(value)) {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.INVALID_INTEGER_FORMAT,
			});
			return z.NEVER;
		}
		const normalized = typeof value === 'number' ? value.toString() : value;
		const trimmed = normalized.trim();
		try {
			const bigInt = BigInt(trimmed);
			if (bigInt < -9223372036854775808n || bigInt > 9223372036854775807n) {
				ctx.addIssue({
					code: 'custom',
					message: ValidationErrorCodes.INTEGER_OUT_OF_INT64_RANGE,
				});
				return z.NEVER;
			}
			return bigInt;
		} catch {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.INVALID_INTEGER_FORMAT,
			});
			return z.NEVER;
		}
	})
	.describe('fluxer:Int64Type');
export const UnsignedInt64Type = z
	.union([z.string(), z.number().int()])
	.transform((value, ctx) => {
		if (typeof value === 'number' && !Number.isSafeInteger(value)) {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.INVALID_INTEGER_FORMAT,
			});
			return z.NEVER;
		}
		const normalized = typeof value === 'number' ? value.toString() : value;
		const trimmed = normalized.trim();
		if (!/^\d+$/.test(trimmed)) {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.INVALID_INTEGER_FORMAT,
			});
			return z.NEVER;
		}
		try {
			const bigInt = BigInt(trimmed);
			if (bigInt < 0n || bigInt > 9223372036854775807n) {
				ctx.addIssue({
					code: 'custom',
					message: ValidationErrorCodes.INTEGER_OUT_OF_INT64_RANGE,
				});
				return z.NEVER;
			}
			return bigInt;
		} catch {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.INVALID_INTEGER_FORMAT,
			});
			return z.NEVER;
		}
	})
	.describe('fluxer:UnsignedInt64Type');
export const Int64StringType = z
	.string()
	.regex(/^-?\d+$/)
	.describe('fluxer:Int64StringType');
const SNOWFLAKE_REGEX = /^(0|[1-9][0-9]*)$/;
const UNSIGNED_INT64_STRING_REGEX = /^\d+$/;
const MAX_UINT64_VALUE = 18446744073709551615n;
export const UnsignedInt64StringType = z
	.string()
	.regex(UNSIGNED_INT64_STRING_REGEX)
	.superRefine((value, ctx) => {
		try {
			if (BigInt(value) > MAX_UINT64_VALUE) {
				ctx.addIssue({
					code: 'custom',
					message: ValidationErrorCodes.INTEGER_OUT_OF_INT64_RANGE,
				});
			}
		} catch {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.INVALID_INTEGER_FORMAT,
			});
		}
	})
	.describe('fluxer:UnsignedInt64StringType');
export const SnowflakeStringType = z.string().regex(SNOWFLAKE_REGEX).describe('fluxer:SnowflakeStringType');
const BitflagStringType = z.string().regex(UNSIGNED_INT64_STRING_REGEX).describe('fluxer:BitflagStringType');
const HEX_STRING_16_REGEX = /^[a-f0-9]{16}$/;
export const HexString16Type = z.string().regex(HEX_STRING_16_REGEX).describe('fluxer:HexString16Type');
const HEX_STRING_32_REGEX = /^[a-f0-9]{32}$/;
export const HexString32Type = z.string().regex(HEX_STRING_32_REGEX).describe('fluxer:HexString32Type');
export const SnowflakeType = z
	.union([z.string(), z.number().int()])
	.transform((value, ctx) => {
		if (typeof value === 'number' && !Number.isSafeInteger(value)) {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.INVALID_SNOWFLAKE_FORMAT,
			});
			return z.NEVER;
		}
		const normalized = typeof value === 'number' ? value.toString() : value;
		const trimmed = normalized.trim();
		if (!SNOWFLAKE_REGEX.test(trimmed)) {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.INVALID_SNOWFLAKE_FORMAT,
			});
			return z.NEVER;
		}
		try {
			const bigInt = BigInt(trimmed);
			if (bigInt < 0n || bigInt > 9223372036854775807n) {
				ctx.addIssue({
					code: 'custom',
					message: ValidationErrorCodes.SNOWFLAKE_OUT_OF_RANGE,
				});
				return z.NEVER;
			}
			return bigInt;
		} catch {
			ctx.addIssue({
				code: 'custom',
				message: ValidationErrorCodes.INVALID_SNOWFLAKE_FORMAT,
			});
			return z.NEVER;
		}
	})
	.describe('fluxer:SnowflakeType');
export const ColorType = z
	.number()
	.int()
	.min(0x000000, ValidationErrorCodes.COLOR_VALUE_TOO_LOW)
	.max(0xffffff, ValidationErrorCodes.COLOR_VALUE_TOO_HIGH)
	.describe('fluxer:ColorType');
export const Int32Type = z.number().int().min(0).max(2147483647).describe('fluxer:Int32Type');
export const SignedInt32Type = z.number().int().min(-2147483648).max(2147483647).describe('fluxer:SignedInt32Type');
export const NonNegativeSafeIntegerType = z
	.number()
	.int()
	.min(0)
	.max(Number.MAX_SAFE_INTEGER)
	.describe('fluxer:NonNegativeSafeIntegerType');
const INTEGER_STRING_REGEX = /^[+-]?\d+$/;

function coerceNumericStringToNumber(value: unknown): unknown {
	if (typeof value !== 'string') {
		return value;
	}
	const trimmed = value.trim();
	if (trimmed.length === 0 || !INTEGER_STRING_REGEX.test(trimmed)) {
		return value;
	}
	const parsed = Number(trimmed);
	return Number.isNaN(parsed) ? value : parsed;
}

export function coerceNumberFromString<T extends z.ZodNumber>(schema: T) {
	return z.preprocess((value) => coerceNumericStringToNumber(value), schema);
}

export function withStringLengthRangeValidation(
	schema: z.ZodString,
	minLength: number,
	maxLength: number,
	errorCode: string,
) {
	return schema.superRefine((value, ctx) => {
		if (value.length < minLength || value.length > maxLength) {
			const params: Record<string, unknown> = {min: minLength, max: maxLength};
			if (minLength === maxLength) {
				params.length = minLength;
			}
			ctx.addIssue({code: 'custom', message: errorCode, params});
		}
	});
}

export function createStringType(minLength = 1, maxLength = 256) {
	const errorMessage =
		minLength === maxLength ? ValidationErrorCodes.STRING_LENGTH_EXACT : ValidationErrorCodes.STRING_LENGTH_INVALID;
	return z
		.string()
		.transform(normalizeString)
		.pipe(withStringLengthRangeValidation(z.string(), minLength, maxLength, errorMessage));
}

export function createUnboundedStringType() {
	return z.string().transform(normalizeString);
}

// biome-ignore lint/complexity/useRegexLiterals: The literal form trips noControlCharactersInRegex for C0/C1 controls.
const C0_C1_CTRL_REGEX = new RegExp('[\\u0000-\\u0008\\u000B\\u000C\\u000E-\\u001F\\u007F\\u0080-\\u009F]', 'g');
const JOIN_CONTROLS_REGEX = /(?:\u200C|\u200D)/g;
const WJ_BOM_REGEX = /(?:\u2060|\uFEFF)/g;
const BIDI_CTRL_REGEX = /[\u200E\u200F\u202A-\u202E\u2066-\u2069]/g;
const MISC_INVISIBLES_REGEX = /[\u00AD\u180E\uFFFE\uFFFF]/g;
const TAG_CHARS_REGEX = /[\u{E0000}-\u{E007F}]/gu;
const VARIATION_SELECTORS_BASIC = /[\uFE00-\uFE0F]/g;
const VARIATION_SELECTORS_IDEOGRAPHIC = /[\u{E0100}-\u{E01EF}]/gu;
const UNICODE_SPACES_REGEX = /[\s\u00A0\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000]/g;

export function removeStandaloneSurrogates(value: string): string {
	return Array.from(value)
		.filter((char) => {
			if (char.length > 1) {
				return true;
			}
			const codePoint = char.codePointAt(0);
			if (codePoint === undefined) {
				return false;
			}
			return codePoint < 0xd800 || codePoint > 0xdfff;
		})
		.join('');
}

export function normalizeWhitespace(s: string): string {
	if (s.length > MAX_STRING_PROCESSING_LENGTH) {
		throw new Error(ValidationErrorCodes.STRING_LENGTH_INVALID);
	}
	return s.replace(UNICODE_SPACES_REGEX, ' ').replace(/\s+/g, ' ').trim();
}

export function stripInvisibles(s: string): string {
	if (s.length > MAX_STRING_PROCESSING_LENGTH) {
		throw new Error(ValidationErrorCodes.STRING_LENGTH_INVALID);
	}
	return s
		.replace(C0_C1_CTRL_REGEX, '')
		.replace(JOIN_CONTROLS_REGEX, '')
		.replace(WJ_BOM_REGEX, '')
		.replace(BIDI_CTRL_REGEX, '')
		.replace(MISC_INVISIBLES_REGEX, '')
		.replace(TAG_CHARS_REGEX, '');
}

export function stripVariationSelectors(s: string): string {
	if (s.length > MAX_STRING_PROCESSING_LENGTH) {
		throw new Error(ValidationErrorCodes.STRING_LENGTH_INVALID);
	}
	return s.replace(VARIATION_SELECTORS_BASIC, '').replace(VARIATION_SELECTORS_IDEOGRAPHIC, '');
}

interface EnumEntryJson {
	n: string;
	v: string | number;
	d?: string;
}

export function createNamedLiteral<T extends number>(value: T, name: string, description?: string) {
	const entry: EnumEntryJson = {n: name, v: value};
	if (description) entry.d = description;
	return z.literal(value).describe(`fluxer:EnumValue:${JSON.stringify(entry)}`);
}

export function createNamedLiteralUnion<T extends number>(
	pairs: ReadonlyArray<readonly [T, string] | readonly [T, string, string?]>,
	description?: string,
) {
	const literals = pairs.map(([value]) => z.literal(value));
	const entries: Array<EnumEntryJson> = pairs.map(([value, name, desc]) => {
		const entry: EnumEntryJson = {n: name, v: value};
		if (desc) entry.d = desc;
		return entry;
	});
	const descPart = description ? ` ${description}` : '';
	return z
		.union(literals as [z.ZodLiteral<T>, z.ZodLiteral<T>, ...Array<z.ZodLiteral<T>>])
		.describe(`fluxer:EnumValues:${JSON.stringify(entries)}${descPart}`);
}

export function createNamedStringLiteralUnion<T extends string>(
	pairs: ReadonlyArray<readonly [T, string] | readonly [T, string, string?]>,
	description?: string,
) {
	const literals = pairs.map(([value]) => z.literal(value));
	const entries: Array<EnumEntryJson> = pairs.map(([value, name, desc]) => {
		const entry: EnumEntryJson = {n: name, v: value};
		if (desc) entry.d = desc;
		return entry;
	});
	const descPart = description ? ` ${description}` : '';
	return z
		.union(literals as [z.ZodLiteral<T>, z.ZodLiteral<T>, ...Array<z.ZodLiteral<T>>])
		.describe(`fluxer:EnumValues:${JSON.stringify(entries)}${descPart}`);
}

export function createNamedObject<T extends z.ZodRawShape>(typeName: string, shape: T, description?: string) {
	const descPart = description ? ` ${description}` : '';
	return z.object(shape).describe(`fluxer:NamedObject:${typeName}${descPart}`);
}

type FlexibleStringLiteralUnionOperand<T extends string> = z.ZodLiteral<T> | z.ZodString;
type FlexibleStringLiteralUnionOperands<T extends string> = [
	z.ZodLiteral<T>,
	z.ZodLiteral<T>,
	...Array<FlexibleStringLiteralUnionOperand<T>>,
];

function createFlexibleStringLiteralUnionOperands<T extends string>(
	literals: Array<z.ZodLiteral<T>>,
): FlexibleStringLiteralUnionOperands<T> {
	const [first, second, ...rest] = literals;
	if (!first || !second) {
		throw new Error('createFlexibleStringLiteralUnion requires at least two literals');
	}
	return [first, second, ...rest, z.string()];
}

export function createFlexibleStringLiteralUnion<T extends string>(
	pairs: ReadonlyArray<readonly [T, string] | readonly [T, string, string?]>,
	description?: string,
) {
	const literals = pairs.map(([value]) => z.literal(value));
	const entries: Array<EnumEntryJson> = pairs.map(([value, name, desc]) => {
		const entry: EnumEntryJson = {n: name, v: value};
		if (desc) entry.d = desc;
		return entry;
	});
	const descPart = description ? ` ${description}` : '';
	const flexibleUnionOperands = createFlexibleStringLiteralUnionOperands(literals);
	return z.union(flexibleUnionOperands).describe(`fluxer:FlexibleEnumValues:${JSON.stringify(entries)}${descPart}`);
}

export function createInt32EnumType<T extends number>(
	pairs: ReadonlyArray<readonly [T, string] | readonly [T, string, string?]>,
	description?: string,
	typeName?: string,
) {
	const entries: Array<EnumEntryJson> = pairs.map(([value, name, desc]) => {
		const entry: EnumEntryJson = {n: name, v: value};
		if (desc) entry.d = desc;
		return entry;
	});
	const allowed = new Set<number>(pairs.map(([value]) => value));
	const typeNamePart = typeName ? `:${typeName}` : '';
	const descPart = description ? ` ${description}` : '';
	return Int32Type.refine((value): value is T => allowed.has(value), {
		message: `Expected one of [${[...allowed].join(', ')}]`,
	}).describe(`fluxer:Int32Enum${typeNamePart}:${JSON.stringify(entries)}${descPart}`);
}

type BitflagConstantsObject = Readonly<Record<string, number | bigint>>;
type BitflagDescriptionsObject<T extends BitflagConstantsObject> = Readonly<Partial<Record<keyof T, string>>>;

interface BitflagEntryJson {
	n: string;
	v: string;
	d?: string;
}

function formatBitflagAnnotation<T extends BitflagConstantsObject>(
	constants: T,
	descriptions?: BitflagDescriptionsObject<T>,
): string {
	const entries: Array<BitflagEntryJson> = Object.entries(constants)
		.filter(([, value]) => typeof value === 'number' || typeof value === 'bigint')
		.map(([name, value]) => {
			const desc = descriptions?.[name as keyof T];
			const entry: BitflagEntryJson = {n: name, v: value.toString()};
			if (desc) entry.d = desc;
			return entry;
		});
	return JSON.stringify(entries);
}

export function createBitflagStringType<T extends BitflagConstantsObject>(
	constants: T,
	descriptionOrDescriptions?: string | BitflagDescriptionsObject<T>,
	description?: string,
	typeName?: string,
) {
	const descriptions = typeof descriptionOrDescriptions === 'object' ? descriptionOrDescriptions : undefined;
	const overallDescription = typeof descriptionOrDescriptions === 'string' ? descriptionOrDescriptions : description;
	const annotation = formatBitflagAnnotation(constants, descriptions);
	const typeNamePart = typeName ? `:${typeName}` : '';
	const descPart = overallDescription ? ` ${overallDescription}` : '';
	return BitflagStringType.describe(`fluxer:Bitflags64${typeNamePart}:${annotation}${descPart}`);
}

export function createBitflagInt32Type<T extends BitflagConstantsObject>(
	constants: T,
	descriptionOrDescriptions?: string | BitflagDescriptionsObject<T>,
	description?: string,
	typeName?: string,
) {
	const descriptions = typeof descriptionOrDescriptions === 'object' ? descriptionOrDescriptions : undefined;
	const overallDescription = typeof descriptionOrDescriptions === 'string' ? descriptionOrDescriptions : description;
	const annotation = formatBitflagAnnotation(constants, descriptions);
	const typeNamePart = typeName ? `:${typeName}` : '';
	const descPart = overallDescription ? ` ${overallDescription}` : '';
	return Int32Type.describe(`fluxer:Bitflags32${typeNamePart}:${annotation}${descPart}`);
}

export function createPermissionStringType<T extends BitflagConstantsObject>(
	constants: T,
	descriptionOrDescriptions?: string | BitflagDescriptionsObject<T>,
	description?: string,
	typeName?: string,
) {
	const descriptions = typeof descriptionOrDescriptions === 'object' ? descriptionOrDescriptions : undefined;
	const overallDescription = typeof descriptionOrDescriptions === 'string' ? descriptionOrDescriptions : description;
	const annotation = formatBitflagAnnotation(constants, descriptions);
	const typeNamePart = typeName ? `:${typeName}` : '';
	const descPart = overallDescription ? ` ${overallDescription}` : '';
	return z
		.string()
		.regex(UNSIGNED_INT64_STRING_REGEX)
		.describe(`fluxer:Permissions${typeNamePart}:${annotation}${descPart}`);
}

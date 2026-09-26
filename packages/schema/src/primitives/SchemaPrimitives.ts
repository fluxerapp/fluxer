// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {type BitflagEntry, type EnumEntry, schemaMetadata, withSchemaMetadata} from '@fluxer/schema/src/SchemaMetadata';
import {z} from 'zod';

export function withOpenApiType<T extends z.ZodType>(schema: T, typeName: string): T {
	return withSchemaMetadata(schema, {...schemaMetadata.get(schema), name: typeName});
}

export function withFieldDescription<T extends z.ZodType>(schema: T, fieldDescription: string): T {
	return withSchemaMetadata(schema.describe(fieldDescription), schemaMetadata.get(schema) ?? {});
}

const MESSAGE_REMOVED_FORMAT_REGEX = /\u202E/g;
// biome-ignore lint/complexity/useRegexLiterals: The literal form trips noControlCharactersInRegex for form feed.
const MESSAGE_REMOVED_CONTROL_REGEX = new RegExp('\\u000C', 'g');
export const MAX_STRING_PROCESSING_LENGTH = 10000;

export function normalizeString(value: string): string {
	return value.replace(MESSAGE_REMOVED_CONTROL_REGEX, '').replace(MESSAGE_REMOVED_FORMAT_REGEX, '').trim();
}

const MIN_INT64_VALUE = -9223372036854775808n;
const MAX_INT64_VALUE = 9223372036854775807n;
const INTEGER_STRING_REGEX = /^[+-]?\d+$/;
const SNOWFLAKE_REGEX = /^(0|[1-9][0-9]*)$/;
const UNSIGNED_INT64_STRING_REGEX = /^\d+$/;
const MAX_UINT64_VALUE = 18446744073709551615n;

interface BigIntTypeOptions {
	minimum: bigint;
	maximum: bigint;
	pattern: RegExp;
	invalidFormatCode: string;
	outOfRangeCode: string;
}

function createBigIntType({minimum, maximum, pattern, invalidFormatCode, outOfRangeCode}: BigIntTypeOptions) {
	return z.union([z.string(), z.number().int()]).transform((value, ctx) => {
		const normalized = String(value).trim();
		if (!pattern.test(normalized)) {
			ctx.addIssue({code: 'custom', message: invalidFormatCode});
			return z.NEVER;
		}
		const parsed = BigInt(normalized);
		if (parsed < minimum || parsed > maximum) {
			ctx.addIssue({code: 'custom', message: outOfRangeCode});
			return z.NEVER;
		}
		return parsed;
	});
}

export const Int64Type = createBigIntType({
	minimum: MIN_INT64_VALUE,
	maximum: MAX_INT64_VALUE,
	pattern: INTEGER_STRING_REGEX,
	invalidFormatCode: ValidationErrorCodes.INVALID_INTEGER_FORMAT,
	outOfRangeCode: ValidationErrorCodes.INTEGER_OUT_OF_INT64_RANGE,
}).register(schemaMetadata, {name: 'Int64Type', format: 'int64'});
export const UnsignedInt64Type = createBigIntType({
	minimum: 0n,
	maximum: MAX_INT64_VALUE,
	pattern: UNSIGNED_INT64_STRING_REGEX,
	invalidFormatCode: ValidationErrorCodes.INVALID_INTEGER_FORMAT,
	outOfRangeCode: ValidationErrorCodes.INTEGER_OUT_OF_INT64_RANGE,
}).register(schemaMetadata, {name: 'UnsignedInt64Type', format: 'int64'});
export const Int64StringType = z
	.string()
	.regex(/^-?\d+$/)
	.register(schemaMetadata, {name: 'Int64StringType', format: 'int64'});
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
	.register(schemaMetadata, {name: 'UnsignedInt64StringType', format: 'int64'});
export const SnowflakeStringType = z
	.string()
	.regex(SNOWFLAKE_REGEX)
	.register(schemaMetadata, {name: 'SnowflakeStringType', format: 'snowflake'});
const BitflagStringType = z
	.string()
	.regex(UNSIGNED_INT64_STRING_REGEX)
	.register(schemaMetadata, {name: 'BitflagStringType'});
const HEX_STRING_16_REGEX = /^[a-f0-9]{16}$/;
export const HexString16Type = z
	.string()
	.regex(HEX_STRING_16_REGEX)
	.register(schemaMetadata, {name: 'HexString16Type'});
const HEX_STRING_32_REGEX = /^[a-f0-9]{32}$/;
export const HexString32Type = z
	.string()
	.regex(HEX_STRING_32_REGEX)
	.register(schemaMetadata, {name: 'HexString32Type'});
export const SnowflakeType = createBigIntType({
	minimum: 0n,
	maximum: MAX_INT64_VALUE,
	pattern: SNOWFLAKE_REGEX,
	invalidFormatCode: ValidationErrorCodes.INVALID_SNOWFLAKE_FORMAT,
	outOfRangeCode: ValidationErrorCodes.SNOWFLAKE_OUT_OF_RANGE,
}).register(schemaMetadata, {name: 'SnowflakeType', format: 'snowflake'});
export const ColorType = z
	.number()
	.int()
	.min(0x000000, ValidationErrorCodes.COLOR_VALUE_TOO_LOW)
	.max(0xffffff, ValidationErrorCodes.COLOR_VALUE_TOO_HIGH)
	.register(schemaMetadata, {name: 'ColorType'});
export const Int32Type = z
	.number()
	.int()
	.min(0)
	.max(2147483647)
	.register(schemaMetadata, {name: 'Int32Type', format: 'int32'});
export const SignedInt32Type = z
	.number()
	.int()
	.min(-2147483648)
	.max(2147483647)
	.register(schemaMetadata, {name: 'SignedInt32Type', format: 'int32'});
export const NonNegativeSafeIntegerType = z
	.number()
	.int()
	.min(0)
	.max(Number.MAX_SAFE_INTEGER)
	.register(schemaMetadata, {name: 'NonNegativeSafeIntegerType'});

function coerceNumericStringToNumber(value: unknown): unknown {
	if (typeof value !== 'string') {
		return value;
	}
	const trimmed = value.trim();
	if (trimmed.length === 0 || !INTEGER_STRING_REGEX.test(trimmed)) {
		return value;
	}
	return Number(trimmed);
}

export function coerceNumberFromString<T extends z.ZodType<number, number>>(schema: T) {
	return z.preprocess(coerceNumericStringToNumber, schema);
}

export function withStringLengthRangeValidation<T extends z.ZodType<string>>(
	schema: T,
	minLength: number,
	maxLength: number,
	errorCode: string,
) {
	return schema
		.superRefine((value, ctx) => {
			if (value.length < minLength || value.length > maxLength) {
				const params: Record<string, unknown> = {min: minLength, max: maxLength};
				if (minLength === maxLength) {
					params.length = minLength;
				}
				ctx.addIssue({code: 'custom', message: errorCode, params});
			}
		})
		.meta({...(minLength > 0 ? {minLength} : {}), maxLength});
}

export function createStringType(minLength = 1, maxLength = 256) {
	const errorMessage =
		minLength === maxLength ? ValidationErrorCodes.STRING_LENGTH_EXACT : ValidationErrorCodes.STRING_LENGTH_INVALID;
	return z
		.string()
		.overwrite(normalizeString)
		.pipe(withStringLengthRangeValidation(z.string(), minLength, maxLength, errorMessage));
}

export function createUnboundedStringType() {
	return z.string().overwrite(normalizeString);
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

type NamedLiteralPairs<T extends string | number> = ReadonlyArray<readonly [T, string, string?]>;

function getEnumEntries<T extends string | number>(pairs: NamedLiteralPairs<T>): Array<EnumEntry> {
	return pairs.map(([value, name, description]) => ({name, value, ...(description ? {description} : {})}));
}

export function createNamedLiteral<T extends number>(value: T, name: string, description?: string) {
	return withSchemaMetadata(z.literal(value), {enumEntries: [{name, value, ...(description ? {description} : {})}]});
}

function createNamedUnion<T extends string | number>(pairs: NamedLiteralPairs<T>, description?: string) {
	if (pairs.length < 2) {
		throw new Error('Named literal unions require at least two values');
	}
	return withSchemaMetadata(z.union(pairs.map(([value]) => z.literal(value))).describe(description ?? ''), {
		enumEntries: getEnumEntries(pairs),
	});
}

export function createNamedLiteralUnion<T extends number>(pairs: NamedLiteralPairs<T>, description?: string) {
	return createNamedUnion(pairs, description);
}

export function createNamedStringLiteralUnion<T extends string>(pairs: NamedLiteralPairs<T>, description?: string) {
	return createNamedUnion(pairs, description);
}

export function createNamedObject<T extends z.ZodRawShape>(typeName: string, shape: T, description?: string) {
	return z
		.object(shape)
		.describe(description ?? '')
		.register(schemaMetadata, {name: typeName});
}

export function createFlexibleStringLiteralUnion<T extends string>(pairs: NamedLiteralPairs<T>, description?: string) {
	if (pairs.length < 2) {
		throw new Error('Flexible string literal unions require at least two values');
	}
	return withSchemaMetadata(
		z.union([...pairs.map(([value]) => z.literal(value)), z.string()]).describe(description ?? ''),
		{
			enumEntries: getEnumEntries(pairs),
			openEnum: true,
		},
	);
}

export function createInt32EnumType<T extends number>(
	pairs: NamedLiteralPairs<T>,
	description?: string,
	typeName?: string,
) {
	const values = pairs.map(([value]) => value);
	const allowed = new Set<number>(values);
	return Int32Type.refine((value) => allowed.has(value), {
		error: `Expected one of [${[...allowed].join(', ')}]`,
	})
		.pipe(z.literal(values))
		.describe(description ?? '')
		.register(schemaMetadata, {
			name: typeName,
			enumEntries: getEnumEntries(pairs),
			format: 'int32',
		});
}

type BitflagConstantsObject = Readonly<Record<string, number | bigint>>;
type BitflagDescriptionsObject<T extends BitflagConstantsObject> = Readonly<Partial<Record<keyof T, string>>>;

function createBitflagType<TSchema extends z.ZodType, T extends BitflagConstantsObject>(
	schema: TSchema,
	constants: T,
	descriptionOrDescriptions?: string | BitflagDescriptionsObject<T>,
	description?: string,
	typeName?: string,
) {
	const descriptions = typeof descriptionOrDescriptions === 'object' ? descriptionOrDescriptions : undefined;
	const overallDescription = typeof descriptionOrDescriptions === 'string' ? descriptionOrDescriptions : description;
	const bitflagValues: Array<BitflagEntry> = Object.entries(constants).map(([name, value]) => ({
		name,
		value: value.toString(),
		...(descriptions?.[name as keyof T] ? {description: descriptions[name as keyof T]} : {}),
	}));
	return withSchemaMetadata(schema.describe(overallDescription ?? ''), {
		name: typeName,
		bitflagValues,
		format: schema instanceof z.ZodNumber ? 'int32' : 'int64',
	});
}

export function createBitflagStringType<T extends BitflagConstantsObject>(
	constants: T,
	descriptionOrDescriptions?: string | BitflagDescriptionsObject<T>,
	description?: string,
	typeName?: string,
) {
	return createBitflagType(BitflagStringType, constants, descriptionOrDescriptions, description, typeName);
}

export function createBitflagInt32Type<T extends BitflagConstantsObject>(
	constants: T,
	descriptionOrDescriptions?: string | BitflagDescriptionsObject<T>,
	description?: string,
	typeName?: string,
) {
	return createBitflagType(Int32Type, constants, descriptionOrDescriptions, description, typeName);
}

export function createPermissionStringType<T extends BitflagConstantsObject>(
	constants: T,
	descriptionOrDescriptions?: string | BitflagDescriptionsObject<T>,
	description?: string,
	typeName?: string,
) {
	return createBitflagType(BitflagStringType, constants, descriptionOrDescriptions, description, typeName);
}

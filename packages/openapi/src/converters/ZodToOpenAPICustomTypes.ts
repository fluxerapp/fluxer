// SPDX-License-Identifier: AGPL-3.0-or-later
import {
	Base64ImageTypeRef,
	DiscriminatorTypeRef,
	EmailTypeRef,
	Int32TypeRef,
	Int64StringTypeRef,
	Int64TypeRef,
	LocaleRef,
	PasswordTypeRef,
	PhoneNumberTypeRef,
	SnowflakeTypeRef,
	UnsignedInt64TypeRef,
	UsernameTypeRef,
} from '@fluxer/openapi/src/converters/BuiltInSchemas';
import {applyEnumEntryExtensions, setBitflagValues} from '@fluxer/openapi/src/converters/OpenAPIExtensions';
import {getCustomTypeMetadata, getZodParent} from '@fluxer/openapi/src/converters/ZodInternals';
import {
	type FluxerTypeAnnotation,
	parseFluxerTypeAnnotation,
} from '@fluxer/openapi/src/converters/ZodToOpenAPIAnnotationParser';
import {
	getDescription,
	getInnerType,
	getOptions,
	getZodTypeName,
	isStringNumberIntUnion,
} from '@fluxer/openapi/src/converters/ZodToOpenAPIIntrospection';
import {CustomSchemaType} from '@fluxer/openapi/src/schemas/CustomSchemaType';
import type {OpenAPISchema, OpenAPISchemaOrRef} from '@fluxer/openapi/src/Types';
import type {ZodTypeAny} from 'zod';

const bitflagSchemaRegistry = new Map<string, OpenAPISchema>();
const int32EnumSchemaRegistry = new Map<string, OpenAPISchema>();
function getCustomType(schema: ZodTypeAny, depth = 0): string | undefined {
	if (depth > 20) return undefined;
	const direct = getCustomTypeMetadata(schema);
	if (typeof direct === 'string') return direct;
	const parent = getZodParent(schema);
	if (parent) {
		return getCustomType(parent, depth + 1);
	}
	return undefined;
}
function getRefForCustomTypeName(typeName: string): OpenAPISchemaOrRef | null {
	const registryRef = CustomSchemaType.getRef(typeName);
	if (registryRef) return registryRef;
	switch (typeName) {
		case 'SnowflakeType':
		case 'SnowflakeStringType':
			return SnowflakeTypeRef;
		case 'Int32Type':
			return Int32TypeRef;
		case 'Int64Type':
			return Int64TypeRef;
		case 'Int64StringType':
			return Int64StringTypeRef;
		case 'UnsignedInt64Type':
			return UnsignedInt64TypeRef;
		case 'UsernameType':
			return UsernameTypeRef;
		case 'DiscriminatorType':
			return DiscriminatorTypeRef;
		case 'EmailType':
			return EmailTypeRef;
		case 'PasswordType':
			return PasswordTypeRef;
		case 'PhoneNumberType':
			return PhoneNumberTypeRef;
		case 'Base64ImageType':
			return Base64ImageTypeRef;
		case 'Locale':
			return LocaleRef;
		default:
			return null;
	}
}
function makeSchemaRef(schemaName: string, description: string | undefined): OpenAPISchemaOrRef {
	if (description) {
		return {$ref: `#/components/schemas/${schemaName}`, description};
	}
	return {$ref: `#/components/schemas/${schemaName}`};
}
function applyInt32EnumEntries(schema: OpenAPISchema, fluxer: FluxerTypeAnnotation): void {
	if (!fluxer.enumEntries || fluxer.enumEntries.length === 0) {
		return;
	}
	applyEnumEntryExtensions(schema, fluxer.enumEntries);
}
function makeInt32EnumSchema(fluxer: FluxerTypeAnnotation): OpenAPISchema {
	const schema: OpenAPISchema = {type: 'integer', format: 'int32'};
	applyInt32EnumEntries(schema, fluxer);
	if (fluxer.userDescription) {
		schema.description = fluxer.userDescription;
	}
	return schema;
}
function getInt32EnumSchema(fluxer: FluxerTypeAnnotation): OpenAPISchemaOrRef {
	if (!fluxer.bitflagTypeName) {
		return makeInt32EnumSchema(fluxer);
	}
	const schemaName = fluxer.bitflagTypeName;
	if (!int32EnumSchemaRegistry.has(schemaName)) {
		int32EnumSchemaRegistry.set(schemaName, makeInt32EnumSchema(fluxer));
	}
	return makeSchemaRef(schemaName, fluxer.fieldDescription);
}
function makeBitflagSchema(fluxer: FluxerTypeAnnotation, integer: boolean): OpenAPISchema {
	const schema: OpenAPISchema = integer
		? {type: 'integer', format: 'int32', minimum: 0, maximum: 2147483647}
		: {type: 'string', format: 'int64', pattern: '^[0-9]+$'};
	if (fluxer.bitflagValues && fluxer.bitflagValues.length > 0) {
		setBitflagValues(schema, fluxer.bitflagValues);
	}
	if (fluxer.userDescription) {
		schema.description = fluxer.userDescription;
	}
	return schema;
}
function getBitflagSchema(fluxer: FluxerTypeAnnotation, integer: boolean): OpenAPISchemaOrRef {
	if (!fluxer.bitflagTypeName) {
		return makeBitflagSchema(fluxer, integer);
	}
	const schemaName = fluxer.bitflagTypeName;
	if (!bitflagSchemaRegistry.has(schemaName)) {
		bitflagSchemaRegistry.set(schemaName, makeBitflagSchema(fluxer, integer));
	}
	return makeSchemaRef(schemaName, fluxer.fieldDescription);
}
export function getFluxerCustomTypeSchema(schema: ZodTypeAny, depth = 0): OpenAPISchemaOrRef | null {
	if (depth > 15) return null;
	const customType = getCustomType(schema);
	if (customType) {
		const ref = getRefForCustomTypeName(customType);
		if (ref) return ref;
	}
	const description = getDescription(schema);
	const fluxer = parseFluxerTypeAnnotation(description);
	if (fluxer) {
		const ref = getRefForCustomTypeName(fluxer.typeName);
		if (ref) return ref;
		if (fluxer.typeName === 'Int32Enum') {
			return getInt32EnumSchema(fluxer);
		}
		if (fluxer.typeName === 'Bitflags64') {
			return getBitflagSchema(fluxer, false);
		}
		if (fluxer.typeName === 'Bitflags32') {
			return getBitflagSchema(fluxer, true);
		}
		if (fluxer.typeName === 'Permissions') {
			return getBitflagSchema(fluxer, false);
		}
		const customSchema = FLUXER_CUSTOM_TYPES[fluxer.typeName];
		return customSchema ? {...customSchema} : null;
	}
	const zodTypeName = getZodTypeName(schema);
	if (zodTypeName === 'ZodEffects' || zodTypeName === 'effect' || zodTypeName === 'pipe') {
		const inner = getInnerType(schema);
		if (inner) {
			const innerType = getZodTypeName(inner);
			if (innerType === 'ZodUnion' || innerType === 'union') {
				const options = getOptions(inner);
				if (isStringNumberIntUnion(options)) {
					return SnowflakeTypeRef;
				}
			}
			const innerCustomSchema = getFluxerCustomTypeSchema(inner, depth + 1);
			if (innerCustomSchema) {
				return innerCustomSchema;
			}
		}
	}
	if (
		zodTypeName === 'ZodPipeline' ||
		zodTypeName === 'pipe' ||
		zodTypeName === 'ZodOptional' ||
		zodTypeName === 'optional' ||
		zodTypeName === 'ZodDefault' ||
		zodTypeName === 'default'
	) {
		const inner = getInnerType(schema);
		if (inner) {
			const innerCustomSchema = getFluxerCustomTypeSchema(inner, depth + 1);
			if (innerCustomSchema) {
				return innerCustomSchema;
			}
		}
	}
	return null;
}
export function isSnowflakeType(schema: ZodTypeAny, depth = 0): boolean {
	if (depth > 10) return false;
	const customTypeSchema = getFluxerCustomTypeSchema(schema, depth);
	if (customTypeSchema === SnowflakeTypeRef) {
		return true;
	}
	const zodTypeName = getZodTypeName(schema);
	if (
		zodTypeName === 'ZodEffects' ||
		zodTypeName === 'effect' ||
		zodTypeName === 'ZodPipeline' ||
		zodTypeName === 'pipe'
	) {
		const inner = getInnerType(schema);
		if (inner) {
			return isSnowflakeType(inner, depth + 1);
		}
	}
	return false;
}
const FLUXER_CUSTOM_TYPES: Record<string, OpenAPISchema> = {
	Int64Type: {type: 'string', format: 'int64', pattern: '^-?[0-9]+$'},
	Int64StringType: {type: 'string', format: 'int64', pattern: '^-?[0-9]+$'},
	UnsignedInt64Type: {type: 'string', format: 'int64', pattern: '^[0-9]+$'},
	PermissionStringType: {type: 'string', format: 'int64', pattern: '^[0-9]+$'},
	BitflagStringType: {type: 'string', format: 'int64', pattern: '^[0-9]+$'},
	ColorType: {type: 'integer', minimum: 0, maximum: 16777215, format: 'int32'},
	Int32Type: {type: 'integer', minimum: 0, maximum: 2147483647, format: 'int32'},
	EmailType: {type: 'string', format: 'email'},
	PasswordType: {type: 'string', minLength: 8, maxLength: 256},
	UsernameType: {type: 'string', minLength: 1, maxLength: 32, pattern: '^[a-zA-Z0-9_]+$'},
	PhoneNumberType: {type: 'string', pattern: '^\\+[1-9]\\d{1,14}$'},
	URLType: {type: 'string', format: 'uri'},
	QueryBooleanType: {type: 'boolean'},
	DateTimeType: {type: 'string', format: 'date-time'},
	SnowflakeType: {
		type: 'string',
		format: 'snowflake',
		pattern: '^(0|[1-9][0-9]*)$',
	},
	SnowflakeStringType: {
		type: 'string',
		format: 'snowflake',
		pattern: '^(0|[1-9][0-9]*)$',
	},
};
export function getRegisteredBitflagSchemas(): Record<string, OpenAPISchema> {
	const result: Record<string, OpenAPISchema> = {};
	for (const [name, schema] of bitflagSchemaRegistry) {
		result[name] = schema;
	}
	return result;
}
export function getRegisteredInt32EnumSchemas(): Record<string, OpenAPISchema> {
	const result: Record<string, OpenAPISchema> = {};
	for (const [name, schema] of int32EnumSchemaRegistry) {
		result[name] = schema;
	}
	return result;
}

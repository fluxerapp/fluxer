// SPDX-License-Identifier: AGPL-3.0-or-later

import {setEnumNames} from '@fluxer/openapi/src/converters/OpenAPIExtensions';
import {
	getZodDefinition,
	getZodSchemaDescription,
	getZodStringConstraintProperties,
	getZodTypeFromUnknown,
	getZodValues,
	type ZodCheck,
} from '@fluxer/openapi/src/converters/ZodInternals';
import {parseFluxerTypeAnnotation} from '@fluxer/openapi/src/converters/ZodToOpenAPIAnnotationParser';
import type {OpenAPISchema} from '@fluxer/openapi/src/Types';
import type {ZodTypeAny} from 'zod';

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

export function getZodTypeName(schema: ZodTypeAny): string {
	const def = getZodDefinition(schema);
	return def.typeName ?? (typeof def.type === 'string' ? def.type : undefined) ?? 'unknown';
}
export function getInnerType(schema: ZodTypeAny): ZodTypeAny | undefined {
	const def = getZodDefinition(schema);
	return def.in ?? def.innerType ?? def.schema;
}
export function getShape(schema: ZodTypeAny): Record<string, ZodTypeAny> | undefined {
	const def = getZodDefinition(schema);
	if (typeof def.shape === 'function') {
		return def.shape();
	}
	if (typeof def.shape === 'object' && def.shape !== null) {
		return def.shape;
	}
	return undefined;
}
export function getOptions(schema: ZodTypeAny): Array<ZodTypeAny> | undefined {
	const options = getZodDefinition(schema).options;
	return Array.isArray(options) ? options : undefined;
}
export function getLiteralValues(schema: ZodTypeAny): Array<unknown> | null {
	const def = getZodDefinition(schema);
	if (Array.isArray(def.values)) {
		return def.values.length > 0 ? def.values : null;
	}
	if ('value' in def) {
		return [def.value];
	}
	const values = getZodValues(schema);
	if (values instanceof Set) {
		return Array.from(values);
	}
	return null;
}
interface EnumInfo {
	values: Array<string | number>;
	enumNames?: Array<string | null>;
}
function buildEnumInfoFromRecord(record: Record<string, unknown>): EnumInfo | null {
	const entries = Object.entries(record);
	const values: Array<string | number> = [];
	const enumNames: Array<string | null> = [];
	for (const [key, value] of entries) {
		if (typeof value !== 'string' && typeof value !== 'number') {
			continue;
		}
		const numericKey = Number(key);
		const shouldInclude = typeof value === 'number' || Number.isNaN(numericKey);
		if (!shouldInclude) {
			continue;
		}
		values.push(value);
		enumNames.push(Number.isNaN(numericKey) ? key : null);
	}
	if (values.length === 0) {
		return null;
	}
	const hasNames = enumNames.some((name) => typeof name === 'string' && name.length > 0);
	if (hasNames) {
		return {values, enumNames};
	}
	return {values};
}
export function getEnumInfo(schema: ZodTypeAny): EnumInfo | null {
	const def = getZodDefinition(schema);
	if (Array.isArray(def.values)) {
		const values = def.values.filter(
			(val): val is string | number => typeof val === 'string' || typeof val === 'number',
		);
		return values.length > 0 ? {values} : null;
	}
	if (isRecord(def.entries)) {
		const info = buildEnumInfoFromRecord(def.entries);
		if (info) {
			return info;
		}
	}
	if (isRecord(def.values)) {
		const info = buildEnumInfoFromRecord(def.values);
		if (info) {
			return info;
		}
	}
	const values = getZodValues(schema);
	if (values instanceof Set) {
		const filtered = Array.from(values).filter(
			(val): val is string | number => typeof val === 'string' || typeof val === 'number',
		);
		return filtered.length > 0 ? {values: filtered} : null;
	}
	return null;
}
export function buildEnumSchemaFromInfo(enumInfo: EnumInfo): OpenAPISchema {
	const {values, enumNames} = enumInfo;
	const schema: OpenAPISchema = {enum: values};
	const allNumbers = values.every((val) => typeof val === 'number');
	const allStrings = values.every((val) => typeof val === 'string');
	if (allNumbers) {
		const allInts = values.every((val) => typeof val === 'number' && Number.isInteger(val));
		schema.type = allInts ? 'integer' : 'number';
	} else if (allStrings) {
		schema.type = 'string';
	}
	if (enumNames?.some((name) => typeof name === 'string' && name.length > 0)) {
		setEnumNames(schema, enumNames);
	}
	return schema;
}
export function getLiteralSchema(values: Array<unknown>): OpenAPISchema {
	const allNumbers = values.every((val) => typeof val === 'number');
	const allStrings = values.every((val) => typeof val === 'string');
	const allBooleans = values.every((val) => typeof val === 'boolean');
	if (allNumbers) {
		const enumValues = values.filter((val): val is number => typeof val === 'number');
		const allInts = values.every((val) => typeof val === 'number' && Number.isInteger(val));
		return {
			type: allInts ? 'integer' : 'number',
			enum: enumValues,
		};
	}
	if (allStrings) {
		const enumValues = values.filter((val): val is string => typeof val === 'string');
		return {
			type: 'string',
			enum: enumValues,
		};
	}
	if (allBooleans) {
		const enumValues = values.filter((val): val is boolean => typeof val === 'boolean');
		return {
			type: 'boolean',
			enum: enumValues,
		};
	}
	const enumValues = values.filter(
		(val): val is string | number | boolean =>
			typeof val === 'string' || typeof val === 'number' || typeof val === 'boolean',
	);
	if (enumValues.length === 0) {
		if (values.some((val) => val === null)) {
			return {type: 'null'};
		}
		return {};
	}
	const hasNull = values.some((val) => val === null);
	const schema: OpenAPISchema = {enum: enumValues};
	if (hasNull) {
		schema.nullable = true;
	}
	return schema;
}
export function getJsonValueSchema(depth = 0): OpenAPISchema {
	if (depth >= 2) {
		return {
			anyOf: [
				{type: 'string'},
				{type: 'number'},
				{type: 'boolean'},
				{type: 'object', additionalProperties: true},
				{type: 'null'},
			],
		};
	}
	return {
		anyOf: [
			{type: 'string'},
			{type: 'number'},
			{type: 'boolean'},
			{type: 'object', additionalProperties: getJsonValueSchema(depth + 1)},
			{type: 'array', items: getJsonValueSchema(depth + 1)},
			{type: 'null'},
		],
	};
}
export function getArrayType(schema: ZodTypeAny): ZodTypeAny | undefined {
	const def = getZodDefinition(schema);
	return def.element ?? getZodTypeFromUnknown(def.type) ?? getZodTypeFromUnknown(def.items);
}
export function getCheckKind(check: ZodCheck): string {
	if (check.kind) return check.kind;
	if (check.type) return check.type;
	if (check.def?.check) return check.def.check;
	const v4Def = check._zod?.def;
	if (v4Def && typeof v4Def.check === 'string') {
		if (v4Def.check === 'min_length') return 'min';
		if (v4Def.check === 'max_length') return 'max';
		if (v4Def.check === 'greater_than') return 'min';
		if (v4Def.check === 'less_than') return 'max';
		if (v4Def.check === 'multiple_of') return 'multipleOf';
		if (v4Def.check === 'string_format' && typeof v4Def.format === 'string') {
			if (v4Def.format === 'starts_with') return 'startsWith';
			if (v4Def.format === 'ends_with') return 'endsWith';
			return v4Def.format;
		}
		return v4Def.check;
	}
	const name = check.constructor?.name ?? '';
	if (name.includes('MinLength') || name.includes('GreaterThan')) return 'min';
	if (name.includes('MaxLength') || name.includes('LessThan')) return 'max';
	if (name.includes('Length')) return 'length';
	if (name.includes('Regex')) return 'regex';
	if (name.includes('Email')) return 'email';
	if (name.includes('Url')) return 'url';
	if (name.includes('Uuid')) return 'uuid';
	if (name.includes('DateTime')) return 'datetime';
	if (name.includes('NumberFormat')) return 'number_format';
	return name.toLowerCase();
}
export function getChecks(schema: ZodTypeAny): Array<ZodCheck> {
	const checks = getZodDefinition(schema).checks;
	return Array.isArray(checks) ? checks : [];
}
export function extractStringConstraints(schema: ZodTypeAny): {
	minLength?: number;
	maxLength?: number;
	format?: string;
	pattern?: string;
} {
	const result: {
		minLength?: number;
		maxLength?: number;
		format?: string;
		pattern?: string;
	} = {};
	const stringProperties = getZodStringConstraintProperties(schema);
	if (stringProperties.minLength != null) result.minLength = stringProperties.minLength;
	if (stringProperties.maxLength != null) result.maxLength = stringProperties.maxLength;
	if (stringProperties.format != null) {
		if (stringProperties.format === 'url') {
			result.format = 'uri';
		} else if (stringProperties.format === 'datetime') {
			result.format = 'date-time';
		} else if (stringProperties.format !== 'regex') {
			result.format = stringProperties.format;
		}
	}
	if (stringProperties._regex != null) result.pattern = stringProperties._regex.source;
	return result;
}
const INTEGER_NUMBER_FORMATS = new Set(['safeint', 'int32', 'uint32']);
export function extractNumberConstraints(schema: ZodTypeAny): {
	minimum?: number;
	maximum?: number;
	isInt?: boolean;
	format?: string;
} {
	const checks = getChecks(schema);
	const result: {
		minimum?: number;
		maximum?: number;
		isInt?: boolean;
		format?: string;
	} = {};
	for (const check of checks) {
		if (check.minValue != null) result.minimum = check.minValue;
		if (check.maxValue != null) result.maximum = check.maxValue;
		if (check.isInt === true) result.isInt = true;
		if (check.format != null) result.format = check.format;
	}
	const definition = getZodDefinition(schema);
	if (definition.check === 'number_format' && definition.format != null) {
		result.format = definition.format;
		if (INTEGER_NUMBER_FORMATS.has(definition.format)) {
			result.isInt = true;
		}
	}
	return result;
}
export function getDefaultValue(schema: ZodTypeAny): unknown {
	const def = getZodDefinition(schema);
	if (typeof def.defaultValue === 'function') {
		try {
			return def.defaultValue();
		} catch {
			return undefined;
		}
	}
	return undefined;
}
export function getDescription(schema: ZodTypeAny): string | undefined {
	const schemaDesc = getZodSchemaDescription(schema);
	if (schemaDesc) return schemaDesc;
	return getZodDefinition(schema).description;
}
export function getUserDescription(schema: ZodTypeAny): string | undefined {
	const description = getDescription(schema);
	const fluxer = parseFluxerTypeAnnotation(description);
	if (fluxer) return fluxer.userDescription;
	return description;
}
export function getTupleItems(schema: ZodTypeAny): Array<ZodTypeAny> | undefined {
	const items = getZodDefinition(schema).items;
	return Array.isArray(items) ? items : undefined;
}
export function getTupleRest(schema: ZodTypeAny): ZodTypeAny | undefined {
	return getZodDefinition(schema).rest;
}
export function getMapValueType(schema: ZodTypeAny): ZodTypeAny | undefined {
	return getZodDefinition(schema).valueType;
}
export function getPromiseType(schema: ZodTypeAny): ZodTypeAny | undefined {
	return getZodTypeFromUnknown(getZodDefinition(schema).type);
}
export function getCatchValue(schema: ZodTypeAny): unknown {
	const def = getZodDefinition(schema);
	if (typeof def.catchValue === 'function') {
		try {
			return def.catchValue();
		} catch {
			return undefined;
		}
	}
	return def.catchValue;
}
export function isStringNumberIntUnion(options: Array<ZodTypeAny> | undefined): boolean {
	if (!options || options.length !== 2) return false;
	const hasString = options.some((o) => {
		const name = getZodTypeName(o);
		return name === 'ZodString' || name === 'string';
	});
	const hasIntNumber = options.some((o) => {
		const name = getZodTypeName(o);
		if (name === 'ZodNumber' || name === 'number') {
			const checks = getChecks(o);
			return checks.some((c) => {
				const kind = getCheckKind(c);
				return kind === 'int' || kind === 'integer' || c.isInt === true;
			});
		}
		return false;
	});
	return hasString && hasIntNumber;
}

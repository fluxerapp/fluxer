// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getEnumDescriptions,
	getNumericEnumValues,
	setEnumDescriptions,
	setEnumNames,
	setSnowflakeKeyType,
} from '@fluxer/openapi/src/converters/OpenAPIExtensions';
import {escapeRegex, parseFluxerTypeAnnotation} from '@fluxer/openapi/src/converters/ZodToOpenAPIAnnotationParser';
import {getFluxerCustomTypeSchema, isSnowflakeType} from '@fluxer/openapi/src/converters/ZodToOpenAPICustomTypes';

export {
	getRegisteredBitflagSchemas,
	getRegisteredInt32EnumSchemas,
} from '@fluxer/openapi/src/converters/ZodToOpenAPICustomTypes';

import {
	getSchemaNameMetadata,
	getZodDefinition,
	setSchemaNameMetadata,
} from '@fluxer/openapi/src/converters/ZodInternals';
import {
	buildEnumSchemaFromInfo,
	extractNumberConstraints,
	extractStringConstraints,
	getArrayType,
	getCatchValue,
	getCheckKind,
	getChecks,
	getDefaultValue,
	getDescription,
	getEnumInfo,
	getInnerType,
	getJsonValueSchema,
	getLiteralSchema,
	getLiteralValues,
	getMapValueType,
	getOptions,
	getPromiseType,
	getShape,
	getTupleItems,
	getTupleRest,
	getUserDescription,
	getZodTypeName,
} from '@fluxer/openapi/src/converters/ZodToOpenAPIIntrospection';
import type {OpenAPISchema, OpenAPISchemaOrRef} from '@fluxer/openapi/src/Types';
import type {ZodTypeAny} from 'zod';

export function setSchemaName(schema: ZodTypeAny, name: string): void {
	setSchemaNameMetadata(schema, name);
}

function getSchemaName(schema: ZodTypeAny): string | undefined {
	return getSchemaNameMetadata(schema);
}

function isSchemaOptional(schema: ZodTypeAny, depth = 0): boolean {
	if (depth > 10) return false;
	const typeName = getZodTypeName(schema);
	if (typeName === 'ZodOptional' || typeName === 'optional') {
		return true;
	}
	if (typeName === 'ZodDefault' || typeName === 'default') {
		return true;
	}
	if (typeName === 'ZodNullable' || typeName === 'nullable') {
		const inner = getInnerType(schema);
		if (inner) {
			return isSchemaOptional(inner, depth + 1);
		}
	}
	if (typeName === 'ZodEffects' || typeName === 'effect' || typeName === 'ZodPipeline' || typeName === 'pipe') {
		const inner = getInnerType(schema);
		if (inner) {
			return isSchemaOptional(inner, depth + 1);
		}
	}
	return false;
}
function addDescription(result: OpenAPISchemaOrRef, schema: ZodTypeAny): OpenAPISchemaOrRef {
	if ('$ref' in result) {
		return result;
	}
	const description = getUserDescription(schema);
	if (description) {
		result.description = description;
	}
	return result;
}
function isOpenAPISchema(schema: OpenAPISchemaOrRef): schema is OpenAPISchema {
	return !('$ref' in schema);
}
function makeNullableSchema(inner: OpenAPISchemaOrRef): OpenAPISchema {
	if (isOpenAPISchema(inner)) {
		const keys = Object.keys(inner).filter((k) => k !== 'description');
		if (inner.oneOf && keys.length === 1) {
			const result: OpenAPISchema = {oneOf: [...inner.oneOf, {type: 'null'}]};
			if (inner.description) result.description = inner.description;
			return result;
		}
		if (inner.anyOf && keys.length === 1) {
			const result: OpenAPISchema = {anyOf: [...inner.anyOf, {type: 'null'}]};
			if (inner.description) result.description = inner.description;
			return result;
		}
	}
	return {
		anyOf: [inner, {type: 'null'}],
	};
}
const discriminatedUnionBranchRegistry = new Map<string, OpenAPISchema>();

export function getRegisteredDiscriminatedUnionBranchSchemas(): Record<string, OpenAPISchema> {
	const result: Record<string, OpenAPISchema> = {};
	for (const [name, schema] of discriminatedUnionBranchRegistry) {
		result[name] = schema;
	}
	return result;
}

const namedObjectRegistry = new Map<string, OpenAPISchema>();

export function getRegisteredNamedObjectSchemas(): Record<string, OpenAPISchema> {
	const result: Record<string, OpenAPISchema> = {};
	for (const [name, schema] of namedObjectRegistry) {
		result[name] = schema;
	}
	return result;
}

function makeNamedObjectRef(name: string, fieldDescription: string | undefined): OpenAPISchemaOrRef {
	if (fieldDescription) {
		return {$ref: `#/components/schemas/${name}`, description: fieldDescription};
	}
	return {$ref: `#/components/schemas/${name}`};
}

function toPascalCase(value: string): string {
	return value
		.split(/[^A-Za-z0-9]+/)
		.filter((word) => word.length > 0)
		.map((word) => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
		.join('');
}

function discriminatedUnionBranchSuffix(
	branch: OpenAPISchema,
	discriminator: string,
	index: number,
	usedSuffixes: Set<string>,
): string {
	let base = '';
	const discriminatorProp = branch.properties?.[discriminator];
	if (discriminatorProp && isOpenAPISchema(discriminatorProp)) {
		const enumNames = (discriminatorProp as {['x-enumNames']?: Array<string | null>})['x-enumNames'];
		const enumValues = discriminatorProp.enum;
		if (Array.isArray(enumNames) && typeof enumNames[0] === 'string') {
			base = toPascalCase(enumNames[0]);
		} else if (Array.isArray(enumValues) && enumValues.length === 1 && typeof enumValues[0] === 'string') {
			base = toPascalCase(enumValues[0]);
		}
	}
	if (base.length === 0) {
		base = `Variant${index}`;
	}
	let suffix = base;
	if (usedSuffixes.has(suffix)) {
		suffix = `${base}${index}`;
	}
	usedSuffixes.add(suffix);
	return suffix;
}

function registerDiscriminatedUnionBranches(
	unionName: string,
	discriminator: string,
	branches: Array<OpenAPISchemaOrRef>,
): Array<OpenAPISchemaOrRef> {
	const usedSuffixes = new Set<string>();
	return branches.map((branch, index) => {
		if (!isOpenAPISchema(branch)) {
			return branch;
		}
		const suffix = discriminatedUnionBranchSuffix(branch, discriminator, index, usedSuffixes);
		const branchName = `${suffix}${unionName}`;
		if (!discriminatedUnionBranchRegistry.has(branchName)) {
			discriminatedUnionBranchRegistry.set(branchName, branch);
		}
		return {$ref: `#/components/schemas/${branchName}`};
	});
}

export function zodToOpenAPISchema(schema: ZodTypeAny, depth = 0): OpenAPISchemaOrRef {
	if (depth > 20) {
		return {type: 'object'};
	}
	const schemaName = getSchemaName(schema);
	if (schemaName && depth > 0) {
		return {$ref: `#/components/schemas/${schemaName}`};
	}
	const customTypeSchema = getFluxerCustomTypeSchema(schema);
	if (customTypeSchema) {
		return addDescription(customTypeSchema, schema);
	}
	const typeName = getZodTypeName(schema);
	switch (typeName) {
		case 'ZodString':
		case 'string': {
			const result: OpenAPISchema = {type: 'string'};
			const strConstraints = extractStringConstraints(schema);
			if (strConstraints.minLength != null) result.minLength = strConstraints.minLength;
			if (strConstraints.maxLength != null) result.maxLength = strConstraints.maxLength;
			if (strConstraints.format != null) result.format = strConstraints.format;
			if (strConstraints.pattern != null) result.pattern = strConstraints.pattern;
			const checks = getChecks(schema);
			for (const check of checks) {
				const v4Def = check._zod?.def;
				const kind = getCheckKind(check);
				if (kind === 'min') {
					const val = typeof check.value === 'number' ? check.value : check.minimum;
					const v4Val = v4Def?.minimum;
					if (typeof val === 'number') {
						result.minLength = val;
					} else if (typeof v4Val === 'number') {
						result.minLength = v4Val;
					}
				}
				if (kind === 'max') {
					const val = typeof check.value === 'number' ? check.value : check.maximum;
					const v4Val = v4Def?.maximum;
					if (typeof val === 'number') {
						result.maxLength = val;
					} else if (typeof v4Val === 'number') {
						result.maxLength = v4Val;
					}
				}
				if (kind === 'length' && typeof check.value === 'number') {
					result.minLength = check.value;
					result.maxLength = check.value;
				}
				if (kind === 'email') {
					result.format = 'email';
				}
				if (kind === 'url') {
					result.format = 'uri';
				}
				if (kind === 'uuid') {
					result.format = 'uuid';
				}
				if (kind === 'cuid') {
					result.format = 'cuid';
				}
				if (kind === 'cuid2') {
					result.format = 'cuid2';
				}
				if (kind === 'ulid') {
					result.format = 'ulid';
				}
				if (kind === 'datetime' || kind === 'date') {
					result.format = 'date-time';
				}
				if (kind === 'time') {
					result.format = 'time';
				}
				if (kind === 'duration') {
					result.format = 'duration';
				}
				if (kind === 'ip') {
					result.format = 'ip';
				}
				if (kind === 'base64') {
					result.format = 'byte';
				}
				if (kind === 'regex') {
					const regex = check.regex ?? check.value;
					if (regex instanceof RegExp) {
						result.pattern = regex.source;
					} else if (v4Def?.pattern instanceof RegExp) {
						result.pattern = v4Def.pattern.source;
					}
				}
				if (kind === 'includes' && typeof check.value === 'string') {
					result.pattern = `.*${escapeRegex(check.value)}.*`;
				}
				if (kind === 'startsWith' && typeof check.value === 'string') {
					result.pattern = `^${escapeRegex(check.value)}.*`;
				}
				if (kind === 'endsWith' && typeof check.value === 'string') {
					result.pattern = `.*${escapeRegex(check.value)}$`;
				}
				if ((kind === 'includes' || kind === 'startsWith' || kind === 'endsWith') && v4Def?.pattern instanceof RegExp) {
					result.pattern = v4Def.pattern.source;
				}
			}
			return addDescription(result, schema);
		}
		case 'ZodNumber':
		case 'number': {
			const result: OpenAPISchema = {type: 'number'};
			const numConstraints = extractNumberConstraints(schema);
			if (numConstraints.isInt) {
				result.type = 'integer';
			}
			if (numConstraints.minimum != null) {
				result.minimum = numConstraints.minimum;
			}
			if (numConstraints.maximum != null) {
				result.maximum = numConstraints.maximum;
			}
			const checks = getChecks(schema);
			for (const check of checks) {
				const v4Def = check._zod?.def;
				const kind = getCheckKind(check);
				if (kind === 'int' || kind === 'number_format') {
					if (check.isInt) result.type = 'integer';
				}
				if (kind === 'min') {
					const val = typeof check.value === 'number' ? check.value : check.minimum;
					if (typeof val === 'number') {
						result.minimum = val;
						if (check.inclusive === false) {
							result.exclusiveMinimum = val;
							delete result.minimum;
						}
					} else if (typeof v4Def?.value === 'number') {
						result.minimum = v4Def.value;
						if (v4Def.inclusive === false) {
							result.exclusiveMinimum = v4Def.value;
							delete result.minimum;
						}
					}
				}
				if (kind === 'max') {
					const val = typeof check.value === 'number' ? check.value : check.maximum;
					if (typeof val === 'number') {
						result.maximum = val;
						if (check.inclusive === false) {
							result.exclusiveMaximum = val;
							delete result.maximum;
						}
					} else if (typeof v4Def?.value === 'number') {
						result.maximum = v4Def.value;
						if (v4Def.inclusive === false) {
							result.exclusiveMaximum = v4Def.value;
							delete result.maximum;
						}
					}
				}
				if (kind === 'multipleOf' && typeof check.value === 'number') {
					result.multipleOf = check.value;
				} else if (kind === 'multipleOf' && typeof v4Def?.value === 'number') {
					result.multipleOf = v4Def.value;
				}
				if (kind === 'finite') {
					result.format = 'double';
				}
			}
			const SAFE_INT_MIN = Number.MIN_SAFE_INTEGER;
			const SAFE_INT_MAX = Number.MAX_SAFE_INTEGER;
			if (
				(result.minimum ?? result.exclusiveMinimum) === SAFE_INT_MIN &&
				(result.maximum ?? result.exclusiveMaximum) === SAFE_INT_MAX
			) {
				result.minimum = undefined;
				result.maximum = undefined;
				result.exclusiveMinimum = undefined;
				result.exclusiveMaximum = undefined;
			}
			if (result.type === 'integer') {
				const min = result.minimum ?? result.exclusiveMinimum;
				const max = result.maximum ?? result.exclusiveMaximum;
				if (min != null && max != null) {
					result.format = min >= -2147483648 && max <= 2147483647 ? 'int32' : 'int64';
				} else if (min == null && max == null) {
					result.format = 'int53';
				} else {
					result.format = 'int64';
				}
			}
			const description = getDescription(schema);
			const fluxer = parseFluxerTypeAnnotation(description);
			if (fluxer?.typeName === 'EnumValues' && fluxer.enumEntries && fluxer.enumEntries.length > 0) {
				const enumValues = getNumericEnumValues(fluxer.enumEntries);
				if (enumValues) {
					result.enum = enumValues;
					setEnumNames(
						result,
						fluxer.enumEntries.map((entry) => entry.name),
					);
					const descriptions = getEnumDescriptions(fluxer.enumEntries);
					if (descriptions) {
						setEnumDescriptions(result, descriptions);
					}
				}
			}
			return addDescription(result, schema);
		}
		case 'ZodBoolean':
		case 'boolean':
			return addDescription({type: 'boolean'}, schema);
		case 'ZodArray':
		case 'array': {
			const itemType = getArrayType(schema);
			const result: OpenAPISchema = {
				type: 'array',
				items: itemType ? zodToOpenAPISchema(itemType, depth + 1) : {type: 'string'},
			};
			const checks = getChecks(schema);
			for (const check of checks) {
				const kind = getCheckKind(check);
				const v4Def = check._zod?.def;
				if (kind === 'min') {
					const val = typeof check.value === 'number' ? check.value : (check.minimum ?? v4Def?.minimum);
					if (typeof val === 'number') {
						result.minItems = val;
					}
				}
				if (kind === 'max') {
					const val = typeof check.value === 'number' ? check.value : (check.maximum ?? v4Def?.maximum);
					if (typeof val === 'number') {
						result.maxItems = val;
					}
				}
				if (kind === 'length') {
					const val = typeof check.value === 'number' ? check.value : v4Def?.value;
					if (typeof val === 'number') {
						result.minItems = val;
						result.maxItems = val;
					}
				}
			}
			return addDescription(result, schema);
		}
		case 'ZodSet':
		case 'set': {
			const def = getZodDefinition(schema);
			const result: OpenAPISchema = {
				type: 'array',
				uniqueItems: true,
				items: def.valueType ? zodToOpenAPISchema(def.valueType, depth + 1) : {type: 'string'},
			};
			if (def.minSize?.value != null) {
				result.minItems = def.minSize.value;
			}
			if (def.maxSize?.value != null) {
				result.maxItems = def.maxSize.value;
			}
			return addDescription(result, schema);
		}
		case 'ZodObject':
		case 'object': {
			const shape = getShape(schema);
			if (!shape) {
				return {type: 'object'};
			}
			const objectAnnotation = parseFluxerTypeAnnotation(getDescription(schema));
			const namedObjectName =
				objectAnnotation?.typeName === 'NamedObject' ? objectAnnotation.objectName : undefined;
			if (namedObjectName && depth > 0 && namedObjectRegistry.has(namedObjectName)) {
				return makeNamedObjectRef(namedObjectName, objectAnnotation?.fieldDescription);
			}
			if (namedObjectName) {
				namedObjectRegistry.set(namedObjectName, {type: 'object'});
			}
			const properties: Record<string, OpenAPISchemaOrRef> = {};
			const required: Array<string> = [];
			for (const [key, value] of Object.entries(shape)) {
				properties[key] = zodToOpenAPISchema(value, depth + 1);
				if (!isSchemaOptional(value)) {
					required.push(key);
				}
			}
			const result: OpenAPISchema = {
				type: 'object',
				properties,
			};
			if (required.length > 0) {
				result.required = required;
			}
			if (namedObjectName) {
				if (objectAnnotation?.userDescription) {
					result.description = objectAnnotation.userDescription;
				}
				namedObjectRegistry.set(namedObjectName, result);
				if (depth > 0) {
					return makeNamedObjectRef(namedObjectName, objectAnnotation?.fieldDescription);
				}
				return result;
			}
			return addDescription(result, schema);
		}
		case 'ZodOptional':
		case 'optional': {
			const inner = getInnerType(schema);
			if (inner) {
				const innerSchema = zodToOpenAPISchema(inner, depth + 1);
				return addDescription(innerSchema, schema);
			}
			return {};
		}
		case 'ZodNullable':
		case 'nullable': {
			const inner = getInnerType(schema);
			if (inner) {
				const innerSchema = zodToOpenAPISchema(inner, depth + 1);
				return addDescription(makeNullableSchema(innerSchema), schema);
			}
			return {type: 'null'};
		}
		case 'ZodDefault':
		case 'default': {
			const inner = getInnerType(schema);
			const defaultValue = getDefaultValue(schema);
			if (inner) {
				const innerSchema = zodToOpenAPISchema(inner, depth + 1);
				if (defaultValue !== undefined) {
					if (!('$ref' in innerSchema)) {
						innerSchema.default = defaultValue;
					}
				}
				return addDescription(innerSchema, schema);
			}
			return {};
		}
		case 'ZodUnion':
		case 'union': {
			const options = getOptions(schema);
			if (!options || options.length === 0) {
				return {};
			}
			const description = getDescription(schema);
			const fluxer = parseFluxerTypeAnnotation(description);
			if (fluxer?.typeName === 'FlexibleEnumValues' && fluxer.enumEntries && fluxer.enumEntries.length > 0) {
				const result: OpenAPISchema = {type: 'string'};
				setEnumNames(
					result,
					fluxer.enumEntries.map((entry) => entry.name),
				);
				const descriptions = getEnumDescriptions(fluxer.enumEntries);
				if (descriptions) {
					setEnumDescriptions(result, descriptions);
				}
				const knownValues = fluxer.enumEntries.map((e) => String(e.value)).join(', ');
				const baseDescription = fluxer.userDescription ?? '';
				result.description = baseDescription
					? `${baseDescription} Known values: ${knownValues} (other values allowed)`
					: `Known values: ${knownValues} (other values allowed)`;
				return result;
			}
			const allLiterals = options.every((opt) => {
				const name = getZodTypeName(opt);
				return name === 'ZodLiteral' || name === 'literal';
			});
			if (allLiterals) {
				const literalValues = options.map((opt) => getLiteralValues(opt));
				if (literalValues.every((vals) => Array.isArray(vals) && vals.length > 0)) {
					const flattened = literalValues.flatMap((vals) => vals ?? []);
					const literalSchema = getLiteralSchema(flattened);
					const fluxerForLiterals = parseFluxerTypeAnnotation(description);
					if (
						fluxerForLiterals?.typeName === 'EnumValues' &&
						fluxerForLiterals.enumNames &&
						fluxerForLiterals.enumNames.length === flattened.length
					) {
						setEnumNames(literalSchema, fluxerForLiterals.enumNames);
						if (fluxerForLiterals.enumEntries && fluxerForLiterals.enumEntries.length === flattened.length) {
							const descriptions: Array<string | null> = [];
							for (let i = 0; i < flattened.length; i++) {
								const literalValue = flattened[i];
								const entry = fluxerForLiterals.enumEntries[i];
								if (entry && (entry.value === literalValue || String(entry.value) === String(literalValue))) {
									descriptions.push(entry.description ?? null);
								} else {
									descriptions.push(null);
								}
							}
							setEnumDescriptions(literalSchema, descriptions);
						}
					}
					return addDescription(literalSchema, schema);
				}
			}
			const oneOfBranches = options.map((opt) => zodToOpenAPISchema(opt, depth + 1));
			const unionName = getSchemaName(schema);
			const unionDiscriminator = getZodDefinition(schema).discriminator;
			if (unionName && typeof unionDiscriminator === 'string') {
				return addDescription(
					{oneOf: registerDiscriminatedUnionBranches(unionName, unionDiscriminator, oneOfBranches)},
					schema,
				);
			}
			return addDescription({oneOf: oneOfBranches}, schema);
		}
		case 'ZodLiteral':
		case 'literal': {
			const values = getLiteralValues(schema);
			if (values && values.length > 0) {
				if (values.length === 1 && values[0] === null) {
					return addDescription({type: 'null'}, schema);
				}
				const literalSchema = getLiteralSchema(values);
				const description = getDescription(schema);
				const fluxer = parseFluxerTypeAnnotation(description);
				if (fluxer?.typeName === 'EnumValue' && fluxer.enumNames && fluxer.enumNames.length > 0) {
					setEnumNames(literalSchema, fluxer.enumNames);
					if (fluxer.enumEntries && fluxer.enumEntries.length > 0 && fluxer.enumEntries[0].description) {
						setEnumDescriptions(literalSchema, [fluxer.enumEntries[0].description]);
					}
				}
				return addDescription(literalSchema, schema);
			}
			return {};
		}
		case 'ZodEnum':
		case 'enum': {
			const info = getEnumInfo(schema);
			if (!info) {
				return addDescription({type: 'string', enum: []}, schema);
			}
			return addDescription(buildEnumSchemaFromInfo(info), schema);
		}
		case 'ZodEffects':
		case 'effect': {
			const inner = getInnerType(schema);
			if (inner) {
				const innerSchema = zodToOpenAPISchema(inner, depth + 1);
				return addDescription(innerSchema, schema);
			}
			return {};
		}
		case 'pipe':
		case 'ZodPipeline': {
			const def = getZodDefinition(schema);
			const outType = def.out ? getZodTypeName(def.out) : undefined;
			const target = outType === 'transform' || outType === 'ZodTransform' ? def.in : (def.out ?? def.in);
			if (target) {
				const innerSchema = zodToOpenAPISchema(target, depth + 1);
				return addDescription(innerSchema, schema);
			}
			return {};
		}
		case 'ZodIntersection':
		case 'intersection': {
			const def = getZodDefinition(schema);
			const schemas: Array<OpenAPISchemaOrRef> = [];
			if (def.left) {
				schemas.push(zodToOpenAPISchema(def.left, depth + 1));
			}
			if (def.right) {
				schemas.push(zodToOpenAPISchema(def.right, depth + 1));
			}
			if (schemas.length === 0) {
				return {};
			}
			if (schemas.length === 1) {
				return addDescription(schemas[0], schema);
			}
			return addDescription({allOf: schemas}, schema);
		}
		case 'ZodRecord':
		case 'record': {
			const def = getZodDefinition(schema);
			const result: OpenAPISchema = {
				type: 'object',
				additionalProperties: def.valueType ? zodToOpenAPISchema(def.valueType, depth + 1) : true,
			};
			if (def.keyType && isSnowflakeType(def.keyType)) {
				result.patternProperties = {
					'^(0|[1-9][0-9]*)$': def.valueType ? zodToOpenAPISchema(def.valueType, depth + 1) : true,
				};
				setSnowflakeKeyType(result);
			}
			return addDescription(result, schema);
		}
		case 'ZodAny':
		case 'any':
			return addDescription(getJsonValueSchema(), schema);
		case 'ZodUnknown':
		case 'unknown':
			return addDescription(getJsonValueSchema(), schema);
		case 'ZodVoid':
		case 'void':
			return addDescription({}, schema);
		case 'ZodNull':
		case 'null':
			return addDescription({type: 'null'}, schema);
		case 'ZodUndefined':
		case 'undefined':
			return addDescription({}, schema);
		case 'ZodBigInt':
		case 'bigint':
			return addDescription({type: 'string', format: 'int64'}, schema);
		case 'ZodDate':
		case 'date':
			return addDescription({type: 'string', format: 'date-time'}, schema);
		case 'ZodIso':
		case 'ZodIsoDateTime':
		case 'ZodIsoDate':
		case 'ZodIsoTime':
		case 'iso': {
			const def = getZodDefinition(schema);
			const isoType = def.isoType ?? (typeof def.type === 'string' ? def.type : undefined) ?? def.kind;
			if (isoType === 'date') {
				return addDescription({type: 'string', format: 'date'}, schema);
			}
			if (isoType === 'time') {
				return addDescription({type: 'string', format: 'time'}, schema);
			}
			return addDescription({type: 'string', format: 'date-time'}, schema);
		}
		case 'ZodUrl':
		case 'url':
			return addDescription({type: 'string', format: 'uri'}, schema);
		case 'ZodLazy':
		case 'lazy': {
			const def = getZodDefinition(schema);
			if (def.getter) {
				try {
					const inner = def.getter();
					const innerSchema = zodToOpenAPISchema(inner, depth + 1);
					return addDescription(innerSchema, schema);
				} catch {
					return {type: 'object'};
				}
			}
			return {type: 'object'};
		}
		case 'ZodTuple':
		case 'tuple': {
			const tupleItems = getTupleItems(schema);
			const rest = getTupleRest(schema);
			if (!tupleItems || tupleItems.length === 0) {
				const result: OpenAPISchema = {type: 'array', items: {}};
				if (rest) {
					result.items = zodToOpenAPISchema(rest, depth + 1);
				}
				return addDescription(result, schema);
			}
			const prefixItems = tupleItems.map((item) => zodToOpenAPISchema(item, depth + 1));
			const result: OpenAPISchema = {
				type: 'array',
				prefixItems,
				minItems: tupleItems.length,
				maxItems: rest ? undefined : tupleItems.length,
			};
			if (rest) {
				result.items = zodToOpenAPISchema(rest, depth + 1);
			} else {
				result.items = false;
			}
			return addDescription(result, schema);
		}
		case 'ZodMap':
		case 'map': {
			const valueType = getMapValueType(schema);
			const result: OpenAPISchema = {
				type: 'object',
				additionalProperties: valueType ? zodToOpenAPISchema(valueType, depth + 1) : true,
			};
			return addDescription(result, schema);
		}
		case 'ZodPromise':
		case 'promise': {
			const promiseType = getPromiseType(schema);
			if (promiseType) {
				const innerSchema = zodToOpenAPISchema(promiseType, depth + 1);
				return addDescription(innerSchema, schema);
			}
			return addDescription({}, schema);
		}
		case 'ZodFunction':
		case 'function':
			return addDescription({}, schema);
		case 'ZodNaN':
		case 'nan':
			return addDescription({type: 'number'}, schema);
		case 'ZodNever':
		case 'never':
			return {not: {}};
		case 'ZodSymbol':
		case 'symbol':
			return addDescription({type: 'string'}, schema);
		case 'ZodBranded':
		case 'branded': {
			const inner = getInnerType(schema);
			if (inner) {
				const innerSchema = zodToOpenAPISchema(inner, depth + 1);
				return addDescription(innerSchema, schema);
			}
			return addDescription({}, schema);
		}
		case 'ZodCatch':
		case 'catch': {
			const inner = getInnerType(schema);
			const catchValue = getCatchValue(schema);
			if (inner) {
				const innerSchema = zodToOpenAPISchema(inner, depth + 1);
				if (catchValue !== undefined) {
					if (!('$ref' in innerSchema)) {
						innerSchema.default = catchValue;
					}
				}
				return addDescription(innerSchema, schema);
			}
			return addDescription({}, schema);
		}
		case 'ZodReadonly':
		case 'readonly': {
			const inner = getInnerType(schema);
			if (inner) {
				const innerSchema = zodToOpenAPISchema(inner, depth + 1);
				return addDescription(innerSchema, schema);
			}
			return addDescription({}, schema);
		}
		case 'ZodNativeEnum':
		case 'nativeEnum': {
			const info = getEnumInfo(schema);
			if (!info) {
				return addDescription({}, schema);
			}
			return addDescription(buildEnumSchemaFromInfo(info), schema);
		}
		case 'ZodTemplateLiteral':
		case 'templateLiteral': {
			const def = getZodDefinition(schema);
			const result: OpenAPISchema = {type: 'string'};
			if (def.pattern) {
				result.pattern = def.pattern.source;
			}
			return addDescription(result, schema);
		}
		case 'ZodEmail':
		case 'email':
			return addDescription({type: 'string', format: 'email'}, schema);
		case 'ZodUuid':
		case 'uuid':
			return addDescription({type: 'string', format: 'uuid'}, schema);
		case 'ZodCuid':
		case 'cuid':
			return addDescription({type: 'string', format: 'cuid'}, schema);
		case 'ZodCuid2':
		case 'cuid2':
			return addDescription({type: 'string', format: 'cuid2'}, schema);
		case 'ZodUlid':
		case 'ulid':
			return addDescription({type: 'string', format: 'ulid'}, schema);
		case 'ZodIp':
		case 'ip':
			return addDescription({type: 'string', format: 'ip'}, schema);
		case 'ZodBase64':
		case 'base64':
			return addDescription({type: 'string', format: 'byte'}, schema);
		case 'ZodDuration':
		case 'duration':
			return addDescription({type: 'string', format: 'duration'}, schema);
		default:
			return addDescription({type: 'object'}, schema);
	}
}

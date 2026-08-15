// SPDX-License-Identifier: AGPL-3.0-or-later
import type {ZodTypeAny} from 'zod';

const FLUXER_SCHEMA_NAME = Symbol('fluxer.openapi.schemaName');
const FLUXER_CUSTOM_TYPE_KEY = '__fluxer_custom_type__';

export interface ZodCheckDefinition {
	check?: unknown;
	type?: unknown;
	format?: unknown;
	minimum?: unknown;
	maximum?: unknown;
	value?: unknown;
	inclusive?: unknown;
	pattern?: unknown;
}

export interface ZodCheck {
	kind?: string;
	type?: string;
	value?: unknown;
	minimum?: unknown;
	maximum?: unknown;
	inclusive?: boolean;
	exact?: boolean;
	message?: string;
	regex?: RegExp;
	minValue?: number;
	maxValue?: number;
	isInt?: boolean;
	isFinite?: boolean;
	format?: string;
	def?: {
		check?: string;
		type?: string;
	};
	_zod?: {
		def?: ZodCheckDefinition;
	};
}

interface ZodSizeLimit {
	value?: number;
}

interface ZodInternalDefinition {
	typeName?: string;
	type?: string | ZodTypeAny;
	innerType?: ZodTypeAny;
	schema?: ZodTypeAny;
	in?: ZodTypeAny;
	out?: ZodTypeAny;
	shape?: (() => Record<string, ZodTypeAny>) | Record<string, ZodTypeAny>;
	options?: Array<ZodTypeAny> | Map<unknown, ZodTypeAny>;
	value?: unknown;
	values?: unknown;
	entries?: Record<string, unknown>;
	element?: ZodTypeAny;
	items?: Array<ZodTypeAny> | ZodTypeAny;
	checks?: Array<ZodCheck>;
	defaultValue?: unknown | (() => unknown);
	description?: string;
	rest?: ZodTypeAny;
	valueType?: ZodTypeAny;
	keyType?: ZodTypeAny;
	catchValue?: unknown | (() => unknown);
	left?: ZodTypeAny;
	right?: ZodTypeAny;
	discriminator?: string;
	minSize?: ZodSizeLimit;
	maxSize?: ZodSizeLimit;
	isoType?: string;
	kind?: string;
	getter?: () => ZodTypeAny;
	pattern?: RegExp;
	transform?: (input: unknown) => unknown;
}

interface ZodInternalSchema {
	_def?: ZodInternalDefinition;
	_zod?: {
		parent?: ZodTypeAny;
		values?: Set<unknown>;
	};
	values?: Set<unknown>;
	description?: string;
	minLength?: number | null;
	maxLength?: number | null;
	format?: string | null;
	_regex?: RegExp;
	[FLUXER_SCHEMA_NAME]?: string;
	[FLUXER_CUSTOM_TYPE_KEY]?: string;
}

interface ZodStringConstraintProperties {
	minLength?: number | null;
	maxLength?: number | null;
	format?: string | null;
	_regex?: RegExp;
}

function getInternalSchema(schema: ZodTypeAny): ZodInternalSchema {
	return schema as ZodInternalSchema;
}

export function getZodDefinition(schema: ZodTypeAny): ZodInternalDefinition {
	return getInternalSchema(schema)._def ?? {};
}

export function getZodParent(schema: ZodTypeAny): ZodTypeAny | undefined {
	return getInternalSchema(schema)._zod?.parent;
}

export function getZodValues(schema: ZodTypeAny): Set<unknown> | undefined {
	const internal = getInternalSchema(schema);
	return internal.values ?? internal._zod?.values;
}

export function getZodSchemaDescription(schema: ZodTypeAny): string | undefined {
	return getInternalSchema(schema).description;
}

export function getZodStringConstraintProperties(schema: ZodTypeAny): ZodStringConstraintProperties {
	const internal = getInternalSchema(schema);
	return {
		minLength: internal.minLength,
		maxLength: internal.maxLength,
		format: internal.format,
		_regex: internal._regex,
	};
}

export function setSchemaNameMetadata(schema: ZodTypeAny, name: string): void {
	getInternalSchema(schema)[FLUXER_SCHEMA_NAME] = name;
}

export function getSchemaNameMetadata(schema: ZodTypeAny): string | undefined {
	return getInternalSchema(schema)[FLUXER_SCHEMA_NAME];
}

export function getCustomTypeMetadata(schema: ZodTypeAny): string | undefined {
	return getInternalSchema(schema)[FLUXER_CUSTOM_TYPE_KEY];
}

export function getZodTypeFromUnknown(value: unknown): ZodTypeAny | undefined {
	if (typeof value !== 'object' || value === null) {
		return undefined;
	}
	if ('_def' in value || '_zod' in value) {
		return value as ZodTypeAny;
	}
	return undefined;
}

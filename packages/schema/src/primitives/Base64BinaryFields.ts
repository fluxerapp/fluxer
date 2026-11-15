// SPDX-License-Identifier: AGPL-3.0-or-later

import {schemaMetadata, withSchemaMetadata} from '@fluxer/schema/src/SchemaMetadata';
import type {z} from 'zod';

interface BodyFieldPaths {
	binary: Set<string>;
	other: Set<string>;
}

export function withBase64Binary<T extends z.ZodType>(schema: T): T {
	return withSchemaMetadata(schema, {...schemaMetadata.get(schema), base64Binary: true});
}

export function childBodyFieldPath(parent: string, key: string): string {
	return parent === '' ? key : `${parent}.${key}`;
}

export function arrayItemBodyFieldPath(parent: string): string {
	return `${parent}[]`;
}

export function collectBodyFieldPaths(schema: z.core.$ZodType): BodyFieldPaths {
	const paths: BodyFieldPaths = {binary: new Set(), other: new Set()};
	visitBodyField(schema, '', paths, new Set());
	return paths;
}

function visitBodyField(
	schema: z.core.$ZodType,
	path: string,
	paths: BodyFieldPaths,
	visiting: Set<z.core.$ZodType>,
): void {
	if (schemaMetadata.get(schema)?.base64Binary) {
		paths.binary.add(path);
		return;
	}
	if (visiting.has(schema)) return;
	visiting.add(schema);
	const def = (schema as z.core.$ZodTypes)._zod.def;
	switch (def.type) {
		case 'optional':
		case 'nullable':
		case 'default':
		case 'prefault':
		case 'nonoptional':
		case 'readonly':
		case 'catch':
			visitBodyField(def.innerType, path, paths, visiting);
			break;
		case 'pipe':
			visitBodyField(def.in._zod.def.type === 'transform' ? def.out : def.in, path, paths, visiting);
			break;
		case 'lazy':
			visitBodyField(def.getter(), path, paths, visiting);
			break;
		case 'union':
			for (const option of def.options) visitBodyField(option, path, paths, visiting);
			break;
		case 'intersection':
			visitBodyField(def.left, path, paths, visiting);
			visitBodyField(def.right, path, paths, visiting);
			break;
		case 'object':
			if (path !== '') paths.other.add(path);
			for (const [key, field] of Object.entries(def.shape)) {
				visitBodyField(field, childBodyFieldPath(path, key), paths, visiting);
			}
			break;
		case 'array':
			if (path !== '') paths.other.add(path);
			visitBodyField(def.element, arrayItemBodyFieldPath(path), paths, visiting);
			break;
		default:
			if (path !== '') paths.other.add(path);
	}
	visiting.delete(schema);
}

/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import * as fs from 'node:fs';
import * as path from 'node:path';

interface JsonSchema {
	$schema?: string;
	title?: string;
	type?: string;
	description?: string;
	additionalProperties?: boolean | JsonSchema;
	required?: Array<string>;
	properties?: Record<string, JsonSchema>;
	$ref?: string;
	$defs?: Record<string, JsonSchema>;
	anyOf?: Array<JsonSchema>;
	allOf?: Array<JsonSchema>;
	oneOf?: Array<JsonSchema>;
	enum?: Array<string | number | boolean>;
	default?: unknown;
	minimum?: number;
	maximum?: number;
	items?: JsonSchema;
	if?: JsonSchema;
	then?: JsonSchema;
	else?: JsonSchema;
	const?: unknown;
}

interface DefFile {
	[key: string]: JsonSchema;
}

const SCHEMA_DIR = path.dirname(new URL(import.meta.url).pathname);
const DEFS_DIR = path.join(SCHEMA_DIR, 'defs');
const ROOT_SCHEMA_PATH = path.join(SCHEMA_DIR, 'root.json');
const OUTPUT_SCHEMA_PATH = path.join(SCHEMA_DIR, '..', 'ConfigSchema.json');
const OUTPUT_ZOD_PATH = path.join(SCHEMA_DIR, '..', 'MasterZodSchema.generated.tsx');

function readJsonFile<T>(filePath: string): T {
	const content = fs.readFileSync(filePath, 'utf-8');
	return JSON.parse(content) as T;
}

function collectDefFiles(dir: string): Array<string> {
	const files: Array<string> = [];
	const entries = fs.readdirSync(dir, {withFileTypes: true});

	for (const entry of entries) {
		const fullPath = path.join(dir, entry.name);
		if (entry.isDirectory()) {
			files.push(...collectDefFiles(fullPath));
		} else if (entry.name.endsWith('.json')) {
			files.push(fullPath);
		}
	}

	return files;
}

function bundleSchema(): JsonSchema {
	const rootSchema = readJsonFile<JsonSchema>(ROOT_SCHEMA_PATH);
	const defFiles = collectDefFiles(DEFS_DIR);

	const allDefs: Record<string, JsonSchema> = {};

	for (const defFile of defFiles) {
		const defs = readJsonFile<DefFile>(defFile);
		for (const [name, schema] of Object.entries(defs)) {
			allDefs[name] = schema;
		}
	}

	rootSchema.$defs = allDefs;
	stripAdditionalPropertiesFalse(rootSchema);

	return rootSchema;
}

function stripAdditionalPropertiesFalse(schema: JsonSchema): void {
	// We want configs to be forward-compatible and allow extra keys, but still document
	// the set of known properties in the schema itself.
	if (schema.additionalProperties === false) {
		delete schema.additionalProperties;
	}

	if (schema.$defs) {
		for (const def of Object.values(schema.$defs)) {
			stripAdditionalPropertiesFalse(def);
		}
	}

	if (schema.properties) {
		for (const prop of Object.values(schema.properties)) {
			stripAdditionalPropertiesFalse(prop);
		}
	}

	if (schema.items) {
		stripAdditionalPropertiesFalse(schema.items);
	}

	if (schema.if) {
		stripAdditionalPropertiesFalse(schema.if);
	}
	if (schema.then) {
		stripAdditionalPropertiesFalse(schema.then);
	}
	if (schema.else) {
		stripAdditionalPropertiesFalse(schema.else);
	}

	if (schema.anyOf) {
		for (const sub of schema.anyOf) {
			stripAdditionalPropertiesFalse(sub);
		}
	}
	if (schema.allOf) {
		for (const sub of schema.allOf) {
			stripAdditionalPropertiesFalse(sub);
		}
	}
	if (schema.oneOf) {
		for (const sub of schema.oneOf) {
			stripAdditionalPropertiesFalse(sub);
		}
	}

	if (schema.additionalProperties && typeof schema.additionalProperties === 'object') {
		stripAdditionalPropertiesFalse(schema.additionalProperties);
	}
}

function extractRefName(ref: string): string {
	const match = ref.match(/#\/\$defs\/(.+)/);
	if (match) {
		return match[1];
	}
	throw new Error(`Invalid $ref format: ${ref}`);
}

function snakeToPascal(str: string): string {
	return str
		.split('_')
		.map((part) => part.charAt(0).toUpperCase() + part.slice(1))
		.join('');
}

function buildDependencyGraph(defs: Record<string, JsonSchema>): Map<string, Set<string>> {
	const graph = new Map<string, Set<string>>();

	for (const name of Object.keys(defs)) {
		graph.set(name, new Set());
	}

	function collectRefs(schema: JsonSchema, currentDef: string): void {
		if (schema.$ref) {
			const refName = extractRefName(schema.$ref);
			graph.get(currentDef)?.add(refName);
		}

		if (schema.properties) {
			for (const propSchema of Object.values(schema.properties)) {
				collectRefs(propSchema, currentDef);
			}
		}

		if (schema.items) {
			collectRefs(schema.items, currentDef);
		}

		if (schema.if) {
			collectRefs(schema.if, currentDef);
		}
		if (schema.then) {
			collectRefs(schema.then, currentDef);
		}
		if (schema.else) {
			collectRefs(schema.else, currentDef);
		}
	}

	for (const [name, schema] of Object.entries(defs)) {
		collectRefs(schema, name);
	}

	return graph;
}

function topologicalSort(graph: Map<string, Set<string>>): Array<string> {
	const visited = new Set<string>();
	const result: Array<string> = [];

	function visit(node: string): void {
		if (visited.has(node)) {
			return;
		}
		visited.add(node);

		const deps = graph.get(node);
		if (deps) {
			for (const dep of deps) {
				visit(dep);
			}
		}

		result.push(node);
	}

	for (const node of graph.keys()) {
		visit(node);
	}

	return result;
}

function generateZodType(schema: JsonSchema, defs: Record<string, JsonSchema>, indent: string = ''): string {
	if (schema.$ref) {
		const refName = extractRefName(schema.$ref);
		return `${snakeToPascal(refName)}Schema`;
	}

	if (schema.enum) {
		const enumValues = schema.enum.map((v) => JSON.stringify(v)).join(', ');
		return `z.enum([${enumValues}])`;
	}

	if (schema.type === 'string') {
		return 'z.string()';
	}

	if (schema.type === 'number' || schema.type === 'integer') {
		let result = 'z.number()';
		if (schema.minimum !== undefined) {
			result += `.min(${schema.minimum})`;
		}
		if (schema.maximum !== undefined) {
			result += `.max(${schema.maximum})`;
		}
		return result;
	}

	if (schema.type === 'boolean') {
		return 'z.boolean()';
	}

	if (schema.type === 'array') {
		if (schema.items) {
			const itemType = generateZodType(schema.items, defs, indent);
			return `z.array(${itemType})`;
		}
		return 'z.array(z.unknown())';
	}

	if (schema.type === 'object') {
		return generateZodObject(schema, defs, indent);
	}

	return 'z.unknown()';
}

function generateZodObject(schema: JsonSchema, defs: Record<string, JsonSchema>, indent: string = ''): string {
	if (!schema.properties) {
		return 'z.object({})';
	}

	const requiredSet = new Set(schema.required || []);
	const lines: Array<string> = [];
	const innerIndent = `${indent}\t`;

	for (const [propName, propSchema] of Object.entries(schema.properties)) {
		let propType = generateZodType(propSchema, defs, innerIndent);

		if (propSchema.description) {
			propType += `.describe(${JSON.stringify(propSchema.description)})`;
		}

		if (propSchema.default !== undefined) {
			if (
				propSchema.$ref &&
				typeof propSchema.default === 'object' &&
				propSchema.default !== null &&
				Object.keys(propSchema.default).length === 0
			) {
				const refName = extractRefName(propSchema.$ref);
				const schemaName = `${snakeToPascal(refName)}Schema`;
				propType += `.default(() => ${schemaName}.parse({}))`;
			} else if (
				typeof propSchema.default === 'object' &&
				propSchema.default !== null &&
				!Array.isArray(propSchema.default) &&
				Object.keys(propSchema.default).length === 0
			) {
				propType += '.default(() => ({}))';
			} else {
				propType += `.default(${JSON.stringify(propSchema.default)})`;
			}
		} else if (!requiredSet.has(propName)) {
			propType += '.optional()';
		}

		lines.push(`${innerIndent}${propName}: ${propType},`);
	}

	return `z.object({\n${lines.join('\n')}\n${indent}})`;
}

function generateZodSchema(bundledSchema: JsonSchema): string {
	const defs = bundledSchema.$defs || {};

	const depGraph = buildDependencyGraph(defs);
	const sortedDefs = topologicalSort(depGraph);

	const defSchemas: Array<string> = [];

	for (const defName of sortedDefs) {
		const defSchema = defs[defName];
		if (!defSchema) {
			continue;
		}

		const schemaName = `${snakeToPascal(defName)}Schema`;
		const zodType = generateZodType(defSchema, defs, '');

		defSchemas.push(`export const ${schemaName} = ${zodType};`);
	}

	const rootZodType = generateRootSchema(bundledSchema, defs);

	const output = `/*
 * Copyright (C) 2026 Fluxer Contributors
 * AUTO-GENERATED FILE - DO NOT EDIT
 * Generated from ConfigSchema.json by schema/bundle.ts
 */

import {z} from 'zod';

${defSchemas.join('\n\n')}

export const MasterConfigSchema = ${rootZodType};
export type MasterConfigSchema = z.infer<typeof MasterConfigSchema>;

import type {DerivedEndpoints} from './EndpointDerivation';

export type MasterConfig = MasterConfigSchema & {
	endpoints: DerivedEndpoints;
};
`;

	return output;
}

function generateRootSchema(schema: JsonSchema, defs: Record<string, JsonSchema>): string {
	if (!schema.properties) {
		return 'z.object({})';
	}

	const requiredSet = new Set(schema.required || []);
	const lines: Array<string> = [];

	for (const [propName, propSchema] of Object.entries(schema.properties)) {
		let propType: string;

		if (propSchema.$ref) {
			const refName = extractRefName(propSchema.$ref);
			propType = `${snakeToPascal(refName)}Schema`;
		} else if (propSchema.enum) {
			const enumValues = propSchema.enum.map((v) => JSON.stringify(v)).join(', ');
			propType = `z.enum([${enumValues}])`;
		} else if (propSchema.type === 'string') {
			propType = 'z.string()';
		} else if (propSchema.type === 'number' || propSchema.type === 'integer') {
			propType = 'z.number()';
			if (propSchema.minimum !== undefined) {
				propType += `.min(${propSchema.minimum})`;
			}
			if (propSchema.maximum !== undefined) {
				propType += `.max(${propSchema.maximum})`;
			}
		} else if (propSchema.type === 'boolean') {
			propType = 'z.boolean()';
		} else if (propSchema.type === 'array') {
			if (propSchema.items) {
				const itemType = generateZodType(propSchema.items, defs, '\t\t');
				propType = `z.array(${itemType})`;
			} else {
				propType = 'z.array(z.unknown())';
			}
		} else if (propSchema.type === 'object') {
			propType = generateZodObject(propSchema, defs, '\t');
		} else {
			propType = 'z.unknown()';
		}

		if (propSchema.description) {
			propType += `.describe(${JSON.stringify(propSchema.description)})`;
		}

		if (propSchema.default !== undefined) {
			if (
				propSchema.$ref &&
				typeof propSchema.default === 'object' &&
				propSchema.default !== null &&
				Object.keys(propSchema.default).length === 0
			) {
				const refName = extractRefName(propSchema.$ref);
				const schemaName = `${snakeToPascal(refName)}Schema`;
				propType += `.default(() => ${schemaName}.parse({}))`;
			} else if (
				typeof propSchema.default === 'object' &&
				propSchema.default !== null &&
				!Array.isArray(propSchema.default) &&
				Object.keys(propSchema.default).length === 0
			) {
				propType += '.default(() => ({}))';
			} else {
				propType += `.default(${JSON.stringify(propSchema.default)})`;
			}
		} else if (!requiredSet.has(propName)) {
			propType += '.optional()';
		}

		lines.push(`\t${propName}: ${propType},`);
	}

	return `z.object({\n${lines.join('\n')}\n})`;
}

function main(): void {
	console.log('Bundling JSON Schema...');
	const bundledSchema = bundleSchema();

	console.log(`Writing bundled schema to ${OUTPUT_SCHEMA_PATH}`);
	fs.writeFileSync(OUTPUT_SCHEMA_PATH, `${JSON.stringify(bundledSchema, null, '\t')}\n`);

	console.log('Generating Zod schema...');
	const zodSchema = generateZodSchema(bundledSchema);

	console.log(`Writing Zod schema to ${OUTPUT_ZOD_PATH}`);
	fs.writeFileSync(OUTPUT_ZOD_PATH, zodSchema);

	console.log('Done!');
}

main();

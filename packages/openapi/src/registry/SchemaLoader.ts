// SPDX-License-Identifier: AGPL-3.0-or-later
import * as fs from 'node:fs';
import * as path from 'node:path';
import {pathToFileURL} from 'node:url';
import {setSchemaName, zodToOpenAPISchema} from '@fluxer/openapi/src/converters/ZodToOpenAPI';
import type {OpenAPISchema} from '@fluxer/openapi/src/Types';
import type {z} from 'zod';
export interface LoadedSchema {
	name: string;
	zodSchema: z.ZodTypeAny;
	openAPISchema: OpenAPISchema;
}
function discoverSchemaModules(rootDir: string): Array<string> {
	if (!fs.existsSync(rootDir)) {
		return [];
	}
	const results: Array<string> = [];
	const stack: Array<string> = [rootDir];
	while (stack.length > 0) {
		const currentDir = stack.pop();
		if (!currentDir) break;
		const entries = fs.readdirSync(currentDir, {withFileTypes: true});
		for (const entry of entries) {
			const fullPath = path.join(currentDir, entry.name);
			if (entry.isDirectory()) {
				if (entry.name === 'tests' || entry.name === 'node_modules') continue;
				stack.push(fullPath);
				continue;
			}
			if (!entry.isFile()) continue;
			if (!entry.name.endsWith('.ts')) continue;
			if (entry.name.endsWith('.test.ts')) continue;
			results.push(fullPath);
		}
	}
	return results.sort();
}
function getModulePaths(basePath: string): Array<string> {
	const schemaDomains = path.join(basePath, 'packages', 'schema', 'src', 'domains');
	return discoverSchemaModules(schemaDomains);
}
function isZodSchema(value: unknown): value is z.ZodTypeAny {
	return (
		value !== null &&
		typeof value === 'object' &&
		'_def' in value &&
		typeof (
			value as {
				parse?: unknown;
			}
		).parse === 'function'
	);
}
export async function loadSchemas(basePath: string): Promise<Map<string, LoadedSchema>> {
	const schemas = new Map<string, LoadedSchema>();
	const collectedSchemas: Array<{
		name: string;
		zodSchema: z.ZodTypeAny;
	}> = [];
	const modulePaths = getModulePaths(basePath);
	for (const modulePath of modulePaths) {
		try {
			const moduleExports = await import(pathToFileURL(modulePath).href);
			for (const [exportName, exportValue] of Object.entries(moduleExports)) {
				if (exportName.startsWith('_')) {
					continue;
				}
				if (typeof exportValue === 'function') {
					continue;
				}
				if (isZodSchema(exportValue)) {
					collectedSchemas.push({name: exportName, zodSchema: exportValue});
				}
			}
		} catch (error) {
			console.warn(`Warning: Could not load schema module ${modulePath}:`, error);
		}
	}
	for (const {name, zodSchema} of collectedSchemas) {
		setSchemaName(zodSchema, name);
	}
	for (const {name, zodSchema} of collectedSchemas) {
		const openAPISchemaOrRef = zodToOpenAPISchema(zodSchema);
		if ('$ref' in openAPISchemaOrRef) {
			throw new Error(`Top-level schema export must not be a $ref: ${name}`);
		}
		const openAPISchema: OpenAPISchema = openAPISchemaOrRef;
		schemas.set(name, {
			name,
			zodSchema,
			openAPISchema,
		});
	}
	return schemas;
}

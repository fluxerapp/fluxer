// SPDX-License-Identifier: AGPL-3.0-or-later
import assert from 'node:assert/strict';
import * as fs from 'node:fs';
import * as path from 'node:path';
import {pathToFileURL} from 'node:url';
import {z} from 'zod';

function discoverSchemaModules(rootDir: string): Array<string> {
	const results: Array<string> = [];
	const stack: Array<string> = [rootDir];
	while (stack.length > 0) {
		const currentDir = stack.pop();
		assert(currentDir !== undefined, 'Schema discovery stack must contain a directory');
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
export async function loadSchemas(basePath: string): Promise<Map<string, z.ZodType>> {
	const schemas = new Map<string, z.ZodType>();
	const modulePaths = getModulePaths(basePath);
	for (const modulePath of modulePaths) {
		try {
			const moduleExports = await import(pathToFileURL(modulePath).href);
			for (const [exportName, exportValue] of Object.entries(moduleExports)) {
				if (exportName.startsWith('_')) {
					continue;
				}
				if (!(exportValue instanceof z.ZodType)) continue;
				const existing = schemas.get(exportName);
				if (existing && existing !== exportValue) throw new Error(`Duplicate schema export: ${exportName}`);
				schemas.set(exportName, exportValue);
			}
		} catch (error) {
			throw new Error(`Could not load schema module ${modulePath}`, {cause: error});
		}
	}
	return schemas;
}

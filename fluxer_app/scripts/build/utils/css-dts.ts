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
import postcss from 'postcss';
import postcssModules from 'postcss-modules';
import {PKGS_DIR, SRC_DIR} from '../config';

const RESERVED_KEYWORDS = new Set([
	'break',
	'case',
	'catch',
	'continue',
	'debugger',
	'default',
	'delete',
	'do',
	'else',
	'export',
	'extends',
	'finally',
	'for',
	'function',
	'if',
	'import',
	'in',
	'instanceof',
	'new',
	'return',
	'super',
	'switch',
	'this',
	'throw',
	'try',
	'typeof',
	'var',
	'void',
	'while',
	'with',
	'yield',
	'enum',
	'implements',
	'interface',
	'let',
	'package',
	'private',
	'protected',
	'public',
	'static',
	'await',
	'class',
	'const',
]);

function isValidIdentifier(name: string): boolean {
	if (RESERVED_KEYWORDS.has(name)) {
		return false;
	}
	return /^[a-zA-Z_$][a-zA-Z0-9_$]*$/.test(name);
}

function generateDtsContent(classNames: Record<string, string>): string {
	const validClassNames = Object.keys(classNames).filter(isValidIdentifier);
	const typeMembers = validClassNames.map((name) => `\treadonly ${name}: string;`).join('\n');
	const defaultExportType =
		validClassNames.length > 0 ? `{\n${typeMembers}\n\treadonly [key: string]: string;\n}` : 'Record<string, string>';

	return `declare const styles: ${defaultExportType};\nexport default styles;\n`;
}

async function findCssModuleFiles(dir: string): Promise<Array<string>> {
	const files: Array<string> = [];

	async function walk(currentDir: string): Promise<void> {
		const entries = await fs.promises.readdir(currentDir, {withFileTypes: true});

		for (const entry of entries) {
			const fullPath = path.join(currentDir, entry.name);

			if (entry.isDirectory()) {
				if (entry.name !== 'node_modules' && entry.name !== 'dist' && entry.name !== '.git') {
					await walk(fullPath);
				}
			} else if (entry.name.endsWith('.module.css')) {
				files.push(fullPath);
			}
		}
	}

	await walk(dir);
	return files;
}

async function generateDtsForFile(cssPath: string): Promise<void> {
	const cssContent = await fs.promises.readFile(cssPath, 'utf-8');
	let exportedClassNames: Record<string, string> = {};

	await postcss([
		postcssModules({
			localsConvention: 'camelCaseOnly',
			generateScopedName: '[name]__[local]___[hash:base64:5]',
			getJSON(_cssFileName: string, json: Record<string, string>) {
				exportedClassNames = json;
			},
		}),
	]).process(cssContent, {from: cssPath});

	const dtsPath = `${cssPath}.d.ts`;
	const dtsContent = generateDtsContent(exportedClassNames);
	await fs.promises.writeFile(dtsPath, dtsContent);
}

export async function generateCssDtsForFile(cssPath: string): Promise<void> {
	if (!cssPath.endsWith('.module.css')) {
		return;
	}
	await generateDtsForFile(cssPath);
}

export async function generateAllCssDts(): Promise<void> {
	const srcFiles = await findCssModuleFiles(SRC_DIR);
	const pkgsFiles = await findCssModuleFiles(PKGS_DIR);
	const allFiles = [...srcFiles, ...pkgsFiles];

	console.log(`Generating .d.ts files for ${allFiles.length} CSS modules...`);

	await Promise.all(allFiles.map(generateDtsForFile));

	console.log(`Generated ${allFiles.length} CSS module type definitions.`);
}

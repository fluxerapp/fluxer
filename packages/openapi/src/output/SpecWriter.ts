// SPDX-License-Identifier: AGPL-3.0-or-later
import {execFileSync} from 'node:child_process';
import {randomUUID} from 'node:crypto';
import * as fs from 'node:fs';
import {createRequire} from 'node:module';
import * as path from 'node:path';
import type {OpenAPIDocument} from '@fluxer/openapi/src/OpenAPITypes';
export type WritableOpenAPISpec = OpenAPIDocument;
const require = createRequire(import.meta.url);
function formatSpec(spec: WritableOpenAPISpec): string {
	return execFileSync(
		process.execPath,
		[
			require.resolve('@biomejs/biome/bin/biome'),
			'format',
			'--stdin-file-path',
			path.join(import.meta.dirname, 'openapi.json'),
			'--files-max-size=16777216',
		],
		{input: JSON.stringify(spec), encoding: 'utf-8', maxBuffer: 16777216},
	);
}
export function writeSpec(spec: WritableOpenAPISpec, outputPath: string): void {
	const dir = path.dirname(outputPath);
	if (!fs.existsSync(dir)) {
		fs.mkdirSync(dir, {recursive: true});
	}
	const formatted = formatSpec(spec);
	const tempPath = `${outputPath}.${randomUUID()}.tmp`;
	try {
		fs.writeFileSync(tempPath, formatted, {encoding: 'utf-8', flag: 'wx'});
		fs.renameSync(tempPath, outputPath);
	} finally {
		fs.rmSync(tempPath, {force: true});
	}
}
export function readSpec(inputPath: string): unknown {
	const content = fs.readFileSync(inputPath, 'utf-8');
	return JSON.parse(content);
}
export function getApiPackageOutputPath(basePath: string): string {
	return path.join(basePath, 'fluxer_api', 'src', 'api', 'openapi', 'openapi.json');
}
export function getAdminOutputPath(basePath: string): string {
	return path.join(basePath, 'fluxer_admin', 'openapi-admin.json');
}

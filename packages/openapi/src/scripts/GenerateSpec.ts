#!/usr/bin/env tsx
// SPDX-License-Identifier: AGPL-3.0-or-later
import * as fs from 'node:fs';
import * as path from 'node:path';
import type {OpenAPIGenerationStats, SkippedRoute} from '@fluxer/openapi/src/OpenAPIGenerationTypes';
import {OpenAPIGenerator} from '@fluxer/openapi/src/OpenAPIGenerator';
import {printValidationResult, validateSpec} from '@fluxer/openapi/src/output/SpecValidator';
import {
	getAdminOutputPath,
	getApiPackageOutputPath,
	readSpec,
	type WritableOpenAPISpec,
	writeSpec,
} from '@fluxer/openapi/src/output/SpecWriter';

type GenerateTarget = 'admin' | 'public';
interface GenerateOptions {
	validateOnly: boolean;
	outputPath: string | null;
	target: GenerateTarget | null;
}
interface GeneratedTargetSpec {
	target: GenerateTarget;
	outputPath: string;
	spec: WritableOpenAPISpec;
}
const API_DESCRIPTION =
	'API for Fluxer, a free and open source instant messaging and VoIP chat app built for friends, groups, and communities.';
function parseArgs(): GenerateOptions {
	const args = process.argv.slice(2);
	let validateOnly = false;
	let outputPath: string | null = null;
	let target: GenerateTarget | null = null;
	for (let i = 0; i < args.length; i++) {
		const arg = args[i];
		if (arg === '--validate-only' || arg === '-v') {
			validateOnly = true;
		} else if (arg === '--output' || arg === '-o') {
			const value = args[++i];
			if (!value || value.startsWith('-')) {
				throw new Error('--output requires a file path.');
			}
			outputPath = value;
		} else if (arg === '--target' || arg === '-t') {
			const value = args[++i];
			if (value !== 'admin' && value !== 'public') {
				throw new Error(`Invalid --target "${value}". Expected "public" or "admin".`);
			}
			target = value;
		} else {
			throw new Error(`Unknown argument: ${arg}`);
		}
	}
	return {validateOnly, outputPath, target};
}
function findRepositoryRoot(): string {
	let dir = process.cwd();
	for (;;) {
		const workspacePath = path.join(dir, 'pnpm-workspace.yaml');
		if (fs.existsSync(workspacePath)) {
			return dir;
		}
		const parent = path.dirname(dir);
		if (parent === dir) {
			break;
		}
		dir = parent;
	}
	throw new Error('Could not find repository root (no pnpm-workspace.yaml found)');
}
function getTargetOutputPath(basePath: string, target: GenerateTarget, customOutputPath: string | null): string {
	if (customOutputPath) {
		return customOutputPath;
	}
	return target === 'admin' ? getAdminOutputPath(basePath) : getApiPackageOutputPath(basePath);
}
function reportRoutesLeftOut(target: GenerateTarget, stats: OpenAPIGenerationStats): void {
	const groups: Array<[string, ReadonlyArray<SkippedRoute>]> = [
		['registered but not written to the spec', stats.skippedRoutes],
		['registered but not expressible as an OpenAPI path', stats.untemplatableRoutes],
	];
	for (const [title, routes] of groups) {
		if (routes.length === 0) {
			continue;
		}
		console.log(`${target} routes ${title}: ${routes.length.toString()}`);
		for (const route of [...routes].sort((a, b) => `${a.path} ${a.method}`.localeCompare(`${b.path} ${b.method}`))) {
			console.log(`  ${route.method} ${route.path}  ${route.reason}  (${route.source})`);
		}
	}
}
async function buildTargetSpec(basePath: string, target: GenerateTarget): Promise<WritableOpenAPISpec> {
	const generator = new OpenAPIGenerator({
		basePath,
		title: 'Fluxer API',
		version: '1.0.0',
		description: API_DESCRIPTION,
		serverUrl: 'https://api.fluxer.app/v1',
		routeScope: target,
		schemaTarget: target === 'admin' ? 'openapi-3.0' : 'draft-2020-12',
	});
	const {document, stats} = await generator.generateWithStats();
	reportRoutesLeftOut(target, stats);
	return document;
}
async function validateTargetSpec(target: GenerateTarget, spec: unknown): Promise<boolean> {
	const validationResult = await validateSpec(spec, {
		allowedOpenAPIVersions: target === 'admin' ? ['3.0.3'] : ['3.1.0'],
	});
	printValidationResult(validationResult);
	return validationResult.valid;
}
function printSummary(target: GenerateTarget, spec: WritableOpenAPISpec): void {
	const {
		paths,
		components: {schemas},
	} = spec;
	let operationCount = 0;
	for (const pathItem of Object.values(paths)) {
		operationCount += Object.keys(pathItem).length;
	}
	console.log(`Target: ${target}`);
	console.log(`Paths: ${Object.keys(paths).length}`);
	console.log(`Operations: ${operationCount}`);
	console.log(`Schemas: ${Object.keys(schemas).length}`);
}
async function main(): Promise<void> {
	const {validateOnly, outputPath: customOutputPath, target: requestedTarget} = parseArgs();
	const basePath = findRepositoryRoot();
	if (customOutputPath && !requestedTarget) {
		throw new Error('--output requires --target when generating or validating multiple specs.');
	}
	const targets: Array<GenerateTarget> = requestedTarget ? [requestedTarget] : ['public', 'admin'];
	console.log('Fluxer OpenAPI Specification Generator');
	console.log('======================================');
	console.log(`Base path: ${basePath}`);
	console.log(`Targets: ${targets.join(', ')}`);
	console.log('');
	if (validateOnly) {
		console.log('Running validation only...');
		let valid = true;
		for (const target of targets) {
			const outputPath = getTargetOutputPath(basePath, target, customOutputPath);
			console.log('');
			console.log(`Validating ${target} specification at ${outputPath}...`);
			try {
				const spec = readSpec(outputPath);
				valid = (await validateTargetSpec(target, spec)) && valid;
			} catch (error) {
				console.error(`Failed to validate ${target} specification:`, error);
				valid = false;
			}
		}
		process.exit(valid ? 0 : 1);
	}
	try {
		const generated: Array<GeneratedTargetSpec> = [];
		for (const target of targets) {
			const outputPath = getTargetOutputPath(basePath, target, customOutputPath);
			console.log(`Generating ${target} specification...`);
			const spec = await buildTargetSpec(basePath, target);
			console.log(`Validating ${target} specification...`);
			const valid = await validateTargetSpec(target, spec);
			if (!valid) {
				throw new Error(`${target} specification has validation errors; no specifications were written.`);
			}
			generated.push({target, outputPath, spec});
		}
		for (const {target, outputPath, spec} of generated) {
			console.log(`Writing ${target} specification to ${outputPath}...`);
			writeSpec(spec, outputPath);
			printSummary(target, spec);
			console.log('');
		}
		console.log('OpenAPI specifications generated successfully.');
	} catch (error) {
		console.error('Failed to generate specification:', error);
		process.exit(1);
	}
}
main().catch((error) => {
	console.error('Unhandled error:', error);
	process.exit(1);
});

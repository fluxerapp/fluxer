// SPDX-License-Identifier: AGPL-3.0-or-later
import {BaseResolver, createConfig, lintFromString, type Source} from '@redocly/openapi-core';

interface ValidationIssue {
	path: string;
	message: string;
}

interface ValidationResult {
	valid: boolean;
	errors: Array<ValidationIssue>;
	warnings: Array<ValidationIssue>;
}

interface SpecValidationOptions {
	readonly allowedOpenAPIVersions?: ReadonlyArray<string>;
}

class StandaloneSpecResolver extends BaseResolver {
	public override async loadExternalRef(absoluteRef: string): Promise<Source> {
		throw new Error(`Generated specifications must be self-contained; external reference: ${absoluteRef}`);
	}
}

export async function validateSpec(spec: unknown, options?: SpecValidationOptions): Promise<ValidationResult> {
	const errors: Array<ValidationIssue> = [];
	const warnings: Array<ValidationIssue> = [];
	const allowedVersions = options?.allowedOpenAPIVersions ?? ['3.1.0'];
	const version = typeof spec === 'object' && spec !== null && 'openapi' in spec ? spec.openapi : undefined;
	if (typeof version !== 'string' || !allowedVersions.includes(version)) {
		errors.push({path: 'openapi', message: `Expected ${allowedVersions.join(' or ')}, got "${version}"`});
		return {valid: false, errors, warnings};
	}

	const config = await createConfig({
		extends: [],
		rules: {
			struct: 'error',
			'no-unresolved-refs': 'error',
			'no-identical-paths': 'error',
			'no-ambiguous-paths': 'error',
			'no-enum-type-mismatch': 'error',
			'no-schema-type-mismatch': 'error',
			'no-invalid-schema-examples': 'error',
			'no-invalid-parameter-examples': 'error',
			'no-invalid-media-type-examples': 'error',
			'operation-operationId': 'error',
			'operation-operationId-unique': 'error',
			'operation-parameters-unique': 'error',
			'path-declaration-must-exist': 'error',
			'path-parameters-defined': 'error',
			'security-defined': 'error',
			'security-scopes-defined': 'error',
			'spec-components-invalid-map-name': 'error',
			'spec-ref-siblings': 'error',
		},
	});
	const problems = await lintFromString({
		source: JSON.stringify(spec),
		config,
		externalRefResolver: new StandaloneSpecResolver(),
	});
	for (const problem of problems) {
		const issue = {
			path: problem.location.map((location) => location.pointer ?? '#').join(', '),
			message: `${problem.ruleId}: ${problem.message}`,
		};
		if (problem.severity === 'error') {
			errors.push(issue);
		} else {
			warnings.push(issue);
		}
	}
	return {valid: errors.length === 0, errors, warnings};
}

export function printValidationResult(result: ValidationResult): void {
	console.log(result.valid ? 'Validation passed' : 'Validation failed');
	for (const [heading, issues] of [
		['Errors', result.errors],
		['Warnings', result.warnings],
	] as const) {
		if (issues.length === 0) {
			continue;
		}
		console.log(`\n${heading}:`);
		for (const issue of issues) {
			console.log(`  - [${issue.path}] ${issue.message}`);
		}
	}
}

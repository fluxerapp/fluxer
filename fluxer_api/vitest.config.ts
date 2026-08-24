// SPDX-License-Identifier: AGPL-3.0-or-later

import {availableParallelism} from 'node:os';
import tsconfigPaths from 'vite-tsconfig-paths';
import {configDefaults, defineConfig} from 'vitest/config';

function parseParallelInteger(value: string | undefined, fallback: number): number {
	if (!value) {
		return fallback;
	}
	const parsed = Number.parseInt(value, 10);
	if (!Number.isInteger(parsed) || parsed < 1) {
		return fallback;
	}
	return parsed;
}

function resolveDefaultParallelWorkers(): number {
	return Math.max(2, availableParallelism() - 1);
}

const DEFAULT_PARALLEL_WORKERS = resolveDefaultParallelWorkers();
const configuredMaxWorkers = parseParallelInteger(process.env.API_TEST_MAX_WORKERS, DEFAULT_PARALLEL_WORKERS);
const configuredMaxConcurrency = parseParallelInteger(process.env.API_TEST_MAX_CONCURRENCY, configuredMaxWorkers);

const MODULE_REGISTRY_TEST_FILES = [
	'src/api/gif/GifRequestCountry.test.ts',
	'src/api/risk/__tests__/AccountPolicyService.test.ts',
];

const sharedExclude = [
	...configDefaults.exclude,
	'pkgs/**',
	'../fluxer_desktop/**',
	'**/target/**',
	'**/*Integration.test.ts',
	'**/*ExttestIntegration.test.ts',
];

const sharedTestConfig = {
	globals: true,
	environment: 'node' as const,
	setupFiles: ['./src/api/test/Setup.ts'],
	pool: 'threads' as const,
	testTimeout: 40000,
	hookTimeout: 20000,
	maxConcurrency: configuredMaxConcurrency,
};

export default defineConfig({
	root: process.cwd(),
	cacheDir: './node_modules/.vitest',
	test: {
		maxWorkers: configuredMaxWorkers,
		maxConcurrency: configuredMaxConcurrency,
		fileParallelism: true,
		reporters: ['default', 'json'],
		outputFile: './test-results.json',
		coverage: {
			provider: 'v8',
			reporter: ['text', 'text-summary', 'json', 'html'],
			reportsDirectory: './coverage',
			exclude: [
				'**/node_modules/tests/test*.test.tsx',
				'**/*.test.ts',
				'**/Setup.tsx',
				'**/TestConstants.tsx',
				'**/TestRequestBuilder.tsx',
				'**/TestHelpers.tsx',
			],
		},
		projects: [
			{
				plugins: [tsconfigPaths()],
				test: {
					...sharedTestConfig,
					name: 'api',
					include: ['src/**/*.{test,spec}.{ts,tsx}'],
					exclude: [...sharedExclude, ...MODULE_REGISTRY_TEST_FILES],
					isolate: false,
				},
			},
			{
				plugins: [tsconfigPaths()],
				test: {
					...sharedTestConfig,
					name: 'api-module-registry',
					include: MODULE_REGISTRY_TEST_FILES,
					exclude: sharedExclude,
					isolate: true,
				},
			},
		],
	},
});

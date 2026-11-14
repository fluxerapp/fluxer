// SPDX-License-Identifier: AGPL-3.0-or-later

import {availableParallelism} from 'node:os';
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
	'src/api/voice/tests/VoiceAvailabilityService.test.ts',
	'src/api/billing_documents/BillingDocuments.test.ts',
];

const INSTANCE_POLICY_TEST_FILES = ['src/api/admin/tests/InstanceConfigPendingRegistrationApproval.test.ts'];

const NATIVE_TIMEZONE_TEST_FILES = ['src/api/utils/AgeUtils.test.ts'];

const sharedExclude = [...configDefaults.exclude, 'pkgs/**', '../fluxer_desktop/**', '**/target/**'];

const sharedTestConfig = {
	globals: true,
	environment: 'node' as const,
	setupFiles: ['./src/api/test/Setup.ts'],
	pool: 'threads' as const,
	testTimeout: 40000,
	hookTimeout: 20000,
	maxConcurrency: configuredMaxConcurrency,
	execArgv: ['--no-experimental-webstorage', '--disable-warning=ExperimentalWarning'],
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

		projects: [
			{
				resolve: {tsconfigPaths: true},
				test: {
					...sharedTestConfig,
					name: 'api',
					include: ['src/**/*.{test,spec}.{ts,tsx}'],
					exclude: [
						...sharedExclude,
						...MODULE_REGISTRY_TEST_FILES,
						...INSTANCE_POLICY_TEST_FILES,
						...NATIVE_TIMEZONE_TEST_FILES,
					],
					isolate: false,
				},
			},
			{
				resolve: {tsconfigPaths: true},
				test: {
					...sharedTestConfig,
					name: 'api-module-registry',
					include: MODULE_REGISTRY_TEST_FILES,
					exclude: sharedExclude,
					isolate: true,
				},
			},
			{
				resolve: {tsconfigPaths: true},
				test: {
					...sharedTestConfig,
					name: 'api-instance-policy',
					include: INSTANCE_POLICY_TEST_FILES,
					exclude: sharedExclude,
					isolate: true,
				},
			},
			{
				resolve: {tsconfigPaths: true},
				test: {
					...sharedTestConfig,
					name: 'api-native-timezones',
					include: NATIVE_TIMEZONE_TEST_FILES,
					exclude: sharedExclude,
					pool: 'forks',
					isolate: true,
				},
			},
		],
	},
});

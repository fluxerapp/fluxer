// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineConfig} from 'vitest/config';

export default defineConfig({
	root: process.cwd(),
	resolve: {tsconfigPaths: true},
	cacheDir: './node_modules/.vitest',
	test: {
		globals: true,
		environment: 'node',
		pool: 'threads',
		fileParallelism: true,
		maxConcurrency: 4,
		testTimeout: 10000,
		hookTimeout: 5000,
		isolate: false,
		reporters: ['default', 'json'],
		outputFile: './test-results.json',
	},
});

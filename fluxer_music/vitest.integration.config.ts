// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineConfig} from 'vitest/config';

export default defineConfig({
	resolve: {tsconfigPaths: true},
	test: {
		globals: true,
		environment: 'node',
		include: ['**/*.integration.test.ts'],
		exclude: ['node_modules', 'dist'],
		testTimeout: 30_000,
		hookTimeout: 60_000,
		fileParallelism: false,
		// rtc-node logs at debug level unless NODE_ENV is production.
		env: {NODE_ENV: 'production'},
	},
});

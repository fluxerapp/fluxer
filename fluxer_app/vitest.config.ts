// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineConfig} from 'vitest/config';

export default defineConfig({
	resolve: {tsconfigPaths: true},
	oxc: {
		jsx: {runtime: 'automatic', importSource: 'react'},
	},
	test: {
		globals: true,
		environment: 'node',
		testTimeout: 30_000,
		hookTimeout: 30_000,
		setupFiles: ['./vitest.setup.ts'],
		env: {
			PUBLIC_BUILD_VERSION: process.env.PUBLIC_BUILD_VERSION ?? 'dev',
			PUBLIC_RELEASE_CHANNEL: process.env.PUBLIC_RELEASE_CHANNEL ?? 'development',
		},
		include: ['src/**/*.{test,spec}.{ts,tsx}'],
		exclude: ['node_modules', 'dist'],
		server: {
			deps: {
				inline: [/livekit-client/, /@livekit\//],
			},
		},
	},
});

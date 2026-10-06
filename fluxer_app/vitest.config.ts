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
		include: ['src/**/*.{test,spec}.{ts,tsx}'],
		exclude: ['node_modules', 'dist', '../.claude/**'],
		server: {
			deps: {
				inline: [/livekit-client/, /@livekit\//],
			},
		},
	},
});

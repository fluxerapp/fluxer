// SPDX-License-Identifier: AGPL-3.0-or-later

export default {
	resolve: {tsconfigPaths: true},
	test: {
		globals: true,
		environment: 'node',
		include: ['**/*.{test,spec}.{ts,tsx}'],
		exclude: ['node_modules', 'dist'],
	},
};

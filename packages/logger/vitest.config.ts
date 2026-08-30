// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import {fileURLToPath} from 'node:url';
import tsconfigPaths from 'vite-tsconfig-paths';
import {defineConfig} from 'vitest/config';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default defineConfig({
	plugins: [
		tsconfigPaths({
			root: path.resolve(__dirname, '../..'),
		}),
	],
	test: {
		globals: true,
		environment: 'node',
		include: ['**/*.{test,spec}.{ts,tsx}'],
		exclude: ['node_modules', 'dist'],
		coverage: {
			provider: 'v8',
			reporter: ['text', 'json', 'html'],
			exclude: ['**/*.test.tsx', '**/*.spec.tsx', 'node_modules/'],
		},
	},
});

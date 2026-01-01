/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import path from 'node:path';
import {defineConfig} from 'vitest/config';

export default defineConfig({
	test: {
		environment: 'node',
		globals: true,
		setupFiles: ['./src/test/setup.ts'],
		include: ['src/**/*.{test,spec}.{js,ts}'],
		exclude: ['node_modules', 'dist'],
		coverage: {
			provider: 'v8',
			reporter: ['text', 'html', 'lcov'],
			exclude: ['node_modules', 'dist', 'src/test', '**/*.d.ts', '**/*.config.{js,ts}'],
		},
	},
	resolve: {
		alias: {
			'~': path.resolve(__dirname, './src'),
		},
	},
});

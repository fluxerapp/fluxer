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
	resolve: {
		alias: {
			'~': path.resolve(__dirname, './src'),
			'@pkgs': path.resolve(__dirname, './pkgs'),
		},
		extensions: ['.ts', '.tsx', '.js', '.jsx', '.json'],
	},
	test: {
		environment: 'happy-dom',
		setupFiles: ['./src/test/setup.ts'],
		globals: true,
		include: ['src/**/*.{test,spec}.{ts,tsx,js,jsx}'],
		coverage: {
			provider: 'v8',
			reporter: ['text', 'json', 'html'],
			exclude: ['node_modules/', '**/*.d.ts', '**/*.config.{js,ts}', 'src/test/**/*'],
		},
	},
});

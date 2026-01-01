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
import {fileURLToPath} from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export function getLinguiSwcPluginConfig() {
	return [
		'@lingui/swc-plugin',
		{
			localeDir: 'src/locales/{locale}/messages',
			runtimeModules: {
				i18n: ['@lingui/core', 'i18n'],
				trans: ['@lingui/react', 'Trans'],
			},
			stripNonEssentialFields: false,
		},
	];
}

export function createPoFileRule() {
	return {
		test: /\.po$/,
		type: 'javascript/auto',
		use: {
			loader: path.join(__dirname, 'po-loader.mjs'),
		},
	};
}

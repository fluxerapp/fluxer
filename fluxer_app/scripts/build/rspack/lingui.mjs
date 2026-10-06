// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import {fileURLToPath} from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export function getLinguiSwcPluginConfig() {
	return [
		'@lingui/swc-plugin',
		{
			descriptorFields: 'all',
			runtimeModules: {
				i18n: ['@lingui/core', 'i18n'],
				trans: ['@lingui/react', 'Trans'],
			},
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

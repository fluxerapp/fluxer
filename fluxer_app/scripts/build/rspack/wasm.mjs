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

import * as fs from 'node:fs';
import * as path from 'node:path';
import {fileURLToPath} from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export default function wasmLoader(_source) {
	const callback = this.async();

	if (!callback) {
		throw new Error('Async loader not supported');
	}

	const wasmPath = this.resourcePath;

	fs.promises
		.readFile(wasmPath)
		.then((wasmContent) => {
			const base64 = wasmContent.toString('base64');
			const code = `
				const wasmBase64 = "${base64}";
				const wasmBinary = Uint8Array.from(atob(wasmBase64), c => c.charCodeAt(0));
				export default wasmBinary;
			`;
			callback(null, code);
		})
		.catch((err) => {
			callback(err);
		});
}

export function wasmModuleRule() {
	return {
		test: /\.wasm$/,
		exclude: [/node_modules/],
		type: 'javascript/auto',
		use: [
			{
				loader: path.join(__dirname, 'wasm.mjs'),
			},
		],
	};
}

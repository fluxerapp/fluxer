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

const EXTERNAL_MODULES = [
	'@lingui/cli',
	'@lingui/conf',
	'cosmiconfig',
	'jiti',
	'node:*',
	'crypto',
	'path',
	'fs',
	'os',
	'vm',
	'perf_hooks',
	'util',
	'events',
	'stream',
	'buffer',
	'child_process',
	'cluster',
	'dgram',
	'dns',
	'http',
	'https',
	'module',
	'net',
	'repl',
	'tls',
	'url',
	'worker_threads',
	'readline',
	'zlib',
	'resolve',
];

const EXTERNAL_PATTERNS = [/^node:.*/];

export class ExternalsPlugin {
	apply(compiler) {
		const existingExternals = compiler.options.externals || [];
		const externalsArray = Array.isArray(existingExternals) ? existingExternals : [existingExternals];
		compiler.options.externals = [...externalsArray, ...EXTERNAL_MODULES, ...EXTERNAL_PATTERNS];
	}
}

export function externalsPlugin() {
	return new ExternalsPlugin();
}

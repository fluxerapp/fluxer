// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {fileURLToPath} from 'node:url';
import vm from 'node:vm';

const require = createRequire(import.meta.url);
const esbuild = require('esbuild');

const sourceRoots = {
	'@electron/main/': new URL('../', import.meta.url),
	'@electron/common/': new URL('../../common/', import.meta.url),
};

function resolveSourcePath(specifier) {
	for (const [prefix, root] of Object.entries(sourceRoots)) {
		if (specifier.startsWith(prefix)) {
			return fileURLToPath(new URL(`${specifier.slice(prefix.length)}.ts`, root));
		}
	}
	return null;
}

export function loadTsModule(specifier, {stubs = {}} = {}) {
	const cache = new Map();
	const load = (moduleSpecifier) => {
		if (Object.hasOwn(stubs, moduleSpecifier)) return stubs[moduleSpecifier];
		const sourcePath = resolveSourcePath(moduleSpecifier);
		if (sourcePath === null) throw new Error(`Unexpected import: ${moduleSpecifier}`);
		const cached = cache.get(sourcePath);
		if (cached) return cached.exports;
		const transformed = esbuild.transformSync(readFileSync(sourcePath, 'utf8'), {
			loader: 'ts',
			format: 'cjs',
			platform: 'node',
			target: 'node20',
		}).code;
		const module = {exports: {}};
		cache.set(sourcePath, module);
		const factory = vm.runInThisContext(`(function (require, module, exports) {${transformed}\n})`, {
			filename: sourcePath,
		});
		factory(load, module, module.exports);
		return module.exports;
	};
	return load(specifier);
}

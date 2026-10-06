// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync, rmSync} from 'node:fs';
import {isBuiltin} from 'node:module';
import {dirname, join, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {build} from 'esbuild';

const API_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const OUT_DIR = join(API_ROOT, 'dist');
const WORKSPACE_PREFIXES = ['@app/', '@pkgs/', '@fluxer/'];

function packageNameOf(specifier) {
	const segments = specifier.split('/');
	return specifier.startsWith('@') ? segments.slice(0, 2).join('/') : segments[0];
}

const declaredDependencies = new Set(
	Object.keys(JSON.parse(readFileSync(join(API_ROOT, 'package.json'), 'utf-8')).dependencies),
);
const undeclaredDependencies = new Set();

const workspacePlugin = {
	name: 'fluxer-workspace',
	setup(pluginBuild) {
		pluginBuild.onResolve({filter: /^[^./]/}, (args) => {
			if (WORKSPACE_PREFIXES.some((prefix) => args.path.startsWith(prefix))) {
				return undefined;
			}
			const packageName = packageNameOf(args.path);
			if (!isBuiltin(args.path) && !declaredDependencies.has(packageName)) {
				undeclaredDependencies.add(packageName);
			}
			return {path: args.path, external: true};
		});
	},
};

rmSync(OUT_DIR, {recursive: true, force: true});

await build({
	entryPoints: [join(API_ROOT, 'src/AppEntrypoint.ts'), join(API_ROOT, 'src/WorkerEntrypoint.ts')],
	outdir: OUT_DIR,
	bundle: true,
	platform: 'node',
	format: 'esm',
	target: 'node24',
	charset: 'utf8',
	sourcemap: true,
	sourcesContent: false,
	logLevel: 'info',
	banner: {js: '// SPDX-License-Identifier: AGPL-3.0-or-later'},
	plugins: [workspacePlugin],
});

if (undeclaredDependencies.size > 0) {
	const names = [...undeclaredDependencies].sort().join(', ');
	throw new Error(
		`The bundle imports packages that fluxer_api does not declare as dependencies: ${names}. ` +
			'Workspace packages keep their own node_modules, so the bundle cannot reach them from fluxer_api.',
	);
}

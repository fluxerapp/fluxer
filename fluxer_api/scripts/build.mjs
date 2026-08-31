// SPDX-License-Identifier: AGPL-3.0-or-later

import {existsSync, readFileSync, rmSync, statSync} from 'node:fs';
import {isBuiltin} from 'node:module';
import {dirname, join, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {build} from 'esbuild';

const API_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const REPO_ROOT = resolve(API_ROOT, '..');
const OUT_DIR = join(API_ROOT, 'dist');
const CANDIDATE_SUFFIXES = ['', '.ts', '.tsx', '.js', '.mjs', '/index.ts', '/index.tsx', '/index.js', '/src/index.ts'];

const WORKSPACE_ROOTS = {
	'@app/': join(API_ROOT, 'src'),
	'@pkgs/': join(API_ROOT, 'pkgs'),
	'@fluxer/': join(REPO_ROOT, 'packages'),
};

function resolveWorkspacePath(specifier) {
	for (const [prefix, root] of Object.entries(WORKSPACE_ROOTS)) {
		if (!specifier.startsWith(prefix)) {
			continue;
		}
		const base = join(root, specifier.slice(prefix.length));
		for (const suffix of CANDIDATE_SUFFIXES) {
			const candidate = `${base}${suffix}`;
			if (existsSync(candidate) && statSync(candidate).isFile()) {
				return candidate;
			}
		}
		throw new Error(`Unable to resolve workspace import ${specifier}`);
	}
	return null;
}

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
			const workspacePath = resolveWorkspacePath(args.path);
			if (workspacePath) {
				return {path: workspacePath};
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
	entryPoints: [
		join(API_ROOT, 'src/AppEntrypoint.ts'),
		join(API_ROOT, 'src/WorkerEntrypoint.ts'),
		join(API_ROOT, 'src/ExpressionPackResidueEntrypoint.ts'),
	],
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

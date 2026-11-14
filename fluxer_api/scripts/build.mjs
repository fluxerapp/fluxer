// SPDX-License-Identifier: AGPL-3.0-or-later

import {cpSync, readdirSync, readFileSync, rmSync} from 'node:fs';
import {isBuiltin} from 'node:module';
import {dirname, join, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {build} from 'esbuild';

const API_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const OUT_DIR = join(API_ROOT, 'dist');
const WORKSPACE_PREFIXES = ['@app/', '@pkgs/', '@fluxer/'];
const BUNDLED_LOCALES_DIR = './locales/';
const BUNDLED_FONTS_DIR = './fonts/';
const CERTIFICATE_FONTS = join(API_ROOT, 'pkgs/email/src/operator_pass_certificate/fonts');
const LOCALE_CATALOGS = {
	errors: join(API_ROOT, '../packages/errors/src/i18n/weblate/locales'),
	email: join(API_ROOT, 'pkgs/email/src/email_i18n/weblate/locales'),
	content: join(API_ROOT, 'src/api/content_i18n/weblate/locales'),
};

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
	entryPoints: [
		join(API_ROOT, 'src/AppEntrypoint.ts'),
		join(API_ROOT, 'src/WorkerEntrypoint.ts'),
		join(API_ROOT, 'src/MigrateEntrypoint.ts'),
	],
	outdir: OUT_DIR,
	bundle: true,
	platform: 'node',
	format: 'esm',
	splitting: true,
	target: 'node26',
	charset: 'ascii',
	sourcemap: true,
	sourcesContent: false,
	logLevel: 'info',
	banner: {js: '// SPDX-License-Identifier: AGPL-3.0-or-later'},
	define: {
		FLUXER_BUNDLED_LOCALES_DIR: JSON.stringify(BUNDLED_LOCALES_DIR),
		FLUXER_BUNDLED_FONTS_DIR: JSON.stringify(BUNDLED_FONTS_DIR),
	},
	plugins: [workspacePlugin],
});

for (const [catalog, directory] of Object.entries(LOCALE_CATALOGS)) {
	cpSync(directory, join(OUT_DIR, BUNDLED_LOCALES_DIR, catalog), {recursive: true});
}

cpSync(CERTIFICATE_FONTS, join(OUT_DIR, BUNDLED_FONTS_DIR), {recursive: true});

for (const file of readdirSync(OUT_DIR).filter((name) => name.endsWith('.js'))) {
	const source = readFileSync(join(OUT_DIR, file), 'utf-8');
	const index = source.search(/[\u0080-\uFFFF]/);
	if (index !== -1) {
		const line = source.slice(0, index).split('\n').length;
		throw new Error(
			`${file}:${line} contains a non-ASCII character. ` +
				'V8 stores a module source with any such character as a two-byte string, which doubles its memory. ' +
				'Write the character as a \\u escape.',
		);
	}
}

if (undeclaredDependencies.size > 0) {
	const names = [...undeclaredDependencies].sort().join(', ');
	throw new Error(
		`The bundle imports packages that fluxer_api does not declare as dependencies: ${names}. ` +
			'Workspace packages keep their own node_modules, so the bundle cannot reach them from fluxer_api.',
	);
}

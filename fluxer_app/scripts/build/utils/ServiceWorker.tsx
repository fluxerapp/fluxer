// SPDX-License-Identifier: AGPL-3.0-or-later

import {promises as fs} from 'node:fs';
import * as path from 'node:path';
import {DIST_DIR, SRC_DIR} from '@app_scripts/build/Config';
import * as esbuild from 'esbuild';

interface PrecacheEntry {
	url: string;
	revision: string;
}

const PRECACHE_ROOT_FILES = ['index.html', 'manifest.json', 'browserconfig.xml', 'robots.txt', 'version.json'];

const NEVER_PRECACHED_EXTENSIONS = ['.woff', '.woff2', '.ttf', '.otf', '.eot'];

async function fileRevision(filePath: string): Promise<string> {
	const stat = await fs.stat(filePath);
	return `${stat.size}:${Math.trunc(stat.mtimeMs)}`;
}

function isLocalAssetUrl(value: string): boolean {
	if (!value.startsWith('/')) {
		return false;
	}
	if (value.startsWith('//')) {
		return false;
	}
	const pathname = value.split(/[?#]/, 1)[0].toLowerCase();
	if (NEVER_PRECACHED_EXTENSIONS.some((extension) => pathname.endsWith(extension))) {
		return false;
	}
	return value.startsWith('/assets/') || PRECACHE_ROOT_FILES.some((file) => value === `/${file}`);
}

async function collectPrecacheManifest(): Promise<Array<PrecacheEntry>> {
	const entries = new Map<string, string>();
	for (const file of PRECACHE_ROOT_FILES) {
		const filePath = path.join(DIST_DIR, file);
		try {
			const revision = await fileRevision(filePath);
			entries.set(`/${file}`, revision);
			if (file === 'index.html') {
				entries.set('/', revision);
			}
		} catch {}
	}
	try {
		const html = await fs.readFile(path.join(DIST_DIR, 'index.html'), 'utf8');
		const attributePattern = /\b(?:href|src)=["']([^"']+)["']/g;
		for (const match of html.matchAll(attributePattern)) {
			const url = match[1];
			if (!isLocalAssetUrl(url)) {
				continue;
			}
			const pathname = new URL(url, 'https://local.invalid').pathname;
			const filePath = path.join(DIST_DIR, pathname.slice(1));
			try {
				entries.set(pathname, await fileRevision(filePath));
			} catch {}
		}
	} catch {}
	return Array.from(entries, ([url, revision]) => ({url, revision}));
}

export async function buildServiceWorker(production: boolean): Promise<void> {
	const precacheManifest = await collectPrecacheManifest();
	const buildVersion = process.env.PUBLIC_BUILD_SHA || process.env.BUILD_SHA || String(Date.now());
	await esbuild.build({
		entryPoints: [path.join(SRC_DIR, 'features', 'platform', 'service_worker', 'Worker.ts')],
		bundle: true,
		format: 'iife',
		outfile: path.join(DIST_DIR, 'sw.js'),
		minify: production,
		sourcemap: true,
		target: 'esnext',
		define: {
			__WB_MANIFEST: '[]',
			__FLUXER_PRECACHE_MANIFEST__: JSON.stringify(precacheManifest),
			__FLUXER_SW_VERSION__: JSON.stringify(buildVersion),
		},
	});
}

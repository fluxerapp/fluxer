// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {promises as fs} from 'node:fs';
import * as path from 'node:path';
import {type PrecacheEntry, readAppShellBuildId} from '@app/features/platform/service_worker/WorkerAppShell';
import {DIST_DIR, SRC_DIR} from '@app_scripts/build/Config';
import * as esbuild from 'esbuild';

const PRECACHE_ROOT_FILES = ['manifest.json', 'browserconfig.xml', 'robots.txt', 'version.json'];
const BUILD_ORIGIN = 'https://fluxer.invalid';

function extractCriticalAssetUrls(html: string): Array<string> {
	const urls = new Set<string>();
	for (const [, value] of html.matchAll(/["']([^"'\s<>]*\/assets\/[^"'\s<>]+)["']/gu)) {
		const url = new URL(value.replaceAll('&amp;', '&'), `${BUILD_ORIGIN}/`);
		if (url.protocol !== 'http:' && url.protocol !== 'https:') {
			continue;
		}
		url.hash = '';
		urls.add(url.origin === BUILD_ORIGIN ? `${url.pathname}${url.search}` : url.href);
	}
	return [...urls].sort();
}

async function collectPrecacheManifest(html: string): Promise<Array<PrecacheEntry>> {
	const entries: Array<PrecacheEntry> = await Promise.all(
		extractCriticalAssetUrls(html).map(async (url) => {
			const pathname = new URL(url, BUILD_ORIGIN).pathname;
			const assetPath = pathname.slice(pathname.lastIndexOf('/assets/') + 1);
			const bytes = await fs.readFile(path.join(DIST_DIR, assetPath));
			return {url, integrity: `sha384-${createHash('sha384').update(bytes).digest('base64')}`};
		}),
	);
	for (const file of PRECACHE_ROOT_FILES) {
		await fs.access(path.join(DIST_DIR, file));
		entries.push({url: `/${file}`, integrity: null});
	}
	return entries;
}

export async function buildServiceWorker(production: boolean): Promise<void> {
	const html = await fs.readFile(path.join(DIST_DIR, 'index.html'), 'utf8');
	const version = readAppShellBuildId(html);
	if (!version) throw new Error('index.html has no fluxer-build meta');
	const precacheManifest = await collectPrecacheManifest(html);
	await esbuild.build({
		entryPoints: [path.join(SRC_DIR, 'features', 'platform', 'service_worker', 'Worker.ts')],
		bundle: true,
		format: 'iife',
		outfile: path.join(DIST_DIR, 'sw.js'),
		minify: production,
		sourcemap: true,
		target: 'esnext',
		define: {
			__FLUXER_PRECACHE_MANIFEST__: JSON.stringify(precacheManifest),
			__FLUXER_SW_VERSION__: JSON.stringify(version),
		},
	});
}

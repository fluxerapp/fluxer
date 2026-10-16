// SPDX-License-Identifier: AGPL-3.0-or-later

import {promises as fs} from 'node:fs';
import * as path from 'node:path';
import type {PrecacheEntry} from '@app/features/platform/service_worker/WorkerAppShell';
import {DIST_DIR, SRC_DIR} from '@app_scripts/build/Config';
import * as esbuild from 'esbuild';

const PRECACHE_ROOT_FILES = ['manifest.json', 'browserconfig.xml', 'robots.txt', 'version.json'];

async function fileRevision(filePath: string): Promise<string> {
	const stat = await fs.stat(filePath);
	return `${stat.size}:${Math.trunc(stat.mtimeMs)}`;
}

async function collectPrecacheManifest(): Promise<Array<PrecacheEntry>> {
	const entries = new Map<string, string>();
	for (const file of PRECACHE_ROOT_FILES) {
		const filePath = path.join(DIST_DIR, file);
		try {
			entries.set(`/${file}`, await fileRevision(filePath));
		} catch {}
	}
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

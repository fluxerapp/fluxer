// SPDX-License-Identifier: AGPL-3.0-or-later

import {readdir, readFile, writeFile} from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import type {AstroIntegration} from 'astro';

const MODULE_SCRIPT = /<script type="module" src=/gu;

export function markModuleScriptsLowPriority(html: string): string {
	return html.replace(MODULE_SCRIPT, '<script type="module" fetchpriority="low" src=');
}

async function htmlFiles(dir: string): Promise<Array<string>> {
	const entries = await readdir(dir, {recursive: true, withFileTypes: true});
	return entries
		.filter((entry) => entry.isFile() && entry.name.endsWith('.html'))
		.map((entry) => path.join(entry.parentPath, entry.name));
}

export const lowPriorityModuleScripts = (): AstroIntegration => ({
	name: 'fluxer-docs-low-priority-module-scripts',
	hooks: {
		'astro:build:done': async ({dir}) => {
			for (const file of await htmlFiles(fileURLToPath(dir))) {
				const html = await readFile(file, 'utf8');
				const marked = markModuleScriptsLowPriority(html);
				if (marked !== html) {
					await writeFile(file, marked);
				}
			}
		},
	},
});

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
import {ASSETS_DIR, CDN_ENDPOINT, ROOT_DIR} from '../config';

interface BuildOutput {
	mainScript: string | null;
	cssFiles: Array<string>;
	jsFiles: Array<string>;
	cssBundleFile: string | null;
	vendorScripts: Array<string>;
}

interface GenerateHtmlOptions {
	buildOutput: BuildOutput;
	production: boolean;
}

async function findCssModulesFile(): Promise<string | null> {
	if (!fs.existsSync(ASSETS_DIR)) {
		return null;
	}
	const files = await fs.promises.readdir(ASSETS_DIR);
	const stylesFiles = files.filter((name) => name.startsWith('styles.') && name.endsWith('.css'));
	if (stylesFiles.length === 0) {
		return null;
	}

	let latestFile: string | null = null;
	let latestMtime = 0;

	for (const fileName of stylesFiles) {
		const filePath = path.join(ASSETS_DIR, fileName);
		const stats = await fs.promises.stat(filePath);
		if (latestFile === null || stats.mtimeMs > latestMtime) {
			latestFile = fileName;
			latestMtime = stats.mtimeMs;
		}
	}

	return latestFile ? `assets/${latestFile}` : null;
}

export async function generateHtml(options: GenerateHtmlOptions): Promise<string> {
	const {buildOutput, production} = options;

	const indexHtmlPath = path.join(ROOT_DIR, 'index.html');
	let html = await fs.promises.readFile(indexHtmlPath, 'utf-8');

	const baseUrl = production ? `${CDN_ENDPOINT}/` : '/';

	const cssModulesFile = buildOutput.cssBundleFile ?? (await findCssModulesFile());
	const cssFiles = cssModulesFile ? [cssModulesFile] : buildOutput.cssFiles;

	const cssLinks = cssFiles.map((file) => `<link rel="stylesheet" href="${baseUrl}${file}">`).join('\n');

	const crossOriginAttr = production && baseUrl.startsWith('http') ? ' crossorigin="anonymous"' : '';

	const jsScripts = buildOutput.mainScript
		? `<script type="module" src="${baseUrl}${buildOutput.mainScript}"${crossOriginAttr}></script>`
		: '';

	const buildScriptPreload = (file: string): string =>
		`<link rel="preload" as="script" href="${baseUrl}${file}"${crossOriginAttr}>`;

	const preloadScripts = [
		...(buildOutput.vendorScripts ?? []).map(buildScriptPreload),
		...buildOutput.jsFiles.filter((file) => !file.includes('messages')).map(buildScriptPreload),
	].join('\n');

	html = html.replace(/<script type="module" src="\/src\/index\.tsx"><\/script>/, jsScripts);
	const headInsert = [cssLinks, preloadScripts].filter(Boolean).join('\n');
	html = html.replace('</head>', `${headInsert}\n</head>`);

	return html;
}

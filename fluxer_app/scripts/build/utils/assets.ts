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
import {ASSETS_DIR, DIST_DIR, PKGS_DIR, PUBLIC_DIR} from '../config';

export async function copyPublicAssets(): Promise<void> {
	if (!fs.existsSync(PUBLIC_DIR)) {
		return;
	}

	const files = await fs.promises.readdir(PUBLIC_DIR, {recursive: true});
	for (const file of files) {
		const srcPath = path.join(PUBLIC_DIR, file.toString());
		const destPath = path.join(DIST_DIR, file.toString());

		const stat = await fs.promises.stat(srcPath);
		if (stat.isFile()) {
			await fs.promises.mkdir(path.dirname(destPath), {recursive: true});
			await fs.promises.copyFile(srcPath, destPath);
		}
	}
}

export async function copyWasmFiles(): Promise<void> {
	const libfluxcoreDir = path.join(PKGS_DIR, 'libfluxcore');
	const wasmFile = path.join(libfluxcoreDir, 'libfluxcore_bg.wasm');

	if (fs.existsSync(wasmFile)) {
		await fs.promises.copyFile(wasmFile, path.join(ASSETS_DIR, 'libfluxcore_bg.wasm'));
	}
}

export async function removeUnusedCssAssets(assetsDir: string, keepFiles: Array<string>): Promise<void> {
	if (!fs.existsSync(assetsDir)) {
		return;
	}

	const keepNames = new Set<string>();
	for (const file of keepFiles) {
		const base = path.basename(file);
		keepNames.add(base);
		if (base.endsWith('.css')) {
			keepNames.add(`${base}.map`);
		}
	}

	const entries = await fs.promises.readdir(assetsDir);
	for (const entry of entries) {
		if (!entry.endsWith('.css') && !entry.endsWith('.css.map')) {
			continue;
		}
		if (keepNames.has(entry)) {
			continue;
		}
		await fs.promises.rm(path.join(assetsDir, entry), {force: true});
	}
}

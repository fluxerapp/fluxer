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

async function fileExists(filePath: string): Promise<boolean> {
	try {
		await fs.promises.access(filePath);
		return true;
	} catch {
		return false;
	}
}

async function traverseDir(dir: string, callback: (filePath: string) => Promise<void>): Promise<void> {
	const entries = await fs.promises.readdir(dir, {withFileTypes: true});
	await Promise.all(
		entries.map(async (entry) => {
			const entryPath = path.join(dir, entry.name);
			if (entry.isDirectory()) {
				await traverseDir(entryPath, callback);
				return;
			}
			await callback(entryPath);
		}),
	);
}

export async function cleanEmptySourceMaps(dir: string): Promise<void> {
	if (!(await fileExists(dir))) {
		return;
	}

	await traverseDir(dir, async (filePath) => {
		if (!filePath.endsWith('.js.map')) {
			return;
		}

		let parsed: unknown;
		try {
			const raw = await fs.promises.readFile(filePath, 'utf-8');
			parsed = JSON.parse(raw);
		} catch {
			return;
		}

		if (typeof parsed !== 'object' || parsed === null) {
			return;
		}

		const sources = (parsed as {sources?: Array<unknown>}).sources ?? [];
		if (Array.isArray(sources) && sources.length > 0) {
			return;
		}

		await fs.promises.rm(filePath, {force: true});

		const jsPath = filePath.slice(0, -4);
		if (!(await fileExists(jsPath))) {
			return;
		}

		const jsContent = await fs.promises.readFile(jsPath, 'utf-8');
		const cleaned = jsContent.replace(/(?:\r?\n)?\/\/# sourceMappingURL=.*$/, '');
		if (cleaned !== jsContent) {
			await fs.promises.writeFile(jsPath, cleaned);
		}
	});
}

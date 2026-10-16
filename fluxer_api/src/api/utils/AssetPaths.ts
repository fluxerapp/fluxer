// SPDX-License-Identifier: AGPL-3.0-or-later

import {existsSync} from 'node:fs';
import {dirname, join} from 'node:path';
import {fileURLToPath} from 'node:url';

function findPackageRoot(startDir: string): string {
	let current = startDir;
	while (!existsSync(join(current, 'package.json'))) {
		const parent = dirname(current);
		if (parent === current) {
			throw new Error(`Unable to locate the fluxer_api package root from ${startDir}`);
		}
		current = parent;
	}
	return current;
}

const PACKAGE_ROOT = findPackageRoot(dirname(fileURLToPath(import.meta.url)));

export function resolveAssetPath(...segments: Array<string>): string {
	return join(PACKAGE_ROOT, 'src', 'api', ...segments);
}

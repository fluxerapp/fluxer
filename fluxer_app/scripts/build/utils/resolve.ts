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
import {RESOLVE_EXTENSIONS} from '../config';

export function tryResolveWithExtensions(basePath: string): string | null {
	if (fs.existsSync(basePath)) {
		const stat = fs.statSync(basePath);
		if (stat.isFile()) {
			return basePath;
		}
		if (stat.isDirectory()) {
			for (const ext of RESOLVE_EXTENSIONS) {
				const indexPath = path.join(basePath, `index${ext}`);
				if (fs.existsSync(indexPath)) {
					return indexPath;
				}
			}
		}
	}

	for (const ext of RESOLVE_EXTENSIONS) {
		const withExt = `${basePath}${ext}`;
		if (fs.existsSync(withExt)) {
			return withExt;
		}
	}

	return null;
}

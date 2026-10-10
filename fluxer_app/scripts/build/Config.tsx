// SPDX-License-Identifier: AGPL-3.0-or-later

import * as path from 'node:path';

export const ROOT_DIR = path.resolve(import.meta.dirname, '..', '..');
export const SRC_DIR = path.join(ROOT_DIR, 'src');
export const DIST_DIR = path.join(ROOT_DIR, 'dist');

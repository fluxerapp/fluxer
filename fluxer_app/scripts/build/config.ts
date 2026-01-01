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

import * as path from 'node:path';

export const ROOT_DIR = path.resolve(import.meta.dirname, '..', '..');
export const SRC_DIR = path.join(ROOT_DIR, 'src');
export const DIST_DIR = path.join(ROOT_DIR, 'dist');
export const ASSETS_DIR = path.join(DIST_DIR, 'assets');
export const PKGS_DIR = path.join(ROOT_DIR, 'pkgs');
export const PUBLIC_DIR = path.join(ROOT_DIR, 'assets');

export const CDN_ENDPOINT = 'https://fluxerstatic.com';

export const DEV_PORT = 3000;

export const RESOLVE_EXTENSIONS = ['.tsx', '.ts', '.jsx', '.js', '.json', '.mjs', '.cjs'];

export const LOCALES = [
	'ar',
	'bg',
	'cs',
	'da',
	'de',
	'el',
	'en-GB',
	'en-US',
	'es-419',
	'es-ES',
	'fi',
	'fr',
	'he',
	'hi',
	'hr',
	'hu',
	'id',
	'it',
	'ja',
	'ko',
	'lt',
	'nl',
	'no',
	'pl',
	'pt-BR',
	'ro',
	'ru',
	'sv-SE',
	'th',
	'tr',
	'uk',
	'vi',
	'zh-CN',
	'zh-TW',
];

export const FILE_LOADERS: Record<string, 'file'> = {
	'.woff': 'file',
	'.woff2': 'file',
	'.ttf': 'file',
	'.eot': 'file',
	'.png': 'file',
	'.jpg': 'file',
	'.jpeg': 'file',
	'.gif': 'file',
	'.webp': 'file',
	'.ico': 'file',
	'.mp3': 'file',
	'.wav': 'file',
	'.ogg': 'file',
	'.mp4': 'file',
	'.webm': 'file',
};

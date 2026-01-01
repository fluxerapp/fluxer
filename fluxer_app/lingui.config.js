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

module.exports = {
	locales: [
		'ar',
		'bg',
		'cs',
		'da',
		'de',
		'el',
		'en-GB',
		'en-US',
		'es-ES',
		'es-419',
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
	],
	sourceLocale: 'en-US',
	catalogs: [
		{
			path: 'src/locales/{locale}/messages',
			include: ['src'],
			exclude: ['**/node_modules/**', '**/*.d.ts'],
		},
	],
	format: 'po',
	compileNamespace: 'es',
};

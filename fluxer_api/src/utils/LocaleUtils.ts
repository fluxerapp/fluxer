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

import {Locales} from '~/Constants';

const SUPPORTED_LOCALES = new Set<string>(Object.values(Locales));
const DEFAULT_LOCALE = Locales.EN_US;

export function parseAcceptLanguage(acceptLanguageHeader: string | null | undefined): string {
	if (!acceptLanguageHeader) {
		return DEFAULT_LOCALE;
	}

	try {
		const languages = acceptLanguageHeader
			.split(',')
			.map((lang) => {
				const parts = lang.trim().split(';');
				const locale = parts[0].trim();
				const qMatch = parts[1]?.match(/q=([\d.]+)/);
				const quality = qMatch ? Number.parseFloat(qMatch[1]) : 1.0;
				return {locale, quality};
			})
			.sort((a, b) => b.quality - a.quality);

		for (const {locale} of languages) {
			for (const supportedLocale of SUPPORTED_LOCALES) {
				if (supportedLocale.toLowerCase() === locale.toLowerCase()) {
					return supportedLocale;
				}
			}
		}

		const languagePreferences: Record<string, string> = {
			en: Locales.EN_US,
			es: Locales.ES_ES,
			pt: Locales.PT_BR,
			zh: Locales.ZH_CN,
			sv: Locales.SV_SE,
		};

		for (const {locale} of languages) {
			const languageCode = locale.split('-')[0].toLowerCase();

			if (languagePreferences[languageCode] && SUPPORTED_LOCALES.has(languagePreferences[languageCode])) {
				return languagePreferences[languageCode];
			}

			for (const supportedLocale of SUPPORTED_LOCALES) {
				if (supportedLocale.toLowerCase().startsWith(`${languageCode}-`)) {
					return supportedLocale;
				}
			}
		}

		return DEFAULT_LOCALE;
	} catch (_error) {
		return DEFAULT_LOCALE;
	}
}

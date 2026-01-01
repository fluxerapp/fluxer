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

import {ar} from './locales/ar';
import {bg} from './locales/bg';
import {cs} from './locales/cs';
import {da} from './locales/da';
import {de} from './locales/de';
import {el} from './locales/el';
import {enGB} from './locales/en-GB';
import {enUS} from './locales/en-US';
import {es419} from './locales/es-419';
import {esES} from './locales/es-ES';
import {fi} from './locales/fi';
import {fr} from './locales/fr';
import {he} from './locales/he';
import {hi} from './locales/hi';
import {hr} from './locales/hr';
import {hu} from './locales/hu';
import {id} from './locales/id';
import {it} from './locales/it';
import {ja} from './locales/ja';
import {ko} from './locales/ko';
import {lt} from './locales/lt';
import {nl} from './locales/nl';
import {no} from './locales/no';
import {pl} from './locales/pl';
import {ptBR} from './locales/pt-BR';
import {ro} from './locales/ro';
import {ru} from './locales/ru';
import {svSE} from './locales/sv-SE';
import {th} from './locales/th';
import {tr} from './locales/tr';
import {uk} from './locales/uk';
import {vi} from './locales/vi';
import {zhCN} from './locales/zh-CN';
import {zhTW} from './locales/zh-TW';
import type {EmailTranslations} from './types';

const locales: Record<string, EmailTranslations> = {
	'en-US': enUS,
	'en-GB': enGB,
	ar,
	bg,
	cs,
	da,
	de,
	el,
	'es-ES': esES,
	'es-419': es419,
	fi,
	fr,
	he,
	hi,
	hr,
	hu,
	id,
	it,
	ja,
	ko,
	lt,
	nl,
	no,
	pl,
	'pt-BR': ptBR,
	ro,
	ru,
	'sv-SE': svSE,
	th,
	tr,
	uk,
	vi,
	'zh-CN': zhCN,
	'zh-TW': zhTW,
};

export function getLocaleTranslations(locale: string): EmailTranslations {
	return locales[locale] || locales['en-US'];
}

export function hasLocale(locale: string): boolean {
	return locale in locales;
}

export type {
	EmailTemplate,
	EmailTemplateKey,
	EmailTemplateVariables,
	EmailTranslations,
} from './types';

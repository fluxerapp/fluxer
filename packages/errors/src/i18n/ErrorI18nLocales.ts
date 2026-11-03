// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ErrorI18nKey} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import {loadStaticLocaleFiles} from '@fluxer/i18n/src/runtime/StaticLocaleFiles';

export const ERROR_I18N_LOCALE_MESSAGES = loadStaticLocaleFiles<Partial<Record<ErrorI18nKey, string>>>(
	'errors',
	new URL('./weblate/locales/', import.meta.url),
);

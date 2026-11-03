// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ContentI18nKey} from '@app/api/content_i18n/ContentI18nMessages';
import {loadStaticLocaleFiles} from '@fluxer/i18n/src/runtime/StaticLocaleFiles';

export const CONTENT_I18N_LOCALE_MESSAGES = loadStaticLocaleFiles<Partial<Record<ContentI18nKey, string>>>(
	'content',
	new URL('./weblate/locales/', import.meta.url),
);

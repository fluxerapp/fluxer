// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ContentI18nKey} from '@app/api/content_i18n/ContentI18nMessages';
import CONTENT_I18N_AR_MESSAGES from '@app/api/content_i18n/locales/ar';
import CONTENT_I18N_BG_MESSAGES from '@app/api/content_i18n/locales/bg';
import CONTENT_I18N_CS_MESSAGES from '@app/api/content_i18n/locales/cs';
import CONTENT_I18N_DA_MESSAGES from '@app/api/content_i18n/locales/da';
import CONTENT_I18N_DE_MESSAGES from '@app/api/content_i18n/locales/de';
import CONTENT_I18N_EL_MESSAGES from '@app/api/content_i18n/locales/el';
import CONTENT_I18N_EN_GB_MESSAGES from '@app/api/content_i18n/locales/en-GB';
import CONTENT_I18N_ES_419_MESSAGES from '@app/api/content_i18n/locales/es-419';
import CONTENT_I18N_ES_ES_MESSAGES from '@app/api/content_i18n/locales/es-ES';
import CONTENT_I18N_FI_MESSAGES from '@app/api/content_i18n/locales/fi';
import CONTENT_I18N_FR_MESSAGES from '@app/api/content_i18n/locales/fr';
import CONTENT_I18N_HE_MESSAGES from '@app/api/content_i18n/locales/he';
import CONTENT_I18N_HI_MESSAGES from '@app/api/content_i18n/locales/hi';
import CONTENT_I18N_HR_MESSAGES from '@app/api/content_i18n/locales/hr';
import CONTENT_I18N_HU_MESSAGES from '@app/api/content_i18n/locales/hu';
import CONTENT_I18N_ID_MESSAGES from '@app/api/content_i18n/locales/id';
import CONTENT_I18N_IT_MESSAGES from '@app/api/content_i18n/locales/it';
import CONTENT_I18N_JA_MESSAGES from '@app/api/content_i18n/locales/ja';
import CONTENT_I18N_KO_MESSAGES from '@app/api/content_i18n/locales/ko';
import CONTENT_I18N_LT_MESSAGES from '@app/api/content_i18n/locales/lt';
import CONTENT_I18N_NL_MESSAGES from '@app/api/content_i18n/locales/nl';
import CONTENT_I18N_NO_MESSAGES from '@app/api/content_i18n/locales/no';
import CONTENT_I18N_PL_MESSAGES from '@app/api/content_i18n/locales/pl';
import CONTENT_I18N_PT_BR_MESSAGES from '@app/api/content_i18n/locales/pt-BR';
import CONTENT_I18N_RO_MESSAGES from '@app/api/content_i18n/locales/ro';
import CONTENT_I18N_RU_MESSAGES from '@app/api/content_i18n/locales/ru';
import CONTENT_I18N_SV_SE_MESSAGES from '@app/api/content_i18n/locales/sv-SE';
import CONTENT_I18N_TH_MESSAGES from '@app/api/content_i18n/locales/th';
import CONTENT_I18N_TR_MESSAGES from '@app/api/content_i18n/locales/tr';
import CONTENT_I18N_UK_MESSAGES from '@app/api/content_i18n/locales/uk';
import CONTENT_I18N_VI_MESSAGES from '@app/api/content_i18n/locales/vi';
import CONTENT_I18N_ZH_CN_MESSAGES from '@app/api/content_i18n/locales/zh-CN';
import CONTENT_I18N_ZH_TW_MESSAGES from '@app/api/content_i18n/locales/zh-TW';

export const CONTENT_I18N_LOCALE_MESSAGES = {
	ar: CONTENT_I18N_AR_MESSAGES,
	bg: CONTENT_I18N_BG_MESSAGES,
	cs: CONTENT_I18N_CS_MESSAGES,
	da: CONTENT_I18N_DA_MESSAGES,
	de: CONTENT_I18N_DE_MESSAGES,
	el: CONTENT_I18N_EL_MESSAGES,
	'en-GB': CONTENT_I18N_EN_GB_MESSAGES,
	'es-419': CONTENT_I18N_ES_419_MESSAGES,
	'es-ES': CONTENT_I18N_ES_ES_MESSAGES,
	fi: CONTENT_I18N_FI_MESSAGES,
	fr: CONTENT_I18N_FR_MESSAGES,
	he: CONTENT_I18N_HE_MESSAGES,
	hi: CONTENT_I18N_HI_MESSAGES,
	hr: CONTENT_I18N_HR_MESSAGES,
	hu: CONTENT_I18N_HU_MESSAGES,
	id: CONTENT_I18N_ID_MESSAGES,
	it: CONTENT_I18N_IT_MESSAGES,
	ja: CONTENT_I18N_JA_MESSAGES,
	ko: CONTENT_I18N_KO_MESSAGES,
	lt: CONTENT_I18N_LT_MESSAGES,
	nl: CONTENT_I18N_NL_MESSAGES,
	no: CONTENT_I18N_NO_MESSAGES,
	pl: CONTENT_I18N_PL_MESSAGES,
	'pt-BR': CONTENT_I18N_PT_BR_MESSAGES,
	ro: CONTENT_I18N_RO_MESSAGES,
	ru: CONTENT_I18N_RU_MESSAGES,
	'sv-SE': CONTENT_I18N_SV_SE_MESSAGES,
	th: CONTENT_I18N_TH_MESSAGES,
	tr: CONTENT_I18N_TR_MESSAGES,
	uk: CONTENT_I18N_UK_MESSAGES,
	vi: CONTENT_I18N_VI_MESSAGES,
	'zh-CN': CONTENT_I18N_ZH_CN_MESSAGES,
	'zh-TW': CONTENT_I18N_ZH_TW_MESSAGES,
} as const satisfies Record<string, Partial<Record<ContentI18nKey, string>>>;

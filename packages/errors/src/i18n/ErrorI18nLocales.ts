// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ErrorI18nKey} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import ERROR_I18N_AR_MESSAGES from '@fluxer/errors/src/i18n/locales/ar';
import ERROR_I18N_BG_MESSAGES from '@fluxer/errors/src/i18n/locales/bg';
import ERROR_I18N_CS_MESSAGES from '@fluxer/errors/src/i18n/locales/cs';
import ERROR_I18N_DA_MESSAGES from '@fluxer/errors/src/i18n/locales/da';
import ERROR_I18N_DE_MESSAGES from '@fluxer/errors/src/i18n/locales/de';
import ERROR_I18N_EL_MESSAGES from '@fluxer/errors/src/i18n/locales/el';
import ERROR_I18N_EN_GB_MESSAGES from '@fluxer/errors/src/i18n/locales/en-GB';
import ERROR_I18N_ES_419_MESSAGES from '@fluxer/errors/src/i18n/locales/es-419';
import ERROR_I18N_ES_ES_MESSAGES from '@fluxer/errors/src/i18n/locales/es-ES';
import ERROR_I18N_FI_MESSAGES from '@fluxer/errors/src/i18n/locales/fi';
import ERROR_I18N_FR_MESSAGES from '@fluxer/errors/src/i18n/locales/fr';
import ERROR_I18N_HE_MESSAGES from '@fluxer/errors/src/i18n/locales/he';
import ERROR_I18N_HI_MESSAGES from '@fluxer/errors/src/i18n/locales/hi';
import ERROR_I18N_HR_MESSAGES from '@fluxer/errors/src/i18n/locales/hr';
import ERROR_I18N_HU_MESSAGES from '@fluxer/errors/src/i18n/locales/hu';
import ERROR_I18N_ID_MESSAGES from '@fluxer/errors/src/i18n/locales/id';
import ERROR_I18N_IT_MESSAGES from '@fluxer/errors/src/i18n/locales/it';
import ERROR_I18N_JA_MESSAGES from '@fluxer/errors/src/i18n/locales/ja';
import ERROR_I18N_KO_MESSAGES from '@fluxer/errors/src/i18n/locales/ko';
import ERROR_I18N_LT_MESSAGES from '@fluxer/errors/src/i18n/locales/lt';
import ERROR_I18N_NL_MESSAGES from '@fluxer/errors/src/i18n/locales/nl';
import ERROR_I18N_NO_MESSAGES from '@fluxer/errors/src/i18n/locales/no';
import ERROR_I18N_PL_MESSAGES from '@fluxer/errors/src/i18n/locales/pl';
import ERROR_I18N_PT_BR_MESSAGES from '@fluxer/errors/src/i18n/locales/pt-BR';
import ERROR_I18N_RO_MESSAGES from '@fluxer/errors/src/i18n/locales/ro';
import ERROR_I18N_RU_MESSAGES from '@fluxer/errors/src/i18n/locales/ru';
import ERROR_I18N_SV_SE_MESSAGES from '@fluxer/errors/src/i18n/locales/sv-SE';
import ERROR_I18N_TH_MESSAGES from '@fluxer/errors/src/i18n/locales/th';
import ERROR_I18N_TR_MESSAGES from '@fluxer/errors/src/i18n/locales/tr';
import ERROR_I18N_UK_MESSAGES from '@fluxer/errors/src/i18n/locales/uk';
import ERROR_I18N_VI_MESSAGES from '@fluxer/errors/src/i18n/locales/vi';
import ERROR_I18N_ZH_CN_MESSAGES from '@fluxer/errors/src/i18n/locales/zh-CN';
import ERROR_I18N_ZH_TW_MESSAGES from '@fluxer/errors/src/i18n/locales/zh-TW';

export const ERROR_I18N_LOCALE_MESSAGES = {
	ar: ERROR_I18N_AR_MESSAGES,
	bg: ERROR_I18N_BG_MESSAGES,
	cs: ERROR_I18N_CS_MESSAGES,
	da: ERROR_I18N_DA_MESSAGES,
	de: ERROR_I18N_DE_MESSAGES,
	el: ERROR_I18N_EL_MESSAGES,
	'en-GB': ERROR_I18N_EN_GB_MESSAGES,
	'es-419': ERROR_I18N_ES_419_MESSAGES,
	'es-ES': ERROR_I18N_ES_ES_MESSAGES,
	fi: ERROR_I18N_FI_MESSAGES,
	fr: ERROR_I18N_FR_MESSAGES,
	he: ERROR_I18N_HE_MESSAGES,
	hi: ERROR_I18N_HI_MESSAGES,
	hr: ERROR_I18N_HR_MESSAGES,
	hu: ERROR_I18N_HU_MESSAGES,
	id: ERROR_I18N_ID_MESSAGES,
	it: ERROR_I18N_IT_MESSAGES,
	ja: ERROR_I18N_JA_MESSAGES,
	ko: ERROR_I18N_KO_MESSAGES,
	lt: ERROR_I18N_LT_MESSAGES,
	nl: ERROR_I18N_NL_MESSAGES,
	no: ERROR_I18N_NO_MESSAGES,
	pl: ERROR_I18N_PL_MESSAGES,
	'pt-BR': ERROR_I18N_PT_BR_MESSAGES,
	ro: ERROR_I18N_RO_MESSAGES,
	ru: ERROR_I18N_RU_MESSAGES,
	'sv-SE': ERROR_I18N_SV_SE_MESSAGES,
	th: ERROR_I18N_TH_MESSAGES,
	tr: ERROR_I18N_TR_MESSAGES,
	uk: ERROR_I18N_UK_MESSAGES,
	vi: ERROR_I18N_VI_MESSAGES,
	'zh-CN': ERROR_I18N_ZH_CN_MESSAGES,
	'zh-TW': ERROR_I18N_ZH_TW_MESSAGES,
} as const satisfies Record<string, Partial<Record<ErrorI18nKey, string>>>;

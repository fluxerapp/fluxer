// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmailTemplate, EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';
import EMAIL_I18N_AR_MESSAGES from '@pkgs/email/src/email_i18n/locales/ar';
import EMAIL_I18N_BG_MESSAGES from '@pkgs/email/src/email_i18n/locales/bg';
import EMAIL_I18N_CS_MESSAGES from '@pkgs/email/src/email_i18n/locales/cs';
import EMAIL_I18N_DA_MESSAGES from '@pkgs/email/src/email_i18n/locales/da';
import EMAIL_I18N_DE_MESSAGES from '@pkgs/email/src/email_i18n/locales/de';
import EMAIL_I18N_EL_MESSAGES from '@pkgs/email/src/email_i18n/locales/el';
import EMAIL_I18N_EN_GB_MESSAGES from '@pkgs/email/src/email_i18n/locales/en-GB';
import EMAIL_I18N_ES_419_MESSAGES from '@pkgs/email/src/email_i18n/locales/es-419';
import EMAIL_I18N_ES_ES_MESSAGES from '@pkgs/email/src/email_i18n/locales/es-ES';
import EMAIL_I18N_FI_MESSAGES from '@pkgs/email/src/email_i18n/locales/fi';
import EMAIL_I18N_FR_MESSAGES from '@pkgs/email/src/email_i18n/locales/fr';
import EMAIL_I18N_HE_MESSAGES from '@pkgs/email/src/email_i18n/locales/he';
import EMAIL_I18N_HI_MESSAGES from '@pkgs/email/src/email_i18n/locales/hi';
import EMAIL_I18N_HR_MESSAGES from '@pkgs/email/src/email_i18n/locales/hr';
import EMAIL_I18N_HU_MESSAGES from '@pkgs/email/src/email_i18n/locales/hu';
import EMAIL_I18N_ID_MESSAGES from '@pkgs/email/src/email_i18n/locales/id';
import EMAIL_I18N_IT_MESSAGES from '@pkgs/email/src/email_i18n/locales/it';
import EMAIL_I18N_JA_MESSAGES from '@pkgs/email/src/email_i18n/locales/ja';
import EMAIL_I18N_KO_MESSAGES from '@pkgs/email/src/email_i18n/locales/ko';
import EMAIL_I18N_LT_MESSAGES from '@pkgs/email/src/email_i18n/locales/lt';
import EMAIL_I18N_NL_MESSAGES from '@pkgs/email/src/email_i18n/locales/nl';
import EMAIL_I18N_NO_MESSAGES from '@pkgs/email/src/email_i18n/locales/no';
import EMAIL_I18N_PL_MESSAGES from '@pkgs/email/src/email_i18n/locales/pl';
import EMAIL_I18N_PT_BR_MESSAGES from '@pkgs/email/src/email_i18n/locales/pt-BR';
import EMAIL_I18N_RO_MESSAGES from '@pkgs/email/src/email_i18n/locales/ro';
import EMAIL_I18N_RU_MESSAGES from '@pkgs/email/src/email_i18n/locales/ru';
import EMAIL_I18N_SV_SE_MESSAGES from '@pkgs/email/src/email_i18n/locales/sv-SE';
import EMAIL_I18N_TH_MESSAGES from '@pkgs/email/src/email_i18n/locales/th';
import EMAIL_I18N_TR_MESSAGES from '@pkgs/email/src/email_i18n/locales/tr';
import EMAIL_I18N_UK_MESSAGES from '@pkgs/email/src/email_i18n/locales/uk';
import EMAIL_I18N_VI_MESSAGES from '@pkgs/email/src/email_i18n/locales/vi';
import EMAIL_I18N_ZH_CN_MESSAGES from '@pkgs/email/src/email_i18n/locales/zh-CN';
import EMAIL_I18N_ZH_TW_MESSAGES from '@pkgs/email/src/email_i18n/locales/zh-TW';

export const EMAIL_I18N_LOCALE_MESSAGES = {
	ar: EMAIL_I18N_AR_MESSAGES,
	bg: EMAIL_I18N_BG_MESSAGES,
	cs: EMAIL_I18N_CS_MESSAGES,
	da: EMAIL_I18N_DA_MESSAGES,
	de: EMAIL_I18N_DE_MESSAGES,
	el: EMAIL_I18N_EL_MESSAGES,
	'en-GB': EMAIL_I18N_EN_GB_MESSAGES,
	'es-419': EMAIL_I18N_ES_419_MESSAGES,
	'es-ES': EMAIL_I18N_ES_ES_MESSAGES,
	fi: EMAIL_I18N_FI_MESSAGES,
	fr: EMAIL_I18N_FR_MESSAGES,
	he: EMAIL_I18N_HE_MESSAGES,
	hi: EMAIL_I18N_HI_MESSAGES,
	hr: EMAIL_I18N_HR_MESSAGES,
	hu: EMAIL_I18N_HU_MESSAGES,
	id: EMAIL_I18N_ID_MESSAGES,
	it: EMAIL_I18N_IT_MESSAGES,
	ja: EMAIL_I18N_JA_MESSAGES,
	ko: EMAIL_I18N_KO_MESSAGES,
	lt: EMAIL_I18N_LT_MESSAGES,
	nl: EMAIL_I18N_NL_MESSAGES,
	no: EMAIL_I18N_NO_MESSAGES,
	pl: EMAIL_I18N_PL_MESSAGES,
	'pt-BR': EMAIL_I18N_PT_BR_MESSAGES,
	ro: EMAIL_I18N_RO_MESSAGES,
	ru: EMAIL_I18N_RU_MESSAGES,
	'sv-SE': EMAIL_I18N_SV_SE_MESSAGES,
	th: EMAIL_I18N_TH_MESSAGES,
	tr: EMAIL_I18N_TR_MESSAGES,
	uk: EMAIL_I18N_UK_MESSAGES,
	vi: EMAIL_I18N_VI_MESSAGES,
	'zh-CN': EMAIL_I18N_ZH_CN_MESSAGES,
	'zh-TW': EMAIL_I18N_ZH_TW_MESSAGES,
} as const satisfies Record<string, Partial<Record<EmailTemplateKey, EmailTemplate>>>;

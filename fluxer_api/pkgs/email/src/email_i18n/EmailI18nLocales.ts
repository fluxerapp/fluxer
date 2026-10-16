// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EmailTemplate, EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';
import EMAIL_I18N_AR_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/ar.json' with {type: 'json'};
import EMAIL_I18N_BG_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/bg.json' with {type: 'json'};
import EMAIL_I18N_CS_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/cs.json' with {type: 'json'};
import EMAIL_I18N_DA_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/da.json' with {type: 'json'};
import EMAIL_I18N_DE_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/de.json' with {type: 'json'};
import EMAIL_I18N_EL_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/el.json' with {type: 'json'};
import EMAIL_I18N_EN_GB_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/en-GB.json' with {type: 'json'};
import EMAIL_I18N_ES_419_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/es-419.json' with {type: 'json'};
import EMAIL_I18N_ES_ES_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/es-ES.json' with {type: 'json'};
import EMAIL_I18N_FI_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/fi.json' with {type: 'json'};
import EMAIL_I18N_FR_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/fr.json' with {type: 'json'};
import EMAIL_I18N_HE_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/he.json' with {type: 'json'};
import EMAIL_I18N_HI_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/hi.json' with {type: 'json'};
import EMAIL_I18N_HR_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/hr.json' with {type: 'json'};
import EMAIL_I18N_HU_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/hu.json' with {type: 'json'};
import EMAIL_I18N_ID_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/id.json' with {type: 'json'};
import EMAIL_I18N_IT_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/it.json' with {type: 'json'};
import EMAIL_I18N_JA_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/ja.json' with {type: 'json'};
import EMAIL_I18N_KO_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/ko.json' with {type: 'json'};
import EMAIL_I18N_LT_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/lt.json' with {type: 'json'};
import EMAIL_I18N_NL_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/nl.json' with {type: 'json'};
import EMAIL_I18N_NO_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/no.json' with {type: 'json'};
import EMAIL_I18N_PL_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/pl.json' with {type: 'json'};
import EMAIL_I18N_PT_BR_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/pt-BR.json' with {type: 'json'};
import EMAIL_I18N_RO_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/ro.json' with {type: 'json'};
import EMAIL_I18N_RU_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/ru.json' with {type: 'json'};
import EMAIL_I18N_SV_SE_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/sv-SE.json' with {type: 'json'};
import EMAIL_I18N_TH_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/th.json' with {type: 'json'};
import EMAIL_I18N_TR_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/tr.json' with {type: 'json'};
import EMAIL_I18N_UK_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/uk.json' with {type: 'json'};
import EMAIL_I18N_VI_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/vi.json' with {type: 'json'};
import EMAIL_I18N_ZH_CN_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/zh-CN.json' with {type: 'json'};
import EMAIL_I18N_ZH_TW_MESSAGES from '@pkgs/email/src/email_i18n/weblate/locales/zh-TW.json' with {type: 'json'};

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

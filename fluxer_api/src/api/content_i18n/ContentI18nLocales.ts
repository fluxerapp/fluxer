// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ContentI18nKey} from '@app/api/content_i18n/ContentI18nMessages';
import CONTENT_I18N_AR_MESSAGES from '@app/api/content_i18n/weblate/locales/ar.json' with {type: 'json'};
import CONTENT_I18N_BG_MESSAGES from '@app/api/content_i18n/weblate/locales/bg.json' with {type: 'json'};
import CONTENT_I18N_CS_MESSAGES from '@app/api/content_i18n/weblate/locales/cs.json' with {type: 'json'};
import CONTENT_I18N_DA_MESSAGES from '@app/api/content_i18n/weblate/locales/da.json' with {type: 'json'};
import CONTENT_I18N_DE_MESSAGES from '@app/api/content_i18n/weblate/locales/de.json' with {type: 'json'};
import CONTENT_I18N_EL_MESSAGES from '@app/api/content_i18n/weblate/locales/el.json' with {type: 'json'};
import CONTENT_I18N_EN_GB_MESSAGES from '@app/api/content_i18n/weblate/locales/en-GB.json' with {type: 'json'};
import CONTENT_I18N_ES_419_MESSAGES from '@app/api/content_i18n/weblate/locales/es-419.json' with {type: 'json'};
import CONTENT_I18N_ES_ES_MESSAGES from '@app/api/content_i18n/weblate/locales/es-ES.json' with {type: 'json'};
import CONTENT_I18N_FI_MESSAGES from '@app/api/content_i18n/weblate/locales/fi.json' with {type: 'json'};
import CONTENT_I18N_FR_MESSAGES from '@app/api/content_i18n/weblate/locales/fr.json' with {type: 'json'};
import CONTENT_I18N_HE_MESSAGES from '@app/api/content_i18n/weblate/locales/he.json' with {type: 'json'};
import CONTENT_I18N_HI_MESSAGES from '@app/api/content_i18n/weblate/locales/hi.json' with {type: 'json'};
import CONTENT_I18N_HR_MESSAGES from '@app/api/content_i18n/weblate/locales/hr.json' with {type: 'json'};
import CONTENT_I18N_HU_MESSAGES from '@app/api/content_i18n/weblate/locales/hu.json' with {type: 'json'};
import CONTENT_I18N_ID_MESSAGES from '@app/api/content_i18n/weblate/locales/id.json' with {type: 'json'};
import CONTENT_I18N_IT_MESSAGES from '@app/api/content_i18n/weblate/locales/it.json' with {type: 'json'};
import CONTENT_I18N_JA_MESSAGES from '@app/api/content_i18n/weblate/locales/ja.json' with {type: 'json'};
import CONTENT_I18N_KO_MESSAGES from '@app/api/content_i18n/weblate/locales/ko.json' with {type: 'json'};
import CONTENT_I18N_LT_MESSAGES from '@app/api/content_i18n/weblate/locales/lt.json' with {type: 'json'};
import CONTENT_I18N_NL_MESSAGES from '@app/api/content_i18n/weblate/locales/nl.json' with {type: 'json'};
import CONTENT_I18N_NO_MESSAGES from '@app/api/content_i18n/weblate/locales/no.json' with {type: 'json'};
import CONTENT_I18N_PL_MESSAGES from '@app/api/content_i18n/weblate/locales/pl.json' with {type: 'json'};
import CONTENT_I18N_PT_BR_MESSAGES from '@app/api/content_i18n/weblate/locales/pt-BR.json' with {type: 'json'};
import CONTENT_I18N_RO_MESSAGES from '@app/api/content_i18n/weblate/locales/ro.json' with {type: 'json'};
import CONTENT_I18N_RU_MESSAGES from '@app/api/content_i18n/weblate/locales/ru.json' with {type: 'json'};
import CONTENT_I18N_SV_SE_MESSAGES from '@app/api/content_i18n/weblate/locales/sv-SE.json' with {type: 'json'};
import CONTENT_I18N_TH_MESSAGES from '@app/api/content_i18n/weblate/locales/th.json' with {type: 'json'};
import CONTENT_I18N_TR_MESSAGES from '@app/api/content_i18n/weblate/locales/tr.json' with {type: 'json'};
import CONTENT_I18N_UK_MESSAGES from '@app/api/content_i18n/weblate/locales/uk.json' with {type: 'json'};
import CONTENT_I18N_VI_MESSAGES from '@app/api/content_i18n/weblate/locales/vi.json' with {type: 'json'};
import CONTENT_I18N_ZH_CN_MESSAGES from '@app/api/content_i18n/weblate/locales/zh-CN.json' with {type: 'json'};
import CONTENT_I18N_ZH_TW_MESSAGES from '@app/api/content_i18n/weblate/locales/zh-TW.json' with {type: 'json'};

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

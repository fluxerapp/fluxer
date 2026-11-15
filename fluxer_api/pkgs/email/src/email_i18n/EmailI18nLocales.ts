// SPDX-License-Identifier: AGPL-3.0-or-later

import {loadStaticLocaleFiles} from '@fluxer/i18n/src/runtime/StaticLocaleFiles';
import type {EmailTemplate, EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';

export const EMAIL_I18N_LOCALE_MESSAGES = loadStaticLocaleFiles<Partial<Record<EmailTemplateKey, EmailTemplate>>>(
	'email',
	new URL('./weblate/locales/', import.meta.url),
);

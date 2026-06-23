// SPDX-License-Identifier: AGPL-3.0-or-later

import {identityLocale} from '@fluxer/i18n/src/normalization/IdentityLocale';
import {createStaticI18n} from '@fluxer/i18n/src/runtime/CreateStaticI18n';
import type {I18nResult} from '@fluxer/i18n/src/runtime/I18nTypes';
import {validateMessageTemplateVariables} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {EMAIL_I18N_LOCALE_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nLocales';
import {EMAIL_I18N_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nMessages';
import type {EmailTemplate, EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';

const DEFAULT_LOCALE = 'en-US';
const DEFAULT_EMAIL_TEMPLATE_VARIABLES = {
	product_name: 'Fluxer',
	appeals_email: 'appeals@fluxer.app',
	safety_email: 'safety@fluxer.app',
} satisfies Record<string, string>;
const emailI18n = createStaticI18n<EmailTemplateKey, EmailTemplate, Record<string, unknown>>(
	{
		defaultLocale: DEFAULT_LOCALE,
		defaultMessages: EMAIL_I18N_MESSAGES,
		localeMessages: EMAIL_I18N_LOCALE_MESSAGES,
		normalizeLocale: (locale) => identityLocale(locale),
		onWarning: (message) => {
			if (message.startsWith('Unsupported locale, falling back to en-US:')) {
				console.warn(
					`Unsupported locale for email translations, falling back to en-US: ${message.split(': ').slice(1).join(': ')}`,
				);
			} else {
				console.warn(message);
			}
		},
		validateVariables: (_key, template, variables) =>
			validateMessageTemplateVariables(template.subject, variables) ??
			validateMessageTemplateVariables(template.body, variables),
	},
	(template, variables, mf) => {
		const compiledSubject = String(mf.compile(template.subject)(variables));
		const compiledBody = String(mf.compile(template.body)(variables));
		return {subject: compiledSubject, body: compiledBody};
	},
);

export function getEmailTemplate(
	templateKey: EmailTemplateKey,
	locale: string | null,
	variables: Record<string, unknown>,
): I18nResult<EmailTemplateKey, EmailTemplate> {
	return emailI18n.getTemplate(templateKey, locale, {...DEFAULT_EMAIL_TEMPLATE_VARIABLES, ...variables});
}

export function resetEmailI18n(): void {
	emailI18n.reset();
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {I18nResult} from '@fluxer/i18n/src/runtime/I18nTypes';
import {getEmailTemplate} from '@pkgs/email/src/email_i18n/EmailI18n';
import type {EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {EmailTemplate, EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';

export interface IEmailI18nService {
	getTemplate<T extends EmailTemplateKey>(
		templateKey: T,
		locale: string | null,
		variables: EmailTemplateVariables[T],
		productName: string,
	): I18nResult<EmailTemplateKey, EmailTemplate>;
}

export class EmailI18nService implements IEmailI18nService {
	getTemplate<T extends EmailTemplateKey>(
		templateKey: T,
		locale: string | null,
		variables: EmailTemplateVariables[T],
		productName: string,
	): I18nResult<EmailTemplateKey, EmailTemplate> {
		return getEmailTemplate(templateKey, locale, variables, productName);
	}
}

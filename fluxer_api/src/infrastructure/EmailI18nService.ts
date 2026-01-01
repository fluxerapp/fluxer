/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import MessageFormat from '@messageformat/core';
import {Logger} from '~/Logger';
import {type EmailTemplateKey, type EmailTemplateVariables, getLocaleTranslations, hasLocale} from './email_i18n';

export interface LocalizedEmailTemplate {
	subject: string;
	body: string;
}

export class EmailI18nService {
	private readonly defaultLocale = 'en-US';
	private readonly messageFormatCache: Map<string, MessageFormat> = new Map();

	getTemplate<T extends EmailTemplateKey>(
		templateKey: T,
		locale: string | null,
		variables: EmailTemplateVariables[T],
	): LocalizedEmailTemplate {
		const effectiveLocale = this.getEffectiveLocale(locale);
		const translations = getLocaleTranslations(effectiveLocale);
		const fallbackTranslations = getLocaleTranslations(this.defaultLocale);
		const template = translations[templateKey] ?? fallbackTranslations[templateKey];
		if (!template) {
			throw new Error(`Missing email template ${templateKey} for locale ${effectiveLocale}`);
		}

		const subjectMf = this.getMessageFormat(effectiveLocale);
		const subject = subjectMf.compile(template.subject)(variables);

		const bodyMf = this.getMessageFormat(effectiveLocale);
		const body = bodyMf.compile(template.body)(variables);

		return {subject, body};
	}

	formatDate(
		date: Date,
		locale: string | null,
		options: Intl.DateTimeFormatOptions = {dateStyle: 'full', timeStyle: 'short'},
	): string {
		const effectiveLocale = this.getEffectiveLocale(locale);
		return date.toLocaleString(effectiveLocale, options);
	}

	formatNumber(num: number, locale: string | null): string {
		const effectiveLocale = this.getEffectiveLocale(locale);
		return num.toLocaleString(effectiveLocale);
	}

	private getMessageFormat(locale: string): MessageFormat {
		if (!this.messageFormatCache.has(locale)) {
			this.messageFormatCache.set(locale, new MessageFormat(locale));
		}
		return this.messageFormatCache.get(locale)!;
	}

	private getEffectiveLocale(locale: string | null): string {
		if (!locale) {
			return this.defaultLocale;
		}

		if (!hasLocale(locale)) {
			Logger.warn({locale}, 'Unsupported locale for email, falling back to en-US');
			return this.defaultLocale;
		}

		return locale;
	}
}

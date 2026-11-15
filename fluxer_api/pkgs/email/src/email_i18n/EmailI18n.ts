// SPDX-License-Identifier: AGPL-3.0-or-later

import {compileMessage} from '@fluxer/i18n/src/runtime/CompileMessage';
import {createStaticI18n} from '@fluxer/i18n/src/runtime/CreateStaticI18n';
import type {I18nResult} from '@fluxer/i18n/src/runtime/I18nTypes';
import {validateMessageTemplateVariables} from '@fluxer/i18n/src/runtime/MessageCatalogTypes';
import {getEmailTextDirection, inertEmailValue, isolateEmailValue} from '@pkgs/email/src/email_html/EmailHtmlRenderer';
import {EMAIL_I18N_LOCALE_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nLocales';
import {EMAIL_I18N_MESSAGES} from '@pkgs/email/src/email_i18n/EmailI18nMessages';
import type {EmailTemplateVariables} from '@pkgs/email/src/email_i18n/EmailI18nTypes';
import type {EmailTemplate, EmailTemplateKey} from '@pkgs/email/src/email_i18n/EmailI18nTypes.generated';

const DEFAULT_LOCALE = 'en-US';
const DOUBLED_PERIOD = /(?<!\.)\.\.(?!\.)/g;

const CALENDAR_DAY = /^\d{4}-\d{2}-\d{2}$/;
const INERT_VARIABLES = new Set([
	'channelName',
	'communityName',
	'otherName',
	'premiumName',
	'recipientName',
	'senderName',
	'statement',
	'username',
	'wallName',
]);
const ISOLATED_VARIABLES = new Set([
	'amount',
	'channelName',
	'communityName',
	'amountFormatted',
	'cancelByFormatted',
	'expiryFormatted',
	'nextFormatted',
	'priceFormatted',
	'receivedFormatted',
	'regularFormatted',
	'requestedFormatted',
	'unpaidFormatted',
	'founderName',
	'otherName',
	'premiumName',
	'recipientName',
	'senderName',
	'username',
	'wallName',
	'consentText',
	'appeal_deadline',
	'deletion_date',
]);

function isolateForLocale(value: string, locale: string): string {
	return getEmailTextDirection(locale) === 'rtl' ? isolateEmailValue(value) : value;
}

export function resolveEmailTimeZone(timeZone: string | null | undefined): string | null {
	if (!timeZone) return null;
	try {
		new Intl.DateTimeFormat('en-US', {timeZone});
		return timeZone;
	} catch {
		return null;
	}
}

function formatEmailDate(value: unknown, locale: string, style: string | null, timeZone: string | null): string {
	const calendarDay = typeof value === 'string' && CALENDAR_DAY.test(value);
	const options: Intl.DateTimeFormatOptions = {
		timeZone: calendarDay ? 'UTC' : (timeZone ?? 'UTC'),
		day: 'numeric',
		month: 'short',
		year: 'numeric',
	};
	if (style === 'full') {
		options.weekday = 'long';
		options.month = 'long';
	} else if (style === 'long') {
		options.month = 'long';
	} else if (style === 'short') {
		options.month = 'numeric';
	}
	const date = calendarDay ? new Date(`${value}T00:00:00Z`) : new Date(value as string | number | Date);
	return date.toLocaleDateString(locale, options);
}

function formatEmailTime(value: unknown, locale: string, style: string | null, timeZone: string | null): string {
	const options: Intl.DateTimeFormatOptions = {
		timeZone: timeZone ?? 'UTC',
		hour: 'numeric',
		minute: 'numeric',
		...(timeZone ? {} : {timeZoneName: 'short'}),
	};
	if (style === 'full' || style === 'long') {
		options.second = 'numeric';
	}
	const parts = new Intl.DateTimeFormat(locale, options).formatToParts(new Date(value as string | number | Date));
	const zone = parts.find((part) => part.type === 'timeZoneName');
	if (!zone) return parts.map((part) => part.value).join('');
	const time = parts
		.filter((part) => part.type !== 'timeZoneName')
		.map((part) => part.value)
		.join('')
		.replace(/[\s;,[\]()]+$/u, '')
		.replace(/^[\s;,[\]()]+/u, '');
	return parts.indexOf(zone) === 0 ? `${zone.value} ${time}` : `${time} ${zone.value}`;
}

function createEmailI18n(timeZone: string | null) {
	return createStaticI18n<EmailTemplateKey, EmailTemplate, Record<string, unknown>>(
		{
			defaultLocale: DEFAULT_LOCALE,
			defaultMessages: EMAIL_I18N_MESSAGES,
			localeMessages: EMAIL_I18N_LOCALE_MESSAGES,
			onWarning: (message) => {
				if (message.startsWith('Unsupported locale, falling back to en-US:')) {
					console.warn(
						`Unsupported locale for email translations, falling back to en-US: ${message.split(': ').slice(1).join(': ')}`,
					);
				} else {
					console.warn(message);
				}
			},
			validateVariables: (_key, template, variables) => {
				for (const text of Object.values(template)) {
					const error = validateMessageTemplateVariables(text, variables);
					if (error) {
						return error;
					}
				}
				return null;
			},
			messageFormatOptions: {
				customFormatters: {
					date: (value: unknown, locale: string, style: string | null) =>
						isolateForLocale(formatEmailDate(value, locale, style, timeZone), locale),
					time: (value: unknown, locale: string, style: string | null) =>
						isolateForLocale(formatEmailTime(value, locale, style, timeZone), locale),
				},
			},
		},
		(template, variables, mf) => {
			const compiled: EmailTemplate = {subject: '', intro: ''};
			for (const [field, text] of Object.entries(template)) {
				compiled[field] = String(compileMessage(mf, text)(variables)).replace(DOUBLED_PERIOD, '.');
			}
			return compiled;
		},
	);
}

const emailI18nByTimeZone = new Map<string | null, ReturnType<typeof createEmailI18n>>();

function getEmailI18n(timeZone: string | null): ReturnType<typeof createEmailI18n> {
	let instance = emailI18nByTimeZone.get(timeZone);
	if (!instance) {
		instance = createEmailI18n(timeZone);
		emailI18nByTimeZone.set(timeZone, instance);
	}
	return instance;
}

export function getEmailTemplate<T extends EmailTemplateKey>(
	templateKey: T,
	locale: string | null,
	variables: EmailTemplateVariables[T],
	productName: string,
	timeZone: string | null = null,
): I18nResult<EmailTemplateKey, EmailTemplate> {
	const resolvedLocale = locale ?? DEFAULT_LOCALE;
	const isolated = Object.fromEntries(
		Object.entries(variables as Record<string, unknown>).map(([name, value]) => {
			if (typeof value !== 'string') return [name, value];
			const inert = INERT_VARIABLES.has(name) ? inertEmailValue(value) : value;
			return [name, ISOLATED_VARIABLES.has(name) ? isolateForLocale(inert, resolvedLocale) : inert];
		}),
	);
	return getEmailI18n(resolveEmailTimeZone(timeZone)).getTemplate(templateKey, locale, {
		...isolated,
		product_name: productName,
	});
}

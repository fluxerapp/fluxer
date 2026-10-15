// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveContentLocale} from '@app/api/content_i18n/ContentI18n';

const PERMANENT_SUSPENSION_YEARS = 100;
const PERMANENT_SUSPENSION_THRESHOLD_YEARS = 50;

export function permanentSuspensionEnd(now: Date): Date {
	const end = new Date(now);
	end.setUTCFullYear(end.getUTCFullYear() + PERMANENT_SUSPENSION_YEARS);
	return end;
}

export function isPermanentSuspensionEnd(end: Date, now: Date): boolean {
	return end.getUTCFullYear() - now.getUTCFullYear() >= PERMANENT_SUSPENSION_THRESHOLD_YEARS;
}

function format(date: Date, locale: string | null | undefined, options: Intl.DateTimeFormatOptions): string {
	return new Intl.DateTimeFormat(resolveContentLocale(locale), {...options, timeZone: 'UTC'}).format(date);
}

export function formatNoticeTimeUtc(date: Date, locale: string | null | undefined): string {
	return `${format(date, locale, {dateStyle: 'medium', timeStyle: 'short'})} UTC`;
}

export function formatNoticeDate(date: Date, locale: string | null | undefined): string {
	return format(date, locale, {dateStyle: 'medium'});
}

export function formatNoticeMonth(date: Date, locale: string | null | undefined): string {
	return format(date, locale, {month: 'long', year: 'numeric'});
}

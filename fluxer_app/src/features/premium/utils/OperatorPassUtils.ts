// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {isolateBidi} from '@app/features/i18n/utils/BidiIsolate';
import {getCachedListFormat} from '@app/features/i18n/utils/IntlCache';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {shouldUse12HourFormat} from '@app/features/user/utils/DateFormatting';
import {getCurrentOrDetectedLocale} from '@app/features/user/utils/LocaleUtils';
import {getOperatorPassEarlyBirdEndsOn} from '@fluxer/constants/src/OperatorPassEarlyBird';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {getDateFormatter} from '@fluxer/date_utils/src/DateFormatterCache';
import {
	countOperatorPassCertificateNameCharacters,
	OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH,
	OPERATOR_PASS_WALL_NAME_MAX_LENGTH,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassNames';
import type {
	OperatorPassOfferResponse,
	OperatorPassPassesResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {
	getOperatorPassWallQuoteProblem as getSharedOperatorPassWallQuoteProblem,
	normalizeOperatorPassWallQuote,
	type OperatorPassWallQuoteProblem,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassWallQuote';

export const OPERATOR_PASS_LABS_INVITE_CODE = 'fluxer-labs';

export function focusOperatorPassField(id: string): void {
	requestAnimationFrame(() => {
		const target = document.getElementById(id);
		const control =
			target instanceof HTMLInputElement ? target : target?.querySelector<HTMLElement>('[role="checkbox"]');
		control?.focus();
	});
}

export function focusFirstInvalidOperatorPassField(fields: ReadonlyArray<{id: string; invalid: boolean}>): boolean {
	const field = fields.find((entry) => entry.invalid);
	if (!field) return false;
	focusOperatorPassField(field.id);
	return true;
}

export function isOperatorPassAvailable(): boolean {
	return !RuntimeConfig.isSelfHosted();
}

export function keepsOperatorsThroughVisionary(user: {premiumType: number | null} | null | undefined): boolean {
	return isOperatorPassAvailable() && user?.premiumType === UserPremiumTypes.LIFETIME;
}

export function formatOperatorPassDateTime(iso: string): string {
	const locale = getCurrentOrDetectedLocale();
	return getDateFormatter(locale, {
		dateStyle: 'medium',
		timeStyle: 'short',
		hour12: shouldUse12HourFormat(locale),
	}).format(new Date(iso));
}

export function formatOperatorPassLongDate(iso: string, timeZone?: string): string {
	return getDateFormatter(getCurrentOrDetectedLocale(), {dateStyle: 'long', timeZone}).format(new Date(iso));
}

export function formatOperatorPassDate(iso: string, timeZone?: string): string {
	return getDateFormatter(getCurrentOrDetectedLocale(), {dateStyle: 'medium', timeZone}).format(new Date(iso));
}

export const OPERATOR_PASS_VISIONARY_DATE_TIME_ZONE = 'UTC';

export function formatOperatorPassCertificateNumbers(numbers: ReadonlyArray<number>): string {
	return getCachedListFormat(getCurrentOrDetectedLocale(), {type: 'unit', style: 'short'}).format(
		numbers.map((value) => String(value).padStart(6, '0')),
	);
}

interface OperatorPassCalendarDay {
	year: number;
	month: number;
	day: number;
}

function formatCalendarDay(
	{year, month, day}: OperatorPassCalendarDay,
	dateStyle: 'medium' | 'long' = 'medium',
): string {
	return getDateFormatter(getCurrentOrDetectedLocale(), {dateStyle, timeZone: 'UTC'}).format(
		new Date(Date.UTC(year, month - 1, day)),
	);
}

function parseCalendarDay(day: string): OperatorPassCalendarDay {
	const [year, month, date] = day.split('-').map(Number) as [number, number, number];
	return {year, month, day: date};
}

export function formatOperatorPassCalendarDay(day: string): string {
	return formatCalendarDay(parseCalendarDay(day));
}

export function formatOperatorPassEarlyBirdEnd(priceRisesAt: string): string {
	return formatCalendarDay(parseCalendarDay(getOperatorPassEarlyBirdEndsOn(priceRisesAt)), 'long');
}

export function formatOperatorPassSinceDate(iso: string | Date): string {
	return getDateFormatter(getCurrentOrDetectedLocale(), {dateStyle: 'long'}).format(new Date(iso));
}

export type OperatorPassWallNameProblem = 'required' | 'too_long' | 'rejected';

export function getOperatorPassWallNameProblem(name: string, optOut: boolean): OperatorPassWallNameProblem | null {
	if (optOut) return null;
	const trimmed = normalizeOperatorPassWallName(name);
	if (trimmed.length === 0) return 'required';
	if (trimmed.length > OPERATOR_PASS_WALL_NAME_MAX_LENGTH) return 'too_long';
	return null;
}

export type {OperatorPassWallQuoteProblem};

export function getOperatorPassWallQuoteProblem(quote: string): OperatorPassWallQuoteProblem | null {
	const normalized = normalizeOperatorPassWallQuote(quote);
	return normalized === null ? null : getSharedOperatorPassWallQuoteProblem(normalized);
}

export type OperatorPassCertificateNameProblem = 'required' | 'too_long' | 'rejected';

export function normalizeOperatorPassCertificateName(name: string): string {
	return normalizeOperatorPassWallName(name.normalize('NFC'));
}

export function getOperatorPassCertificateNameProblem(name: string): OperatorPassCertificateNameProblem | null {
	const trimmed = normalizeOperatorPassCertificateName(name);
	if (trimmed.length === 0) return 'required';
	if (countOperatorPassCertificateNameCharacters(trimmed) > OPERATOR_PASS_CERTIFICATE_NAME_MAX_LENGTH)
		return 'too_long';
	return null;
}

export function normalizeOperatorPassWallName(name: string): string {
	return name.replace(/[\r\n]+/gu, ' ').trim();
}

export function getOperatorPassErrorCode(error: unknown): string | null {
	if (!(error instanceof HttpError)) return null;
	const body = error.body;
	if (body && typeof body === 'object' && 'code' in body && typeof body.code === 'string') {
		return body.code;
	}
	return null;
}

export function getOperatorPassOfferUserName(user: OperatorPassOfferResponse['sender']): string {
	if (!user) return '';
	return isolateBidi(user.global_name ?? user.username);
}

export function clampOperatorPassQuantity(value: number, max: number): number {
	if (!Number.isFinite(value)) return 1;
	return Math.min(Math.max(1, Math.floor(value)), Math.max(1, max));
}

export function isOperatorPassLastFreePass(
	holdings: Pick<OperatorPassPassesResponse, 'count' | 'outgoing_offers'>,
): boolean {
	const offered = holdings.outgoing_offers.filter(
		(offer) => offer.status === 'pending' || offer.status === 'accepted',
	).length;
	return holdings.count - offered <= 1;
}

export function isOperatorPassLastPurchase(
	holdings: Pick<OperatorPassPassesResponse, 'count' | 'passes'> | null,
	purchaseId: string,
): boolean {
	if (!holdings || holdings.count === 0) return false;
	const fromPurchase = holdings.passes.filter(
		(pass) => pass.purchase_id === purchaseId && pass.status === 'held',
	).length;
	return holdings.count - fromPurchase <= 0;
}

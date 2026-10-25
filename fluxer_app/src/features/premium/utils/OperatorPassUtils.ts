// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {isolateBidi} from '@app/features/i18n/utils/BidiIsolate';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {shouldUse12HourFormat} from '@app/features/user/utils/DateFormatting';
import {getCurrentOrDetectedLocale} from '@app/features/user/utils/LocaleUtils';
import {getOperatorPassEarlyBirdEndsOn} from '@fluxer/constants/src/OperatorPassEarlyBird';
import {getDateFormatter} from '@fluxer/date_utils/src/DateFormatterCache';
import {
	OPERATOR_PASS_WALL_NAME_MAX_LENGTH,
	type OperatorPassOfferResponse,
	type OperatorPassPassesResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

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

export function hasOperatorPassProfileBadge(user: {operatorPassSince: unknown}): boolean {
	return isOperatorPassAvailable() && user.operatorPassSince != null;
}

export function formatOperatorPassDateTime(iso: string): string {
	const locale = getCurrentOrDetectedLocale();
	return getDateFormatter(locale, {
		dateStyle: 'medium',
		timeStyle: 'short',
		hour12: shouldUse12HourFormat(locale),
	}).format(new Date(iso));
}

export function formatOperatorPassLongDate(iso: string): string {
	return getDateFormatter(getCurrentOrDetectedLocale(), {dateStyle: 'long'}).format(new Date(iso));
}

export function formatOperatorPassDate(iso: string): string {
	return getDateFormatter(getCurrentOrDetectedLocale(), {dateStyle: 'medium'}).format(new Date(iso));
}

interface OperatorPassCalendarDay {
	year: number;
	month: number;
	day: number;
}

function getZonedTime(at: number, timeZone?: string): OperatorPassCalendarDay & {startOfDay: boolean} {
	const parts = new Intl.DateTimeFormat('en-US', {
		timeZone,
		hourCycle: 'h23',
		year: 'numeric',
		month: 'numeric',
		day: 'numeric',
		hour: 'numeric',
		minute: 'numeric',
		second: 'numeric',
	}).formatToParts(new Date(at));
	const value = (type: Intl.DateTimeFormatPartTypes) => Number(parts.find((part) => part.type === type)?.value ?? 0);
	return {
		year: value('year'),
		month: value('month'),
		day: value('day'),
		startOfDay: value('hour') === 0 && value('minute') === 0 && value('second') === 0 && at % 1000 === 0,
	};
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

export function formatOperatorPassUnlockDay(iso: string, timeZone?: string): string {
	const zoned = getZonedTime(Date.parse(iso), timeZone);
	if (zoned.startOfDay) return formatCalendarDay(zoned);
	const next = new Date(Date.UTC(zoned.year, zoned.month - 1, zoned.day + 1));
	return formatCalendarDay({year: next.getUTCFullYear(), month: next.getUTCMonth() + 1, day: next.getUTCDate()});
}

export function hasOperatorPassTimePassed(iso: string | null | undefined, now: number = Date.now()): boolean {
	if (!iso) return true;
	const at = Date.parse(iso);
	return Number.isNaN(at) || at <= now;
}

export function formatOperatorPassSinceDate(iso: string | Date): string {
	return getDateFormatter(getCurrentOrDetectedLocale(), {dateStyle: 'long'}).format(new Date(iso));
}

export type OperatorPassWallNameProblem = 'required' | 'too_long';

export function getOperatorPassWallNameProblem(name: string, optOut: boolean): OperatorPassWallNameProblem | null {
	if (optOut) return null;
	const trimmed = normalizeOperatorPassWallName(name);
	if (trimmed.length === 0) return 'required';
	if (trimmed.length > OPERATOR_PASS_WALL_NAME_MAX_LENGTH) return 'too_long';
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

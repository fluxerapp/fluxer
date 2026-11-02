// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';

const WITHDRAWAL_PERIOD_DAYS = 14;

const EASTERNMOST_BUYER_UTC_OFFSET_MS = ms('4 hours');

const WESTERNMOST_BUYER_UTC_OFFSET_MS = -ms('4 hours');

const FIXED_DATE_HOLIDAYS = new Set(['01-01', '01-06', '05-01', '06-06', '12-24', '12-25', '12-26', '12-31']);

const EASTER_HOLIDAY_OFFSETS_DAYS = [-2, 1, 39];

function calendarDay(year: number, monthIndex: number, date: number): Date {
	return new Date(Date.UTC(year, monthIndex, date));
}

function addDays(day: Date, days: number): Date {
	return calendarDay(day.getUTCFullYear(), day.getUTCMonth(), day.getUTCDate() + days);
}

function getEasterSunday(year: number): Date {
	const a = year % 19;
	const b = Math.floor(year / 100);
	const c = year % 100;
	const d = Math.floor(b / 4);
	const e = b % 4;
	const f = Math.floor((b + 8) / 25);
	const g = Math.floor((b - f + 1) / 3);
	const h = (19 * a + b - d - g + 15) % 30;
	const i = Math.floor(c / 4);
	const k = c % 4;
	const l = (32 + 2 * e + 2 * i - h - k) % 7;
	const m = Math.floor((a + 11 * h + 22 * l) / 451);
	const month = Math.floor((h + l - 7 * m + 114) / 31);
	const date = ((h + l - 7 * m + 114) % 31) + 1;
	return calendarDay(year, month - 1, date);
}

function isMidsummerEve(day: Date): boolean {
	return day.getUTCDay() === 5 && day.getUTCMonth() === 5 && day.getUTCDate() >= 19 && day.getUTCDate() <= 25;
}

export function isWithdrawalPeriodWorkingDay(day: Date): boolean {
	const weekday = day.getUTCDay();
	if (weekday === 0 || weekday === 6 || isMidsummerEve(day)) {
		return false;
	}
	const monthDay = `${String(day.getUTCMonth() + 1).padStart(2, '0')}-${String(day.getUTCDate()).padStart(2, '0')}`;
	if (FIXED_DATE_HOLIDAYS.has(monthDay)) {
		return false;
	}
	const easter = getEasterSunday(day.getUTCFullYear());
	return !EASTER_HOLIDAY_OFFSETS_DAYS.some((offset) => addDays(easter, offset).getTime() === day.getTime());
}

function getLastWithdrawalDay(purchasedAt: Date): Date {
	const latestPurchaseDay = new Date(purchasedAt.getTime() + EASTERNMOST_BUYER_UTC_OFFSET_MS);
	let lastDay = addDays(
		calendarDay(latestPurchaseDay.getUTCFullYear(), latestPurchaseDay.getUTCMonth(), latestPurchaseDay.getUTCDate()),
		WITHDRAWAL_PERIOD_DAYS,
	);
	while (!isWithdrawalPeriodWorkingDay(lastDay)) {
		lastDay = addDays(lastDay, 1);
	}
	return lastDay;
}

export function getWithdrawalPeriodLastDay(purchasedAt: Date): string {
	return getLastWithdrawalDay(purchasedAt).toISOString().slice(0, 10);
}

export function getWithdrawalPeriodEndsAt(purchasedAt: Date): Date {
	return new Date(addDays(getLastWithdrawalDay(purchasedAt), 1).getTime() - WESTERNMOST_BUYER_UTC_OFFSET_MS);
}

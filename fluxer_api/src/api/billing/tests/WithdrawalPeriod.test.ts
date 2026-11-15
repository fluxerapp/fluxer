// SPDX-License-Identifier: AGPL-3.0-or-later

import {getWithdrawalPeriodEndsAt, isWithdrawalPeriodWorkingDay} from '@app/api/billing/WithdrawalPeriod';
import {describe, expect, test} from 'vitest';

const BUYER_TIME_ZONES = [
	'America/Guadeloupe',
	'America/Martinique',
	'America/Cayenne',
	'Atlantic/Azores',
	'Atlantic/Reykjavik',
	'Atlantic/Canary',
	'Atlantic/Madeira',
	'Europe/London',
	'Europe/Dublin',
	'Europe/Lisbon',
	'Europe/Stockholm',
	'Europe/Berlin',
	'Europe/Helsinki',
	'Europe/Athens',
	'Asia/Nicosia',
	'Indian/Mayotte',
	'Indian/Reunion',
];

const wallClockFormats = new Map<string, Intl.DateTimeFormat>();

function wallClock(instant: Date, timeZone: string): {year: number; month: number; day: number; utcMs: number} {
	let format = wallClockFormats.get(timeZone);
	if (!format) {
		format = new Intl.DateTimeFormat('en-US', {
			timeZone,
			hourCycle: 'h23',
			year: 'numeric',
			month: 'numeric',
			day: 'numeric',
			hour: 'numeric',
			minute: 'numeric',
			second: 'numeric',
		});
		wallClockFormats.set(timeZone, format);
	}
	const parts = Object.fromEntries(format.formatToParts(instant).map((part) => [part.type, Number(part.value)]));
	return {
		year: parts.year!,
		month: parts.month!,
		day: parts.day!,
		utcMs: Date.UTC(parts.year!, parts.month! - 1, parts.day!, parts.hour!, parts.minute!, parts.second!),
	};
}

function startOfLocalDay(year: number, month: number, day: number, timeZone: string): Date {
	const wall = Date.UTC(year, month - 1, day);
	let guess = wall;
	for (let round = 0; round < 3; round++) {
		guess = wall - (wallClock(new Date(guess), timeZone).utcMs - guess);
	}
	return new Date(guess);
}

function localDeadline(purchasedAt: Date, timeZone: string): Date {
	const purchaseDay = wallClock(purchasedAt, timeZone);
	let lastDay = new Date(Date.UTC(purchaseDay.year, purchaseDay.month - 1, purchaseDay.day + 14));
	while (!isWithdrawalPeriodWorkingDay(lastDay)) {
		lastDay = new Date(lastDay.getTime() + 24 * 60 * 60 * 1000);
	}
	return startOfLocalDay(lastDay.getUTCFullYear(), lastDay.getUTCMonth() + 1, lastDay.getUTCDate() + 1, timeZone);
}

function day(isoDate: string): Date {
	return new Date(`${isoDate}T00:00:00Z`);
}

describe('withdrawal period', () => {
	test('a Friday 20 November 13:00 CET purchase ends after Friday 4 December in every buyer zone', () => {
		const endsAt = getWithdrawalPeriodEndsAt(new Date('2026-11-20T12:00:00Z'));
		expect(endsAt.toISOString()).toBe('2026-12-05T04:00:00.000Z');
		expect(localDeadline(new Date('2026-11-20T12:00:00Z'), 'Europe/Stockholm').toISOString()).toBe(
			'2026-12-04T23:00:00.000Z',
		);
	});

	test('a last day on a Saturday moves the end to the end of Monday', () => {
		const endsAt = getWithdrawalPeriodEndsAt(new Date('2026-11-21T09:00:00Z'));
		expect(endsAt.toISOString()).toBe('2026-12-08T04:00:00.000Z');
		expect(localDeadline(new Date('2026-11-21T09:00:00Z'), 'Europe/Stockholm').toISOString()).toBe(
			'2026-12-07T23:00:00.000Z',
		);
	});

	test('Christmas and New Year move the end to the next working day', () => {
		expect(getWithdrawalPeriodEndsAt(new Date('2026-12-10T11:00:00Z')).toISOString()).toBe('2026-12-29T04:00:00.000Z');
		expect(getWithdrawalPeriodEndsAt(new Date('2026-12-17T11:00:00Z')).toISOString()).toBe('2027-01-05T04:00:00.000Z');
		expect(getWithdrawalPeriodEndsAt(new Date('2026-12-23T11:00:00Z')).toISOString()).toBe('2027-01-08T04:00:00.000Z');
	});

	test('Easter and Ascension move the end to the next working day', () => {
		expect(getWithdrawalPeriodEndsAt(new Date('2027-03-12T11:00:00Z')).toISOString()).toBe('2027-03-31T04:00:00.000Z');
		expect(getWithdrawalPeriodEndsAt(new Date('2027-04-22T10:00:00Z')).toISOString()).toBe('2027-05-08T04:00:00.000Z');
	});

	test('the holiday set covers weekends, the fixed days, Midsummer Eve and the Easter days', () => {
		const closed = [
			'2026-11-21',
			'2026-11-22',
			'2026-12-24',
			'2026-12-25',
			'2026-12-31',
			'2027-01-01',
			'2027-01-06',
			'2027-03-26',
			'2027-03-29',
			'2027-05-06',
			'2027-06-25',
			'2028-04-14',
			'2028-04-17',
			'2028-05-01',
			'2028-05-25',
			'2028-06-06',
			'2028-06-23',
		];
		const open = ['2026-11-20', '2026-12-28', '2027-01-04', '2027-03-30', '2027-06-24', '2028-06-22', '2028-06-26'];
		expect(closed.filter((date) => isWithdrawalPeriodWorkingDay(day(date)))).toEqual([]);
		expect(open.filter((date) => !isWithdrawalPeriodWorkingDay(day(date)))).toEqual([]);
	});

	test('a period across the end of summer time is never short in local time', () => {
		const purchasedAt = new Date('2026-10-11T10:00:00Z');
		expect(getWithdrawalPeriodEndsAt(purchasedAt).toISOString()).toBe('2026-10-27T04:00:00.000Z');
		expect(localDeadline(purchasedAt, 'Europe/Stockholm').toISOString()).toBe('2026-10-26T23:00:00.000Z');
		expect(localDeadline(purchasedAt, 'Europe/London').toISOString()).toBe('2026-10-27T00:00:00.000Z');
	});

	test('a period across the start of summer time is never short in local time', () => {
		const purchasedAt = new Date('2028-03-14T11:00:00Z');
		expect(getWithdrawalPeriodEndsAt(purchasedAt).toISOString()).toBe('2028-03-29T04:00:00.000Z');
		expect(localDeadline(purchasedAt, 'Europe/Stockholm').toISOString()).toBe('2028-03-28T22:00:00.000Z');
		expect(localDeadline(purchasedAt, 'Atlantic/Azores').toISOString()).toBe('2028-03-29T00:00:00.000Z');
	});

	test('a purchase just after local midnight counts from that local day', () => {
		const purchasedAt = new Date('2026-11-20T23:05:00Z');
		expect(localDeadline(purchasedAt, 'Europe/Stockholm').toISOString()).toBe('2026-12-07T23:00:00.000Z');
		expect(getWithdrawalPeriodEndsAt(purchasedAt).toISOString()).toBe('2026-12-08T04:00:00.000Z');
		const reunion = new Date('2026-11-20T20:05:00Z');
		expect(localDeadline(reunion, 'Indian/Reunion').toISOString()).toBe('2026-12-07T20:00:00.000Z');
		expect(getWithdrawalPeriodEndsAt(reunion).toISOString()).toBe('2026-12-08T04:00:00.000Z');
	});

	test('without knowing the buyer zone, the end is never before the local end in any EU, EEA or UK zone', () => {
		const step = 97 * 60 * 1000;
		const from = Date.parse('2026-09-01T00:00:00Z');
		const to = Date.parse('2028-12-31T00:00:00Z');
		const short: Array<string> = [];
		let maxOvershootMs = 0;
		for (let at = from; at < to; at += step) {
			const purchasedAt = new Date(at);
			const endsAt = getWithdrawalPeriodEndsAt(purchasedAt).getTime();
			for (const timeZone of BUYER_TIME_ZONES) {
				const local = localDeadline(purchasedAt, timeZone).getTime();
				if (endsAt < local) {
					short.push(`${purchasedAt.toISOString()} ${timeZone}`);
				}
				if (timeZone === 'Europe/Stockholm') {
					maxOvershootMs = Math.max(maxOvershootMs, endsAt - local);
				}
			}
		}
		expect(short).toEqual([]);
		expect(maxOvershootMs).toBeLessThanOrEqual(6 * 24 * 60 * 60 * 1000);
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	addCalendarMonths,
	type DueNotice,
	dueSubscriptionNotices,
	type NoticeInput,
	noticeStillValid,
	PAYMENT_CONFIRMATION_REMINDERS,
} from '@app/api/stripe/services/BillingNoticeSchedule';
import {describe, expect, test} from 'vitest';

const DAY = 24 * 60 * 60;
const NOW = Date.UTC(2026, 9, 28, 9, 15) / 1000;

function subscription(patch: Partial<NoticeInput> = {}): NoticeInput {
	const chargeAt = patch.chargeAt ?? NOW + 20 * DAY;
	return {
		status: 'active',
		cancelling: false,
		collectionMethod: 'charge_automatically',
		paused: false,
		product: 'plutonium',
		plan: 'monthly',
		chargeAt,
		billingCycleAnchor: chargeAt,
		trialing: false,
		startDate: NOW - 40 * DAY,
		card: null,
		now: NOW,
		...patch,
	};
}

function yearly(daysAway: number, patch: Partial<NoticeInput> = {}): NoticeInput {
	return subscription({plan: 'yearly', chargeAt: NOW + daysAway * DAY, ...patch});
}

function monthlyWithAnniversaryAt(anniversary: number, chargeAt: number, patch: Partial<NoticeInput> = {}) {
	return subscription({startDate: addCalendarMonths(anniversary, -12), chargeAt, ...patch});
}

function notices(input: NoticeInput): Array<DueNotice['notice']> {
	return dueSubscriptionNotices(input).map((due) => due.notice);
}

const OCTOBER_CARD = {expMonth: 10, expYear: 2026};
const EARLY_NOVEMBER = Date.UTC(2026, 10, 5) / 1000;

describe('renewal notice', () => {
	test('a yearly plan is due 35 days before the charge', () => {
		expect(dueSubscriptionNotices(yearly(35))).toEqual([{notice: 'renewal', chargeAt: NOW + 35 * DAY, late: false}]);
	});

	test('a yearly plan is not due 36 days before the charge', () => {
		expect(notices(yearly(36))).toEqual([]);
	});

	test('a yearly plan 20 days before the charge is due and marked late', () => {
		expect(dueSubscriptionNotices(yearly(20))).toEqual([{notice: 'renewal', chargeAt: NOW + 20 * DAY, late: true}]);
	});

	test('a yearly plan 14 days before the charge is not due', () => {
		expect(notices(yearly(14))).toEqual([]);
	});

	test('a yearly donation is due', () => {
		expect(notices(yearly(33, {product: 'donation'}))).toEqual(['renewal']);
	});
});

describe('annual notice', () => {
	test('a monthly plan is due when its next charge falls on the anniversary 33 days away', () => {
		const chargeAt = NOW + 33 * DAY;
		expect(dueSubscriptionNotices(monthlyWithAnniversaryAt(chargeAt, chargeAt))).toEqual([
			{notice: 'annual', chargeAt, late: false},
		]);
	});

	test('a monthly plan is due when the charge after next crosses the anniversary', () => {
		const chargeAt = NOW + 3 * DAY;
		const following = addCalendarMonths(chargeAt, 1);
		expect(dueSubscriptionNotices(monthlyWithAnniversaryAt(following, chargeAt))).toEqual([
			{notice: 'annual', chargeAt: following, late: false},
		]);
	});

	test('a monthly plan finds the charge after next from its billing anchor day', () => {
		const chargeAt = Date.UTC(2027, 1, 28) / 1000;
		const following = Date.UTC(2027, 2, 31) / 1000;
		const input = monthlyWithAnniversaryAt(Date.UTC(2027, 2, 30) / 1000, chargeAt, {
			billingCycleAnchor: Date.UTC(2026, 0, 31) / 1000,
			now: following - 33 * DAY,
		});
		expect(dueSubscriptionNotices(input)).toEqual([{notice: 'annual', chargeAt: following, late: false}]);
	});

	test('a monthly plan away from its anniversary is not due', () => {
		const chargeAt = NOW + 33 * DAY;
		expect(notices(monthlyWithAnniversaryAt(chargeAt + 90 * DAY, chargeAt))).toEqual([]);
	});

	test('a monthly plan in a trial that ends on the anniversary is due', () => {
		const chargeAt = NOW + 33 * DAY;
		expect(notices(monthlyWithAnniversaryAt(chargeAt, chargeAt, {status: 'trialing', trialing: true}))).toEqual([
			'annual',
		]);
	});

	test('a monthly donation never gets it', () => {
		const chargeAt = NOW + 33 * DAY;
		expect(notices(monthlyWithAnniversaryAt(chargeAt, chargeAt, {product: 'donation'}))).toEqual([]);
	});
});

describe('gift notice', () => {
	function giftTrial(daysAway: number, patch: Partial<NoticeInput> = {}): NoticeInput {
		return subscription({status: 'trialing', trialing: true, chargeAt: NOW + daysAway * DAY, ...patch});
	}

	test('a monthly gift trial is due 8 days before it ends', () => {
		expect(dueSubscriptionNotices(giftTrial(8))).toEqual([{notice: 'gift', chargeAt: NOW + 8 * DAY, late: false}]);
	});

	test('a monthly gift trial 3 days before it ends is due and marked late', () => {
		expect(dueSubscriptionNotices(giftTrial(3))).toEqual([{notice: 'gift', chargeAt: NOW + 3 * DAY, late: true}]);
	});

	test('a yearly gift trial 33 days before it ends gets the renewal notice', () => {
		expect(notices(giftTrial(33, {plan: 'yearly'}))).toEqual(['renewal']);
	});

	test('a yearly gift trial is due 8 days before it ends', () => {
		expect(notices(giftTrial(8, {plan: 'yearly'}))).toEqual(['gift']);
	});
});

describe('exclusions', () => {
	test('a plan that is set to cancel gets nothing', () => {
		expect(notices(yearly(33, {cancelling: true}))).toEqual([]);
	});

	test('a plan paid by invoice gets nothing', () => {
		expect(notices(yearly(33, {collectionMethod: 'send_invoice'}))).toEqual([]);
	});

	test('a plan with paused collection gets nothing', () => {
		expect(notices(yearly(33, {paused: true}))).toEqual([]);
	});

	test('a past due plan gets nothing', () => {
		expect(notices(yearly(33, {status: 'past_due'}))).toEqual([]);
	});

	test('a subscription for another product gets nothing', () => {
		expect(notices(yearly(33, {product: null}))).toEqual([]);
	});
});

describe('card expiring notice', () => {
	test('is due when the card expires before a charge within 14 days', () => {
		const input = subscription({chargeAt: EARLY_NOVEMBER, card: OCTOBER_CARD});
		expect(dueSubscriptionNotices(input)).toEqual([{notice: 'card_expiring', chargeAt: EARLY_NOVEMBER, late: false}]);
	});

	test('is not due when the card expires after the charge', () => {
		expect(notices(subscription({chargeAt: EARLY_NOVEMBER, card: {expMonth: 11, expYear: 2026}}))).toEqual([]);
	});

	test('is not due when the charge is 15 days away', () => {
		expect(notices(subscription({chargeAt: NOW + 15 * DAY, card: OCTOBER_CARD}))).toEqual([]);
	});

	test('is not due without a card the customer can update', () => {
		expect(notices(subscription({chargeAt: EARLY_NOVEMBER, card: null}))).toEqual([]);
	});
});

describe('delivery check', () => {
	test('accepts a renewal notice 16 days before the charge', () => {
		const input = yearly(16);
		expect(noticeStillValid(input, 'renewal', input.chargeAt)).toBe(true);
	});

	test('rejects a renewal notice when the charge date changed', () => {
		const input = yearly(33);
		expect(noticeStillValid(input, 'renewal', input.chargeAt - DAY)).toBe(false);
	});

	test('accepts a shortened gift trial with less than a day left', () => {
		const input = subscription({status: 'trialing', trialing: true, chargeAt: NOW + 5 * 60 * 60});
		expect(noticeStillValid(input, 'gift', input.chargeAt)).toBe(true);
	});

	test('accepts a shortened yearly gift trial with 5 days left', () => {
		const input = yearly(5, {status: 'trialing', trialing: true});
		expect(noticeStillValid(input, 'gift', input.chargeAt)).toBe(true);
	});

	test('rejects a gift notice with less than an hour left', () => {
		const input = subscription({status: 'trialing', trialing: true, chargeAt: NOW + 30 * 60});
		expect(noticeStillValid(input, 'gift', input.chargeAt)).toBe(false);
	});
});

test('every confirmation reminder runs within the 7 day job lifetime', () => {
	expect(PAYMENT_CONFIRMATION_REMINDERS.every(({delaySeconds}) => delaySeconds < 7 * DAY)).toBe(true);
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingRecurringPlan} from '@pkgs/email/src/IEmailService';
import {seconds} from 'itty-time';
import type Stripe from 'stripe';

const DAY = seconds('1 day');
const HOUR = seconds('1 hour');

export const BILLING_NOTICES = ['renewal', 'annual', 'gift', 'card_expiring'] as const;

export type BillingNotice = (typeof BILLING_NOTICES)[number];

type ChargeNotice = Exclude<BillingNotice, 'card_expiring'>;

export type BillingNoticeProduct = 'plutonium' | 'donation';

export const PAYMENT_CONFIRMATION_REMINDERS = [
	{reminder: 1, delaySeconds: seconds('3 days')},
	{reminder: 2, delaySeconds: seconds('6 days')},
] as const;

export const CARD_NOTICE_LEAD_SECONDS = 14 * DAY;

export const GIFT_NOTICE_MIN_LEAD_SECONDS = HOUR;

export interface NoticeInput {
	status: Stripe.Subscription.Status;
	cancelling: boolean;
	collectionMethod: string;
	paused: boolean;
	product: BillingNoticeProduct | null;
	plan: BillingRecurringPlan;
	chargeAt: number;
	billingCycleAnchor: number;
	trialing: boolean;
	startDate: number;
	card: {expMonth: number; expYear: number} | null;
	now: number;
}

export interface DueNotice {
	notice: BillingNotice;
	chargeAt: number;
	late: boolean;
}

interface NoticeWindow {
	minLead: number;
	onTimeAbove: number;
	maxLead: number;
}

const WINDOWS: Record<ChargeNotice, NoticeWindow> = {
	renewal: {minLead: 15 * DAY, onTimeAbove: 31 * DAY, maxLead: 35 * DAY},
	annual: {minLead: 25 * DAY, onTimeAbove: 31 * DAY, maxLead: 35 * DAY},
	gift: {minLead: GIFT_NOTICE_MIN_LEAD_SECONDS, onTimeAbove: 7 * DAY, maxLead: 8 * DAY},
};

const CARD_WINDOW = {minLead: HOUR, maxLead: CARD_NOTICE_LEAD_SECONDS};

function inRange(lead: number, range: {minLead: number; maxLead: number}): boolean {
	return lead >= range.minLead && lead <= range.maxLead;
}

function utcMonthDate(year: number, month: number, day: number, timeOf: Date): number {
	const lastDay = new Date(Date.UTC(year, month + 1, 0)).getUTCDate();
	return (
		Date.UTC(
			year,
			month,
			Math.min(day, lastDay),
			timeOf.getUTCHours(),
			timeOf.getUTCMinutes(),
			timeOf.getUTCSeconds(),
		) / 1000
	);
}

export function addCalendarMonths(unix: number, months: number): number {
	const date = new Date(unix * 1000);
	return utcMonthDate(date.getUTCFullYear(), date.getUTCMonth() + months, date.getUTCDate(), date);
}

function followingMonthlyCharge(chargeAt: number, anchor: number): number {
	const charge = new Date(chargeAt * 1000);
	const anchorDate = new Date(anchor * 1000);
	return utcMonthDate(charge.getUTCFullYear(), charge.getUTCMonth() + 1, anchorDate.getUTCDate(), anchorDate);
}

function excluded(input: NoticeInput): boolean {
	return (
		input.product === null ||
		(input.status !== 'active' && input.status !== 'trialing') ||
		input.cancelling ||
		input.collectionMethod !== 'charge_automatically' ||
		input.paused
	);
}

function anniversaryCrossedBy(startDate: number, charge: number): number | null {
	let years = 1;
	while (addCalendarMonths(startDate, 12 * (years + 1)) <= charge) {
		years += 1;
	}
	const anniversary = addCalendarMonths(startDate, 12 * years);
	return anniversary <= charge && charge < addCalendarMonths(anniversary, 1) ? anniversary : null;
}

function annualCandidates(input: NoticeInput): Array<number> {
	if (input.product !== 'plutonium' || input.plan !== 'monthly') {
		return [];
	}
	const anchor = input.trialing ? input.chargeAt : input.billingCycleAnchor;
	return [input.chargeAt, followingMonthlyCharge(input.chargeAt, anchor)].filter(
		(charge) => anniversaryCrossedBy(input.startDate, charge) !== null,
	);
}

export function annualNoticeAnniversary(input: NoticeInput, chargeAt: number): number | null {
	return anniversaryCrossedBy(input.startDate, chargeAt);
}

function cardExpiresBefore(card: NoticeInput['card'], chargeAt: number): boolean {
	return card !== null && Date.UTC(card.expYear, card.expMonth, 1) / 1000 <= chargeAt;
}

function applies(input: NoticeInput, notice: ChargeNotice): boolean {
	switch (notice) {
		case 'renewal':
			return input.plan === 'yearly';
		case 'gift':
			return input.product === 'plutonium' && input.trialing;
		case 'annual':
			return annualCandidates(input).length > 0;
	}
}

function chargesFor(input: NoticeInput, notice: ChargeNotice): Array<number> {
	return notice === 'annual' ? annualCandidates(input) : [input.chargeAt];
}

export function dueSubscriptionNotices(input: NoticeInput): Array<DueNotice> {
	if (excluded(input)) {
		return [];
	}
	const due: Array<DueNotice> = [];
	for (const notice of ['renewal', 'annual', 'gift'] as const) {
		if (!applies(input, notice)) {
			continue;
		}
		const window = WINDOWS[notice];
		for (const chargeAt of chargesFor(input, notice)) {
			const lead = chargeAt - input.now;
			if (inRange(lead, window)) {
				due.push({notice, chargeAt, late: lead <= window.onTimeAbove});
			}
		}
	}
	if (cardExpiresBefore(input.card, input.chargeAt) && inRange(input.chargeAt - input.now, CARD_WINDOW)) {
		due.push({notice: 'card_expiring', chargeAt: input.chargeAt, late: false});
	}
	return due;
}

export function noticeStillValid(input: NoticeInput, notice: BillingNotice, chargeAt: number): boolean {
	if (excluded(input)) {
		return false;
	}
	const lead = chargeAt - input.now;
	if (notice === 'card_expiring') {
		return chargeAt === input.chargeAt && cardExpiresBefore(input.card, chargeAt) && lead >= CARD_WINDOW.minLead;
	}
	return applies(input, notice) && chargesFor(input, notice).includes(chargeAt) && lead >= WINDOWS[notice].minLead;
}

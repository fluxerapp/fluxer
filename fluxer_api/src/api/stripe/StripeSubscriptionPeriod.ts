// SPDX-License-Identifier: AGPL-3.0-or-later

import {extractId} from '@app/api/stripe/StripeUtils';
import type Stripe from 'stripe';

type StripePaymentIntentWithExpandedLatestCharge = Stripe.PaymentIntent & {
	latest_charge?: string | Stripe.Charge | null;
};
type StripeInvoicePaymentEntry = {
	payment?: {
		charge?: string | Stripe.Charge | null;
		payment_intent?: string | StripePaymentIntentWithExpandedLatestCharge | null;
	} | null;
	status?: string | null;
	status_transitions?: {
		paid_at?: number | null;
	} | null;
};
type StripeInvoiceWithPayments = Stripe.Invoice & {
	payments?: {
		data?: Array<StripeInvoicePaymentEntry>;
	} | null;
};

export function getPrimarySubscriptionItem(subscription: Stripe.Subscription): Stripe.SubscriptionItem | null {
	const items = subscription.items?.data ?? [];
	if (items.length === 0) {
		return null;
	}
	const recurringItem = items.find((item) => Boolean(item.price?.recurring));
	return recurringItem ?? items[0];
}

export function getSubscriptionItemPeriodEndUnix(item: Stripe.SubscriptionItem | null): number | null {
	return item?.current_period_end ?? null;
}

export function getSubscriptionItemPeriodEnd(item: Stripe.SubscriptionItem | null): Date | null {
	const periodEnd = getSubscriptionItemPeriodEndUnix(item);
	return periodEnd == null ? null : new Date(periodEnd * 1000);
}

export function getSubscriptionEntitlementPeriodEndUnix(
	subscription: Stripe.Subscription,
	item: Stripe.SubscriptionItem | null,
): number | null {
	const periodEnd = getSubscriptionItemPeriodEndUnix(item);
	const trialEnd = subscription.trial_end ?? null;
	if (trialEnd != null && periodEnd != null) {
		return Math.max(trialEnd, periodEnd);
	}
	return trialEnd ?? periodEnd;
}

function getSubscriptionCurrentPeriodEnd(subscription: Stripe.Subscription): Date | null {
	const items = subscription.items?.data ?? [];
	if (items.length === 0) {
		return null;
	}
	let latestPeriodEnd: number | null = null;
	for (const item of items) {
		if (item.current_period_end == null) {
			continue;
		}
		if (latestPeriodEnd == null || item.current_period_end > latestPeriodEnd) {
			latestPeriodEnd = item.current_period_end;
		}
	}
	return latestPeriodEnd == null ? null : new Date(latestPeriodEnd * 1000);
}

export function getSubscriptionCurrentPeriodStart(subscription: Stripe.Subscription): Date | null {
	let latestPeriodStart: number | null = null;
	for (const item of subscription.items?.data ?? []) {
		if (item.current_period_start == null) {
			continue;
		}
		if (latestPeriodStart == null || item.current_period_start > latestPeriodStart) {
			latestPeriodStart = item.current_period_start;
		}
	}
	return latestPeriodStart == null ? null : new Date(latestPeriodStart * 1000);
}

export function getSubscriptionPremiumPeriodEnd(subscription: Stripe.Subscription): Date | null {
	if (subscription.cancel_at) {
		return new Date(subscription.cancel_at * 1000);
	}
	const trialEnd = subscription.trial_end ? new Date(subscription.trial_end * 1000) : null;
	const currentPeriodEnd = getSubscriptionCurrentPeriodEnd(subscription);
	if (trialEnd && currentPeriodEnd) {
		return trialEnd > currentPeriodEnd ? trialEnd : currentPeriodEnd;
	}
	return trialEnd ?? currentPeriodEnd;
}

export function getSubscriptionStartDate(subscription: Stripe.Subscription): Date {
	return new Date((subscription.start_date ?? subscription.created) * 1000);
}

export function getInvoiceLatestLinePeriodStart(invoice: Stripe.Invoice): Date | null {
	let latestPeriodStartUnix: number | null = null;
	for (const line of invoice.lines?.data ?? []) {
		const periodStart = line.period?.start ?? null;
		if (periodStart && (latestPeriodStartUnix === null || periodStart > latestPeriodStartUnix)) {
			latestPeriodStartUnix = periodStart;
		}
	}
	return latestPeriodStartUnix ? new Date(latestPeriodStartUnix * 1000) : null;
}

export function getInvoiceLatestLinePeriodEnd(invoice: Stripe.Invoice): Date | null {
	const lines = invoice.lines?.data ?? [];
	let latestPeriodEndUnix: number | null = null;
	for (const line of lines) {
		const periodEnd = line.period?.end ?? null;
		if (periodEnd && (latestPeriodEndUnix === null || periodEnd > latestPeriodEndUnix)) {
			latestPeriodEndUnix = periodEnd;
		}
	}
	return latestPeriodEndUnix ? new Date(latestPeriodEndUnix * 1000) : null;
}

function getFirstInvoicePaymentIntent(
	invoice: Stripe.Invoice | null,
): string | StripePaymentIntentWithExpandedLatestCharge | null {
	const invoiceWithPayments = invoice as StripeInvoiceWithPayments | null;
	const entries = invoiceWithPayments?.payments?.data ?? [];
	for (const entry of entries) {
		const paymentIntent = entry.payment?.payment_intent ?? null;
		if (paymentIntent) {
			return paymentIntent;
		}
	}
	return null;
}

export function getFirstInvoicePaymentIntentId(invoice: Stripe.Invoice | null): string | null {
	const paymentIntent = getFirstInvoicePaymentIntent(invoice);
	if (!paymentIntent) {
		return null;
	}
	if (typeof paymentIntent === 'string') {
		return paymentIntent;
	}
	return paymentIntent.id ?? null;
}

export function getInvoicePaymentRef(invoice: Stripe.Invoice): {
	chargeId: string | null;
	paymentIntentId: string | null;
	paidAt: Date | null;
} | null {
	const candidates = (invoice as StripeInvoiceWithPayments).payments?.data ?? [];
	const preferred =
		candidates.find((c) => c.status === 'paid' && (c.payment?.payment_intent || c.payment?.charge)) ??
		candidates.find((c) => c.payment?.payment_intent || c.payment?.charge) ??
		null;
	if (!preferred) {
		return null;
	}
	const paymentIntentId = extractId(preferred.payment?.payment_intent);
	let chargeId = extractId(preferred.payment?.charge);
	if (!chargeId && preferred.payment?.payment_intent && typeof preferred.payment.payment_intent !== 'string') {
		chargeId = extractId(preferred.payment.payment_intent.latest_charge);
	}
	const paidAtSec = preferred.status_transitions?.paid_at ?? null;
	return {
		chargeId,
		paymentIntentId,
		paidAt: paidAtSec ? new Date(paidAtSec * 1000) : null,
	};
}

export function getSubscriptionBillingCycle(subscription: Stripe.Subscription): 'monthly' | 'yearly' | null {
	const item = getPrimarySubscriptionItem(subscription);
	const interval = item?.price?.recurring?.interval;
	if (interval === 'month') {
		return 'monthly';
	}
	if (interval === 'year') {
		return 'yearly';
	}
	return null;
}

export async function loadSubscriptionSchedule(
	stripe: Stripe | null,
	scheduleRef: Stripe.Subscription['schedule'],
): Promise<Stripe.SubscriptionSchedule | null> {
	if (!scheduleRef) return null;
	if (typeof scheduleRef === 'object') return scheduleRef;
	if (!stripe) return null;
	return stripe.subscriptionSchedules.retrieve(scheduleRef);
}

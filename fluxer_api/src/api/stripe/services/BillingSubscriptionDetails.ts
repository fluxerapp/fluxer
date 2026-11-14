// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import type {ProductRegistry} from '@app/api/stripe/ProductRegistry';
import {
	getPrimarySubscriptionItem,
	getSubscriptionBillingCycle,
	getSubscriptionItemPeriodEndUnix,
	loadSubscriptionSchedule,
} from '@app/api/stripe/StripeSubscriptionPeriod';
import {extractId} from '@app/api/stripe/StripeUtils';
import {
	invoiceTotals,
	plainTotals,
	primaryInvoiceLine,
	subscriptionBillingProduct,
} from '@app/api/stripe/services/BillingInvoiceDetails';
import {
	addCalendarMonths,
	CARD_NOTICE_LEAD_SECONDS,
	type NoticeInput,
} from '@app/api/stripe/services/BillingNoticeSchedule';
import type {BillingPaymentMethod, BillingRecurringPlan, BillingTotals} from '@pkgs/email/src/IEmailService';
import {seconds} from 'itty-time';
import Stripe from 'stripe';

const CANCEL_SOURCE_FRESH_SECONDS = seconds('5 minutes');

export const CLEARED_CANCEL_METADATA = {fluxer_cancel_source: '', fluxer_cancel_requested_at: ''};

const NAMED_PAYMENT_METHODS: Partial<Record<string, string>> = {
	amazon_pay: 'Amazon Pay',
	bacs_debit: 'Bacs Direct Debit',
	link: 'Link',
	mobilepay: 'MobilePay',
	paypal: 'PayPal',
	pix: 'Pix',
	revolut_pay: 'Revolut Pay',
	sepa_debit: 'SEPA Direct Debit',
	upi: 'UPI',
	us_bank_account: 'ACH Direct Debit',
};

const CARD_WALLETS: Partial<Record<string, string>> = {
	apple_pay: 'Apple Pay',
	google_pay: 'Google Pay',
	link: 'Link',
	samsung_pay: 'Samsung Pay',
};

export interface SubscriptionNoticeContext {
	input: NoticeInput;
	paymentMethod: Stripe.PaymentMethod | null;
	schedule: Stripe.SubscriptionSchedule | null;
}

interface UpcomingChargeAmounts {
	next: BillingTotals | null;
	regular: BillingTotals;
	taxIncluded: boolean;
}

export function subscriptionCancelling(
	subscription: Stripe.Subscription,
	schedule: Stripe.SubscriptionSchedule | null,
): boolean {
	return subscription.cancel_at_period_end || subscription.cancel_at != null || schedule?.end_behavior === 'cancel';
}

function subscriptionTrialing(subscription: Stripe.Subscription, now: number): boolean {
	return subscription.trial_end != null && subscription.trial_end > now;
}

function subscriptionChargeAt(subscription: Stripe.Subscription, now: number): number | null {
	if (subscriptionTrialing(subscription, now)) {
		return subscription.trial_end;
	}
	return getSubscriptionItemPeriodEndUnix(getPrimarySubscriptionItem(subscription));
}

export function freshCancelSource(subscription: Stripe.Subscription, at: number): string | null {
	const source = subscription.metadata?.fluxer_cancel_source;
	const requestedAt = Number(subscription.metadata?.fluxer_cancel_requested_at);
	if (!source || !Number.isFinite(requestedAt)) {
		return null;
	}
	return Math.abs(at - requestedAt) <= CANCEL_SOURCE_FRESH_SECONDS ? source : null;
}

function chargePlan(
	subscription: Stripe.Subscription,
	schedule: Stripe.SubscriptionSchedule | null,
	chargeAt: number,
	productRegistry: ProductRegistry,
): BillingRecurringPlan {
	const phase = schedule?.phases.find((candidate) => candidate.start_date === chargeAt);
	const phasePrice = extractId(phase?.items[0]?.price ?? null);
	const phaseCycle = phasePrice ? productRegistry.getProduct(phasePrice)?.billingCycle : undefined;
	if (phaseCycle === 'monthly' || phaseCycle === 'yearly') {
		return phaseCycle;
	}
	return getSubscriptionBillingCycle(subscription) ?? 'monthly';
}

async function paymentMethodObject(
	stripe: Stripe,
	ref: string | Stripe.PaymentMethod | null | undefined,
): Promise<Stripe.PaymentMethod | null> {
	if (!ref) {
		return null;
	}
	return typeof ref === 'string' ? stripe.paymentMethods.retrieve(ref) : ref;
}

async function subscriptionCustomer(
	stripe: Stripe,
	subscription: Stripe.Subscription,
): Promise<Stripe.Customer | null> {
	const customer =
		typeof subscription.customer === 'string'
			? await stripe.customers.retrieve(subscription.customer, {expand: ['invoice_settings.default_payment_method']})
			: subscription.customer;
	return 'deleted' in customer && customer.deleted ? null : (customer as Stripe.Customer);
}

async function defaultPaymentMethod(
	stripe: Stripe,
	subscription: Stripe.Subscription,
): Promise<Stripe.PaymentMethod | null> {
	if (subscription.default_payment_method) {
		return paymentMethodObject(stripe, subscription.default_payment_method);
	}
	if (subscription.default_source) {
		return null;
	}
	const customer = await subscriptionCustomer(stripe, subscription);
	return paymentMethodObject(stripe, customer?.invoice_settings?.default_payment_method);
}

function expiringCard(paymentMethod: Stripe.PaymentMethod | null): NoticeInput['card'] {
	const card = paymentMethod?.type === 'card' ? paymentMethod.card : null;
	if (!card || card.wallet != null) {
		return null;
	}
	return {expMonth: card.exp_month, expYear: card.exp_year};
}

export function billingPaymentMethod(paymentMethod: Stripe.PaymentMethod | null): BillingPaymentMethod {
	if (!paymentMethod) {
		return {kind: 'none'};
	}
	if (paymentMethod.type === 'card' && paymentMethod.card) {
		const wallet = paymentMethod.card.wallet?.type ? CARD_WALLETS[paymentMethod.card.wallet.type] : undefined;
		return wallet
			? {kind: 'named', name: wallet}
			: {kind: 'card', brand: paymentMethod.card.brand, last4: paymentMethod.card.last4};
	}
	const name = NAMED_PAYMENT_METHODS[paymentMethod.type];
	return name ? {kind: 'named', name} : {kind: 'none'};
}

export async function subscriptionNoticeContext(
	stripe: Stripe,
	subscription: Stripe.Subscription,
	productRegistry: ProductRegistry,
	now: number,
	paymentMethod: 'always' | 'when_due',
): Promise<SubscriptionNoticeContext | null> {
	const chargeAt = subscriptionChargeAt(subscription, now);
	if (!chargeAt) {
		return null;
	}
	const schedule = await loadSubscriptionSchedule(stripe, subscription.schedule);
	const needsPaymentMethod = paymentMethod === 'always' || chargeAt - now <= CARD_NOTICE_LEAD_SECONDS;
	const method = needsPaymentMethod ? await defaultPaymentMethod(stripe, subscription) : null;
	return {
		input: {
			status: subscription.status,
			cancelling: subscriptionCancelling(subscription, schedule),
			collectionMethod: subscription.collection_method,
			paused: subscription.pause_collection != null,
			product: subscriptionBillingProduct(subscription, productRegistry),
			plan: chargePlan(subscription, schedule, chargeAt, productRegistry),
			chargeAt,
			billingCycleAnchor: subscription.billing_cycle_anchor,
			trialing: subscriptionTrialing(subscription, now),
			startDate: subscription.start_date,
			card: expiringCard(method),
			now,
		},
		paymentMethod: method,
		schedule,
	};
}

function regularTotals(unitMinor: number, preview: Stripe.Invoice): BillingTotals {
	const previewTotals = invoiceTotals(preview);
	const tax = previewTotals.tax;
	const taxBase = tax?.behavior === 'exclusive' ? preview.total - tax.amountMinor : preview.total;
	if (!tax || taxBase <= 0) {
		return {...plainTotals(unitMinor, preview.currency), reverseCharge: previewTotals.reverseCharge};
	}
	const taxMinor = Math.round((unitMinor * tax.amountMinor) / taxBase);
	const totalMinor = tax.behavior === 'exclusive' ? unitMinor + taxMinor : unitMinor;
	return {
		totalMinor,
		paidMinor: totalMinor,
		currency: preview.currency,
		tax: {amountMinor: taxMinor, behavior: tax.behavior},
		reverseCharge: previewTotals.reverseCharge,
	};
}

function lineUnitAmount(line: Stripe.InvoiceLineItem): number | null {
	const unit = Number(line.pricing?.unit_amount_decimal ?? Number.NaN);
	return Number.isFinite(unit) ? Math.round(unit * (line.quantity ?? 1)) : null;
}

async function discountCoupon(stripe: Stripe, discount: Stripe.Discount): Promise<Stripe.Coupon | null> {
	const coupon = discount.source?.coupon;
	if (!coupon) {
		return null;
	}
	return typeof coupon === 'string' ? stripe.coupons.retrieve(coupon) : coupon;
}

async function discountsLastingPast(
	stripe: Stripe,
	subscription: Stripe.Subscription,
	after: number,
): Promise<Set<string>> {
	const lasting = new Set<string>();
	for (const discount of subscription.discounts ?? []) {
		if (typeof discount === 'string') {
			continue;
		}
		const coupon = await discountCoupon(stripe, discount);
		if (
			coupon?.duration === 'forever' ||
			(coupon?.duration === 'repeating' && discount.end != null && discount.end > after)
		) {
			lasting.add(discount.id);
		}
	}
	return lasting;
}

function lastingDiscountAmount(line: Stripe.InvoiceLineItem, lasting: Set<string>): number {
	return (line.discount_amounts ?? [])
		.filter((discount) => lasting.has(extractId(discount.discount) ?? ''))
		.reduce((sum, discount) => sum + discount.amount, 0);
}

async function previewInvoice(stripe: Stripe, subscriptionId: string): Promise<Stripe.Invoice | null> {
	try {
		return await stripe.invoices.createPreview({subscription: subscriptionId});
	} catch (error) {
		if (!(error instanceof Stripe.errors.StripeError)) {
			throw error;
		}
		Logger.warn({error, subscriptionId}, 'billing_notice_preview_fallback');
		return null;
	}
}

export async function upcomingChargeAmounts(
	stripe: Stripe,
	subscription: Stripe.Subscription,
	chargeAt: number,
): Promise<UpcomingChargeAmounts> {
	const item = getPrimarySubscriptionItem(subscription);
	const listUnit = (item?.price?.unit_amount ?? 0) * (item?.quantity ?? 1);
	const fallback = {
		next: null,
		regular: plainTotals(listUnit, item?.price?.currency ?? subscription.currency),
		taxIncluded: false,
	};
	const preview = await previewInvoice(stripe, subscription.id);
	const line = preview ? primaryInvoiceLine(preview) : null;
	if (!preview || !line || line.period?.start !== chargeAt) {
		if (preview) {
			Logger.warn(
				{subscriptionId: subscription.id, chargeAt, previewPeriodStart: line?.period?.start ?? null},
				'billing_notice_preview_fallback',
			);
		}
		return fallback;
	}
	const next = {...invoiceTotals(preview), totalMinor: preview.amount_due, paidMinor: preview.amount_due};
	const months = getSubscriptionBillingCycle(subscription) === 'yearly' ? 12 : 1;
	const lasting = await discountsLastingPast(stripe, subscription, addCalendarMonths(chargeAt, months));
	const regularUnit = Math.max(0, (lineUnitAmount(line) ?? listUnit) - lastingDiscountAmount(line, lasting));
	return {next, regular: regularTotals(regularUnit, preview), taxIncluded: true};
}

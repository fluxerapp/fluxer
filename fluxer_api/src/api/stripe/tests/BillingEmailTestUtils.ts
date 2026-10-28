// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StripeWebhookEventData} from '@app/api/test/msw/handlers/StripeApiHandlers';

const DAY_SECONDS = 24 * 60 * 60;

type StripeObject = {id: string} & Record<string, unknown>;

export const BILLING_EMAIL_PRICES = {
	monthlyUsd: 'price_monthly_usd',
	monthlyEur: 'price_monthly_eur',
	yearlyUsd: 'price_yearly_usd',
	yearlyEur: 'price_yearly_eur',
	gift1MonthUsd: 'price_gift_1_month_usd',
	gift1MonthEur: 'price_gift_1_month_eur',
	gift1YearUsd: 'price_gift_1_year_usd',
	gift1YearEur: 'price_gift_1_year_eur',
};

function unixNow(): number {
	return Math.floor(Date.now() / 1000);
}

export function invoiceLine(
	priceId: string,
	{amount = 499, days = 30, proration = false}: {amount?: number; days?: number; proration?: boolean} = {},
) {
	const start = unixNow();
	return {
		id: `il_${priceId}_${amount}`,
		object: 'line_item',
		amount,
		currency: 'usd',
		period: {start, end: start + days * DAY_SECONDS},
		pricing: {
			type: 'price_details',
			price_details: {price: priceId, product: 'prod_test'},
			unit_amount_decimal: String(Math.abs(amount)),
		},
		parent: {type: 'subscription_item_details', subscription_item_details: {proration}},
	};
}

export interface InvoiceOptions {
	id: string;
	customerId?: string;
	lines?: Array<ReturnType<typeof invoiceLine>>;
	billingReason?: string;
	status?: string;
	total?: number;
	amountPaid?: number;
	tax?: {amount: number; behavior: 'inclusive' | 'exclusive'};
	reverseCharge?: boolean;
	subscriptionId?: string | null;
	subscriptionMetadata?: Record<string, string>;
	metadata?: Record<string, string>;
	attemptCount?: number;
	invoicePdf?: boolean;
}

export function stripeInvoice(options: InvoiceOptions): StripeObject {
	const now = unixNow();
	const total = options.total ?? 499;
	const taxes = [
		...(options.tax
			? [{amount: options.tax.amount, tax_behavior: options.tax.behavior, taxability_reason: 'standard_rated'}]
			: []),
		...(options.reverseCharge ? [{amount: 0, tax_behavior: 'exclusive', taxability_reason: 'reverse_charge'}] : []),
	];
	const paid = (options.status ?? 'paid') === 'paid';
	return {
		id: options.id,
		object: 'invoice',
		status: options.status ?? 'paid',
		customer: options.customerId ?? 'cus_test_1',
		billing_reason: options.billingReason ?? 'subscription_cycle',
		total,
		amount_due: total,
		amount_paid: paid ? (options.amountPaid ?? total) : 0,
		amount_remaining: paid ? 0 : total,
		attempt_count: options.attemptCount ?? 1,
		currency: 'usd',
		number: `NUM-${options.id}`,
		created: now,
		status_transitions: {paid_at: paid ? now : null, finalized_at: now},
		hosted_invoice_url: `https://invoice.stripe.com/i/${options.id}`,
		invoice_pdf: options.invoicePdf ? `https://pay.stripe.com/invoice/${options.id}/pdf` : null,
		total_taxes: taxes,
		metadata: options.metadata ?? {},
		parent:
			options.subscriptionId === null
				? null
				: {
						type: 'subscription_details',
						subscription_details: {
							subscription: options.subscriptionId ?? 'sub_test_1',
							metadata: options.subscriptionMetadata ?? {},
						},
					},
		lines: {object: 'list', data: options.lines ?? [invoiceLine(BILLING_EMAIL_PRICES.monthlyUsd)], has_more: false},
		payments: {
			object: 'list',
			data: [
				{
					id: `inpay_${options.id}`,
					object: 'invoice_payment',
					invoice: options.id,
					created: now,
					status: paid ? 'paid' : 'open',
					payment: {type: 'payment_intent', payment_intent: `pi_${options.id}`},
				},
			],
			has_more: false,
		},
	};
}

export function stripeRefund(options: {
	id: string;
	paymentIntentId: string;
	amount?: number;
	status?: string;
	createdDaysAgo?: number;
	metadata?: Record<string, string>;
}): StripeObject {
	return {
		id: options.id,
		object: 'refund',
		amount: options.amount ?? 499,
		currency: 'usd',
		status: options.status ?? 'succeeded',
		payment_intent: options.paymentIntentId,
		charge: `ch_${options.paymentIntentId}`,
		created: unixNow() - (options.createdDaysAgo ?? 0) * DAY_SECONDS,
		metadata: options.metadata ?? {},
	};
}

export function objectEvent(type: string, object: StripeObject): StripeWebhookEventData {
	return {type, data: {object}};
}

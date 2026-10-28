// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createTestAccount, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {UserRow} from '@app/api/database/types/UserTypes';
import {ProductType} from '@app/api/stripe/ProductRegistry';
import {sendStripeWebhook, setupSyncStripeWebhookWorker} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	createStripeApiHandlers,
	type StripeApiHandlers,
	type StripeWebhookEventData,
} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, afterEach, beforeAll, beforeEach} from 'vitest';

export const DAY_SECONDS = 24 * 60 * 60;

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

export function unixNow(): number {
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
	nextPaymentAttempt?: number | null;
	intentStatus?: string;
	invoicePdf?: boolean;
}

function defaultNextAttempt(paid: boolean, now: number): number | null {
	return paid ? null : now + 3 * DAY_SECONDS;
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
		next_payment_attempt:
			options.nextPaymentAttempt === undefined ? defaultNextAttempt(paid, now) : options.nextPaymentAttempt,
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
					payment: {
						type: 'payment_intent',
						payment_intent: options.intentStatus
							? {id: `pi_${options.id}`, object: 'payment_intent', status: options.intentStatus}
							: `pi_${options.id}`,
					},
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

export type SubscriptionPatch = Parameters<StripeApiHandlers['put']['subscription']>[1];

export function plutoniumSubscription(customerId: string, patch: SubscriptionPatch = {}): SubscriptionPatch {
	return {
		customer: customerId,
		price_id: BILLING_EMAIL_PRICES.yearlyUsd,
		unit_amount: 4999,
		currency: 'usd',
		interval: 'year',
		current_period_start: unixNow() - 330 * DAY_SECONDS,
		current_period_end: unixNow() + 33 * DAY_SECONDS,
		default_payment_method: 'pm_test_1',
		...patch,
	};
}

export function cardPaymentMethod(
	id: string,
	{
		brand = 'visa',
		last4 = '4242',
		expMonth = 1,
		expYear = 2030,
		wallet = null,
	}: {brand?: string; last4?: string; expMonth?: number; expYear?: number; wallet?: string | null} = {},
): StripeObject {
	return {
		id,
		object: 'payment_method',
		type: 'card',
		customer: 'cus_test_1',
		card: {brand, last4, exp_month: expMonth, exp_year: expYear, country: 'US', wallet: wallet ? {type: wallet} : null},
	};
}

export function namedPaymentMethod(id: string, type: string): StripeObject {
	return {id, object: 'payment_method', type, customer: 'cus_test_1', card: null};
}

export function invoicePreview(options: {
	chargeAt: number;
	amountDue: number;
	unitAmount?: number;
	tax?: {amount: number; behavior: 'inclusive' | 'exclusive'};
	discount?: {id: string; amount: number};
}): StripeObject {
	const unitAmount = options.unitAmount ?? options.amountDue;
	return {
		id: 'upcoming_in',
		object: 'invoice',
		currency: 'usd',
		total: options.amountDue,
		amount_due: options.amountDue,
		total_taxes: options.tax
			? [{amount: options.tax.amount, tax_behavior: options.tax.behavior, taxability_reason: 'standard_rated'}]
			: [],
		lines: {
			object: 'list',
			has_more: false,
			data: [
				{
					...invoiceLine(BILLING_EMAIL_PRICES.yearlyUsd, {amount: options.amountDue, days: 365}),
					period: {start: options.chargeAt, end: options.chargeAt + 365 * DAY_SECONDS},
					quantity: 1,
					discount_amounts: options.discount ? [{amount: options.discount.amount, discount: options.discount.id}] : [],
					pricing: {
						type: 'price_details',
						price_details: {price: BILLING_EMAIL_PRICES.yearlyUsd, product: 'prod_test'},
						unit_amount_decimal: String(unitAmount),
					},
				},
			],
		},
	};
}

export function subscriptionDiscount(id: string, coupon: {duration: string}, end: number | null = null) {
	return {id, object: 'discount', end, source: {type: 'coupon', coupon: {id: `co_${id}`, object: 'coupon', ...coupon}}};
}

export function creditNote(options: {id: string; invoiceId: string; balanceAmount: number | null}): StripeObject {
	return {
		id: options.id,
		object: 'credit_note',
		status: 'issued',
		currency: 'usd',
		total: options.balanceAmount ?? 499,
		invoice: options.invoiceId,
		pdf: `https://pay.stripe.com/credit_notes/${options.id}/pdf`,
		customer_balance_transaction:
			options.balanceAmount === null
				? null
				: {id: `cbtxn_${options.id}`, object: 'customer_balance_transaction', amount: -options.balanceAmount},
	};
}

export function billingEmailTestContext() {
	let harness: ApiTestHarness;
	let stripe: StripeApiHandlers;
	let originalWebhookSecret: string | undefined;
	let originalPrices: typeof Config.stripe.prices | undefined;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		setupSyncStripeWebhookWorker();
		originalWebhookSecret = Config.stripe.webhookSecret;
		originalPrices = Config.stripe.prices;
		Config.stripe.webhookSecret = 'whsec_test_secret';
		Config.stripe.prices = BILLING_EMAIL_PRICES;
		stripe = createStripeApiHandlers();
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.webhookSecret = originalWebhookSecret;
		Config.stripe.prices = originalPrices;
	});
	beforeEach(async () => {
		await harness.resetData();
		stripe.reset();
		server.use(...stripe.handlers);
		await clearTestEmails(harness);
	});
	afterEach(() => {
		server.resetHandlers();
	});

	async function createBuyer(customerId: string, patch: Partial<UserRow> = {}) {
		const account = await createTestAccount(harness);
		const userRepository = new UserRepository();
		const userId = createUserID(BigInt(account.userId));
		await userRepository.patchUpsert(
			userId,
			{stripe_customer_id: customerId, ...patch},
			(await userRepository.findUnique(userId))!.toRow(),
		);
		return account;
	}

	async function createSubscriber(customerId: string, patch: Partial<UserRow> = {}) {
		const subscriptionId = `sub_${customerId}`;
		const account = await createBuyer(customerId, {
			premium_type: UserPremiumTypes.SUBSCRIPTION,
			premium_until: new Date(Date.now() + 2 * DAY_SECONDS * 1000),
			stripe_subscription_id: subscriptionId,
			premium_billing_cycle: 'monthly',
			...patch,
		});
		const payments = new PaymentRepository();
		const checkoutSessionId = `cs_${customerId}`;
		await payments.createPayment({
			checkout_session_id: checkoutSessionId,
			user_id: createUserID(BigInt(account.userId)),
			price_id: BILLING_EMAIL_PRICES.monthlyUsd,
			product_type: ProductType.MONTHLY_SUBSCRIPTION,
			status: 'completed',
			is_gift: false,
			created_at: new Date(),
		});
		await payments.updatePayment({checkout_session_id: checkoutSessionId, subscription_id: subscriptionId});
		return account;
	}

	async function emailsOfType(recipient: string, type: string) {
		return (await listTestEmails(harness, {recipient})).filter((email) => email.type === type);
	}

	async function webhook(event: StripeWebhookEventData) {
		await sendStripeWebhook(harness, event);
	}

	return {
		get harness() {
			return harness;
		},
		get stripe() {
			return stripe;
		},
		createBuyer,
		createSubscriber,
		emailsOfType,
		webhook,
	};
}

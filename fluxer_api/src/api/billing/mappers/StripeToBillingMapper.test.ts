// SPDX-License-Identifier: AGPL-3.0-or-later

import type Stripe from 'stripe';
import {describe, expect, it} from 'vitest';
import {
	computeStripeUpdatedAt,
	mapStripeChargeToRow,
	mapStripeCheckoutSessionToRow,
	mapStripeCustomerToRow,
	mapStripeDisputeToRow,
	mapStripeInvoiceToRow,
	mapStripePaymentIntentToRow,
	mapStripePaymentMethodToRow,
	mapStripePriceToRow,
	mapStripeProductToRow,
	mapStripeRefundToRow,
	mapStripeSubscriptionToRow,
	normalizeBillingSubscriptionRow,
	type StripeSubscriptionPayload,
	safeMetadata,
	unixToDate,
} from './StripeToBillingMapper';

const NOW_UNIX = 1700000000;

function stripeFixture<T>(value: object): T {
	return value as T;
}

function expectTupleGetValue(value: object, index: number, expected: unknown): void {
	if (!('get' in value) || typeof value.get !== 'function') {
		throw new Error('Expected tuple getter');
	}
	expect(value.get(index)).toBe(expected);
}

describe('unixToDate', () => {
	it('returns null for null/undefined', () => {
		expect(unixToDate(null)).toBeNull();
		expect(unixToDate(undefined)).toBeNull();
	});
	it('converts epoch seconds to Date', () => {
		const d = unixToDate(NOW_UNIX);
		expect(d).toBeInstanceOf(Date);
		expect(d!.getTime()).toBe(NOW_UNIX * 1000);
	});
});

describe('safeMetadata', () => {
	it('returns null for null/undefined', () => {
		expect(safeMetadata(null)).toBeNull();
		expect(safeMetadata(undefined)).toBeNull();
	});
	it('returns null for empty metadata', () => {
		expect(safeMetadata({})).toBeNull();
	});
	it('returns Map preserving entries', () => {
		const m = safeMetadata({foo: 'bar', baz: 'qux'});
		expect(m).toBeInstanceOf(Map);
		expect(m!.get('foo')).toBe('bar');
		expect(m!.get('baz')).toBe('qux');
		expect(m!.size).toBe(2);
	});
});

describe('computeStripeUpdatedAt', () => {
	it('returns a Date', () => {
		const d = computeStripeUpdatedAt({created: NOW_UNIX});
		expect(d).toBeInstanceOf(Date);
		expect(d.getTime()).toBe(NOW_UNIX * 1000);
	});
	it('prefers status_transitions over created', () => {
		const later = NOW_UNIX + 5000;
		const d = computeStripeUpdatedAt({created: NOW_UNIX, status_transitions: {paid_at: later}});
		expect(d.getTime()).toBe(later * 1000);
	});
	it('prefers explicit terminal timestamps over created', () => {
		const later = NOW_UNIX + 9999;
		const d = computeStripeUpdatedAt({created: NOW_UNIX, canceled_at: later});
		expect(d.getTime()).toBe(later * 1000);
	});
	it('falls back to Date.now() when nothing is present', () => {
		const before = Date.now();
		const d = computeStripeUpdatedAt({});
		const after = Date.now();
		expect(d.getTime()).toBeGreaterThanOrEqual(before);
		expect(d.getTime()).toBeLessThanOrEqual(after);
	});
});

describe('mapStripeCustomerToRow', () => {
	it('maps a normal customer with email', () => {
		const c = stripeFixture<Stripe.Customer>({
			id: 'cus_normal',
			email: 'a@b.com',
			name: 'Alice',
			description: null,
			phone: null,
			invoice_settings: {default_payment_method: 'pm_123'},
			invoice_prefix: 'PFX',
			currency: 'usd',
			delinquent: false,
			balance: -500,
			livemode: true,
			metadata: {},
			created: NOW_UNIX,
		});
		const {primary, byUserId} = mapStripeCustomerToRow(c);
		expect(primary.provider_id).toBe('cus_normal');
		expect(primary.email).toBe('a@b.com');
		expect(primary.default_payment_method).toBe('pm_123');
		expect(primary.balance).toBe(-500n);
		expect(primary.deleted).toBe(false);
		expect(primary.stripe_created_at?.getTime()).toBe(NOW_UNIX * 1000);
		expect(primary.version).toBeNull();
		expect(byUserId).toBeNull();
	});
	it('maps a deleted customer with deleted=true', () => {
		const c = stripeFixture<Stripe.DeletedCustomer>({
			id: 'cus_gone',
			deleted: true,
		});
		const {primary, byUserId} = mapStripeCustomerToRow(c);
		expect(primary.deleted).toBe(true);
		expect(primary.email).toBeNull();
		expect(byUserId).toBeNull();
	});
	it('produces non-null byUserId when knownUserId hint is provided', () => {
		const c = stripeFixture<Stripe.Customer>({
			id: 'cus_user',
			email: 'u@b.com',
			invoice_settings: {default_payment_method: null},
			created: NOW_UNIX,
			livemode: false,
			metadata: {},
		});
		const userId = 12345n;
		const {primary, byUserId} = mapStripeCustomerToRow(c, {knownUserId: userId});
		expect(primary.user_id).toBe(userId);
		expect(byUserId).not.toBeNull();
		expect(byUserId!.user_id).toBe(userId);
		expect(byUserId!.provider_id).toBe('cus_user');
		expect(byUserId!.livemode).toBe(false);
	});
	it('expanded default_payment_method object is reduced to id', () => {
		const c = stripeFixture<Stripe.Customer>({
			id: 'cus_exp',
			invoice_settings: {default_payment_method: {id: 'pm_exp'}},
			created: NOW_UNIX,
			metadata: {},
		});
		const {primary} = mapStripeCustomerToRow(c);
		expect(primary.default_payment_method).toBe('pm_exp');
	});
});

describe('mapStripeProductToRow', () => {
	it('smoke maps minimal product', () => {
		const p = stripeFixture<Stripe.Product>({
			id: 'prod_x',
			name: 'Test',
			description: null,
			active: true,
			livemode: true,
			metadata: {},
			images: ['a', 'b'],
			created: NOW_UNIX,
		});
		const row = mapStripeProductToRow(p);
		expect(row.provider_id).toBe('prod_x');
		expect(row.name).toBe('Test');
		expect(row.images).toEqual(['a', 'b']);
		expect(row.version).toBeNull();
	});
});

describe('mapStripePriceToRow', () => {
	it('smoke maps recurring price', () => {
		const p = stripeFixture<Stripe.Price>({
			id: 'price_x',
			product: 'prod_x',
			active: true,
			currency: 'usd',
			unit_amount: 999,
			billing_scheme: 'per_unit',
			type: 'recurring',
			recurring: {interval: 'month', interval_count: 1, usage_type: 'licensed'},
			livemode: true,
			lookup_key: null,
			metadata: {},
			tax_behavior: 'inclusive',
			created: NOW_UNIX,
		});
		const row = mapStripePriceToRow(p);
		expect(row.provider_id).toBe('price_x');
		expect(row.product_id).toBe('prod_x');
		expect(row.unit_amount).toBe(999n);
		expect(row.interval).toBe('month');
		expect(row.interval_count).toBe(1);
	});
});

describe('mapStripePaymentMethodToRow', () => {
	it('maps card type with apple_pay wallet', () => {
		const pm = stripeFixture<Stripe.PaymentMethod>({
			id: 'pm_card',
			customer: 'cus_x',
			type: 'card',
			card: {
				brand: 'visa',
				last4: '4242',
				exp_month: 12,
				exp_year: 2030,
				funding: 'credit',
				country: 'US',
				fingerprint: 'fp_xx',
				wallet: {type: 'apple_pay'},
			},
			billing_details: {email: 'b@b.com', name: 'Bob', address: {country: 'US', postal_code: '94110'}},
			livemode: true,
			metadata: {},
			created: NOW_UNIX,
		});
		const {primary, byCustomer} = mapStripePaymentMethodToRow(pm, {isDefault: true});
		expect(primary.type).toBe('card');
		expect(primary.card_brand).toBe('visa');
		expect(primary.card_last4).toBe('4242');
		expect(primary.card_wallet_type).toBe('apple_pay');
		expect(primary.is_default).toBe(true);
		expect(byCustomer).not.toBeNull();
		expect(byCustomer!.customer_id).toBe('cus_x');
		expect(byCustomer!.is_default).toBe(true);
	});
});

describe('mapStripeSubscriptionToRow', () => {
	function buildSub(overrides: Partial<StripeSubscriptionPayload> = {}): StripeSubscriptionPayload {
		return {
			id: 'sub_1',
			customer: 'cus_1',
			status: 'active',
			cancel_at_period_end: false,
			cancel_at: null,
			canceled_at: null,
			cancellation_details: null,
			trial_start: null,
			trial_end: null,
			items: {
				data: [
					{
						id: 'si_1',
						quantity: 2,
						price: {id: 'price_1', product: 'prod_1', unit_amount: 999},
						current_period_start: NOW_UNIX,
						current_period_end: NOW_UNIX + 30 * 86400,
					},
				],
			},
			default_payment_method: null,
			latest_invoice: null,
			collection_method: 'charge_automatically',
			currency: 'usd',
			livemode: true,
			metadata: {},
			created: NOW_UNIX,
			...overrides,
		};
	}
	it('maps an active subscription with one item', () => {
		const {primary, byCustomer, byUser} = mapStripeSubscriptionToRow(buildSub());
		expect(primary.provider_id).toBe('sub_1');
		expect(primary.primary_price_id).toBe('price_1');
		expect(primary.primary_product_id).toBe('prod_1');
		expect(primary.quantity).toBe(2);
		expect(primary.item_count).toBe(1);
		expect(primary.items?.length).toBe(1);
		expect(primary.items![0]).toEqual(['si_1', 'price_1', 2, 999n]);
		expectTupleGetValue(primary.items![0], 3, 999n);
		expect(primary.current_period_end?.getTime()).toBe((NOW_UNIX + 30 * 86400) * 1000);
		expect(byCustomer.provider_id).toBe('sub_1');
		expect(byUser).toBeNull();
	});
	it('normalizes Cassandra tuple subscription items back to array values', () => {
		const {primary} = mapStripeSubscriptionToRow(buildSub());
		const tupleLike = {
			length: 4,
			get(index: number): unknown {
				return ['si_1', 'price_1', 2, 999n][index];
			},
		};
		const normalized = normalizeBillingSubscriptionRow({
			...primary,
			items: [tupleLike],
		});
		expect(normalized.items![0]).toEqual(['si_1', 'price_1', 2, 999n]);
		expectTupleGetValue(normalized.items![0], 3, 999n);
	});
	it('maps cancel_at_period_end=true', () => {
		const cancelAt = NOW_UNIX + 60 * 86400;
		const {primary} = mapStripeSubscriptionToRow(buildSub({cancel_at_period_end: true, cancel_at: cancelAt}));
		expect(primary.cancel_at_period_end).toBe(true);
		expect(primary.cancel_at?.getTime()).toBe(cancelAt * 1000);
	});
	it('maps cancellation_details reason+comment', () => {
		const {primary} = mapStripeSubscriptionToRow(
			buildSub({
				canceled_at: NOW_UNIX + 100,
				cancellation_details: {reason: 'cancellation_requested', comment: 'too_expensive'},
			}),
		);
		expect(primary.cancellation_reason).toBe('cancellation_requested');
		expect(primary.cancellation_comment).toBe('too_expensive');
	});
	it('emits byUser when knownUserId hint provided', () => {
		const {byUser} = mapStripeSubscriptionToRow(buildSub(), {knownUserId: 42n});
		expect(byUser).not.toBeNull();
		expect(byUser!.user_id).toBe(42n);
		expect(byUser!.customer_id).toBe('cus_1');
	});
});

describe('mapStripeInvoiceToRow', () => {
	it('maps a paid invoice with two payments', () => {
		const inv = stripeFixture<Stripe.Invoice>({
			id: 'in_1',
			customer: 'cus_1',
			parent: {type: 'subscription_details', subscription_details: {subscription: 'sub_1'}},
			status: 'paid',
			number: 'INV-001',
			currency: 'usd',
			amount_due: 1000,
			amount_paid: 1000,
			amount_remaining: 0,
			subtotal: 1000,
			tax: 0,
			total: 1000,
			starting_balance: 0,
			ending_balance: 0,
			application_fee_amount: null,
			attempt_count: 1,
			attempted: true,
			auto_advance: false,
			billing_reason: 'subscription_cycle',
			collection_method: 'charge_automatically',
			description: null,
			hosted_invoice_url: 'https://x',
			invoice_pdf: 'https://x.pdf',
			period_start: NOW_UNIX,
			period_end: NOW_UNIX + 86400,
			due_date: null,
			next_payment_attempt: null,
			status_transitions: {finalized_at: NOW_UNIX + 1, paid_at: NOW_UNIX + 2, voided_at: null},
			payments: {
				data: [
					{
						id: 'inpay_1',
						status: 'paid',
						is_default: true,
						amount_paid: 1000,
						amount_requested: 1000,
						currency: 'usd',
						created: NOW_UNIX + 1,
						status_transitions: {paid_at: NOW_UNIX + 2},
						payment: {payment_intent: 'pi_1', charge: 'ch_1'},
					},
					{
						id: 'inpay_2',
						status: 'canceled',
						is_default: false,
						amount_paid: 0,
						amount_requested: 1000,
						currency: 'usd',
						created: NOW_UNIX,
						payment: {payment_intent: {id: 'pi_2'}, charge: {id: 'ch_2'}},
					},
				],
			},
			livemode: true,
			metadata: {},
			created: NOW_UNIX,
		});
		const result = mapStripeInvoiceToRow(inv);
		expect(result.primary.provider_id).toBe('in_1');
		expect(result.primary.subscription_id).toBe('sub_1');
		expect(result.primary.payment_ids).toEqual(['inpay_1', 'inpay_2']);
		expect(result.primary.paid_at?.getTime()).toBe((NOW_UNIX + 2) * 1000);
		expect(result.payments.length).toBe(2);
		expect(result.payments[0]!.primary.provider_id).toBe('inpay_1');
		expect(result.payments[0]!.primary.payment_intent_id).toBe('pi_1');
		expect(result.payments[0]!.primary.charge_id).toBe('ch_1');
		expect(result.payments[0]!.primary.is_default).toBe(true);
		expect(result.payments[1]!.primary.payment_intent_id).toBe('pi_2');
		expect(result.payments[1]!.primary.charge_id).toBe('ch_2');
		expect(result.bySubscription).not.toBeNull();
		expect(result.bySubscription!.subscription_id).toBe('sub_1');
	});
});

describe('mapStripePaymentIntentToRow', () => {
	it('maps a succeeded PI with customer', () => {
		const pi = stripeFixture<Stripe.PaymentIntent>({
			id: 'pi_x',
			customer: 'cus_1',
			invoice: null,
			status: 'succeeded',
			amount: 2000,
			amount_received: 2000,
			amount_capturable: 0,
			currency: 'usd',
			capture_method: 'automatic',
			confirmation_method: 'automatic',
			payment_method: 'pm_1',
			payment_method_types: ['card'],
			setup_future_usage: null,
			canceled_at: null,
			cancellation_reason: null,
			latest_charge: 'ch_1',
			last_payment_error: null,
			livemode: true,
			metadata: {},
			created: NOW_UNIX,
		});
		const {primary, byCustomer} = mapStripePaymentIntentToRow(pi);
		expect(primary.provider_id).toBe('pi_x');
		expect(primary.customer_id).toBe('cus_1');
		expect(primary.last_charge_id).toBe('ch_1');
		expect(byCustomer).not.toBeNull();
		expect(byCustomer!.customer_id).toBe('cus_1');
	});
	it('byCustomer is null when customer is null', () => {
		const pi = stripeFixture<Stripe.PaymentIntent>({
			id: 'pi_no_cust',
			customer: null,
			invoice: null,
			status: 'requires_payment_method',
			amount: 100,
			currency: 'usd',
			payment_method_types: ['card'],
			created: NOW_UNIX,
			metadata: {},
		});
		const {byCustomer} = mapStripePaymentIntentToRow(pi);
		expect(byCustomer).toBeNull();
	});
});

describe('mapStripeChargeToRow', () => {
	it('maps a card charge with brand/last4', () => {
		const c = stripeFixture<Stripe.Charge>({
			id: 'ch_1',
			customer: 'cus_1',
			payment_intent: 'pi_1',
			invoice: 'in_1',
			status: 'succeeded',
			amount: 1500,
			amount_captured: 1500,
			amount_refunded: 0,
			currency: 'usd',
			captured: true,
			paid: true,
			refunded: false,
			disputed: false,
			payment_method: 'pm_1',
			payment_method_details: {type: 'card', card: {brand: 'mastercard', last4: '5555', country: 'US'}},
			billing_details: {address: {country: 'US'}},
			receipt_url: 'https://r',
			outcome: {type: 'authorized', risk_level: 'normal', seller_message: 'Approved'},
			livemode: true,
			metadata: {},
			created: NOW_UNIX,
		});
		const {primary, byCustomer} = mapStripeChargeToRow(c);
		expect(primary.card_brand).toBe('mastercard');
		expect(primary.card_last4).toBe('5555');
		expect(primary.card_country).toBe('US');
		expect(byCustomer).not.toBeNull();
	});
});

describe('mapStripeRefundToRow', () => {
	it('payment_intent is a string', () => {
		const r = stripeFixture<Stripe.Refund>({
			id: 're_1',
			charge: 'ch_1',
			payment_intent: 'pi_1',
			status: 'succeeded',
			amount: 500,
			currency: 'usd',
			reason: 'requested_by_customer',
			receipt_number: null,
			failure_reason: null,
			livemode: true,
			metadata: {},
			created: NOW_UNIX,
		});
		const result = mapStripeRefundToRow(r);
		expect(result.primary.payment_intent_id).toBe('pi_1');
		expect(result.primary.charge_id).toBe('ch_1');
		expect(result.byCharge).not.toBeNull();
		expect(result.byPaymentIntent).not.toBeNull();
		expect(result.byInvoice).toBeNull();
	});
	it('payment_intent is an expanded object; hints carry through', () => {
		const r = stripeFixture<Stripe.Refund>({
			id: 're_2',
			charge: null,
			payment_intent: {id: 'pi_2'},
			status: 'succeeded',
			amount: 250,
			currency: 'usd',
			created: NOW_UNIX,
			metadata: {},
			livemode: true,
		});
		const result = mapStripeRefundToRow(r, {invoiceId: 'in_z', customerId: 'cus_z', userId: 7n});
		expect(result.primary.payment_intent_id).toBe('pi_2');
		expect(result.primary.charge_id).toBeNull();
		expect(result.primary.invoice_id).toBe('in_z');
		expect(result.primary.customer_id).toBe('cus_z');
		expect(result.primary.user_id).toBe(7n);
		expect(result.byInvoice).not.toBeNull();
		expect(result.byInvoice!.invoice_id).toBe('in_z');
		expect(result.byCharge).toBeNull();
	});
});

describe('mapStripeCheckoutSessionToRow', () => {
	it('subscription-mode with subscription as string', () => {
		const cs = stripeFixture<Stripe.Checkout.Session>({
			id: 'cs_sub',
			customer: 'cus_1',
			mode: 'subscription',
			status: 'complete',
			payment_status: 'paid',
			subscription: 'sub_1',
			payment_intent: null,
			setup_intent: null,
			invoice: 'in_1',
			amount_subtotal: 999,
			amount_total: 999,
			currency: 'usd',
			expires_at: NOW_UNIX + 3600,
			livemode: true,
			metadata: {},
			created: NOW_UNIX,
		});
		const {primary, byCustomer} = mapStripeCheckoutSessionToRow(cs);
		expect(primary.subscription_id).toBe('sub_1');
		expect(primary.invoice_id).toBe('in_1');
		expect(primary.payment_intent_id).toBeNull();
		expect(byCustomer).not.toBeNull();
	});
	it('payment-mode with payment_intent as object', () => {
		const cs = stripeFixture<Stripe.Checkout.Session>({
			id: 'cs_pay',
			customer: null,
			mode: 'payment',
			status: 'complete',
			payment_status: 'paid',
			subscription: null,
			payment_intent: {id: 'pi_pay'},
			setup_intent: null,
			invoice: null,
			amount_total: 5000,
			currency: 'usd',
			created: NOW_UNIX,
			metadata: {},
			livemode: true,
		});
		const {primary, byCustomer} = mapStripeCheckoutSessionToRow(cs);
		expect(primary.payment_intent_id).toBe('pi_pay');
		expect(byCustomer).toBeNull();
	});
});

describe('mapStripeDisputeToRow', () => {
	it('needs_response status', () => {
		const d = stripeFixture<Stripe.Dispute>({
			id: 'dp_1',
			charge: 'ch_1',
			payment_intent: 'pi_1',
			status: 'needs_response',
			reason: 'fraudulent',
			amount: 1000,
			currency: 'usd',
			is_charge_refundable: true,
			evidence_details: {due_by: NOW_UNIX + 7 * 86400, submission_count: 0},
			livemode: true,
			metadata: {},
			created: NOW_UNIX,
		});
		const {primary, byCharge} = mapStripeDisputeToRow(d, {customerId: 'cus_1', userId: 99n});
		expect(primary.status).toBe('needs_response');
		expect(primary.charge_id).toBe('ch_1');
		expect(primary.customer_id).toBe('cus_1');
		expect(primary.user_id).toBe(99n);
		expect(primary.evidence_due_by?.getTime()).toBe((NOW_UNIX + 7 * 86400) * 1000);
		expect(byCharge).not.toBeNull();
		expect(byCharge!.charge_id).toBe('ch_1');
	});
});

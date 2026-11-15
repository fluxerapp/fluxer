// SPDX-License-Identifier: AGPL-3.0-or-later

import {setDistributionPricingConfig} from '@app/api/stripe/DistributionPricing';
import {distributionRefundMetadata, tagInvoiceDistribution} from '@app/api/stripe/DistributionStripeMetadata';
import type Stripe from 'stripe';
import {afterEach, beforeEach, expect, test} from 'vitest';

const CHANNEL_KEYS = {
	distribution: 'ios_marketplace',
	distribution_rate_bps: '500',
	distribution_rule_version: '1',
	distribution_base_unit_amount: '499',
	distribution_unit_amount: '526',
	distribution_extra_unit_amount: '27',
};

beforeEach(() => {
	setDistributionPricingConfig({
		rules: [
			{
				id: 'ios_marketplace',
				active: true,
				rate_bps: 500,
				products: ['plutonium_subscription'],
				rule_version: 1,
				prices: {'1:price_base': {price_id: 'price_ch', rate_bps: 500, base_unit_amount: 499, unit_amount: 526}},
			},
		],
	});
});

afterEach(() => {
	setDistributionPricingConfig(null);
});

test('refunds carry the distribution keys of the payment they reverse', async () => {
	const stripe = {
		paymentIntents: {retrieve: async () => ({metadata: {user_id: '1', ...CHANNEL_KEYS}})},
		charges: {retrieve: async () => ({metadata: {}, payment_intent: 'pi_1'})},
	} as unknown as Stripe;
	expect(await distributionRefundMetadata(stripe, {payment_intent: 'pi_1'})).toEqual(CHANNEL_KEYS);
	expect(await distributionRefundMetadata(stripe, {charge: 'ch_1'})).toEqual(CHANNEL_KEYS);
});

test('a refund still goes through when the payment cannot be read', async () => {
	const stripe = {
		paymentIntents: {
			retrieve: async () => {
				throw new Error('unavailable');
			},
		},
	} as unknown as Stripe;
	expect(await distributionRefundMetadata(stripe, {payment_intent: 'pi_1'})).toEqual({});
});

test('a renewal invoice on a channel price tags its payment intent and charge', async () => {
	const updates: Array<[string, Record<string, string>]> = [];
	const stripe = {
		invoicePayments: {list: async () => ({data: [{payment: {payment_intent: 'pi_renew'}}]})},
		paymentIntents: {
			update: async (id: string, params: {metadata: Record<string, string>}) => {
				updates.push([id, params.metadata]);
				return {latest_charge: 'ch_renew'};
			},
		},
		charges: {
			update: async (id: string, params: {metadata: Record<string, string>}) => {
				updates.push([id, params.metadata]);
				return {};
			},
		},
	} as unknown as Stripe;
	const invoice = {
		id: 'in_renew',
		billing_reason: 'subscription_cycle',
		lines: {data: [{amount: 526, pricing: {price_details: {price: 'price_ch'}}}]},
	} as unknown as Stripe.Invoice;
	await tagInvoiceDistribution(stripe, invoice);
	expect(updates).toEqual([
		['pi_renew', CHANNEL_KEYS],
		['ch_renew', CHANNEL_KEYS],
	]);
	updates.length = 0;
	const standard = {...invoice, lines: {data: [{amount: 499, pricing: {price_details: {price: 'price_base'}}}]}};
	await tagInvoiceDistribution(stripe, standard as unknown as Stripe.Invoice);
	expect(updates).toEqual([]);
});

test('a plan change off the channel price is not tagged from the credit for unused time', async () => {
	const updates: Array<string> = [];
	const stripe = {
		invoicePayments: {list: async () => ({data: [{payment: {payment_intent: 'pi_change'}}]})},
		paymentIntents: {
			update: async (id: string) => {
				updates.push(id);
				return {latest_charge: null};
			},
		},
	} as unknown as Stripe;
	const invoice = {
		id: 'in_change',
		billing_reason: 'subscription_update',
		lines: {
			data: [
				{amount: -300, pricing: {price_details: {price: 'price_ch'}}},
				{amount: 4999, pricing: {price_details: {price: 'price_base_yearly'}}},
			],
		},
	} as unknown as Stripe.Invoice;
	await tagInvoiceDistribution(stripe, invoice);
	expect(updates).toEqual([]);
});

test('a failed tag fails the webhook so Stripe sends it again', async () => {
	const stripe = {
		invoicePayments: {
			list: async () => {
				throw new Error('unavailable');
			},
		},
	} as unknown as Stripe;
	const invoice = {
		id: 'in_renew',
		billing_reason: 'subscription_cycle',
		lines: {data: [{amount: 526, pricing: {price_details: {price: 'price_ch'}}}]},
	} as unknown as Stripe.Invoice;
	await expect(tagInvoiceDistribution(stripe, invoice)).rejects.toThrow('unavailable');
});

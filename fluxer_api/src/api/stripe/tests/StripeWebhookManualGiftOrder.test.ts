// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, listTestEmails, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {GiftCode} from '@app/api/models/GiftCode';
import {MANUAL_GIFT_ORDER_PRODUCT_TYPE} from '@app/api/stripe/services/StripeManualGiftOrderHandler';
import {
	mockStripeWebhookSecret,
	restoreStripeWebhookSecret,
	sendStripeWebhook,
	setupSyncStripeWebhookWorker,
} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	createCheckoutCompletedEvent,
	createStripeApiHandlers,
	type StripeApiHandlers,
} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const MONTH_PRICE = 'price_manual_gift_1_month_usd';
const YEAR_PRICE = 'price_manual_gift_1_year_usd';
const MONTH_UNIT = 499;
const YEAR_UNIT = 4999;

interface OrderLine {
	price: string;
	quantity: number;
	unit: number;
	lookupKey?: string;
}

const ORDER: Array<OrderLine> = [
	{price: MONTH_PRICE, quantity: 3, unit: MONTH_UNIT},
	{price: YEAR_PRICE, quantity: 2, unit: YEAR_UNIT},
];

function orderTotal(lines: Array<OrderLine>): number {
	return lines.reduce((sum, line) => sum + line.quantity * line.unit, 0);
}

describe('Stripe webhook manual gift orders', () => {
	let harness: ApiTestHarness;
	let stripe: StripeApiHandlers;
	let originalPrices: typeof Config.stripe.prices;
	const userRepository = new UserRepository();

	beforeAll(async () => {
		harness = await createApiTestHarness();
		mockStripeWebhookSecret('whsec_test_manual_gift');
		setupSyncStripeWebhookWorker();
		originalPrices = Config.stripe.prices;
	});
	afterAll(async () => {
		await harness.shutdown();
		restoreStripeWebhookSecret();
		Config.stripe.prices = originalPrices;
	});
	beforeEach(async () => {
		await harness.resetData();
		Config.stripe.prices = {...originalPrices, gift1MonthUsd: MONTH_PRICE, gift1YearUsd: YEAR_PRICE};
		stripe = createStripeApiHandlers();
		server.use(...stripe.handlers);
	});
	afterEach(() => {
		server.resetHandlers();
	});

	function idOf(account: TestAccount): UserID {
		return createUserID(BigInt(account.userId));
	}

	function useLineItems(lines: Array<OrderLine>): void {
		server.use(
			http.get('https://api.stripe.com/v1/checkout/sessions/:id/line_items', () =>
				HttpResponse.json({
					object: 'list',
					has_more: false,
					url: '/v1/checkout/sessions/line_items',
					data: lines.map((line, index) => ({
						id: `li_${index}`,
						object: 'item',
						description: line.price,
						currency: 'usd',
						quantity: line.quantity,
						amount_subtotal: line.quantity * line.unit,
						amount_total: line.quantity * line.unit,
						amount_tax: 0,
						amount_discount: 0,
						price: {
							id: line.price,
							object: 'price',
							type: 'one_time',
							currency: 'usd',
							unit_amount: line.unit,
							lookup_key: line.lookupKey ?? null,
						},
					})),
				}),
			),
		);
	}

	function orderEvent(
		buyer: TestAccount,
		paymentIntentId: string,
		options: {
			type?: 'checkout.session.completed' | 'checkout.session.async_payment_succeeded';
			paymentStatus?: 'paid' | 'unpaid';
			marker?: boolean;
			lines?: Array<OrderLine>;
		} = {},
	) {
		return createCheckoutCompletedEvent({
			type: options.type,
			sessionId: `cs_${paymentIntentId}`,
			paymentIntentId,
			mode: 'payment',
			paymentStatus: options.paymentStatus,
			amountTotal: orderTotal(options.lines ?? ORDER),
			currency: 'usd',
			metadata: options.marker === false ? {} : {fluxer_manual_gift_order: 'true', user_id: buyer.userId},
		});
	}

	async function placeOrder(
		buyer: TestAccount,
		paymentIntentId: string,
		lines: Array<OrderLine> = ORDER,
	): Promise<Array<GiftCode>> {
		useLineItems(lines);
		await sendStripeWebhook(harness, orderEvent(buyer, paymentIntentId, {lines}));
		return userRepository.findGiftCodesByPaymentIntent(paymentIntentId);
	}

	async function redeem(code: GiftCode, recipient: TestAccount): Promise<void> {
		await userRepository.redeemGiftCode(code.code, idOf(recipient));
		await createBuilder(harness, recipient.token)
			.post(`/test/users/${recipient.userId}/premium`)
			.body({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: new Date(Date.now() + 30 * MS_PER_DAY).toISOString(),
			})
			.execute();
	}

	async function refund(paymentIntentId: string, amountRefunded: number, refunded: boolean): Promise<void> {
		await sendStripeWebhook(harness, {
			type: 'charge.refunded',
			data: {
				object: {
					id: `ch_${paymentIntentId}`,
					payment_intent: paymentIntentId,
					amount: orderTotal(ORDER),
					amount_refunded: amountRefunded,
					refunded,
				},
			},
		});
	}

	async function sendDispute(
		type: 'charge.dispute.created' | 'charge.dispute.closed',
		paymentIntentId: string,
		status: string,
	) {
		await sendStripeWebhook(harness, {
			type,
			data: {
				object: {
					id: `dp_${paymentIntentId}`,
					object: 'dispute',
					amount: orderTotal(ORDER),
					currency: 'usd',
					charge: `ch_${paymentIntentId}`,
					created: Math.floor((Date.now() + 20 * MS_PER_DAY) / 1000),
					evidence: {},
					evidence_details: {has_evidence: false, due_by: Math.floor((Date.now() + 10 * MS_PER_DAY) / 1000)},
					payment_intent: paymentIntentId,
					reason: 'general',
					status,
					balance_transactions: [],
				},
			},
		});
	}

	async function liveCodes(paymentIntentId: string): Promise<Array<GiftCode>> {
		return (await userRepository.findGiftCodesByPaymentIntent(paymentIntentId)).filter((code) => !code.revokedAt);
	}

	function countByMonths(codes: Array<GiftCode>): Record<number, number> {
		const counts: Record<number, number> = {};
		for (const code of codes) {
			counts[code.durationMonths ?? 0] = (counts[code.durationMonths ?? 0] ?? 0) + 1;
		}
		return counts;
	}

	test('a paid order creates one code per unit in the buyer gift inventory', async () => {
		const buyer = await createTestAccount(harness);
		const before = (await userRepository.findUnique(idOf(buyer)))?.giftInventoryServerSeq ?? 0;
		stripe.put.checkoutSession({
			id: 'cs_pi_manual_happy',
			mode: 'payment',
			subscription: null,
			payment_intent: 'pi_manual_happy',
			amount_total: orderTotal(ORDER),
			metadata: {fluxer_manual_gift_order: 'true', user_id: buyer.userId},
		});
		const codes = await placeOrder(buyer, 'pi_manual_happy');
		expect(countByMonths(codes)).toEqual({1: 3, 12: 2});
		for (const code of codes) {
			expect(code.createdByUserId).toBe(idOf(buyer));
			expect(code.checkoutSessionId).toBe('cs_pi_manual_happy');
		}
		const inventory = await createBuilder<Array<{code: string}>>(harness, buyer.token)
			.get('/users/@me/gifts')
			.execute();
		expect(inventory.map((gift) => gift.code).sort()).toEqual(codes.map((code) => code.code).sort());
		const user = await userRepository.findUnique(idOf(buyer));
		expect(user?.giftInventoryServerSeq).toBe(before + 1);
		expect(user?.hasEverPurchased).toBe(true);
		const payment = await userRepository.getPaymentByCheckoutSession('cs_pi_manual_happy');
		expect(payment).toMatchObject({
			status: 'completed',
			isGift: true,
			productType: MANUAL_GIFT_ORDER_PRODUCT_TYPE,
			paymentIntentId: 'pi_manual_happy',
			amountCents: orderTotal(ORDER),
		});
		const receipts = (await listTestEmails(harness, {recipient: buyer.email})).filter(
			(email) => email.type === 'plutonium_payment_receipt',
		);
		expect(receipts).toHaveLength(1);
	});

	test('a replayed webhook creates no extra codes and an interrupted order is completed', async () => {
		const buyer = await createTestAccount(harness);
		await userRepository.createGiftCode({
			code: 'MANUAL_PARTIAL_1',
			duration_months: null,
			duration_type: 'months',
			duration_quantity: 1,
			created_at: new Date(),
			created_by_user_id: idOf(buyer),
			redeemed_at: null,
			redeemed_by_user_id: null,
			stripe_payment_intent_id: 'pi_manual_replay',
			visionary_sequence_number: null,
			checkout_session_id: 'cs_pi_manual_replay',
			version: 1,
		});
		await placeOrder(buyer, 'pi_manual_replay');
		const codes = await placeOrder(buyer, 'pi_manual_replay');
		expect(countByMonths(codes)).toEqual({1: 3, 12: 2});
		expect(codes.map((code) => code.code)).toContain('MANUAL_PARTIAL_1');
	});

	test('an order with a price that is not a gift price creates nothing', async () => {
		const buyer = await createTestAccount(harness);
		const codes = await placeOrder(buyer, 'pi_manual_unknown', [
			{price: MONTH_PRICE, quantity: 2, unit: MONTH_UNIT},
			{price: 'price_unknown_thing', quantity: 1, unit: 1000},
		]);
		expect(codes).toHaveLength(0);
		expect(await userRepository.getPaymentByCheckoutSession('cs_pi_manual_unknown')).toBeNull();
	});

	test('gift prices outside the billing config are recognised by their lookup key', async () => {
		const buyer = await createTestAccount(harness);
		const codes = await placeOrder(buyer, 'pi_manual_lookup', [
			{price: 'price_catalog_month', quantity: 2, unit: MONTH_UNIT, lookupKey: 'gift_1_month_usd'},
			{price: 'price_catalog_year', quantity: 1, unit: YEAR_UNIT, lookupKey: 'gift_1_year_usd'},
		]);
		expect(countByMonths(codes)).toEqual({1: 2, 12: 1});
	});

	test('a lookup key that is not a gift slot creates nothing', async () => {
		const buyer = await createTestAccount(harness);
		const codes = await placeOrder(buyer, 'pi_manual_lookup_monthly', [
			{price: 'price_catalog_sub', quantity: 1, unit: MONTH_UNIT, lookupKey: 'monthly_usd'},
		]);
		expect(codes).toHaveLength(0);
	});

	test('an order above 200 codes creates nothing', async () => {
		const buyer = await createTestAccount(harness);
		const codes = await placeOrder(buyer, 'pi_manual_cap', [
			{price: MONTH_PRICE, quantity: 150, unit: MONTH_UNIT},
			{price: YEAR_PRICE, quantity: 51, unit: YEAR_UNIT},
		]);
		expect(codes).toHaveLength(0);
		expect(await userRepository.getPaymentByCheckoutSession('cs_pi_manual_cap')).toBeNull();
	});

	test('a session without the marker is still skipped', async () => {
		const buyer = await createTestAccount(harness);
		useLineItems(ORDER);
		await sendStripeWebhook(harness, orderEvent(buyer, 'pi_manual_no_marker', {marker: false}));
		expect(await userRepository.findGiftCodesByPaymentIntent('pi_manual_no_marker')).toHaveLength(0);
		expect(await userRepository.getPaymentByCheckoutSession('cs_pi_manual_no_marker')).toBeNull();
	});

	test('an unpaid order waits for the async payment to succeed', async () => {
		const buyer = await createTestAccount(harness);
		useLineItems(ORDER);
		await sendStripeWebhook(harness, orderEvent(buyer, 'pi_manual_async', {paymentStatus: 'unpaid'}));
		expect(await userRepository.findGiftCodesByPaymentIntent('pi_manual_async')).toHaveLength(0);
		await sendStripeWebhook(
			harness,
			orderEvent(buyer, 'pi_manual_async', {type: 'checkout.session.async_payment_succeeded'}),
		);
		expect(countByMonths(await userRepository.findGiftCodesByPaymentIntent('pi_manual_async'))).toEqual({1: 3, 12: 2});
	});

	test('a full refund takes back every code of the order', async () => {
		const buyer = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		const codes = await placeOrder(buyer, 'pi_manual_full_refund');
		await redeem(codes[0], recipient);
		await refund('pi_manual_full_refund', orderTotal(ORDER), true);
		const after = await userRepository.findGiftCodesByPaymentIntent('pi_manual_full_refund');
		expect(after.filter((code) => !code.redeemedByUserId && !code.revokedAt)).toHaveLength(0);
		expect((await userRepository.findUnique(idOf(recipient)))?.premiumType ?? UserPremiumTypes.NONE).toBe(
			UserPremiumTypes.NONE,
		);
	});

	test('a partial refund takes back unredeemed codes worth the refunded amount', async () => {
		const buyer = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		const codes = await placeOrder(buyer, 'pi_manual_partial');
		const redeemed = codes.find((code) => code.durationMonths === 1)!;
		await redeem(redeemed, recipient);
		await refund('pi_manual_partial', 2 * MONTH_UNIT, false);
		expect(countByMonths(await liveCodes('pi_manual_partial'))).toEqual({1: 1, 12: 2});
		expect((await liveCodes('pi_manual_partial')).map((code) => code.code)).toContain(redeemed.code);
		expect((await userRepository.findUnique(idOf(recipient)))?.premiumType).toBe(UserPremiumTypes.SUBSCRIPTION);
		await refund('pi_manual_partial', 2 * MONTH_UNIT + YEAR_UNIT, false);
		expect(countByMonths(await liveCodes('pi_manual_partial'))).toEqual({1: 1, 12: 1});
		await refund('pi_manual_partial', 2 * MONTH_UNIT + 2 * YEAR_UNIT + MONTH_UNIT, false);
		expect((await userRepository.findUnique(idOf(recipient)))?.premiumType ?? UserPremiumTypes.NONE).toBe(
			UserPremiumTypes.NONE,
		);
	});

	test('a lost dispute takes back every code of the order', async () => {
		const buyer = await createTestAccount(harness);
		const codes = await placeOrder(buyer, 'pi_manual_dispute');
		expect(codes).toHaveLength(5);
		await sendDispute('charge.dispute.created', 'pi_manual_dispute', 'needs_response');
		expect(await liveCodes('pi_manual_dispute')).toHaveLength(5);
		await sendDispute('charge.dispute.closed', 'pi_manual_dispute', 'lost');
		expect(await liveCodes('pi_manual_dispute')).toHaveLength(0);
	});
});

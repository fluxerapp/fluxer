// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {getBillingRepository} from '../../middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {
	createInvoiceFinalizationFailedEvent,
	createInvoicePaidEvent,
	createInvoicePaymentActionRequiredEvent,
	createInvoicePaymentFailedEvent,
	createInvoiceUpdatedEvent,
	createMockWebhookPayload,
	createStripeApiHandlers,
	type StripeApiHandlers,
	type StripeWebhookEventData,
} from '../../test/msw/handlers/StripeApiHandlers';
import {server} from '../../test/msw/server';
import {createBuilder} from '../../test/TestRequestBuilder';
import {ProductType} from '../ProductRegistry';
import {setupSyncStripeWebhookWorker} from './StripeWebhookTestUtils';

const MOCK_PRICES = {
	monthlyUsd: 'price_monthly_usd',
	monthlyEur: 'price_monthly_eur',
	yearlyUsd: 'price_yearly_usd',
	yearlyEur: 'price_yearly_eur',
	visionaryUsd: 'price_visionary_usd',
	visionaryEur: 'price_visionary_eur',
	giftVisionaryUsd: 'price_gift_visionary_usd',
	giftVisionaryEur: 'price_gift_visionary_eur',
	gift1MonthUsd: 'price_gift_1_month_usd',
	gift1MonthEur: 'price_gift_1_month_eur',
	gift1YearUsd: 'price_gift_1_year_usd',
	gift1YearEur: 'price_gift_1_year_eur',
};

describe('Stripe Webhook - Invoice Events', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let originalWebhookSecret: string | undefined;
	let originalPrices: typeof Config.stripe.prices | undefined;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		setupSyncStripeWebhookWorker();
		originalWebhookSecret = Config.stripe.webhookSecret;
		originalPrices = Config.stripe.prices;
		Config.stripe.webhookSecret = 'whsec_test_secret';
		Config.stripe.prices = MOCK_PRICES;
		stripeHandlers = createStripeApiHandlers();
		server.use(...stripeHandlers.handlers);
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.webhookSecret = originalWebhookSecret;
		Config.stripe.prices = originalPrices;
	});
	beforeEach(async () => {
		await harness.resetData();
		stripeHandlers.reset();
		server.use(...stripeHandlers.handlers);
	});
	afterEach(() => {
		server.resetHandlers();
	});
	function createWebhookSignature(payload: string, timestamp: number, secret: string): string {
		const signedPayload = `${timestamp}.${payload}`;
		const signature = crypto.createHmac('sha256', secret).update(signedPayload).digest('hex');
		return `t=${timestamp},v1=${signature}`;
	}
	async function sendWebhook(eventData: StripeWebhookEventData): Promise<{
		received: boolean;
	}> {
		const {payload, timestamp} = createMockWebhookPayload(eventData);
		const signature = createWebhookSignature(payload, timestamp, Config.stripe.webhookSecret!);
		return createBuilder<{
			received: boolean;
		}>(harness, '')
			.post('/stripe/webhook')
			.header('stripe-signature', signature)
			.header('content-type', 'application/json')
			.body(payload)
			.execute();
	}
	async function sendWebhookExpectStripeError(eventData: StripeWebhookEventData): Promise<void> {
		const {payload, timestamp} = createMockWebhookPayload(eventData);
		const signature = createWebhookSignature(payload, timestamp, Config.stripe.webhookSecret!);
		await createBuilder(harness, '')
			.post('/stripe/webhook')
			.header('stripe-signature', signature)
			.header('content-type', 'application/json')
			.body(payload)
			.expect(400, APIErrorCodes.STRIPE_ERROR)
			.execute();
	}
	async function createPaymentRecord(params: {
		userId: string;
		subscriptionId: string;
		priceId: string;
		productType: string;
	}): Promise<void> {
		const {userId, subscriptionId, priceId, productType} = params;
		const checkoutSessionId = `cs_test_${crypto.randomUUID()}`;
		const {PaymentRepository} = await import('../../user/repositories/PaymentRepository');
		const paymentRepo = new PaymentRepository();
		await paymentRepo.createPayment({
			checkout_session_id: checkoutSessionId,
			user_id: createUserID(BigInt(userId)),
			price_id: priceId,
			product_type: productType,
			status: 'completed',
			is_gift: false,
			created_at: new Date(),
		});
		await paymentRepo.updatePayment({
			checkout_session_id: checkoutSessionId,
			subscription_id: subscriptionId,
			stripe_customer_id: `cus_test_${Date.now()}`,
			payment_intent_id: `pi_test_${Date.now()}`,
			amount_cents: 2500,
			currency: 'usd',
			completed_at: new Date(),
		});
	}
	async function setSubscriptionUserState(params: {
		accountUserId: string;
		subscriptionId: string;
		customerId: string;
		premiumUntil: Date;
		premiumWillCancel?: boolean;
	}): Promise<void> {
		const {UserRepository} = await import('../../user/repositories/UserRepository');
		const userRepository = new UserRepository();
		const userId = createUserID(BigInt(params.accountUserId));
		await userRepository.patchUpsert(
			userId,
			{
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: params.premiumUntil,
				premium_will_cancel: params.premiumWillCancel ?? false,
				stripe_subscription_id: params.subscriptionId,
				stripe_customer_id: params.customerId,
			},
			(await userRepository.findUnique(userId))!.toRow(),
		);
	}
	describe('invoice.payment_succeeded', () => {
		test('processes recurring subscription payment successfully', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_test_${Date.now()}`;
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const eventData: StripeWebhookEventData = {
				type: 'invoice.payment_succeeded',
				data: {
					object: {
						id: `in_test_${Date.now()}`,
						billing_reason: 'subscription_cycle',
						parent: {subscription_details: {subscription: subscriptionId}},
					},
				},
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const user = await createBuilder<{
				premium_type: number | null;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(user.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(user.premium_until).not.toBeNull();
			const premiumUntil = new Date(user.premium_until!);
			const now = new Date();
			const daysDiff = (premiumUntil.getTime() - now.getTime()) / (1000 * 60 * 60 * 24);
			expect(daysDiff).toBeGreaterThanOrEqual(27);
			expect(daysDiff).toBeLessThanOrEqual(31);
			const mirroredSubscription = await getBillingRepository().subscriptions.findById(subscriptionId);
			expect(mirroredSubscription?.user_id?.toString()).toBe(account.userId);
			expect(mirroredSubscription?.status).toBe('active');
			expect(mirroredSubscription?.current_period_end?.toISOString()).toBe(premiumUntil.toISOString());
		});
		test('restores original subscription start date after paid renewal recovery', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = 'sub_recovered_invoice_renewal_1';
			const driftedPremiumSince = new Date(Date.now() - 24 * 60 * 60 * 1000);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_since: driftedPremiumSince.toISOString(),
					premium_until: new Date(Date.now() - 60 * 60 * 1000).toISOString(),
					premium_will_cancel: true,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_1',
				})
				.execute();
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const eventData: StripeWebhookEventData = {
				type: 'invoice.payment_succeeded',
				data: {
					object: {
						id: `in_test_${Date.now()}`,
						billing_reason: 'subscription_cycle',
						parent: {subscription_details: {subscription: subscriptionId}},
					},
				},
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const user = await createBuilder<{
				premium_since: string | null;
				premium_type: number | null;
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(user.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(user.premium_will_cancel).toBe(false);
			expect(user.premium_since).not.toBeNull();
			expect(new Date(user.premium_since!).getTime()).toBeLessThan(driftedPremiumSince.getTime());
		});
		test('does not extend premium twice for duplicate invoice payment succeeded events with different event ids', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = 'sub_duplicate_invoice_renewal_1';
			const invoiceId = 'in_duplicate_invoice_renewal_1';
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const firstEventData: StripeWebhookEventData = {
				id: 'evt_duplicate_invoice_renewal_1_a',
				type: 'invoice.payment_succeeded',
				data: {
					object: {
						id: invoiceId,
						billing_reason: 'subscription_cycle',
						parent: {subscription_details: {subscription: subscriptionId}},
					},
				},
			};
			const firstResult = await sendWebhook(firstEventData);
			expect(firstResult.received).toBe(true);
			const userAfterFirst = await createBuilder<{
				premium_type: number | null;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(userAfterFirst.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(userAfterFirst.premium_until).not.toBeNull();
			const premiumUntilAfterFirst = new Date(userAfterFirst.premium_until!);
			const daysAfterFirst = (premiumUntilAfterFirst.getTime() - Date.now()) / (1000 * 60 * 60 * 24);
			expect(daysAfterFirst).toBeGreaterThanOrEqual(27);
			expect(daysAfterFirst).toBeLessThanOrEqual(31);
			const secondEventData: StripeWebhookEventData = {
				...firstEventData,
				id: 'evt_duplicate_invoice_renewal_1_b',
			};
			const secondResult = await sendWebhook(secondEventData);
			expect(secondResult.received).toBe(true);
			const userAfterSecond = await createBuilder<{
				premium_type: number | null;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(userAfterSecond.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(userAfterSecond.premium_until).toBe(userAfterFirst.premium_until);
		});
		test('skips first invoice for new subscription', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_test_${Date.now()}`;
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const eventData: StripeWebhookEventData = {
				type: 'invoice.payment_succeeded',
				data: {
					object: {
						id: `in_test_${Date.now()}`,
						billing_reason: 'subscription_create',
						parent: {subscription_details: {subscription: subscriptionId}},
					},
				},
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const user = await createBuilder<{
				premium_type: number;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(user.premium_type).toBe(UserPremiumTypes.NONE);
			expect(user.premium_until).toBeNull();
		});
		test('skips manual invoice payments with no subscription context', async () => {
			const eventData: StripeWebhookEventData = {
				type: 'invoice.payment_succeeded',
				data: {
					object: {
						id: `in_test_${Date.now()}`,
						billing_reason: 'manual',
						collection_method: 'send_invoice',
					},
				},
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
		});
		test('handles missing subscription info gracefully', async () => {
			const subscriptionId = `sub_test_nonexistent_${Date.now()}`;
			server.use(
				http.get('https://api.stripe.com/v1/subscriptions/:id', () =>
					HttpResponse.json(
						{
							error: {
								type: 'invalid_request_error',
								message: 'No such subscription',
								code: 'resource_missing',
							},
						},
						{status: 404},
					),
				),
			);
			const eventData: StripeWebhookEventData = {
				type: 'invoice.payment_succeeded',
				data: {
					object: {
						id: `in_test_${Date.now()}`,
						billing_reason: 'subscription_cycle',
						parent: {subscription_details: {subscription: subscriptionId}},
					},
				},
			};
			await sendWebhookExpectStripeError(eventData);
		});
		test('anchors yearly subscription renewal premium_until to Stripe period_end', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_test_${Date.now()}`;
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.yearlyUsd,
				productType: ProductType.YEARLY_SUBSCRIPTION,
			});
			const eventData: StripeWebhookEventData = {
				type: 'invoice.payment_succeeded',
				data: {
					object: {
						id: `in_test_${Date.now()}`,
						billing_reason: 'subscription_cycle',
						parent: {subscription_details: {subscription: subscriptionId}},
					},
				},
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const user = await createBuilder<{
				premium_type: number | null;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(user.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(user.premium_until).not.toBeNull();
			const premiumUntil = new Date(user.premium_until!);
			const now = new Date();
			const daysDiff = (premiumUntil.getTime() - now.getTime()) / (1000 * 60 * 60 * 24);
			expect(daysDiff).toBeGreaterThanOrEqual(27);
			expect(daysDiff).toBeLessThanOrEqual(32);
		});
		test('anchors renewal to Stripe period_end regardless of existing premium time', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_test_${Date.now()}`;
			const oneMonthLater = new Date(Date.now() + 30 * 24 * 60 * 60 * 1000);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: oneMonthLater.toISOString(),
					stripe_subscription_id: subscriptionId,
				})
				.execute();
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const userBefore = await createBuilder<{
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			const beforeExpiry = new Date(userBefore.premium_until!);
			const eventData: StripeWebhookEventData = {
				type: 'invoice.payment_succeeded',
				data: {
					object: {
						id: `in_test_${Date.now()}`,
						billing_reason: 'subscription_cycle',
						parent: {subscription_details: {subscription: subscriptionId}},
					},
				},
			};
			await sendWebhook(eventData);
			const userAfter = await createBuilder<{
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			const afterExpiry = new Date(userAfter.premium_until!);
			const daysDifference = (afterExpiry.getTime() - beforeExpiry.getTime()) / (1000 * 60 * 60 * 24);
			expect(Math.abs(daysDifference)).toBeLessThanOrEqual(3);
		});
		test('skips zero-amount subscription_update invoice without granting an extra monthly cycle', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_test_${Date.now()}`;
			const baselinePremiumUntil = new Date('2026-08-25T21:57:05.000Z');
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: baselinePremiumUntil.toISOString(),
					stripe_subscription_id: subscriptionId,
				})
				.execute();
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const eventData: StripeWebhookEventData = {
				type: 'invoice.payment_succeeded',
				data: {
					object: {
						id: `in_test_${Date.now()}`,
						billing_reason: 'subscription_update',
						parent: {subscription_details: {subscription: subscriptionId}},
						amount_paid: 0,
						amount_due: 0,
						total: 0,
					},
				},
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const userAfter = await createBuilder<{
				premium_type: number | null;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(userAfter.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(userAfter.premium_until).toBe(baselinePremiumUntil.toISOString());
		});
		test('skips paid subscription_update invoices so interval switches do not grant extra time', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_test_${Date.now()}`;
			const baselinePremiumUntil = new Date('2026-08-25T21:57:05.000Z');
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: baselinePremiumUntil.toISOString(),
					premium_billing_cycle: 'monthly',
					stripe_subscription_id: subscriptionId,
				})
				.execute();
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const eventData: StripeWebhookEventData = {
				type: 'invoice.payment_succeeded',
				data: {
					object: {
						id: `in_test_${Date.now()}`,
						billing_reason: 'subscription_update',
						parent: {subscription_details: {subscription: subscriptionId}},
						amount_paid: 1250,
						amount_due: 1250,
						total: 1250,
					},
				},
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const userAfter = await createBuilder<{
				premium_billing_cycle: string | null;
				premium_type: number | null;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(userAfter.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(userAfter.premium_billing_cycle).toBe('monthly');
			expect(userAfter.premium_until).toBe(baselinePremiumUntil.toISOString());
		});
	});
	describe('invoice.payment_failed', () => {
		test('marks subscription premium as canceling without extending unpaid future time', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_failed_${Date.now()}`;
			const failedPeriodStart = new Date(Date.now() + 10 * 24 * 60 * 60 * 1000);
			const failedPeriodEnd = new Date(Date.now() + 40 * 24 * 60 * 60 * 1000);
			const existingPremiumUntil = new Date(Date.now() + 30 * 24 * 60 * 60 * 1000);
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const {UserRepository} = await import('../../user/repositories/UserRepository');
			const userRepository = new UserRepository();
			const userId = createUserID(BigInt(account.userId));
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: existingPremiumUntil,
					premium_will_cancel: false,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_failed_invoice',
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const eventData = createInvoicePaymentFailedEvent({
				invoiceId: `in_failed_${Date.now()}`,
				customerId: 'cus_test_failed_invoice',
				subscriptionId,
				amountDue: 2500,
			});
			eventData.data.object.billing_reason = 'subscription_cycle';
			eventData.data.object.lines = {
				data: [
					{
						period: {
							start: Math.floor(failedPeriodStart.getTime() / 1000),
							end: Math.floor(failedPeriodEnd.getTime() / 1000),
						},
						parent: {
							subscription_item_details: {
								subscription: subscriptionId,
							},
						},
					},
				],
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const me = await createBuilder<{
				premium_type: number | null;
				premium_until: string | null;
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(me.premium_until).toBe(new Date(Math.floor(failedPeriodStart.getTime() / 1000) * 1000).toISOString());
			expect(me.premium_will_cancel).toBe(true);
		});
		test('ignores non-renewal invoice payment failures', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_failed_update_${Date.now()}`;
			const baselinePremiumUntil = new Date(Date.now() + 14 * 24 * 60 * 60 * 1000);
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const {UserRepository} = await import('../../user/repositories/UserRepository');
			const userRepository = new UserRepository();
			const userId = createUserID(BigInt(account.userId));
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: baselinePremiumUntil,
					premium_will_cancel: false,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_failed_update',
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const eventData = createInvoicePaymentFailedEvent({
				invoiceId: `in_failed_update_${Date.now()}`,
				customerId: 'cus_test_failed_update',
				subscriptionId,
				amountDue: 500,
			});
			eventData.data.object.billing_reason = 'subscription_update';
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const me = await createBuilder<{
				premium_until: string | null;
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_until).toBe(baselinePremiumUntil.toISOString());
			expect(me.premium_will_cancel).toBe(false);
		});
	});
	describe('invoice collection issue events', () => {
		test('handles invoice.payment_action_required like a recurring access issue', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_action_required_${Date.now()}`;
			const failedPeriodStart = new Date(Date.now() + 7 * 24 * 60 * 60 * 1000);
			const failedPeriodEnd = new Date(Date.now() + 37 * 24 * 60 * 60 * 1000);
			const existingPremiumUntil = new Date(Date.now() + 20 * 24 * 60 * 60 * 1000);
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			await setSubscriptionUserState({
				accountUserId: account.userId,
				subscriptionId,
				customerId: 'cus_test_action_required',
				premiumUntil: existingPremiumUntil,
			});
			const eventData = createInvoicePaymentActionRequiredEvent({
				invoiceId: `in_action_required_${Date.now()}`,
				customerId: 'cus_test_action_required',
				subscriptionId,
				amountDue: 2500,
			});
			eventData.data.object.billing_reason = 'subscription_cycle';
			eventData.data.object.lines = {
				data: [
					{
						period: {
							start: Math.floor(failedPeriodStart.getTime() / 1000),
							end: Math.floor(failedPeriodEnd.getTime() / 1000),
						},
						parent: {
							subscription_item_details: {
								subscription: subscriptionId,
							},
						},
					},
				],
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const me = await createBuilder<{
				premium_until: string | null;
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_until).toBe(new Date(Math.floor(failedPeriodStart.getTime() / 1000) * 1000).toISOString());
			expect(me.premium_will_cancel).toBe(true);
		});
		test('handles invoice.finalization_failed like a recurring access issue', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_finalization_failed_${Date.now()}`;
			const failedPeriodStart = new Date(Date.now() + 5 * 24 * 60 * 60 * 1000);
			const failedPeriodEnd = new Date(Date.now() + 35 * 24 * 60 * 60 * 1000);
			const existingPremiumUntil = new Date(Date.now() + 18 * 24 * 60 * 60 * 1000);
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			await setSubscriptionUserState({
				accountUserId: account.userId,
				subscriptionId,
				customerId: 'cus_test_finalization_failed',
				premiumUntil: existingPremiumUntil,
			});
			const eventData = createInvoiceFinalizationFailedEvent({
				invoiceId: `in_finalization_failed_${Date.now()}`,
				customerId: 'cus_test_finalization_failed',
				subscriptionId,
			});
			eventData.data.object.billing_reason = 'subscription_cycle';
			eventData.data.object.lines = {
				data: [
					{
						period: {
							start: Math.floor(failedPeriodStart.getTime() / 1000),
							end: Math.floor(failedPeriodEnd.getTime() / 1000),
						},
						parent: {
							subscription_item_details: {
								subscription: subscriptionId,
							},
						},
					},
				],
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const me = await createBuilder<{
				premium_until: string | null;
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_until).toBe(new Date(Math.floor(failedPeriodStart.getTime() / 1000) * 1000).toISOString());
			expect(me.premium_will_cancel).toBe(true);
		});
		test('handles actionable invoice.updated events as recurring collection issues', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_invoice_updated_${Date.now()}`;
			const failedPeriodStart = new Date(Date.now() + 6 * 24 * 60 * 60 * 1000);
			const failedPeriodEnd = new Date(Date.now() + 36 * 24 * 60 * 60 * 1000);
			const existingPremiumUntil = new Date(Date.now() + 22 * 24 * 60 * 60 * 1000);
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			await setSubscriptionUserState({
				accountUserId: account.userId,
				subscriptionId,
				customerId: 'cus_test_invoice_updated',
				premiumUntil: existingPremiumUntil,
			});
			const eventData = createInvoiceUpdatedEvent({
				invoiceId: `in_invoice_updated_${Date.now()}`,
				customerId: 'cus_test_invoice_updated',
				subscriptionId,
				amountDue: 2500,
				attemptCount: 2,
				attempted: true,
				status: 'open',
				paid: false,
			});
			eventData.data.object.billing_reason = 'subscription_cycle';
			eventData.data.object.lines = {
				data: [
					{
						period: {
							start: Math.floor(failedPeriodStart.getTime() / 1000),
							end: Math.floor(failedPeriodEnd.getTime() / 1000),
						},
						parent: {
							subscription_item_details: {
								subscription: subscriptionId,
							},
						},
					},
				],
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const me = await createBuilder<{
				premium_until: string | null;
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_until).toBe(new Date(Math.floor(failedPeriodStart.getTime() / 1000) * 1000).toISOString());
			expect(me.premium_will_cancel).toBe(true);
		});
		test('ignores invoice.updated when it does not reflect a collection issue', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_invoice_updated_noop_${Date.now()}`;
			const baselinePremiumUntil = new Date(Date.now() + 11 * 24 * 60 * 60 * 1000);
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			await setSubscriptionUserState({
				accountUserId: account.userId,
				subscriptionId,
				customerId: 'cus_test_invoice_updated_noop',
				premiumUntil: baselinePremiumUntil,
			});
			const eventData = createInvoiceUpdatedEvent({
				invoiceId: `in_invoice_updated_noop_${Date.now()}`,
				customerId: 'cus_test_invoice_updated_noop',
				subscriptionId,
				amountDue: 2500,
				attemptCount: 0,
				attempted: false,
				status: 'open',
				paid: false,
				nextPaymentAttempt: null,
			});
			eventData.data.object.billing_reason = 'subscription_cycle';
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const me = await createBuilder<{
				premium_until: string | null;
				premium_will_cancel: boolean;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_until).toBe(baselinePremiumUntil.toISOString());
			expect(me.premium_will_cancel).toBe(false);
		});
	});
	describe('invoice.paid', () => {
		test('treats invoice.paid as a successful renewal event', async () => {
			const account = await createTestAccount(harness);
			const subscriptionId = `sub_paid_alias_${Date.now()}`;
			await createPaymentRecord({
				userId: account.userId,
				subscriptionId,
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const eventData = createInvoicePaidEvent({
				invoiceId: `in_paid_alias_${Date.now()}`,
				customerId: 'cus_paid_alias',
				subscriptionId,
				amountPaid: 2500,
				currency: 'usd',
			});
			eventData.data.object.billing_reason = 'subscription_cycle';
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const me = await createBuilder<{
				premium_type: number | null;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(me.premium_until).not.toBeNull();
		});
	});
});

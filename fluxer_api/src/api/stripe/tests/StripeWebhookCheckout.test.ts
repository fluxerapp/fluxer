// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createTestAccount, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {ProductType} from '@app/api/stripe/ProductRegistry';
import {
	sendStripeWebhook,
	sendStripeWebhookExpectStripeError,
	sendStripeWebhookRaw,
	setupSyncStripeWebhookWorker,
} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	createCheckoutCompletedEvent,
	createStripeApiHandlers,
	type StripeApiHandlers,
	type StripeWebhookEventData,
} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const MOCK_PRICES = {
	monthlyUsd: 'price_monthly_usd',
	monthlyEur: 'price_monthly_eur',
	monthlyBrl: 'price_monthly_brl',
	monthlyInr: 'price_monthly_inr',
	monthlyPln: 'price_monthly_pln',
	monthlyTry: 'price_monthly_try',
	yearlyUsd: 'price_yearly_usd',
	yearlyEur: 'price_yearly_eur',
	yearlyBrl: 'price_yearly_brl',
	yearlyInr: 'price_yearly_inr',
	yearlyPln: 'price_yearly_pln',
	yearlyTry: 'price_yearly_try',
	visionaryUsd: 'price_visionary_usd',
	visionaryEur: 'price_visionary_eur',
	giftVisionaryUsd: 'price_gift_visionary_usd',
	giftVisionaryEur: 'price_gift_visionary_eur',
	gift1MonthUsd: 'price_gift_1_month_usd',
	gift1MonthEur: 'price_gift_1_month_eur',
	gift1MonthBrl: 'price_gift_1_month_brl',
	gift1MonthInr: 'price_gift_1_month_inr',
	gift1MonthPln: 'price_gift_1_month_pln',
	gift1MonthTry: 'price_gift_1_month_try',
	gift1YearUsd: 'price_gift_1_year_usd',
	gift1YearEur: 'price_gift_1_year_eur',
	gift1YearBrl: 'price_gift_1_year_brl',
	gift1YearInr: 'price_gift_1_year_inr',
	gift1YearPln: 'price_gift_1_year_pln',
	gift1YearTry: 'price_gift_1_year_try',
};

describe('StripeWebhookService - checkout.session.completed', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let originalWebhookSecret: string | undefined;
	let originalPrices: typeof Config.stripe.prices | undefined;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		originalWebhookSecret = Config.stripe.webhookSecret;
		originalPrices = Config.stripe.prices;
		Config.stripe.webhookSecret = 'whsec_test_secret';
		Config.stripe.prices = MOCK_PRICES;
		setupSyncStripeWebhookWorker();
		stripeHandlers = createStripeApiHandlers();
		server.use(...stripeHandlers.handlers);
	});
	beforeEach(async () => {
		await harness.resetData();
		stripeHandlers.reset();
		server.use(...stripeHandlers.handlers);
	});
	afterEach(() => {
		server.resetHandlers();
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.webhookSecret = originalWebhookSecret;
		Config.stripe.prices = originalPrices;
	});
	describe('premium checkout', () => {
		test('processes completed premium checkout session successfully', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_premium_success_123';
			const {PaymentRepository} = await import('@app/api/user/repositories/PaymentRepository');
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: createUserID(BigInt(account.userId)),
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'pending',
				is_gift: false,
				created_at: new Date(),
			});
			const eventData = createCheckoutCompletedEvent({
				sessionId,
				customerId: 'cus_test_1',
				subscriptionId: 'sub_test_1',
				metadata: {},
			});
			const result = await sendStripeWebhook(harness, eventData);
			expect(result.received).toBe(true);
			const updatedPayment = await userRepository.getPaymentByCheckoutSession(sessionId);
			expect(updatedPayment?.status).toBe('completed');
			expect(updatedPayment?.stripeCustomerId).toBe('cus_test_1');
			expect(updatedPayment?.subscriptionId).toBe('sub_test_1');
			const user = await createBuilder<{
				premium_type: number;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(user.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
		});
		test('allows localized BRL PIX subscription checkout when checkout.session.completed has no payment intent', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_localized_brl_pix_subscription';
			const {PaymentRepository} = await import('@app/api/user/repositories/PaymentRepository');
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: createUserID(BigInt(account.userId)),
				price_id: MOCK_PRICES.monthlyBrl,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'pending',
				is_gift: false,
				created_at: new Date(),
			});
			const eventData = createCheckoutCompletedEvent({
				sessionId,
				customerId: 'cus_localized_brl_pix',
				subscriptionId: 'sub_localized_brl_pix',
				amountTotal: 1288,
				currency: 'brl',
				metadata: {
					country_code: 'BR',
					payment_method: 'pix',
				},
			});
			eventData.data.object.payment_intent = null;
			eventData.data.object.payment_method_types = ['pix'];
			const result = await sendStripeWebhook(harness, eventData);
			expect(result.received).toBe(true);
			const updatedPayment = await userRepository.getPaymentByCheckoutSession(sessionId);
			expect(updatedPayment?.status).toBe('completed');
			expect(updatedPayment?.paymentIntentId).toBeNull();
			expect(updatedPayment?.subscriptionId).toBe('sub_localized_brl_pix');
			expect(stripeHandlers.spies.retrievedPaymentIntents).toHaveLength(0);
			expect(stripeHandlers.spies.createdRefunds).toHaveLength(0);
			expect(stripeHandlers.spies.cancelledSubscriptions).toHaveLength(0);
			const user = await createBuilder<{
				premium_type: number;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(user.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
		});
		test('updates user with Stripe customer ID on first purchase', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_first_purchase_customer_123';
			const {PaymentRepository} = await import('@app/api/user/repositories/PaymentRepository');
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: createUserID(BigInt(account.userId)),
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'pending',
				is_gift: false,
				created_at: new Date(),
			});
			const eventData = createCheckoutCompletedEvent({
				sessionId,
				customerId: 'cus_new_123',
				metadata: {},
			});
			const result = await sendStripeWebhook(harness, eventData);
			expect(result.received).toBe(true);
			const user = await userRepository.findUnique(createUserID(BigInt(account.userId)));
			expect(user?.stripeCustomerId).toBe('cus_new_123');
		});
		test('processes duplicate checkout delivery idempotently across concurrency and retry', async () => {
			const account = await createTestAccount(harness);
			const {PaymentRepository} = await import('@app/api/user/repositories/PaymentRepository');
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			await paymentRepository.createPayment({
				checkout_session_id: 'cs_duplicate_checkout_delivery_1',
				user_id: createUserID(BigInt(account.userId)),
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'pending',
				is_gift: false,
				created_at: new Date(),
			});
			const eventData: StripeWebhookEventData = {
				...createCheckoutCompletedEvent({
					sessionId: 'cs_duplicate_checkout_delivery_1',
					customerId: 'cus_duplicate_checkout_delivery_1',
					subscriptionId: 'sub_duplicate_checkout_delivery_1',
					metadata: {},
				}),
				id: 'evt_duplicate_checkout_delivery_1',
			};
			const concurrentResponses = await Promise.all([
				sendStripeWebhookRaw(harness, eventData),
				sendStripeWebhookRaw(harness, eventData),
			]);
			const statuses = concurrentResponses.map((response) => response.response.status).sort();
			expect(statuses).toEqual([200, 400]);
			expect(
				concurrentResponses.some((response) => response.text.includes('Stripe webhook event in-flight, retry')),
			).toBe(true);
			const userAfterConcurrent = await createBuilder<{
				premium_type: number;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(userAfterConcurrent.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(userAfterConcurrent.premium_until).not.toBeNull();
			const premiumUntilAfterConcurrent = new Date(userAfterConcurrent.premium_until!);
			const daysUntilAfterConcurrent = (premiumUntilAfterConcurrent.getTime() - Date.now()) / (1000 * 60 * 60 * 24);
			expect(daysUntilAfterConcurrent).toBeGreaterThanOrEqual(27);
			expect(daysUntilAfterConcurrent).toBeLessThanOrEqual(31);
			const retryEventData: StripeWebhookEventData = {
				...eventData,
				id: 'evt_duplicate_checkout_delivery_retry_1',
			};
			const retryResponse = await sendStripeWebhook(harness, retryEventData);
			expect(retryResponse.received).toBe(true);
			const userAfterRetry = await createBuilder<{
				premium_type: number;
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(userAfterRetry.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(userAfterRetry.premium_until).toBe(userAfterConcurrent.premium_until);
			const paymentAfterRetry = await userRepository.getPaymentByCheckoutSession('cs_duplicate_checkout_delivery_1');
			expect(paymentAfterRetry?.status).toBe('completed');
			expect(paymentAfterRetry?.subscriptionId).toBe('sub_duplicate_checkout_delivery_1');
		});
		test('skips already processed payment', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_already_completed_123';
			const {PaymentRepository} = await import('@app/api/user/repositories/PaymentRepository');
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: createUserID(BigInt(account.userId)),
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			const beforeUser = await createBuilder<{
				premium_type: number;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			const eventData = createCheckoutCompletedEvent({
				sessionId,
				metadata: {},
			});
			const result = await sendStripeWebhook(harness, eventData);
			expect(result.received).toBe(true);
			const afterUser = await createBuilder<{
				premium_type: number;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(afterUser.premium_type).toBe(beforeUser.premium_type);
			const paymentAfterWebhook = await userRepository.getPaymentByCheckoutSession(sessionId);
			expect(paymentAfterWebhook?.status).toBe('completed');
		});
		test('handles missing payment record gracefully', async () => {
			const eventData = createCheckoutCompletedEvent({
				sessionId: 'cs_nonexistent_123',
				metadata: {},
			});
			const result = await sendStripeWebhook(harness, eventData);
			expect(result.received).toBe(true);
		});
		test('handles external donate checkout sessions without internal payment records', async () => {
			const eventData: StripeWebhookEventData = {
				type: 'checkout.session.completed',
				data: {
					object: {
						id: 'cs_external_donate_123',
						object: 'checkout.session',
						mode: 'payment',
						status: 'complete',
						payment_status: 'paid',
						submit_type: 'donate',
						payment_link: 'plink_external_123',
						payment_intent: 'pi_external_123',
						amount_total: 1000,
						currency: 'usd',
						customer: null,
						customer_email: null,
						customer_details: {
							email: 'external-donor@example.com',
							name: 'External Donor',
						},
						metadata: {},
					},
				},
			};
			const result = await sendStripeWebhook(harness, eventData);
			expect(result.received).toBe(true);
		});
		test('handles gift purchase correctly', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_gift_purchase_123';
			const {PaymentRepository} = await import('@app/api/user/repositories/PaymentRepository');
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: createUserID(BigInt(account.userId)),
				price_id: MOCK_PRICES.gift1MonthUsd,
				product_type: ProductType.GIFT_1_MONTH,
				status: 'pending',
				is_gift: true,
				created_at: new Date(),
			});
			const eventData = createCheckoutCompletedEvent({
				sessionId,
				metadata: {},
			});
			const result = await sendStripeWebhook(harness, eventData);
			expect(result.received).toBe(true);
			const updatedPayment = await userRepository.getPaymentByCheckoutSession(sessionId);
			expect(updatedPayment?.status).toBe('completed');
			expect(updatedPayment?.giftCode).not.toBeNull();
			const user = await createBuilder<{
				premium_type: number;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(user.premium_type).toBe(UserPremiumTypes.NONE);
		});
	});
	describe('donation checkout', () => {
		test('handles donation without email gracefully', async () => {
			const eventData = createCheckoutCompletedEvent({
				metadata: {is_donation: 'true'},
			});
			await sendStripeWebhookExpectStripeError(harness, eventData);
		});
		test('records donation with valid email and subscription', async () => {
			await clearTestEmails(harness);
			const donationEmail = 'donor@example.com';
			const eventData = createCheckoutCompletedEvent({
				customerId: 'cus_donation_1',
				subscriptionId: 'sub_donation_1',
				metadata: {is_donation: 'true', donation_email: donationEmail},
			});
			const result = await sendStripeWebhook(harness, eventData);
			expect(result.received).toBe(true);
			const {DonationRepository} = await import('@app/api/donation/DonationRepository');
			const donationRepository = new DonationRepository();
			const donor = await donationRepository.findDonorByEmail(donationEmail);
			expect(donor).not.toBeNull();
			expect(donor?.stripeCustomerId).toBe('cus_donation_1');
			expect(donor?.stripeSubscriptionId).toBe('sub_donation_1');
			const emails = await listTestEmails(harness, {recipient: donationEmail});
			const confirmationEmail = emails.find((e) => e.type === 'donation_confirmation');
			expect(confirmationEmail).toBeDefined();
			expect(confirmationEmail?.to).toBe(donationEmail);
		});
	});
});

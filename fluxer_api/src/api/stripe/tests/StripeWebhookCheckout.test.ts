// SPDX-License-Identifier: AGPL-3.0-or-later

import {clearTestEmails, createTestAccount, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {upsertOne} from '@app/api/database/CassandraQueryExecution';
import {getPaymentDisputeRepository} from '@app/api/middleware/ServiceSingletons';
import {seedOpenPaymentDisputeReview} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {ProductType} from '@app/api/stripe/ProductRegistry';
import {
	createTestPayment,
	sendStripeWebhook,
	sendStripeWebhookExpectStripeError,
	sendStripeWebhookRaw,
	setupSyncStripeWebhookWorker,
} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {Payments} from '@app/api/Tables';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	createCheckoutCompletedEvent,
	createStripeApiHandlers,
	type StripeApiHandlers,
	type StripeWebhookEventData,
} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {MS_PER_DAY} from '@fluxer/date_utils/src/DateConstants';
import {afterAll, afterEach, assert, beforeAll, beforeEach, describe, expect, test} from 'vitest';

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
		test('records the payment intent of the first invoice so a later refund marks the payment refunded', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_premium_invoice_intent';
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
			await sendStripeWebhook(
				harness,
				createCheckoutCompletedEvent({
					sessionId,
					customerId: 'cus_test_1',
					subscriptionId: 'sub_test_1',
					paymentIntentId: null,
					invoiceId: 'in_premium_invoice_intent',
					metadata: {},
				}),
			);
			const completed = await userRepository.getPaymentByCheckoutSession(sessionId);
			expect(completed?.paymentIntentId).toBe('pi_in_premium_invoice_intent');
			await sendStripeWebhook(harness, {
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_pi_in_premium_invoice_intent',
						payment_intent: 'pi_in_premium_invoice_intent',
						amount: 2500,
						amount_refunded: 2500,
						refunded: true,
					},
				},
			});
			expect((await userRepository.getPaymentByCheckoutSession(sessionId))?.status).toBe('refunded');
		});
		test('grants no premium when the subscription was cancelled before the checkout webhook', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_premium_cancelled_before_fulfilment';
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			stripeHandlers = createStripeApiHandlers({subscriptions: {sub_cancelled_early: {status: 'canceled'}}});
			server.use(...stripeHandlers.handlers);
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: createUserID(BigInt(account.userId)),
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'pending',
				is_gift: false,
				created_at: new Date(),
			});
			await sendStripeWebhook(
				harness,
				createCheckoutCompletedEvent({
					sessionId,
					customerId: 'cus_test_1',
					subscriptionId: 'sub_cancelled_early',
					metadata: {},
				}),
			);
			expect((await userRepository.getPaymentByCheckoutSession(sessionId))?.status).toBe('completed');
			const user = await userRepository.findUnique(createUserID(BigInt(account.userId)));
			expect(user?.premiumType ?? null).toBeNull();
			expect(user?.stripeSubscriptionId ?? null).toBeNull();
		});
		test('leaves a payment pending when its price ID is missing', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const sessionId = 'cs_missing_price';
			const paymentRepository = new PaymentRepository();
			await createTestPayment(harness, userId, sessionId, {
				priceId: MOCK_PRICES.monthlyUsd,
				productType: ProductType.MONTHLY_SUBSCRIPTION,
			});
			const payment = await paymentRepository.getPaymentByCheckoutSession(sessionId);
			assert(payment !== null);
			await upsertOne(Payments.upsertAll({...payment.toRow(), price_id: null}));

			await sendStripeWebhookExpectStripeError(harness, createCheckoutCompletedEvent({sessionId, metadata: {}}));

			expect((await paymentRepository.getPaymentByCheckoutSession(sessionId))?.status).toBe('pending');
			expect((await new UserRepository().findUnique(userId))?.premiumType).toBeNull();
		});

		test.each([
			['valid timestamp without consent', '2025-01-02T03:04:05.000Z', null, '2025-01-02T03:04:05.000Z'],
			['valid timestamp with consent', '2025-01-02T03:04:05.000Z', 'accepted', '2025-01-02T03:04:05.000Z'],
			['invalid timestamp with consent', 'invalid', 'accepted', 'now'],
			['empty timestamp with consent', '', 'accepted', 'now'],
			['invalid timestamp without consent', 'invalid', null, null],
			['empty timestamp without consent', '', null, null],
		] as const)('recovers a missing payment with %s', async (_label, timestamp, consent, expected) => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_recovered_waiver';
			const event = createCheckoutCompletedEvent({
				sessionId,
				metadata: {
					user_id: account.userId,
					price_id: MOCK_PRICES.monthlyUsd,
					product_type: ProductType.MONTHLY_SUBSCRIPTION,
					eu_withdrawal_waiver_accepted_at: timestamp,
				},
			});
			event.data.object.consent = {terms_of_service: consent};
			const before = Date.now();

			await sendStripeWebhook(harness, event);

			const payment = await new PaymentRepository().getPaymentByCheckoutSession(sessionId);
			expect(payment?.status).toBe('completed');
			const acceptedAt = payment?.euWithdrawalWaiverAcceptedAt ?? null;
			if (expected === 'now') {
				expect(acceptedAt?.getTime()).toBeGreaterThanOrEqual(before);
				expect(acceptedAt?.getTime()).toBeLessThanOrEqual(Date.now());
			} else {
				expect(acceptedAt?.toISOString() ?? null).toBe(expected);
			}
		});

		test('flags an open payment dispute review when a checkout opened before the dispute completes', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const sessionId = 'cs_premium_during_dispute';
			await new PaymentRepository().createPayment({
				checkout_session_id: sessionId,
				user_id: userId,
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'pending',
				is_gift: false,
				created_at: new Date(),
			});
			await seedOpenPaymentDisputeReview(userId, 'dp_checkout_during_dispute');
			await sendStripeWebhook(
				harness,
				createCheckoutCompletedEvent({
					sessionId,
					customerId: 'cus_during_dispute',
					subscriptionId: 'sub_during_dispute',
					metadata: {},
				}),
			);
			const review = await getPaymentDisputeRepository().findReview('dp_checkout_during_dispute');
			expect(review?.dispute_purchase_at).toBeInstanceOf(Date);
		});

		test('allows localized BRL PIX subscription checkout when checkout.session.completed has no payment intent', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_localized_brl_pix_subscription';
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
			expect(statuses).toEqual([HttpStatus.OK, HttpStatus.BAD_REQUEST]);
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
			assert(userAfterConcurrent.premium_until !== null);
			const premiumUntilAfterConcurrent = new Date(userAfterConcurrent.premium_until);
			const daysUntilAfterConcurrent = (premiumUntilAfterConcurrent.getTime() - Date.now()) / MS_PER_DAY;
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

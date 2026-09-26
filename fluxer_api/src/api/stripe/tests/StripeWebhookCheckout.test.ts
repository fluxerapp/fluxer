// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {clearDonationTestEmails, listDonationTestEmails} from '@app/api/donation/tests/DonationTestUtils';
import {ProductType} from '@app/api/stripe/ProductRegistry';
import {setupSyncStripeWebhookWorker} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	createCheckoutCompletedEvent,
	createMockWebhookPayload,
	createStripeApiHandlers,
	type StripeApiHandlers,
	type StripeWebhookEventData,
} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {HttpResponse, http} from 'msw';
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
		stripeHandlers = createStripeApiHandlers({
			charges: {
				ch_pi_localized_brl_card_br: {
					currency: 'brl',
					payment_intent: 'pi_localized_brl_card_br',
					payment_method_details: {
						type: 'card',
						card: {
							fingerprint: 'fp_localized_brl_card_br',
							country: 'BR',
						},
					},
				},
				ch_pi_localized_brl_card_us: {
					currency: 'brl',
					payment_intent: 'pi_localized_brl_card_us',
					payment_method_details: {
						type: 'card',
						card: {
							fingerprint: 'fp_localized_brl_card_us',
							country: 'US',
						},
					},
				},
			},
			paymentIntents: {
				pi_localized_brl_card_br: {
					currency: 'brl',
					latest_charge: 'ch_pi_localized_brl_card_br',
					status: 'succeeded',
				},
				pi_localized_brl_card_us: {
					currency: 'brl',
					latest_charge: 'ch_pi_localized_brl_card_us',
					status: 'succeeded',
				},
			},
			setupIntents: {
				seti_localized_brl_card_br: {
					customer: 'cus_test_existing',
					payment_method: {
						id: 'pm_localized_brl_card_br',
						object: 'payment_method',
						type: 'card',
						card: {
							country: 'BR',
						},
						customer: 'cus_test_existing',
					},
					status: 'succeeded',
				},
				seti_localized_brl_card_us: {
					customer: 'cus_test_existing',
					payment_method: {
						id: 'pm_localized_brl_card_us',
						object: 'payment_method',
						type: 'card',
						card: {
							country: 'US',
						},
						customer: 'cus_test_existing',
					},
					status: 'succeeded',
				},
			},
		});
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
	async function sendWebhookRaw(eventData: StripeWebhookEventData): Promise<{
		response: Response;
		text: string;
		json: unknown;
	}> {
		const {payload, timestamp} = createMockWebhookPayload(eventData);
		const signature = createWebhookSignature(payload, timestamp, Config.stripe.webhookSecret!);
		return createBuilder(harness, '')
			.post('/stripe/webhook')
			.header('stripe-signature', signature)
			.header('content-type', 'application/json')
			.body(payload)
			.executeRaw();
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
			const result = await sendWebhook(eventData);
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
		test('allows localized BRL checkout when the card is issued in Brazil', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_localized_brl_card_br';
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
				customerId: 'cus_localized_brl_card_br',
				subscriptionId: 'sub_localized_brl_card_br',
				paymentIntentId: 'pi_localized_brl_card_br',
				amountTotal: 1288,
				currency: 'brl',
				metadata: {country_code: 'BR'},
			});
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const updatedPayment = await userRepository.getPaymentByCheckoutSession(sessionId);
			expect(updatedPayment?.status).toBe('completed');
			expect(stripeHandlers.spies.createdRefunds).toHaveLength(0);
			expect(stripeHandlers.spies.cancelledSubscriptions).toHaveLength(0);
			expect(stripeHandlers.spies.retrievedPaymentIntents).toContain('pi_localized_brl_card_br');
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
			const result = await sendWebhook(eventData);
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
		test('allows localized TRY card subscription checkout when checkout.session.completed has no payment intent', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_localized_try_card_no_payment_intent';
			const {PaymentRepository} = await import('@app/api/user/repositories/PaymentRepository');
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: createUserID(BigInt(account.userId)),
				price_id: MOCK_PRICES.monthlyTry,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'pending',
				is_gift: false,
				created_at: new Date(),
			});
			server.use(
				http.get('https://api.stripe.com/v1/subscriptions/:id', ({params}) => {
					if (params.id !== 'sub_localized_try_card_no_payment_intent') {
						return;
					}
					return HttpResponse.json({
						id: params.id,
						object: 'subscription',
						customer: 'cus_localized_try_card_no_payment_intent',
						status: 'active',
						current_period_start: Math.floor(Date.now() / 1000) - 3600,
						start_date: Math.floor(Date.now() / 1000) - 3600,
						default_payment_method: {
							id: 'pm_localized_try_card_tr',
							object: 'payment_method',
							type: 'card',
							card: {
								country: 'TR',
							},
						},
						items: {
							object: 'list',
							data: [
								{
									id: 'si_localized_try_card_tr',
									object: 'subscription_item',
									price: {
										id: MOCK_PRICES.monthlyTry,
										object: 'price',
										unit_amount: 22999,
										currency: 'try',
										recurring: {
											interval: 'month',
											interval_count: 1,
										},
										type: 'recurring',
										active: true,
										livemode: false,
									},
									quantity: 1,
									current_period_end: Math.floor(Date.now() / 1000) + 30 * 24 * 60 * 60,
								},
							],
							has_more: false,
							url: `/v1/subscription_items?subscription=${params.id}`,
						},
						cancel_at: null,
						cancel_at_period_end: false,
						canceled_at: null,
						collection_method: 'charge_automatically',
						livemode: false,
						metadata: {},
					});
				}),
			);
			const eventData = createCheckoutCompletedEvent({
				sessionId,
				customerId: 'cus_localized_try_card_no_payment_intent',
				subscriptionId: 'sub_localized_try_card_no_payment_intent',
				amountTotal: 22999,
				currency: 'try',
				metadata: {
					country_code: 'TR',
					payment_method: 'card',
				},
			});
			eventData.data.object.payment_intent = null;
			eventData.data.object.payment_method_types = ['card', 'link'];
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const updatedPayment = await userRepository.getPaymentByCheckoutSession(sessionId);
			expect(updatedPayment?.status).toBe('completed');
			expect(updatedPayment?.paymentIntentId).toBeNull();
			expect(updatedPayment?.subscriptionId).toBe('sub_localized_try_card_no_payment_intent');
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
		test('rejects localized BRL checkout when the card is issued outside Brazil', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_localized_brl_card_us';
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
				customerId: 'cus_localized_brl_card_us',
				subscriptionId: 'sub_localized_brl_card_us',
				paymentIntentId: 'pi_localized_brl_card_us',
				amountTotal: 1288,
				currency: 'brl',
				metadata: {country_code: 'BR'},
			});
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const updatedPayment = await userRepository.getPaymentByCheckoutSession(sessionId);
			expect(updatedPayment?.status).toBe('failed');
			expect(updatedPayment?.subscriptionId).toBe('sub_localized_brl_card_us');
			expect(stripeHandlers.spies.createdRefunds).toHaveLength(1);
			expect(stripeHandlers.spies.cancelledSubscriptions).toContain('sub_localized_brl_card_us');
			const user = await createBuilder<{
				premium_type: number;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(user.premium_type).toBe(UserPremiumTypes.NONE);
		});
		test('continues localized card preapproval into paid checkout when the card matches the requested country', async () => {
			const account = await createTestAccount(harness);
			server.use(
				http.get('https://api.stripe.com/v1/subscriptions', () => {
					return HttpResponse.json({
						object: 'list',
						url: '/v1/subscriptions',
						has_more: false,
						data: [],
					});
				}),
			);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/security-flags`)
				.body({email_verified: true})
				.execute();
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({stripe_customer_id: 'cus_test_existing'})
				.execute();
			const preapprovalResponse = await createBuilder<{
				url: string;
			}>(harness, account.token)
				.post('/stripe/checkout/subscription/preapproval')
				.body({price_id: MOCK_PRICES.monthlyBrl, country_code: 'BR'})
				.execute();
			const preapprovalSession = stripeHandlers.spies.createdCheckoutSessions[0];
			const successUrl = new URL(preapprovalSession?.success_url ?? 'https://example.com');
			const token = successUrl.searchParams.get('token');
			const preapprovalSessionId = preapprovalResponse.url.split('/').pop();
			expect(token).toBeTruthy();
			expect(preapprovalSessionId).toBeTruthy();
			if (!token || !preapprovalSessionId) {
				throw new Error('Expected localized card preapproval token and session id');
			}
			const webhookResult = await sendWebhook(
				createCheckoutCompletedEvent({
					sessionId: preapprovalSessionId,
					customerId: 'cus_test_existing',
					mode: 'setup',
					setupIntentId: 'seti_localized_brl_card_br',
					metadata: preapprovalSession?.metadata ?? {},
				}),
			);
			expect(webhookResult.received).toBe(true);
			const continueResponse = await createBuilder<{
				status: string;
				url?: string;
			}>(harness, '')
				.post('/stripe/checkout/subscription/preapproval/continue')
				.body({token})
				.execute();
			expect(continueResponse.status).toBe('ready');
			expect(continueResponse.url).toMatch(/^https:\/\/checkout\.stripe\.com/);
			expect(stripeHandlers.spies.retrievedSetupIntents).toContain('seti_localized_brl_card_br');
			expect(stripeHandlers.spies.updatedCustomers).toContainEqual({
				id: 'cus_test_existing',
				params: {
					invoice_settings: {
						default_payment_method: 'pm_localized_brl_card_br',
					},
				},
			});
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(2);
			expect(stripeHandlers.spies.createdCheckoutSessions[1]?.mode).toBe('subscription');
			expect(stripeHandlers.spies.createdCheckoutSessions[1]?.customer).toBe('cus_test_existing');
			expect(stripeHandlers.spies.createdCheckoutSessions[1]?.metadata?.country_code).toBe('BR');
		});
		test('keeps localized card preapproval rejected when the card country does not match', async () => {
			const account = await createTestAccount(harness);
			server.use(
				http.get('https://api.stripe.com/v1/subscriptions', () => {
					return HttpResponse.json({
						object: 'list',
						url: '/v1/subscriptions',
						has_more: false,
						data: [],
					});
				}),
			);
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/security-flags`)
				.body({email_verified: true})
				.execute();
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/premium`)
				.body({stripe_customer_id: 'cus_test_existing'})
				.execute();
			const preapprovalResponse = await createBuilder<{
				url: string;
			}>(harness, account.token)
				.post('/stripe/checkout/subscription/preapproval')
				.body({price_id: MOCK_PRICES.monthlyBrl, country_code: 'BR'})
				.execute();
			const preapprovalSession = stripeHandlers.spies.createdCheckoutSessions[0];
			const successUrl = new URL(preapprovalSession?.success_url ?? 'https://example.com');
			const token = successUrl.searchParams.get('token');
			const preapprovalSessionId = preapprovalResponse.url.split('/').pop();
			expect(token).toBeTruthy();
			expect(preapprovalSessionId).toBeTruthy();
			if (!token || !preapprovalSessionId) {
				throw new Error('Expected localized card preapproval token and session id');
			}
			const webhookResult = await sendWebhook(
				createCheckoutCompletedEvent({
					sessionId: preapprovalSessionId,
					customerId: 'cus_test_existing',
					mode: 'setup',
					setupIntentId: 'seti_localized_brl_card_us',
					metadata: preapprovalSession?.metadata ?? {},
				}),
			);
			expect(webhookResult.received).toBe(true);
			const continueResponse = await createBuilder<{
				status: string;
				reason?: string;
				actual_country?: string | null;
			}>(harness, '')
				.post('/stripe/checkout/subscription/preapproval/continue')
				.body({token})
				.execute();
			expect(continueResponse.status).toBe('rejected');
			expect(continueResponse.reason).toBe('country_mismatch');
			expect(continueResponse.actual_country).toBe('US');
			expect(stripeHandlers.spies.retrievedSetupIntents).toContain('seti_localized_brl_card_us');
			expect(stripeHandlers.spies.updatedCustomers).toHaveLength(0);
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
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
			const result = await sendWebhook(eventData);
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
			const concurrentResponses = await Promise.all([sendWebhookRaw(eventData), sendWebhookRaw(eventData)]);
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
			const retryResponse = await sendWebhook(retryEventData);
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
			const result = await sendWebhook(eventData);
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
			const result = await sendWebhook(eventData);
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
			const result = await sendWebhook(eventData);
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
			const result = await sendWebhook(eventData);
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
		test('allows localized BRL gift checkout when the card is issued in Brazil', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_gift_brl_card_br';
			const {PaymentRepository} = await import('@app/api/user/repositories/PaymentRepository');
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: createUserID(BigInt(account.userId)),
				price_id: MOCK_PRICES.gift1MonthBrl,
				product_type: ProductType.GIFT_1_MONTH,
				status: 'pending',
				is_gift: true,
				created_at: new Date(),
			});
			const eventData = createCheckoutCompletedEvent({
				sessionId,
				customerId: 'cus_gift_brl_card_br',
				paymentIntentId: 'pi_localized_brl_card_br',
				amountTotal: 1288,
				currency: 'brl',
				mode: 'payment',
				metadata: {country_code: 'BR'},
			});
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const updatedPayment = await userRepository.getPaymentByCheckoutSession(sessionId);
			expect(updatedPayment?.status).toBe('completed');
			expect(updatedPayment?.giftCode).not.toBeNull();
			expect(stripeHandlers.spies.createdRefunds).toHaveLength(0);
		});
		test('rejects localized BRL gift checkout when the card is issued outside Brazil', async () => {
			const account = await createTestAccount(harness);
			const sessionId = 'cs_gift_brl_card_us';
			const {PaymentRepository} = await import('@app/api/user/repositories/PaymentRepository');
			const {UserRepository} = await import('@app/api/user/repositories/UserRepository');
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: createUserID(BigInt(account.userId)),
				price_id: MOCK_PRICES.gift1MonthBrl,
				product_type: ProductType.GIFT_1_MONTH,
				status: 'pending',
				is_gift: true,
				created_at: new Date(),
			});
			const eventData = createCheckoutCompletedEvent({
				sessionId,
				customerId: 'cus_gift_brl_card_us',
				paymentIntentId: 'pi_localized_brl_card_us',
				amountTotal: 1288,
				currency: 'brl',
				mode: 'payment',
				metadata: {country_code: 'BR'},
			});
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const updatedPayment = await userRepository.getPaymentByCheckoutSession(sessionId);
			expect(updatedPayment?.status).toBe('failed');
			expect(updatedPayment?.giftCode).toBeNull();
			expect(stripeHandlers.spies.createdRefunds).toHaveLength(1);
		});
	});
	describe('donation checkout', () => {
		test('handles donation without email gracefully', async () => {
			const eventData = createCheckoutCompletedEvent({
				metadata: {is_donation: 'true'},
			});
			await sendWebhookExpectStripeError(eventData);
		});
		test('records donation with valid email and subscription', async () => {
			await clearDonationTestEmails(harness);
			const donationEmail = 'donor@example.com';
			const eventData = createCheckoutCompletedEvent({
				customerId: 'cus_donation_1',
				subscriptionId: 'sub_donation_1',
				metadata: {is_donation: 'true', donation_email: donationEmail},
			});
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const {DonationRepository} = await import('@app/api/donation/DonationRepository');
			const donationRepository = new DonationRepository();
			const donor = await donationRepository.findDonorByEmail(donationEmail);
			expect(donor).not.toBeNull();
			expect(donor?.stripeCustomerId).toBe('cus_donation_1');
			expect(donor?.stripeSubscriptionId).toBe('sub_donation_1');
			const emails = await listDonationTestEmails(harness, {recipient: donationEmail});
			const confirmationEmail = emails.find((e) => e.type === 'donation_confirmation');
			expect(confirmationEmail).toBeDefined();
			expect(confirmationEmail?.to).toBe(donationEmail);
		});
	});
});

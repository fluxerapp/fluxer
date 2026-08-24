// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {
	createMockWebhookPayload,
	createStripeApiHandlers,
	createSubscriptionDeletedEvent,
	createSubscriptionUpdatedEvent,
	type StripeApiHandlers,
	type StripeWebhookEventData,
} from '../../test/msw/handlers/StripeApiHandlers';
import {server} from '../../test/msw/server';
import {createBuilder} from '../../test/TestRequestBuilder';
import {PaymentRepository} from '../../user/repositories/PaymentRepository';
import {UserRepository} from '../../user/repositories/UserRepository';
import {ProductType} from '../ProductRegistry';
import {setupSyncStripeWebhookWorker} from './StripeWebhookTestUtils';

const MOCK_PRICES = {
	monthlyUsd: 'price_monthly_usd',
};

describe('Stripe Webhook Subscription Lifecycle', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let originalWebhookSecret: string | undefined;
	let originalPrices: typeof Config.stripe.prices | undefined;
	let paymentRepository: PaymentRepository;
	let userRepository: UserRepository;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		setupSyncStripeWebhookWorker();
		originalWebhookSecret = Config.stripe.webhookSecret;
		originalPrices = Config.stripe.prices;
		Config.stripe.webhookSecret = 'whsec_test_secret';
		Config.stripe.prices = MOCK_PRICES;
		stripeHandlers = createStripeApiHandlers();
		server.use(...stripeHandlers.handlers);
		paymentRepository = new PaymentRepository();
		userRepository = new UserRepository();
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.webhookSecret = originalWebhookSecret;
		Config.stripe.prices = originalPrices;
	});
	beforeEach(async () => {
		server.use(...stripeHandlers.handlers);
		await harness.resetData();
		stripeHandlers.reset();
	});
	afterEach(() => {
		server.resetHandlers();
		server.use(...stripeHandlers.handlers);
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
	describe('customer.subscription.updated', () => {
		test('updates subscription cancellation status when cancel_at_period_end is true', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const subscriptionId = 'sub_test_monthly';
			const sessionId = `cs_test_cancel_${Date.now()}`;
			const cancelAt = Math.floor(Date.now() / 1000) + 30 * 24 * 60 * 60;
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: userId,
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepository.updatePayment({
				checkout_session_id: sessionId,
				subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				status: 'completed',
			});
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_1',
					premium_since: new Date(),
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const eventData = createSubscriptionUpdatedEvent({
				subscriptionId,
				cancelAtPeriodEnd: true,
			});
			eventData.data.object.cancel_at = cancelAt;
			eventData.data.object.items = {
				data: [{current_period_end: cancelAt}],
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const response = await harness.requestJson({
				path: '/users/@me',
				method: 'GET',
				headers: {authorization: account.token},
			});
			const user = (await response.json()) as {
				premium_will_cancel: boolean;
				premium_until: string | null;
			};
			expect(user.premium_will_cancel).toBe(true);
			expect(user.premium_until).not.toBeNull();
		});
		test('preserves gifted extension when updating subscription', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const subscriptionId = 'sub_test_gifted';
			const sessionId = `cs_test_gifted_${Date.now()}`;
			const currentPeriodEnd = Math.floor(Date.now() / 1000) + 30 * 24 * 60 * 60;
			const initialPremiumUntil = new Date(Date.now() + 10 * 24 * 60 * 60 * 1000);
			const giftExtensionEndsAt = new Date(Date.now() + 90 * 24 * 60 * 60 * 1000);
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: userId,
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepository.updatePayment({
				checkout_session_id: sessionId,
				subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				status: 'completed',
			});
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: initialPremiumUntil,
					premium_gift_extension_ends_at: giftExtensionEndsAt,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_1',
					premium_since: new Date(),
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const eventData = createSubscriptionUpdatedEvent({
				subscriptionId,
				cancelAtPeriodEnd: false,
			});
			eventData.data.object.cancel_at = null;
			eventData.data.object.items = {
				data: [{current_period_end: currentPeriodEnd}],
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const response = await harness.requestJson({
				path: '/users/@me',
				method: 'GET',
				headers: {authorization: account.token},
			});
			const user = (await response.json()) as {
				premium_will_cancel: boolean;
				premium_until: string | null;
			};
			expect(user.premium_will_cancel).toBe(false);
			expect(user.premium_until).not.toBeNull();
			const premiumUntil = new Date(user.premium_until!);
			expect(premiumUntil.getTime()).toBeGreaterThan(new Date(currentPeriodEnd * 1000).getTime());
			const updatedUser = await userRepository.findUnique(userId);
			expect(updatedUser?.premiumUntil?.toISOString()).toBe(new Date(currentPeriodEnd * 1000).toISOString());
			expect(updatedUser?.premiumGiftExtensionEndsAt?.toISOString()).toBe(giftExtensionEndsAt.toISOString());
		});
		test('does not grant unpaid future time for past_due subscriptions and disables grace', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const subscriptionId = 'sub_test_past_due';
			const sessionId = `cs_test_past_due_${Date.now()}`;
			const existingPremiumUntil = new Date(Date.now() + 45 * 24 * 60 * 60 * 1000);
			const unpaidFuturePeriodEnd = Math.floor(Date.now() / 1000) + 90 * 24 * 60 * 60;
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: userId,
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepository.updatePayment({
				checkout_session_id: sessionId,
				subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_past_due',
				status: 'completed',
			});
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: existingPremiumUntil,
					premium_will_cancel: false,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_past_due',
					premium_since: new Date(),
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const eventData = createSubscriptionUpdatedEvent({
				subscriptionId,
				customerId: 'cus_test_past_due',
				status: 'past_due',
				cancelAtPeriodEnd: false,
			});
			eventData.data.object.cancel_at = null;
			eventData.data.object.items = {
				data: [{current_period_end: unpaidFuturePeriodEnd}],
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const updatedUser = await userRepository.findUnique(userId);
			expect(updatedUser?.premiumWillCancel).toBe(true);
			expect(updatedUser?.premiumUntil?.toISOString()).toBe(existingPremiumUntil.toISOString());
			expect(updatedUser?.stripeSubscriptionId).toBe(subscriptionId);
			expect(updatedUser?.stripeCustomerId).toBe('cus_test_past_due');
		});
		test('does not clear premium when period end is missing', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const subscriptionId = 'sub_test_missing_period';
			const sessionId = `cs_test_missing_period_${Date.now()}`;
			const futureDate = new Date(Date.now() + 90 * 24 * 60 * 60 * 1000);
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: userId,
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepository.updatePayment({
				checkout_session_id: sessionId,
				subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				status: 'completed',
			});
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: futureDate,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_1',
					premium_since: new Date(),
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const eventData = createSubscriptionUpdatedEvent({
				subscriptionId,
				cancelAtPeriodEnd: false,
			});
			eventData.data.object.items = {data: []};
			server.use(
				http.get('https://api.stripe.com/v1/subscriptions/:id', ({params}) =>
					HttpResponse.json({
						id: params.id,
						object: 'subscription',
						customer: 'cus_test_1',
						status: 'active',
						cancel_at: null,
						cancel_at_period_end: false,
						items: {
							data: [
								{
									id: 'si_missing_period',
									object: 'subscription_item',
									price: {
										id: MOCK_PRICES.monthlyUsd,
										object: 'price',
										unit_amount: 2500,
										currency: 'usd',
										recurring: {interval: 'month', interval_count: 1},
										type: 'recurring',
										active: true,
										livemode: false,
									},
									quantity: 1,
								},
							],
						},
						livemode: false,
					}),
				),
			);
			await sendWebhookExpectStripeError(eventData);
			const response = await harness.requestJson({
				path: '/users/@me',
				method: 'GET',
				headers: {authorization: account.token},
			});
			const user = (await response.json()) as {
				premium_until: string | null;
			};
			expect(user.premium_until).not.toBeNull();
			const premiumUntil = new Date(user.premium_until!);
			expect(premiumUntil.getTime()).toBeGreaterThanOrEqual(futureDate.getTime() - 1000);
		});
		test('handles customer.subscription.pending_update_applied like a subscription update', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const subscriptionId = 'sub_test_pending_update_applied';
			const sessionId = `cs_test_pending_update_applied_${Date.now()}`;
			const currentPeriodEnd = Math.floor(Date.now() / 1000) + 30 * 24 * 60 * 60;
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: userId,
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepository.updatePayment({
				checkout_session_id: sessionId,
				subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_pending_update_applied',
				status: 'completed',
			});
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_pending_update_applied',
					premium_since: new Date(),
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const eventData = createSubscriptionUpdatedEvent({
				subscriptionId,
				customerId: 'cus_test_pending_update_applied',
				status: 'active',
				cancelAtPeriodEnd: false,
			});
			eventData.type = 'customer.subscription.pending_update_applied';
			eventData.data.object.items = {
				data: [{current_period_end: currentPeriodEnd}],
			};
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const me = await createBuilder<{
				premium_until: string | null;
			}>(harness, account.token)
				.get('/users/@me')
				.execute();
			expect(me.premium_until).not.toBeNull();
		});
	});
	describe('customer.subscription.deleted', () => {
		test('starts grace for non-lifetime users on subscription deletion', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const subscriptionId = 'sub_test_to_delete';
			const sessionId = `cs_test_delete_${Date.now()}`;
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: userId,
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepository.updatePayment({
				checkout_session_id: sessionId,
				subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				status: 'completed',
			});
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_1',
					premium_since: new Date(),
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const beforeUser = await userRepository.findUnique(userId);
			expect(beforeUser?.premiumType).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(beforeUser?.stripeSubscriptionId).toBe(subscriptionId);
			const eventData = createSubscriptionDeletedEvent({subscriptionId});
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const afterUser = await userRepository.findUnique(userId);
			expect(afterUser?.premiumType).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(afterUser?.premiumGraceEndsAt).not.toBeNull();
			expect(afterUser?.premiumWillCancel).toBe(false);
			expect(afterUser?.stripeSubscriptionId).toBeNull();
		});
		test('preserves lifetime premium on subscription deletion', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const subscriptionId = 'sub_test_lifetime_user';
			const sessionId = `cs_test_lifetime_${Date.now()}`;
			await paymentRepository.createPayment({
				checkout_session_id: sessionId,
				user_id: userId,
				price_id: MOCK_PRICES.monthlyUsd,
				product_type: ProductType.MONTHLY_SUBSCRIPTION,
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepository.updatePayment({
				checkout_session_id: sessionId,
				subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				status: 'completed',
			});
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.LIFETIME,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_1',
					premium_since: new Date(),
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const beforeUser = await userRepository.findUnique(userId);
			expect(beforeUser?.premiumType).toBe(UserPremiumTypes.LIFETIME);
			const eventData = createSubscriptionDeletedEvent({subscriptionId});
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const afterUser = await userRepository.findUnique(userId);
			expect(afterUser?.premiumType).toBe(UserPremiumTypes.LIFETIME);
			expect(afterUser?.stripeSubscriptionId).toBeNull();
		});
		test('cuts premium off at the cancellation moment when the subscription is cancelled before its paid period ends', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const subscriptionId = 'sub_test_cancel_early';
			const premiumUntil = new Date(Date.now() + 30 * 24 * 60 * 60 * 1000);
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_1',
					premium_since: new Date(),
					premium_until: premiumUntil,
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const endedAt = Math.floor(Date.now() / 1000) - 60;
			const eventData = createSubscriptionDeletedEvent({subscriptionId, endedAt});
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const afterUser = await userRepository.findUnique(userId);
			expect(afterUser?.premiumUntil?.getTime()).toBe(endedAt * 1000);
			expect(afterUser?.premiumGraceEndsAt?.getTime()).toBe(endedAt * 1000);
			expect(afterUser?.stripeSubscriptionId).toBeNull();
			const {checkHasActivePaidPremium} = await import('../../user/UserHelpers');
			expect(checkHasActivePaidPremium(afterUser!)).toBe(false);
		});
		test('grants standard grace when the subscription is cancelled at the end of its paid period', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const subscriptionId = 'sub_test_cancel_natural';
			const premiumUntil = new Date(Date.now() - 60 * 1000);
			await userRepository.patchUpsert(
				userId,
				{
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					stripe_subscription_id: subscriptionId,
					stripe_customer_id: 'cus_test_1',
					premium_since: new Date(),
					premium_until: premiumUntil,
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const endedAt = Math.floor(Date.now() / 1000);
			const eventData = createSubscriptionDeletedEvent({subscriptionId, endedAt});
			const result = await sendWebhook(eventData);
			expect(result.received).toBe(true);
			const afterUser = await userRepository.findUnique(userId);
			expect(afterUser?.premiumUntil?.getTime()).toBe(premiumUntil.getTime());
			expect(afterUser?.premiumGraceEndsAt).not.toBeNull();
			expect(afterUser!.premiumGraceEndsAt!.getTime()).toBe(endedAt * 1000 + 3 * 24 * 60 * 60 * 1000);
			const {checkHasActivePaidPremium} = await import('../../user/UserHelpers');
			expect(checkHasActivePaidPremium(afterUser!)).toBe(true);
		});
		test('processes donation subscription deletion', async () => {
			const subscriptionId = 'sub_donor_delete_test';
			const donorEmail = 'donor-delete@example.com';
			const {DonationRepository} = await import('../../donation/DonationRepository');
			const donationRepository = new DonationRepository();
			await donationRepository.createDonor({
				email: donorEmail,
				stripeCustomerId: 'cus_donor_delete_123',
				businessName: null,
				taxId: null,
				taxIdType: null,
				stripeSubscriptionId: subscriptionId,
				subscriptionAmountCents: 2500,
				subscriptionCurrency: 'usd',
				subscriptionInterval: 'month',
				subscriptionCurrentPeriodEnd: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
			});
			const donorBefore = await donationRepository.findDonorByEmail(donorEmail);
			expect(donorBefore).not.toBeNull();
			expect(donorBefore?.stripeSubscriptionId).toBe(subscriptionId);
			const deleteEvent = createSubscriptionDeletedEvent({subscriptionId});
			const deleteResult = await sendWebhook(deleteEvent);
			expect(deleteResult.received).toBe(true);
			const donorAfter = await donationRepository.findDonorByEmail(donorEmail);
			expect(donorAfter?.stripeSubscriptionId).toBeNull();
		});
	});
});

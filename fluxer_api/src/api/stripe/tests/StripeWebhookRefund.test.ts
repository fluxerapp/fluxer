// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {HttpResponse, http} from 'msw';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createUserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createMockWebhookPayload, type StripeWebhookEventData} from '../../test/msw/handlers/StripeApiHandlers';
import {server} from '../../test/msw/server';
import {createBuilder} from '../../test/TestRequestBuilder';
import {UserRepository} from '../../user/repositories/UserRepository';
import {setupSyncStripeWebhookWorker} from './StripeWebhookTestUtils';

describe('Stripe Webhook Refund', () => {
	let harness: ApiTestHarness;
	let originalWebhookSecret: string | undefined;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		setupSyncStripeWebhookWorker();
		originalWebhookSecret = Config.stripe.webhookSecret;
		Config.stripe.webhookSecret = 'whsec_test_secret';
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.webhookSecret = originalWebhookSecret;
	});
	beforeEach(async () => {
		await harness.resetData();
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
	describe('charge.refunded', () => {
		test('revokes premium and records first refund', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const {PaymentRepository} = await import('../../user/repositories/PaymentRepository');
			const {UserRepository} = await import('../../user/repositories/UserRepository');
			const paymentRepository = new PaymentRepository();
			const userRepository = new UserRepository();
			const paymentIntentId = 'pi_test_refund_first_123';
			const checkoutSessionId = 'cs_test_refund_first_123';
			await paymentRepository.createPayment({
				checkout_session_id: checkoutSessionId,
				user_id: userId,
				price_id: 'price_test_monthly',
				product_type: 'monthly_subscription',
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepository.updatePayment({
				checkout_session_id: checkoutSessionId,
				payment_intent_id: paymentIntentId,
				completed_at: new Date(),
			});
			await sendWebhook({
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_refund_123',
						payment_intent: paymentIntentId,
						amount_refunded: 2500,
					},
				},
			});
			const updatedUser = await userRepository.findUnique(userId);
			expect(updatedUser).not.toBeNull();
			expect(updatedUser!.firstRefundAt).not.toBeNull();
			const updatedPayment = await userRepository.getPaymentByPaymentIntent(paymentIntentId);
			expect(updatedPayment).not.toBeNull();
			expect(updatedPayment!.status).toBe('refunded');
		});
		test('applies permanent purchase block on second refund', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const {UserRepository} = await import('../../user/repositories/UserRepository');
			const userRepository = new UserRepository();
			const firstRefundDate = new Date('2024-01-01');
			await userRepository.patchUpsert(
				userId,
				{
					first_refund_at: firstRefundDate,
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const {PaymentRepository} = await import('../../user/repositories/PaymentRepository');
			const paymentRepository = new PaymentRepository();
			const paymentIntentId = 'pi_test_refund_second_456';
			const checkoutSessionId = 'cs_test_refund_second_456';
			await paymentRepository.createPayment({
				checkout_session_id: checkoutSessionId,
				user_id: userId,
				price_id: 'price_test_monthly',
				product_type: 'monthly_subscription',
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepository.updatePayment({
				checkout_session_id: checkoutSessionId,
				payment_intent_id: paymentIntentId,
				completed_at: new Date(),
			});
			await sendWebhook({
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_refund_456',
						payment_intent: paymentIntentId,
						amount_refunded: 2500,
					},
				},
			});
			const updatedUser = await userRepository.findUnique(userId);
			expect(updatedUser).not.toBeNull();
			expect(updatedUser!.firstRefundAt).toEqual(firstRefundDate);
			expect(updatedUser!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(PremiumFlags.PURCHASE_DISABLED);
			const updatedPayment = await userRepository.getPaymentByPaymentIntent(paymentIntentId);
			expect(updatedPayment).not.toBeNull();
			expect(updatedPayment!.status).toBe('refunded');
		});
		test('falls back to customer ID when payment intent is not indexed (subscription mode)', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const {UserRepository} = await import('../../user/repositories/UserRepository');
			const userRepository = new UserRepository();
			const stripeCustomerId = 'cus_test_subscription_fallback';
			await userRepository.patchUpsert(
				userId,
				{stripe_customer_id: stripeCustomerId},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			await sendWebhook({
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_refund_sub_789',
						payment_intent: 'pi_test_not_indexed_789',
						customer: stripeCustomerId,
						amount_refunded: 4999,
					},
				},
			});
			const updatedUser = await userRepository.findUnique(userId);
			expect(updatedUser).not.toBeNull();
			expect(updatedUser!.firstRefundAt).not.toBeNull();
		});
		test('skips premium action for donation customer refund', async () => {
			const donationCustomerId = 'cus_test_donation_refund';
			const {DonationRepository} = await import('../../donation/DonationRepository');
			const donationRepository = new DonationRepository();
			await donationRepository.createDonor({
				email: 'donor-refund-test@example.com',
				stripeCustomerId: donationCustomerId,
				businessName: null,
				taxId: null,
				taxIdType: null,
				stripeSubscriptionId: null,
				subscriptionAmountCents: null,
				subscriptionCurrency: null,
				subscriptionInterval: null,
				subscriptionCurrentPeriodEnd: null,
				subscriptionCancelAt: null,
			});
			const result = await sendWebhook({
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_refund_donation_101',
						payment_intent: 'pi_test_donation_not_indexed_101',
						customer: donationCustomerId,
						amount_refunded: 2500,
					},
				},
			});
			expect(result.received).toBe(true);
		});
		test('preserves remaining premium when another redeemed gift is still active', async () => {
			const purchaser = await createTestAccount(harness);
			const redeemer = await createTestAccount(harness);
			const userRepository = new UserRepository();
			await userRepository.createGiftCode({
				code: 'GIFT_REFUND_FIRST',
				duration_months: null,
				duration_type: 'months',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: new Date(),
				redeemed_by_user_id: createUserID(BigInt(redeemer.userId)),
				stripe_payment_intent_id: 'pi_test_gift_refund_first',
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			await userRepository.createGiftCode({
				code: 'GIFT_REFUND_SECOND',
				duration_months: null,
				duration_type: 'months',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: new Date(),
				redeemed_by_user_id: createUserID(BigInt(redeemer.userId)),
				stripe_payment_intent_id: 'pi_test_gift_refund_second',
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			await createBuilder(harness, redeemer.token)
				.post(`/test/users/${redeemer.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: new Date(Date.now() + 60 * 24 * 60 * 60 * 1000).toISOString(),
				})
				.execute();
			await sendWebhook({
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_refund_gift_multi_123',
						payment_intent: 'pi_test_gift_refund_first',
						amount_refunded: 2500,
					},
				},
			});
			const updatedRedeemer = await createBuilder<{
				premium_type: number;
				premium_until: string | null;
			}>(harness, redeemer.token)
				.get('/users/@me')
				.execute();
			expect(updatedRedeemer.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(updatedRedeemer.premium_until).not.toBeNull();
		});
		test('handles gift refund when redeemer has stripe_customer_id but no active subscription', async () => {
			const purchaser = await createTestAccount(harness);
			const redeemer = await createTestAccount(harness);
			const userRepository = new UserRepository();
			const redeemerId = createUserID(BigInt(redeemer.userId));
			await userRepository.createGiftCode({
				code: 'GIFT_REFUND_STRIPE_CUS_ONLY',
				duration_months: null,
				duration_type: 'months',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: new Date(),
				redeemed_by_user_id: redeemerId,
				stripe_payment_intent_id: 'pi_test_gift_refund_stripe_cus',
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			await createBuilder(harness, redeemer.token)
				.post(`/test/users/${redeemer.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString(),
					stripe_customer_id: 'cus_test_refund_redeemer',
				})
				.execute();
			const redeemerBefore = await createBuilder<{
				premium_type: number;
				premium_until: string | null;
			}>(harness, redeemer.token)
				.get('/users/@me')
				.execute();
			expect(redeemerBefore.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			await sendWebhook({
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_refund_stripe_cus_only',
						payment_intent: 'pi_test_gift_refund_stripe_cus',
						amount_refunded: 2500,
					},
				},
			});
			const updatedRedeemer = await userRepository.findUnique(redeemerId);
			expect(updatedRedeemer).not.toBeNull();
			expect(updatedRedeemer!.stripeCustomerId).toBe('cus_test_refund_redeemer');
		});
		test('preserves lifetime premium overrides on gift refund', async () => {
			const purchaser = await createTestAccount(harness);
			const redeemer = await createTestAccount(harness);
			const userRepository = new UserRepository();
			await userRepository.createGiftCode({
				code: 'GIFT_REFUND_OVERRIDE',
				duration_months: null,
				duration_type: 'months',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: new Date(),
				redeemed_by_user_id: createUserID(BigInt(redeemer.userId)),
				stripe_payment_intent_id: 'pi_test_gift_refund_override',
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			await createBuilder(harness, redeemer.token)
				.post(`/test/users/${redeemer.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.LIFETIME,
				})
				.execute();
			await sendWebhook({
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_refund_gift_override_123',
						payment_intent: 'pi_test_gift_refund_override',
						amount_refunded: 2500,
					},
				},
			});
			const updatedRedeemer = await createBuilder<{
				premium_type: number;
			}>(harness, redeemer.token)
				.get('/users/@me')
				.execute();
			expect(updatedRedeemer.premium_type).toBe(UserPremiumTypes.LIFETIME);
		});
	});
	describe('refund.updated', () => {
		test('finalizes self-serve cooldown and cancels the subscription once the refund is confirmed succeeded', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const userRepository = new UserRepository();
			const subscriptionId = 'sub_test_webhook_finalize';
			await userRepository.patchUpsert(
				userId,
				{stripe_subscription_id: subscriptionId},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			server.use(
				http.delete('https://api.stripe.com/v1/subscriptions/:id', ({params}) =>
					HttpResponse.json({id: params.id, object: 'subscription', status: 'canceled'}),
				),
			);
			await sendWebhook({
				type: 'refund.updated',
				data: {
					object: {
						id: 'pyr_test_webhook_finalize',
						status: 'succeeded',
						amount: 2499,
						currency: 'brl',
						metadata: {
							refund_kind: 'self_serve',
							user_id: account.userId.toString(),
							invoice_id: 'in_test_webhook_finalize',
							subscription_id: subscriptionId,
						},
					},
				},
			});
			const updatedUser = await userRepository.findUnique(userId);
			expect(updatedUser!.firstRefundAt).not.toBeNull();
		});
		test('does not finalize cooldown while the refund is still pending', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const userRepository = new UserRepository();
			await sendWebhook({
				type: 'refund.updated',
				data: {
					object: {
						id: 'pyr_test_webhook_pending',
						status: 'pending',
						amount: 2499,
						currency: 'brl',
						metadata: {
							refund_kind: 'self_serve',
							user_id: account.userId.toString(),
							invoice_id: 'in_test_webhook_pending',
						},
					},
				},
			});
			const updatedUser = await userRepository.findUnique(userId);
			expect(updatedUser!.firstRefundAt).toBeNull();
		});
	});
	describe('refund.failed', () => {
		test('does not finalize cooldown when the refund ultimately fails', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const userRepository = new UserRepository();
			await sendWebhook({
				type: 'refund.failed',
				data: {
					object: {
						id: 'pyr_test_webhook_failed',
						status: 'failed',
						failure_reason: 'unknown',
						amount: 2499,
						currency: 'brl',
						metadata: {
							refund_kind: 'self_serve',
							user_id: account.userId.toString(),
							invoice_id: 'in_test_webhook_failed',
						},
					},
				},
			});
			const updatedUser = await userRepository.findUnique(userId);
			expect(updatedUser!.firstRefundAt).toBeNull();
		});
	});
});

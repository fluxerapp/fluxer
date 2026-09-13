// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {
	mockStripeWebhookSecret,
	restoreStripeWebhookSecret,
	setupSyncStripeWebhookWorker,
} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	createMockWebhookPayload,
	createStripeApiHandlers,
	type StripeWebhookEventData,
} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface UserDataExistsResponse {
	user_exists: boolean;
	has_deleted_flag: boolean;
	has_self_deleted_flag: boolean;
	pending_deletion_at: string | null;
	deletion_reason_code: string | null;
	flags: string;
}

describe('Stripe Webhook Dispute Events', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		mockStripeWebhookSecret('whsec_test_dispute');
		setupSyncStripeWebhookWorker();
	});
	afterAll(async () => {
		await harness.shutdown();
		restoreStripeWebhookSecret();
	});
	beforeEach(async () => {
		await harness.resetData();
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
	describe('charge.dispute.created', () => {
		test('schedules account deletion on chargeback for direct purchase', async () => {
			const purchaser = await createTestAccount(harness);
			const paymentIntentId = 'pi_test_chargeback_123';
			const paymentRepo = new PaymentRepository();
			await paymentRepo.createPayment({
				checkout_session_id: 'cs_test_chargeback',
				user_id: createUserID(BigInt(purchaser.userId)),
				price_id: 'price_test_monthly',
				product_type: 'monthly_subscription',
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepo.updatePayment({
				checkout_session_id: 'cs_test_chargeback',
				payment_intent_id: paymentIntentId,
			});
			const eventData: StripeWebhookEventData = {
				type: 'charge.dispute.created',
				data: {
					object: {
						id: 'dp_test_123',
						payment_intent: paymentIntentId,
						status: 'needs_response',
					},
				},
			};
			await sendWebhook(eventData);
			const updatedUser = await createBuilderWithoutAuth<UserDataExistsResponse>(harness)
				.get(`/test/users/${purchaser.userId}/data-exists`)
				.execute();
			expect(updatedUser.has_deleted_flag).toBe(true);
			expect(updatedUser.deletion_reason_code).toBe(DeletionReasons.BILLING_DISPUTE_OR_ABUSE);
			expect(updatedUser.pending_deletion_at).not.toBeNull();
			const deletionDate = new Date(updatedUser.pending_deletion_at!);
			const now = new Date();
			const daysDifference = (deletionDate.getTime() - now.getTime()) / (24 * 60 * 60 * 1000);
			expect(daysDifference).toBeGreaterThan(58);
			expect(daysDifference).toBeLessThan(62);
		});
		test('reports fraudulent disputes to Stripe and populates active default blocklists', async () => {
			const purchaser = await createTestAccount(harness);
			const paymentIntentId = 'pi_test_chargeback_fraud_123';
			const customerId = 'cus_test_chargeback_fraud';
			const chargeId = 'ch_test_chargeback_fraud';
			const billingEmail = 'fraudster@example.com';
			const customerPurchaseIp = '203.0.113.42';
			const cardFingerprint = 'fp_test_chargeback_fraud';
			const paymentRepo = new PaymentRepository();
			const stripeHandlers = createStripeApiHandlers({
				charges: {
					[chargeId]: {
						billing_details: {email: billingEmail},
						customer: customerId,
						payment_intent: paymentIntentId,
						payment_method_details: {
							card: {fingerprint: cardFingerprint},
						},
					},
				},
			});
			server.use(...stripeHandlers.handlers);
			await paymentRepo.createPayment({
				checkout_session_id: 'cs_test_chargeback_fraud',
				user_id: createUserID(BigInt(purchaser.userId)),
				price_id: 'price_test_monthly',
				product_type: 'monthly_subscription',
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepo.updatePayment({
				checkout_session_id: 'cs_test_chargeback_fraud',
				payment_intent_id: paymentIntentId,
				stripe_customer_id: customerId,
			});
			await sendWebhook({
				type: 'charge.dispute.created',
				data: {
					object: {
						charge: chargeId,
						evidence: {
							customer_email_address: billingEmail,
							customer_purchase_ip: customerPurchaseIp,
						},
						id: 'dp_test_chargeback_fraud',
						payment_intent: paymentIntentId,
						reason: 'fraudulent',
						status: 'needs_response',
					},
				},
			});
			expect(stripeHandlers.spies.updatedCharges).toEqual([
				{
					id: chargeId,
					params: {
						fraud_details: {
							user_report: 'fraudulent',
						},
					},
				},
			]);
			expect(stripeHandlers.spies.createdValueListItems).toEqual(
				expect.arrayContaining([
					{value: billingEmail, valueListId: expect.any(String)},
					{value: cardFingerprint, valueListId: expect.any(String)},
					{value: customerId, valueListId: expect.any(String)},
					{value: customerPurchaseIp, valueListId: expect.any(String)},
				]),
			);
		});
		test('revokes premium and schedules deletion for gift chargeback', async () => {
			const purchaser = await createTestAccount(harness);
			const redeemer = await createTestAccount(harness);
			const paymentIntentId = 'pi_test_gift_chargeback_123';
			const giftCode = 'GIFT_CHARGEBACK_TEST';
			const userRepository = new UserRepository();
			const paymentRepo = new PaymentRepository();
			await userRepository.createGiftCode({
				code: giftCode,
				duration_months: null,
				duration_type: 'months',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: new Date(),
				redeemed_by_user_id: createUserID(BigInt(redeemer.userId)),
				stripe_payment_intent_id: paymentIntentId,
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			await paymentRepo.createPayment({
				checkout_session_id: 'cs_test_gift_chargeback',
				user_id: createUserID(BigInt(purchaser.userId)),
				price_id: 'price_test_monthly',
				product_type: 'monthly_subscription',
				status: 'completed',
				is_gift: true,
				created_at: new Date(),
			});
			await paymentRepo.updatePayment({
				checkout_session_id: 'cs_test_gift_chargeback',
				payment_intent_id: paymentIntentId,
				gift_code: giftCode,
			});
			await createBuilder(harness, redeemer.token)
				.post(`/test/users/${redeemer.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString(),
				})
				.execute();
			const redeemerBeforeDispute = await createBuilder<{
				premium_type: number;
			}>(harness, redeemer.token)
				.get('/users/@me')
				.execute();
			expect(redeemerBeforeDispute.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			const eventData: StripeWebhookEventData = {
				type: 'charge.dispute.created',
				data: {
					object: {
						id: 'dp_test_gift_123',
						payment_intent: paymentIntentId,
						status: 'needs_response',
					},
				},
			};
			await sendWebhook(eventData);
			const redeemerAfterDispute = await createBuilder<{
				premium_type: number;
			}>(harness, redeemer.token)
				.get('/users/@me')
				.execute();
			expect(redeemerAfterDispute.premium_type).toBe(UserPremiumTypes.NONE);
			const purchaserAfterDispute = await createBuilderWithoutAuth<UserDataExistsResponse>(harness)
				.get(`/test/users/${purchaser.userId}/data-exists`)
				.execute();
			expect(purchaserAfterDispute.has_deleted_flag).toBe(true);
			expect(purchaserAfterDispute.deletion_reason_code).toBe(DeletionReasons.BILLING_DISPUTE_OR_ABUSE);
			expect(purchaserAfterDispute.pending_deletion_at).not.toBeNull();
		});
		test('preserves remaining premium when other redeemed gifts still cover the user', async () => {
			const purchaser = await createTestAccount(harness);
			const redeemer = await createTestAccount(harness);
			const userRepository = new UserRepository();
			const redeemedAt = new Date();
			const expectedRemainingPremiumUntil = new Date(redeemedAt.getTime() + 7 * 24 * 60 * 60 * 1000);
			await userRepository.createGiftCode({
				code: 'GIFT_CHARGEBACK_FIRST',
				duration_months: null,
				duration_type: 'weeks',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: redeemedAt,
				redeemed_by_user_id: createUserID(BigInt(redeemer.userId)),
				stripe_payment_intent_id: 'pi_test_gift_chargeback_first',
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			await userRepository.createGiftCode({
				code: 'GIFT_CHARGEBACK_SECOND',
				duration_months: null,
				duration_type: 'weeks',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: redeemedAt,
				redeemed_by_user_id: createUserID(BigInt(redeemer.userId)),
				stripe_payment_intent_id: 'pi_test_gift_chargeback_second',
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			await createBuilder(harness, redeemer.token)
				.post(`/test/users/${redeemer.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_gift_extension_ends_at: new Date(redeemedAt.getTime() + 2 * 7 * 24 * 60 * 60 * 1000).toISOString(),
				})
				.execute();
			await sendWebhook({
				type: 'charge.dispute.created',
				data: {
					object: {
						id: 'dp_test_gift_multi_123',
						payment_intent: 'pi_test_gift_chargeback_first',
						status: 'needs_response',
					},
				},
			});
			const redeemerAfterDispute = await createBuilder<{
				premium_type: number;
				premium_until: string | null;
			}>(harness, redeemer.token)
				.get('/users/@me')
				.execute();
			expect(redeemerAfterDispute.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(redeemerAfterDispute.premium_until).not.toBeNull();
			const premiumUntil = new Date(redeemerAfterDispute.premium_until!);
			expect(Math.abs(premiumUntil.getTime() - expectedRemainingPremiumUntil.getTime())).toBeLessThan(60 * 1000);
		});
		test('handles chargeback when redeemer has stripe_customer_id but no active subscription', async () => {
			const purchaser = await createTestAccount(harness);
			const redeemer = await createTestAccount(harness);
			const paymentIntentId = 'pi_test_gift_chargeback_stripe_cus';
			const giftCode = 'GIFT_CHARGEBACK_STRIPE_CUS_ONLY';
			const userRepository = new UserRepository();
			const paymentRepo = new PaymentRepository();
			const redeemerId = createUserID(BigInt(redeemer.userId));
			await userRepository.createGiftCode({
				code: giftCode,
				duration_months: null,
				duration_type: 'months',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: new Date(),
				redeemed_by_user_id: redeemerId,
				stripe_payment_intent_id: paymentIntentId,
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			await paymentRepo.createPayment({
				checkout_session_id: 'cs_test_gift_chargeback_stripe_cus',
				user_id: createUserID(BigInt(purchaser.userId)),
				price_id: 'price_test_monthly',
				product_type: 'monthly_subscription',
				status: 'completed',
				is_gift: true,
				created_at: new Date(),
			});
			await paymentRepo.updatePayment({
				checkout_session_id: 'cs_test_gift_chargeback_stripe_cus',
				payment_intent_id: paymentIntentId,
				gift_code: giftCode,
			});
			await createBuilder(harness, redeemer.token)
				.post(`/test/users/${redeemer.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString(),
					stripe_customer_id: 'cus_test_chargeback_redeemer',
				})
				.execute();
			const redeemerBefore = await createBuilder<{
				premium_type: number;
			}>(harness, redeemer.token)
				.get('/users/@me')
				.execute();
			expect(redeemerBefore.premium_type).toBe(UserPremiumTypes.SUBSCRIPTION);
			await sendWebhook({
				type: 'charge.dispute.created',
				data: {
					object: {
						id: 'dp_test_gift_stripe_cus_only',
						payment_intent: paymentIntentId,
						status: 'needs_response',
					},
				},
			});
			const updatedRedeemer = await userRepository.findUnique(redeemerId);
			expect(updatedRedeemer).not.toBeNull();
			expect(updatedRedeemer!.stripeCustomerId).toBe('cus_test_chargeback_redeemer');
			const purchaserAfterDispute = await createBuilderWithoutAuth<UserDataExistsResponse>(harness)
				.get(`/test/users/${purchaser.userId}/data-exists`)
				.execute();
			expect(purchaserAfterDispute.has_deleted_flag).toBe(true);
		});
		test('preserves lifetime premium overrides during gift chargeback handling', async () => {
			const purchaser = await createTestAccount(harness);
			const redeemer = await createTestAccount(harness);
			const paymentIntentId = 'pi_test_gift_chargeback_lifetime_override';
			const userRepository = new UserRepository();
			await userRepository.createGiftCode({
				code: 'GIFT_CHARGEBACK_OVERRIDE',
				duration_months: null,
				duration_type: 'months',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: new Date(),
				redeemed_by_user_id: createUserID(BigInt(redeemer.userId)),
				stripe_payment_intent_id: paymentIntentId,
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
				type: 'charge.dispute.created',
				data: {
					object: {
						id: 'dp_test_gift_override_123',
						payment_intent: paymentIntentId,
						status: 'needs_response',
					},
				},
			});
			const redeemerAfterDispute = await createBuilder<{
				premium_type: number;
			}>(harness, redeemer.token)
				.get('/users/@me')
				.execute();
			expect(redeemerAfterDispute.premium_type).toBe(UserPremiumTypes.LIFETIME);
		});
		test('handles dispute for payment intent not found gracefully', async () => {
			const eventData: StripeWebhookEventData = {
				type: 'charge.dispute.created',
				data: {
					object: {
						id: 'dp_test_unknown',
						payment_intent: 'pi_test_does_not_exist',
						status: 'needs_response',
					},
				},
			};
			await sendWebhookExpectStripeError(eventData);
		});
	});
	describe('charge.dispute.closed', () => {
		test('unsuspends account when chargeback is won', async () => {
			const purchaser = await createTestAccount(harness);
			const paymentIntentId = 'pi_test_dispute_won_123';
			const paymentRepo = new PaymentRepository();
			await paymentRepo.createPayment({
				checkout_session_id: 'cs_test_dispute_won',
				user_id: createUserID(BigInt(purchaser.userId)),
				price_id: 'price_test_monthly',
				product_type: 'monthly_subscription',
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepo.updatePayment({
				checkout_session_id: 'cs_test_dispute_won',
				payment_intent_id: paymentIntentId,
			});
			const userRepository = new UserRepository();
			const userId = createUserID(BigInt(purchaser.userId));
			const user = await userRepository.findUniqueAssert(userId);
			await userRepository.updateDeletionSchedule(user, {
				flags: (user.flags | UserFlags.DELETED) & ~UserFlags.SELF_DELETED,
				deletion_reason_code: DeletionReasons.BILLING_DISPUTE_OR_ABUSE,
				pending_deletion_at: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
			});
			const userBeforeWin = await createBuilderWithoutAuth<UserDataExistsResponse>(harness)
				.get(`/test/users/${purchaser.userId}/data-exists`)
				.execute();
			expect(userBeforeWin.has_deleted_flag).toBe(true);
			expect(userBeforeWin.deletion_reason_code).toBe(DeletionReasons.BILLING_DISPUTE_OR_ABUSE);
			const eventData: StripeWebhookEventData = {
				type: 'charge.dispute.closed',
				data: {
					object: {
						id: 'dp_test_won',
						payment_intent: paymentIntentId,
						status: 'won',
					},
				},
			};
			await sendWebhook(eventData);
			const userAfterWin = await createBuilderWithoutAuth<UserDataExistsResponse>(harness)
				.get(`/test/users/${purchaser.userId}/data-exists`)
				.execute();
			expect(userAfterWin.has_deleted_flag).toBe(false);
			expect(userAfterWin.deletion_reason_code).toBeNull();
			expect(userAfterWin.pending_deletion_at).toBeNull();
		});
		test('does not unsuspend when chargeback is lost', async () => {
			const purchaser = await createTestAccount(harness);
			const paymentIntentId = 'pi_test_dispute_lost_123';
			const paymentRepo = new PaymentRepository();
			await paymentRepo.createPayment({
				checkout_session_id: 'cs_test_dispute_lost',
				user_id: createUserID(BigInt(purchaser.userId)),
				price_id: 'price_test_monthly',
				product_type: 'monthly_subscription',
				status: 'completed',
				is_gift: false,
				created_at: new Date(),
			});
			await paymentRepo.updatePayment({
				checkout_session_id: 'cs_test_dispute_lost',
				payment_intent_id: paymentIntentId,
			});
			const userRepository = new UserRepository();
			const userId = createUserID(BigInt(purchaser.userId));
			const user = await userRepository.findUniqueAssert(userId);
			await userRepository.updateDeletionSchedule(user, {
				flags: (user.flags | UserFlags.DELETED) & ~UserFlags.SELF_DELETED,
				deletion_reason_code: DeletionReasons.BILLING_DISPUTE_OR_ABUSE,
				pending_deletion_at: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
			});
			const eventData: StripeWebhookEventData = {
				type: 'charge.dispute.closed',
				data: {
					object: {
						id: 'dp_test_lost',
						payment_intent: paymentIntentId,
						status: 'lost',
					},
				},
			};
			await sendWebhook(eventData);
			const userAfterLoss = await createBuilderWithoutAuth<UserDataExistsResponse>(harness)
				.get(`/test/users/${purchaser.userId}/data-exists`)
				.execute();
			expect(userAfterLoss.has_deleted_flag).toBe(true);
			expect(userAfterLoss.deletion_reason_code).toBe(DeletionReasons.BILLING_DISPUTE_OR_ABUSE);
			expect(userAfterLoss.pending_deletion_at).not.toBeNull();
		});
		test('handles dispute closed for payment intent not found gracefully', async () => {
			const eventData: StripeWebhookEventData = {
				type: 'charge.dispute.closed',
				data: {
					object: {
						id: 'dp_test_unknown_closed',
						payment_intent: 'pi_test_does_not_exist',
						status: 'won',
					},
				},
			};
			await sendWebhookExpectStripeError(eventData);
		});
	});
});

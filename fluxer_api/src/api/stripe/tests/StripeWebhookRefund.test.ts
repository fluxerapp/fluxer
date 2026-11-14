// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {sendStripeWebhook, setupSyncStripeWebhookWorker} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {server} from '@app/api/test/msw/server';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {PremiumFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {HttpResponse, http} from 'msw';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

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
	function useRefundListHandler(chargeId: string, refunds: Array<Record<string, unknown>> = []): void {
		server.use(
			http.get(
				({request}) => request.url === `https://api.stripe.com/v1/refunds?charge=${chargeId}&limit=100`,
				() =>
					HttpResponse.json({
						object: 'list',
						data: refunds,
						has_more: false,
						url: '/v1/refunds',
					}),
			),
		);
	}
	describe('charge.refunded', () => {
		async function seedRefundedPayment(
			userId: ReturnType<typeof createUserID>,
			suffix: string,
		): Promise<{paymentIntentId: string; chargeId: string}> {
			const {PaymentRepository} = await import('@app/api/user/repositories/PaymentRepository');
			const paymentRepository = new PaymentRepository();
			const paymentIntentId = `pi_test_refund_${suffix}`;
			const checkoutSessionId = `cs_test_refund_${suffix}`;
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
			return {paymentIntentId, chargeId: `ch_test_refund_${suffix}`};
		}
		function customerRefund(id: string, chargeId: string, paymentIntentId: string, created: number) {
			return {
				id,
				object: 'refund',
				charge: chargeId,
				payment_intent: paymentIntentId,
				amount: 1200,
				currency: 'usd',
				status: 'succeeded',
				created,
				metadata: {},
			};
		}
		test('marks the payment refunded without starting the self-serve refund cooldown', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const userRepository = new UserRepository();
			const {paymentIntentId, chargeId} = await seedRefundedPayment(userId, 'support_123');
			useRefundListHandler(chargeId);
			await sendStripeWebhook(harness, {
				type: 'charge.refunded',
				data: {object: {id: chargeId, payment_intent: paymentIntentId, amount_refunded: 2500, refunded: true}},
			});
			const updatedUser = await userRepository.findUnique(userId);
			expect(updatedUser!.lastRefundAt).toBeNull();
			expect(updatedUser!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(0);
			const updatedPayment = await userRepository.getPaymentByPaymentIntent(paymentIntentId);
			expect(updatedPayment!.status).toBe('refunded');
		});
		test('never blocks purchases however many refunds a user has had', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const userRepository = new UserRepository();
			const earlierRefundAt = new Date('2024-01-01');
			await userRepository.patchUpsert(
				userId,
				{first_refund_at: earlierRefundAt},
				(await userRepository.findUnique(userId))!.toRow(),
			);
			const {paymentIntentId, chargeId} = await seedRefundedPayment(userId, 'many_456');
			const nowSeconds = Math.floor(Date.now() / 1000);
			const refunds = [
				customerRefund('re_test_refund_many_456_1', chargeId, paymentIntentId, nowSeconds - 600),
				customerRefund('re_test_refund_many_456_2', chargeId, paymentIntentId, nowSeconds),
			];
			await sendStripeWebhook(harness, {
				type: 'charge.refunded',
				data: {
					object: {
						id: chargeId,
						payment_intent: paymentIntentId,
						amount_refunded: 2400,
						refunded: true,
						refunds: {object: 'list', has_more: false, url: `/v1/charges/${chargeId}/refunds`, data: refunds},
					},
				},
			});
			const updatedUser = await userRepository.findUnique(userId);
			expect(updatedUser!.lastRefundAt).toEqual(earlierRefundAt);
			expect(updatedUser!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(0);
		});
		test('skips premium action for donation customer refund', async () => {
			const donationCustomerId = 'cus_test_donation_refund';
			const {DonationRepository} = await import('@app/api/donation/DonationRepository');
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
			useRefundListHandler('ch_test_refund_donation_101');
			const result = await sendStripeWebhook(harness, {
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_refund_donation_101',
						payment_intent: 'pi_test_donation_not_indexed_101',
						customer: donationCustomerId,
						amount_refunded: 2500,
						refunded: true,
					},
				},
			});
			expect(result.received).toBe(true);
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
			useRefundListHandler('ch_test_refund_stripe_cus_only');
			await sendStripeWebhook(harness, {
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_refund_stripe_cus_only',
						payment_intent: 'pi_test_gift_refund_stripe_cus',
						amount_refunded: 2500,
						refunded: true,
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
			useRefundListHandler('ch_test_refund_gift_override_123');
			await sendStripeWebhook(harness, {
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_refund_gift_override_123',
						payment_intent: 'pi_test_gift_refund_override',
						amount_refunded: 2500,
						refunded: true,
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
		test('keeps a gift code after a partial refund and revokes it once the whole charge is refunded', async () => {
			const purchaser = await createTestAccount(harness);
			const userRepository = new UserRepository();
			await userRepository.createGiftCode({
				code: 'GIFT_PARTIAL_REFUND',
				duration_months: null,
				duration_type: 'months',
				duration_quantity: 12,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: null,
				redeemed_by_user_id: null,
				stripe_payment_intent_id: 'pi_test_gift_partial_refund',
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			useRefundListHandler('ch_test_gift_partial_refund');
			const refundEvent = (amountRefunded: number, refunded: boolean) => ({
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_gift_partial_refund',
						payment_intent: 'pi_test_gift_partial_refund',
						amount: 4999,
						amount_refunded: amountRefunded,
						refunded,
					},
				},
			});
			await sendStripeWebhook(harness, refundEvent(100, false));
			expect((await userRepository.findGiftCode('GIFT_PARTIAL_REFUND'))?.revokedAt ?? null).toBeNull();
			await sendStripeWebhook(harness, refundEvent(4999, true));
			expect((await userRepository.findGiftCode('GIFT_PARTIAL_REFUND'))?.revokedAt).not.toBeNull();
		});
		test('ends a gift after a partial withdrawal refund', async () => {
			const purchaser = await createTestAccount(harness);
			const userRepository = new UserRepository();
			await userRepository.createGiftCode({
				code: 'GIFT_PRO_RATA_WITHDRAWAL',
				duration_months: null,
				duration_type: 'months',
				duration_quantity: 1,
				created_at: new Date(),
				created_by_user_id: createUserID(BigInt(purchaser.userId)),
				redeemed_at: null,
				redeemed_by_user_id: null,
				stripe_payment_intent_id: 'pi_test_gift_pro_rata_withdrawal',
				visionary_sequence_number: null,
				checkout_session_id: null,
				version: 1,
			});
			useRefundListHandler('ch_test_gift_pro_rata_withdrawal', [
				{
					id: 're_test_gift_pro_rata_withdrawal',
					object: 'refund',
					charge: 'ch_test_gift_pro_rata_withdrawal',
					payment_intent: 'pi_test_gift_pro_rata_withdrawal',
					amount: 4500,
					currency: 'usd',
					status: 'succeeded',
					created: Math.floor(Date.now() / 1000),
					metadata: {refund_kind: 'withdrawal', user_id: purchaser.userId},
				},
			]);
			await sendStripeWebhook(harness, {
				type: 'charge.refunded',
				data: {
					object: {
						id: 'ch_test_gift_pro_rata_withdrawal',
						payment_intent: 'pi_test_gift_pro_rata_withdrawal',
						amount: 4999,
						amount_refunded: 4500,
						refunded: false,
					},
				},
			});
			expect((await userRepository.findGiftCode('GIFT_PRO_RATA_WITHDRAWAL'))?.revokedAt).not.toBeNull();
		});
	});
	describe('refund.updated', () => {
		test('finalizes self-serve cooldown and cancels the subscription once the refund is confirmed succeeded', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const userRepository = new UserRepository();
			const subscriptionId = 'sub_test_webhook_finalize';
			const refundCreated = Math.floor(Date.now() / 1000) - 120;
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
			await sendStripeWebhook(harness, {
				type: 'refund.updated',
				data: {
					object: {
						id: 'pyr_test_webhook_finalize',
						status: 'succeeded',
						created: refundCreated,
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
			expect(updatedUser!.lastRefundAt!.getTime()).toBe(refundCreated * 1000);
		});
		test('does not finalize cooldown while the refund is still pending', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const userRepository = new UserRepository();
			await sendStripeWebhook(harness, {
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
			expect(updatedUser!.lastRefundAt).toBeNull();
		});
	});
	describe('refund.failed', () => {
		test('does not finalize cooldown when the refund ultimately fails', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));
			const userRepository = new UserRepository();
			await sendStripeWebhook(harness, {
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
			expect(updatedUser!.lastRefundAt).toBeNull();
		});
	});
});

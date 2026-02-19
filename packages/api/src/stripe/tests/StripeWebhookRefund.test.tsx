/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import crypto from 'node:crypto';
import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {createUserID} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import {createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {
	createMockWebhookPayload,
	type StripeWebhookEventData,
} from '@fluxer/api/src/test/msw/handlers/StripeApiHandlers';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Stripe Webhook Refund', () => {
	let harness: Awaited<ReturnType<typeof createApiTestHarness>>;
	let originalWebhookSecret: string | undefined;

	beforeAll(async () => {
		harness = await createApiTestHarness();
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

	async function sendWebhook(eventData: StripeWebhookEventData): Promise<{received: boolean}> {
		const {payload, timestamp} = createMockWebhookPayload(eventData);
		const signature = createWebhookSignature(payload, timestamp, Config.stripe.webhookSecret!);

		return createBuilder<{received: boolean}>(harness, '')
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

			const {PaymentRepository} = await import('@fluxer/api/src/user/repositories/PaymentRepository');
			const {UserRepository} = await import('@fluxer/api/src/user/repositories/UserRepository');
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

			const {UserRepository} = await import('@fluxer/api/src/user/repositories/UserRepository');
			const userRepository = new UserRepository();

			const firstRefundDate = new Date('2024-01-01');
			await userRepository.patchUpsert(
				userId,
				{
					first_refund_at: firstRefundDate,
				},
				(await userRepository.findUnique(userId))!.toRow(),
			);

			const {PaymentRepository} = await import('@fluxer/api/src/user/repositories/PaymentRepository');
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
			expect(updatedUser!.flags & UserFlags.PREMIUM_PURCHASE_DISABLED).toBe(
				BigInt(UserFlags.PREMIUM_PURCHASE_DISABLED),
			);

			const updatedPayment = await userRepository.getPaymentByPaymentIntent(paymentIntentId);
			expect(updatedPayment).not.toBeNull();
			expect(updatedPayment!.status).toBe('refunded');
		});

		test('falls back to customer ID when payment intent is not indexed (subscription mode)', async () => {
			const account = await createTestAccount(harness);
			const userId = createUserID(BigInt(account.userId));

			const {UserRepository} = await import('@fluxer/api/src/user/repositories/UserRepository');
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

			const {DonationRepository} = await import('@fluxer/api/src/donation/DonationRepository');
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
	});
});

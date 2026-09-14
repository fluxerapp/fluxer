// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {AdminRepository} from '@app/api/admin/AdminRepository';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {
	createTestPayment,
	createTestUserWithPremium,
	setupSyncStripeWebhookWorker,
} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	createMockWebhookPayload,
	createStripeApiHandlers,
	type StripeWebhookEventData,
} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Stripe Webhook Early Fraud Warning', () => {
	let harness: ApiTestHarness;
	let originalWebhookSecret: string | undefined;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		originalWebhookSecret = Config.stripe.webhookSecret;
		Config.stripe.webhookSecret = 'whsec_test_early_fraud_warning';
		setupSyncStripeWebhookWorker();
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.webhookSecret = originalWebhookSecret;
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
		return await createBuilder<{
			received: boolean;
		}>(harness, '')
			.post('/stripe/webhook')
			.header('content-type', 'application/json')
			.header('stripe-signature', signature)
			.body(payload)
			.execute();
	}
	async function createDirectPurchaseUser({
		customerId,
		paymentIntentId,
		subscriptionId,
	}: {
		customerId: string;
		paymentIntentId: string;
		subscriptionId: string;
	}) {
		const account = await createTestUserWithPremium(harness, UserPremiumTypes.SUBSCRIPTION, {
			premiumUntil: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
			stripeCustomerId: customerId,
			stripeSubscriptionId: subscriptionId,
		});
		await createTestPayment(harness, createUserID(BigInt(account.userId)), `cs_${paymentIntentId}`, {
			paymentIntentId,
			status: 'completed',
			stripeCustomerId: customerId,
			subscriptionId,
		});
		return account;
	}
	test('refunds, reports, blocks, cancels subscription, and schedules deletion for actionable fraud warnings', async () => {
		const chargeId = 'ch_test_efw_full';
		const customerId = 'cus_test_efw_full';
		const paymentIntentId = 'pi_test_efw_full';
		const subscriptionId = 'sub_test_efw_full';
		const billingEmail = 'fraudster@example.com';
		const cardFingerprint = 'fp_test_efw_full';
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
		const account = await createDirectPurchaseUser({
			customerId,
			paymentIntentId,
			subscriptionId,
		});
		await sendWebhook({
			type: 'radar.early_fraud_warning.created',
			data: {
				object: {
					actionable: true,
					charge: chargeId,
					created: Math.floor(Date.now() / 1000),
					fraud_type: 'made_with_stolen_card',
					id: 'issfr_test_full',
					livemode: false,
					object: 'radar.early_fraud_warning',
					payment_intent: paymentIntentId,
				},
			},
		});
		const userRepository = new UserRepository();
		const updatedUser = await userRepository.findUnique(createUserID(BigInt(account.userId)));
		expect(updatedUser).not.toBeNull();
		expect(updatedUser!.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
		expect(updatedUser!.deletionReasonCode).toBe(DeletionReasons.BILLING_DISPUTE_OR_ABUSE);
		expect(updatedUser!.stripeSubscriptionId).toBeNull();
		expect(updatedUser!.pendingDeletionAt).not.toBeNull();
		const daysDifference = (updatedUser!.pendingDeletionAt!.getTime() - Date.now()) / (24 * 60 * 60 * 1000);
		expect(daysDifference).toBeGreaterThan(58);
		expect(daysDifference).toBeLessThan(62);
		expect(stripeHandlers.spies.retrievedCharges).toEqual([chargeId]);
		expect(stripeHandlers.spies.updatedCharges).toHaveLength(1);
		expect(stripeHandlers.spies.createdRefunds).toHaveLength(1);
		expect(stripeHandlers.spies.cancelledSubscriptions).toEqual([subscriptionId]);
		expect(stripeHandlers.spies.createdValueLists).toHaveLength(3);
		expect(stripeHandlers.spies.createdValueListItems).toEqual(
			expect.arrayContaining([
				{value: billingEmail, valueListId: expect.any(String)},
				{value: cardFingerprint, valueListId: expect.any(String)},
				{value: customerId, valueListId: expect.any(String)},
			]),
		);
		const auditRepository = new AdminRepository();
		const auditLogs = await auditRepository.listAllAuditLogsPaginated(50);
		const matchingLogs = auditLogs.filter((log) => {
			return log.action === 'schedule_deletion' && log.targetId === BigInt(account.userId);
		});
		expect(matchingLogs).toHaveLength(1);
		expect(matchingLogs[0]!.adminUserId.toString()).toBe('0');
		expect(matchingLogs[0]!.auditLogReason).toContain('Stripe early fraud warning');
		expect(matchingLogs[0]!.metadata.get('days')).toBe('60');
		expect(matchingLogs[0]!.metadata.get('charge_id')).toBe(chargeId);
	});
	test('does nothing for non-actionable early fraud warnings', async () => {
		const chargeId = 'ch_test_efw_noop';
		const customerId = 'cus_test_efw_noop';
		const paymentIntentId = 'pi_test_efw_noop';
		const subscriptionId = 'sub_test_efw_noop';
		const stripeHandlers = createStripeApiHandlers({
			charges: {
				[chargeId]: {
					customer: customerId,
					payment_intent: paymentIntentId,
				},
			},
		});
		server.use(...stripeHandlers.handlers);
		const account = await createDirectPurchaseUser({
			customerId,
			paymentIntentId,
			subscriptionId,
		});
		await sendWebhook({
			type: 'radar.early_fraud_warning.created',
			data: {
				object: {
					actionable: false,
					charge: chargeId,
					created: Math.floor(Date.now() / 1000),
					fraud_type: 'made_with_stolen_card',
					id: 'issfr_test_noop',
					livemode: false,
					object: 'radar.early_fraud_warning',
					payment_intent: paymentIntentId,
				},
			},
		});
		const userRepository = new UserRepository();
		const unchangedUser = await userRepository.findUnique(createUserID(BigInt(account.userId)));
		expect(unchangedUser).not.toBeNull();
		expect(unchangedUser!.flags & UserFlags.DELETED).toBe(0n);
		expect(unchangedUser!.stripeSubscriptionId).toBe(subscriptionId);
		expect(stripeHandlers.spies.updatedCharges).toHaveLength(0);
		expect(stripeHandlers.spies.createdRefunds).toHaveLength(0);
		expect(stripeHandlers.spies.cancelledSubscriptions).toHaveLength(0);
		expect(stripeHandlers.spies.createdValueLists).toHaveLength(0);
	});
	test('does not duplicate account enforcement when a chargeback follows the same fraud warning', async () => {
		const chargeId = 'ch_test_efw_duplicate';
		const customerId = 'cus_test_efw_duplicate';
		const paymentIntentId = 'pi_test_efw_duplicate';
		const subscriptionId = 'sub_test_efw_duplicate';
		const stripeHandlers = createStripeApiHandlers({
			charges: {
				[chargeId]: {
					customer: customerId,
					payment_intent: paymentIntentId,
				},
			},
		});
		server.use(...stripeHandlers.handlers);
		const account = await createDirectPurchaseUser({
			customerId,
			paymentIntentId,
			subscriptionId,
		});
		await sendWebhook({
			type: 'radar.early_fraud_warning.created',
			data: {
				object: {
					actionable: true,
					charge: chargeId,
					created: Math.floor(Date.now() / 1000),
					fraud_type: 'made_with_stolen_card',
					id: 'issfr_test_duplicate',
					livemode: false,
					object: 'radar.early_fraud_warning',
					payment_intent: paymentIntentId,
				},
			},
		});
		const userRepository = new UserRepository();
		const userAfterEarlyWarning = await userRepository.findUnique(createUserID(BigInt(account.userId)));
		expect(userAfterEarlyWarning?.pendingDeletionAt).not.toBeNull();
		const firstPendingDeletionAt = userAfterEarlyWarning!.pendingDeletionAt!.toISOString();
		await sendWebhook({
			type: 'charge.dispute.created',
			data: {
				object: {
					charge: chargeId,
					id: 'dp_test_duplicate',
					payment_intent: paymentIntentId,
					status: 'needs_response',
				},
			},
		});
		const userAfterChargeback = await userRepository.findUnique(createUserID(BigInt(account.userId)));
		expect(userAfterChargeback!.pendingDeletionAt!.toISOString()).toBe(firstPendingDeletionAt);
		expect(stripeHandlers.spies.cancelledSubscriptions).toEqual([subscriptionId]);
		const auditRepository = new AdminRepository();
		const auditLogs = await auditRepository.listAllAuditLogsPaginated(50);
		const matchingLogs = auditLogs.filter((log) => {
			return log.action === 'schedule_deletion' && log.targetId === BigInt(account.userId);
		});
		expect(matchingLogs).toHaveLength(1);
	});
});

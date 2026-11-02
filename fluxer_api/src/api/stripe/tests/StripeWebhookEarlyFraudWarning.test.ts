// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {findLastTestEmail, listTestEmails} from '@app/api/auth/tests/AuthTestUtils';
import {createApplicationID, createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {OAuth2TokenRepository} from '@app/api/oauth/repositories/OAuth2TokenRepository';
import {
	createTestPayment,
	createTestUserWithPremium,
	sendStripeWebhook,
	setupSyncStripeWebhookWorker,
} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {PremiumFlags, UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const {loggerError} = vi.hoisted(() => ({loggerError: vi.fn()}));

vi.mock('@app/api/Logger', async (importOriginal) => {
	const actual = await importOriginal<typeof import('@app/api/Logger')>();
	return {
		...actual,
		Logger: new Proxy(actual.Logger, {
			get: (target, prop) => (prop === 'error' ? loggerError : Reflect.get(target, prop)),
		}),
	};
});

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
	test('refunds, reports, blocks the card, cancels the subscription the payment paid for, and limits purchases pending review', async () => {
		const chargeId = 'ch_test_efw_full';
		const customerId = 'cus_test_efw_full';
		const invoiceId = 'in_test_efw_full';
		const paymentIntentId = `pi_${invoiceId}`;
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
			invoices: {[invoiceId]: {customer: customerId, subscriptionId}},
			subscriptions: {[subscriptionId]: {customer: customerId, latest_invoice: invoiceId}},
		});
		server.use(...stripeHandlers.handlers);
		const account = await createDirectPurchaseUser({
			customerId,
			paymentIntentId,
			subscriptionId,
		});
		const accountUserId = createUserID(BigInt(account.userId));
		const oauthTokens = new OAuth2TokenRepository();
		const applicationId = createApplicationID(1234567890123456789n);
		await oauthTokens.createAccessToken({
			token_: `access-${account.userId}`,
			application_id: applicationId,
			user_id: accountUserId,
			scope: new Set(['identify']),
			created_at: new Date(),
		});
		await oauthTokens.createRefreshToken({
			token_: `refresh-${account.userId}`,
			application_id: applicationId,
			user_id: accountUserId,
			scope: new Set(['identify']),
			created_at: new Date(),
		});
		await sendStripeWebhook(harness, {
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
		expect(updatedUser!.flags & UserFlags.DELETED).toBe(0n);
		expect(updatedUser!.flags & UserFlags.DISABLED).toBe(0n);
		expect(updatedUser!.pendingDeletionAt).toBeNull();
		expect(updatedUser!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(PremiumFlags.PURCHASE_DISABLED);
		expect(await oauthTokens.getAccessToken(`access-${account.userId}`)).not.toBeNull();
		expect(await oauthTokens.listRefreshTokensForUser(accountUserId)).toHaveLength(1);
		expect(stripeHandlers.spies.retrievedCharges).toEqual([chargeId]);
		expect(stripeHandlers.spies.updatedCharges).toHaveLength(1);
		expect(stripeHandlers.spies.createdRefunds).toHaveLength(1);
		expect(stripeHandlers.spies.cancelledSubscriptions).toEqual([subscriptionId]);
		expect(stripeHandlers.spies.createdValueLists).toHaveLength(1);
		expect(stripeHandlers.spies.createdValueListItems).toEqual([
			{value: cardFingerprint, valueListId: expect.any(String)},
		]);
		const auditRepository = new AdminRepository();
		const auditLogs = await auditRepository.listAllAuditLogsPaginated(50);
		const matchingLogs = auditLogs.filter((log) => {
			return log.action === 'limit_features' && log.targetId === BigInt(account.userId);
		});
		expect(matchingLogs).toHaveLength(1);
		expect(matchingLogs[0]!.adminUserId.toString()).toBe('0');
		expect(matchingLogs[0]!.auditLogReason).toContain('Stripe early fraud warning');
		expect(matchingLogs[0]!.metadata.get('limit')).toBe('purchases');
		expect(matchingLogs[0]!.metadata.get('kind')).toBe('feature_limit');
		expect(matchingLogs[0]!.metadata.get('charge_id')).toBe(chargeId);
		expect(await auditRepository.isEmailBanned(account.email)).toBe(false);
		const emails = await listTestEmails(harness, {recipient: account.email});
		expect(findLastTestEmail(emails, 'account_deletion_scheduled_requested')).toBeNull();
		const email = findLastTestEmail(emails, 'enforcement_notice');
		expect(email?.metadata).toMatchObject({kind_label: 'feature_limit', rule_title: 'Payments'});
		expect(email?.metadata.effects).toContain('until a person on our team reviews this');
		expect(JSON.stringify(email)).not.toContain('made_with_stolen_card');
		expect(JSON.stringify(email)).not.toContain(chargeId);
	});
	test('cancels the subscription a flagged payment paid for when the checkout webhook has not linked it yet', async () => {
		const customerId = 'cus_test_efw_race';
		const invoiceId = 'in_test_efw_race';
		const paymentIntentId = `pi_${invoiceId}`;
		const chargeId = `ch_${paymentIntentId}`;
		const subscriptionId = 'sub_test_efw_race';
		const stripeHandlers = createStripeApiHandlers({
			charges: {[chargeId]: {customer: customerId, payment_intent: paymentIntentId}},
			invoices: {[invoiceId]: {customer: customerId, subscriptionId}},
			subscriptions: {[subscriptionId]: {customer: customerId, latest_invoice: invoiceId}},
		});
		server.use(...stripeHandlers.handlers);
		const account = await createTestUserWithPremium(harness, UserPremiumTypes.NONE, {stripeCustomerId: customerId});
		await sendStripeWebhook(harness, {
			type: 'radar.early_fraud_warning.created',
			data: {
				object: {
					actionable: true,
					charge: chargeId,
					created: Math.floor(Date.now() / 1000),
					fraud_type: 'made_with_stolen_card',
					id: 'issfr_test_race',
					livemode: false,
					object: 'radar.early_fraud_warning',
					payment_intent: paymentIntentId,
				},
			},
		});
		expect(stripeHandlers.spies.cancelledSubscriptions).toEqual([subscriptionId]);
		expect(stripeHandlers.spies.createdRefunds).toHaveLength(1);
		const user = await new UserRepository().findUnique(createUserID(BigInt(account.userId)));
		expect(user!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(PremiumFlags.PURCHASE_DISABLED);
	});
	test('treats a fraud warning on an Operator Pass payment that is not recorded yet as that purchase', async () => {
		const chargeId = 'ch_test_efw_operator_pass';
		const paymentIntentId = 'pi_test_efw_operator_pass';
		const account = await createTestUserWithPremium(harness, UserPremiumTypes.NONE, {});
		const stripeHandlers = createStripeApiHandlers({
			charges: {
				[chargeId]: {
					customer: null,
					payment_intent: paymentIntentId,
					metadata: {operator_pass: 'true', operator_pass_purchaser_id: account.userId},
				},
			},
		});
		server.use(...stripeHandlers.handlers);
		loggerError.mockClear();
		await sendStripeWebhook(harness, {
			type: 'radar.early_fraud_warning.created',
			data: {
				object: {
					actionable: true,
					charge: chargeId,
					created: Math.floor(Date.now() / 1000),
					fraud_type: 'made_with_stolen_card',
					id: 'issfr_test_operator_pass',
					livemode: false,
					object: 'radar.early_fraud_warning',
					payment_intent: paymentIntentId,
				},
			},
		});
		expect(loggerError.mock.calls.map((call) => call[1])).not.toContain(
			'Unable to resolve internal user for Stripe payment fraud signal',
		);
		expect(stripeHandlers.spies.createdRefunds).toHaveLength(1);
		const user = await new UserRepository().findUnique(createUserID(BigInt(account.userId)));
		expect(user!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(0);
	});
	test('leaves the subscription running when the flagged payment paid for an earlier period', async () => {
		const customerId = 'cus_test_efw_older';
		const invoiceId = 'in_test_efw_older';
		const paymentIntentId = `pi_${invoiceId}`;
		const chargeId = `ch_${paymentIntentId}`;
		const subscriptionId = 'sub_test_efw_older';
		const stripeHandlers = createStripeApiHandlers({
			charges: {[chargeId]: {customer: customerId, payment_intent: paymentIntentId}},
			invoices: {
				[invoiceId]: {customer: customerId, subscriptionId},
				in_test_efw_current: {customer: customerId, subscriptionId},
			},
			subscriptions: {[subscriptionId]: {customer: customerId, latest_invoice: 'in_test_efw_current'}},
		});
		server.use(...stripeHandlers.handlers);
		const account = await createDirectPurchaseUser({customerId, paymentIntentId, subscriptionId});
		await sendStripeWebhook(harness, {
			type: 'radar.early_fraud_warning.created',
			data: {
				object: {
					actionable: true,
					charge: chargeId,
					created: Math.floor(Date.now() / 1000),
					fraud_type: 'made_with_stolen_card',
					id: 'issfr_test_older',
					livemode: false,
					object: 'radar.early_fraud_warning',
					payment_intent: paymentIntentId,
				},
			},
		});
		expect(stripeHandlers.spies.createdRefunds).toHaveLength(1);
		expect(stripeHandlers.spies.cancelledSubscriptions).toHaveLength(0);
		const user = await new UserRepository().findUnique(createUserID(BigInt(account.userId)));
		expect(user!.stripeSubscriptionId).toBe(subscriptionId);
		expect(user!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(PremiumFlags.PURCHASE_DISABLED);
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
		await sendStripeWebhook(harness, {
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
	test('does not record a second limit when a chargeback follows the same fraud warning', async () => {
		const chargeId = 'ch_test_efw_duplicate';
		const customerId = 'cus_test_efw_duplicate';
		const invoiceId = 'in_test_efw_duplicate';
		const paymentIntentId = `pi_${invoiceId}`;
		const subscriptionId = 'sub_test_efw_duplicate';
		const stripeHandlers = createStripeApiHandlers({
			charges: {
				[chargeId]: {
					customer: customerId,
					payment_intent: paymentIntentId,
				},
			},
			invoices: {[invoiceId]: {customer: customerId, subscriptionId}},
			subscriptions: {[subscriptionId]: {customer: customerId, latest_invoice: invoiceId}},
		});
		server.use(...stripeHandlers.handlers);
		const account = await createDirectPurchaseUser({
			customerId,
			paymentIntentId,
			subscriptionId,
		});
		await sendStripeWebhook(harness, {
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
		expect(userAfterEarlyWarning!.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(PremiumFlags.PURCHASE_DISABLED);
		await sendStripeWebhook(harness, {
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
		expect(userAfterChargeback!.pendingDeletionAt).toBeNull();
		expect(stripeHandlers.spies.cancelledSubscriptions).toEqual([subscriptionId]);
		const auditRepository = new AdminRepository();
		const auditLogs = await auditRepository.listAllAuditLogsPaginated(50);
		const matchingLogs = auditLogs.filter((log) => {
			return log.action === 'limit_features' && log.targetId === BigInt(account.userId);
		});
		expect(matchingLogs).toHaveLength(1);
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {clearTestEmails, createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {
	close,
	createStaff,
	decisionBody,
	emailsTo,
	endDecision,
	getDecision,
	listDecisions,
	loadUser,
	suspend,
	warn,
} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {
	createTestPayment,
	mockStripeWebhookSecret,
	restoreStripeWebhookSecret,
	sendStripeWebhook,
	setupSyncStripeWebhookWorker,
} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {PaymentRepository} from '@app/api/user/repositories/PaymentRepository';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {PremiumFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import type {EnforcementDecisionResponse} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Enforcement for payment fraud signals', () => {
	let harness: ApiTestHarness;
	let staff: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		mockStripeWebhookSecret('whsec_test_enforcement_payment');
		setupSyncStripeWebhookWorker();
	});

	beforeEach(async () => {
		await harness.reset();
		staff = await createStaff(harness);
	});

	afterEach(() => {
		server.resetHandlers();
	});

	afterAll(async () => {
		await harness?.shutdown();
		restoreStripeWebhookSecret();
	});

	async function purchase(account: TestAccount, paymentIntentId: string): Promise<Date> {
		await createTestPayment(harness, createUserID(BigInt(account.userId)), `cs_${paymentIntentId}`, {
			paymentIntentId,
			status: 'completed',
		});
		const payment = await new PaymentRepository().getPaymentByPaymentIntent(paymentIntentId);
		return payment!.createdAt;
	}

	async function chargeback(paymentIntentId: string, status: 'needs_response' | 'won' | 'lost' = 'needs_response') {
		await sendStripeWebhook(harness, {
			type: status === 'needs_response' ? 'charge.dispute.created' : 'charge.dispute.closed',
			data: {object: {id: `dp_${paymentIntentId}`, payment_intent: paymentIntentId, status}},
		});
	}

	async function reviewQueue(): Promise<Array<EnforcementDecisionResponse>> {
		const {decisions} = await createBuilder<{decisions: Array<EnforcementDecisionResponse>}>(harness, staff.token)
			.get('/admin/enforcement-decisions?review_queue=true')
			.expect(HTTP_STATUS.OK)
			.execute();
		return decisions;
	}

	async function onlyDecision(account: TestAccount): Promise<EnforcementDecisionResponse> {
		const decisions = await listDecisions(harness, staff, account.userId);
		expect(decisions).toHaveLength(1);
		return getDecision(harness, staff, decisions[0]!.decision_id);
	}

	test('a chargeback records an automated purchase limit and never suspends, closes or bans', async () => {
		const target = await createTestAccount(harness);
		const paidAt = await purchase(target, 'pi_enforcement_chargeback');
		await clearTestEmails(harness);
		await chargeback('pi_enforcement_chargeback');
		const user = await loadUser(target);
		expect(user.premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(PremiumFlags.PURCHASE_DISABLED);
		expect(user.flags & (UserFlags.DISABLED | UserFlags.DELETED)).toBe(0n);
		expect(user.tempBannedUntil).toBeNull();
		expect(user.pendingDeletionAt).toBeNull();
		expect(user.enforcementDecisionId).toBeNull();
		expect(await new AdminRepository().isEmailBanned(target.email)).toBe(false);
		const decision = await onlyDecision(target);
		expect(decision).toMatchObject({
			kind: 'feature_limit',
			limit: 'purchases',
			reason_code: DeletionReasons.PAYMENT_FRAUD,
			automation: 'automated',
			source: 'own_review',
			decided_by: '0',
			payment_at: paidAt.toISOString(),
			notice_status: 'sent',
		});
		expect(decision.notes[0]?.body).toContain('Stripe chargeback');
		expect(decision.notice_text).toContain(
			'This is a temporary limit applied automatically while a person on our team reviews it.',
		);
		expect(decision.notice_text).toContain('A payment on your account on ');
		expect(decision.notice_text).not.toContain('fraud');
		expect((await reviewQueue()).map((queued) => queued.decision_id)).toEqual([decision.decision_id]);
		expect(await emailsTo(harness, target, 'enforcement_notice')).toHaveLength(1);
	});

	test('an early fraud warning records the same automated limit', async () => {
		const chargeId = 'ch_enforcement_efw';
		server.use(...createStripeApiHandlers({charges: {[chargeId]: {payment_intent: 'pi_enforcement_efw'}}}).handlers);
		const target = await createTestAccount(harness);
		const paidAt = await purchase(target, 'pi_enforcement_efw');
		await sendStripeWebhook(harness, {
			type: 'radar.early_fraud_warning.created',
			data: {
				object: {
					actionable: true,
					charge: chargeId,
					created: Math.floor(Date.now() / 1000),
					fraud_type: 'made_with_stolen_card',
					id: 'issfr_enforcement',
					livemode: false,
					object: 'radar.early_fraud_warning',
					payment_intent: 'pi_enforcement_efw',
				},
			},
		});
		const decision = await onlyDecision(target);
		expect(decision).toMatchObject({automation: 'automated', limit: 'purchases', payment_at: paidAt.toISOString()});
		expect((await loadUser(target)).pendingDeletionAt).toBeNull();
	});

	test.each([
		['warning', (target: TestAccount) => warn(harness, staff, target.userId, decisionBody())],
		['suspension', (target: TestAccount) => suspend(harness, staff, target.userId)],
		['closure', (target: TestAccount) => close(harness, staff, target.userId)],
	])('a %s by a person takes the account out of the review queue', async (_label, decide) => {
		const target = await createTestAccount(harness);
		await purchase(target, `pi_queue_${_label}`);
		await chargeback(`pi_queue_${_label}`);
		expect(await reviewQueue()).toHaveLength(1);
		await decide(target);
		expect(await reviewQueue()).toEqual([]);
		const limits = (await listDecisions(harness, staff, target.userId)).filter(
			(decision) => decision.kind === 'feature_limit',
		);
		expect(limits.map((decision) => decision.status)).toEqual(['active']);
	});

	test('a person ending the limit clears the flag and the review queue', async () => {
		const target = await createTestAccount(harness);
		await purchase(target, 'pi_queue_end');
		await chargeback('pi_queue_end');
		const decision = await onlyDecision(target);
		await endDecision(harness, staff, decision.decision_id, 'lifted').expect(HTTP_STATUS.OK).execute();
		expect(await reviewQueue()).toEqual([]);
		expect((await loadUser(target)).premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(0);
	});

	test('a won dispute reverses the limit as the system', async () => {
		const target = await createTestAccount(harness);
		await purchase(target, 'pi_dispute_won');
		await chargeback('pi_dispute_won');
		await clearTestEmails(harness);
		await chargeback('pi_dispute_won', 'won');
		const decision = await onlyDecision(target);
		expect(decision).toMatchObject({status: 'ended', end_outcome: 'reversed', ended_by: '0'});
		expect((await loadUser(target)).premiumFlags & PremiumFlags.PURCHASE_DISABLED).toBe(0);
		expect(await reviewQueue()).toEqual([]);
		const [email] = await emailsTo(harness, target, 'enforcement_ended');
		expect(email?.metadata).toMatchObject({outcome: 'reversed', decision_id: decision.decision_id});
	});

	test('a lost dispute leaves the limit in place', async () => {
		const target = await createTestAccount(harness);
		await purchase(target, 'pi_dispute_lost');
		await chargeback('pi_dispute_lost');
		await chargeback('pi_dispute_lost', 'lost');
		expect((await onlyDecision(target)).status).toBe('active');
	});

	test('a won dispute leaves a closure a person decided in place', async () => {
		const target = await createTestAccount(harness);
		await purchase(target, 'pi_dispute_staff_closure');
		const closureId = await close(
			harness,
			staff,
			target.userId,
			decisionBody({reason_code: DeletionReasons.BILLING_DISPUTE_OR_ABUSE}),
		);
		await chargeback('pi_dispute_staff_closure', 'won');
		const user = await loadUser(target);
		expect(user.enforcementDecisionId?.toString()).toBe(closureId);
		expect(user.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
	});

	test('a won dispute still cancels a payment closure from before decisions', async () => {
		const target = await createTestAccount(harness);
		await purchase(target, 'pi_dispute_legacy');
		const users = getUserRepository();
		const user = await loadUser(target);
		await users.updateDeletionSchedule(user, {
			flags: user.flags | UserFlags.DELETED,
			pending_deletion_at: new Date(Date.now() + 30 * 86_400_000),
			deletion_reason_code: DeletionReasons.BILLING_DISPUTE_OR_ABUSE,
		});
		await chargeback('pi_dispute_legacy', 'won');
		const after = await loadUser(target);
		expect(after.flags & UserFlags.DELETED).toBe(0n);
		expect(after.pendingDeletionAt).toBeNull();
	});
});

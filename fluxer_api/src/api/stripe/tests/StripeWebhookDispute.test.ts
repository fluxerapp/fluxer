// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {AdminRepository} from '@app/api/admin/AdminRepository';
import {
	createTestAccount,
	findLastTestEmail,
	listTestEmails,
	setUserACLs,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {createApiContext} from '@app/api/CreateApiContext';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import {Db} from '@app/api/database/CassandraTypes';
import {close, endDecision} from '@app/api/enforcement/tests/EnforcementTestUtils';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {PaymentDisputeRepository} from '@app/api/payment_disputes/PaymentDisputeRepository';
import {
	createTestPayment,
	createTestUserWithPremium,
	mockStripeWebhookSecret,
	restoreStripeWebhookSecret,
	setupSyncStripeWebhookWorker,
} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	createMockWebhookPayload,
	createStripeApiHandlers,
	type StripeApiHandlers,
	type StripeWebhookEventData,
} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {createRecordingSmtpServer} from '@app/api/test/RecordingSmtpServer';
import {createTestWorkerHelpers} from '@app/api/test/SyncTaskWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import closePaymentDisputeAccounts from '@app/api/worker/tasks/ClosePaymentDisputeAccounts';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {UserFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {EnforcementDecisionResponse} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import {EmailService} from '@pkgs/email/src/EmailService';
import {SmtpEmailProvider} from '@pkgs/email/src/SmtpEmailProvider';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const DAY_MS = 24 * 60 * 60 * 1000;
const EXEMPT_REASONS = [
	'fraudulent',
	'unrecognized',
	'debit_not_authorized',
	'duplicate',
	'credit_not_processed',
	'product_not_received',
	'product_unacceptable',
] as const;
const REVIEWABLE_REASONS = [
	'general',
	'subscription_canceled',
	'customer_initiated',
	'bank_cannot_process',
	'incorrect_account_details',
	'insufficient_funds',
	'check_returned',
	'noncompliant',
] as const;
const GIFT_PRICE = 'price_dispute_gift_1_month_usd';

interface ReviewResponse {
	dispute_id: string;
	product: string;
	order_id: string;
	is_gift: boolean;
	reason: string;
	status: string;
	outcome: string | null;
	closed_at: string | null;
	blockers: Array<string>;
	warnable: boolean;
	warning_deadline: string | null;
	warning_email_sent_at: string | null;
	warning_dm_sent_at: string | null;
	warned_at: string | null;
	enforcement_decision_id: string | null;
	awaited_decision_id: string | null;
}

function longDate(iso: string): string {
	return new Intl.DateTimeFormat('en-US', {dateStyle: 'long', timeZone: 'UTC'}).format(new Date(iso));
}

describe('Plutonium payment disputes', () => {
	let harness: ApiTestHarness;
	let stripe: StripeApiHandlers;
	let originalPrices: typeof Config.stripe.prices;
	const userRepository = new UserRepository();
	const disputes = new PaymentDisputeRepository();
	const smtpCleanups = new Set<() => Promise<void>>();
	const pendingDecisions = new Set<Promise<ReviewResponse>>();

	beforeAll(async () => {
		harness = await createApiTestHarness();
		mockStripeWebhookSecret('whsec_test_dispute');
		setupSyncStripeWebhookWorker();
		originalPrices = Config.stripe.prices;
		Config.paymentDisputes.closureEffectiveAt = new Date('2000-01-01T00:00:00Z');
	});
	afterAll(async () => {
		await harness.shutdown();
		restoreStripeWebhookSecret();
		Config.stripe.prices = originalPrices;
		Config.paymentDisputes.closureEffectiveAt = null;
	});
	beforeEach(async () => {
		await harness.resetData();
		Config.stripe.prices = {...originalPrices, gift1MonthUsd: GIFT_PRICE};
		stripe = createStripeApiHandlers();
		server.use(...stripe.handlers);
	});
	afterEach(async () => {
		try {
			for (const cleanup of smtpCleanups) await cleanup();
		} finally {
			smtpCleanups.clear();
			pendingDecisions.clear();
			server.resetHandlers();
		}
	});

	async function warningSmtp(firstDelivery: 'reject' | 'hold') {
		const smtp = await createRecordingSmtpServer({firstDelivery});
		const provider = new SmtpEmailProvider({
			host: '127.0.0.1',
			port: smtp.port,
			username: '',
			password: '',
			tlsMode: 'none',
		});
		const services = createApiContext().services;
		const previous = services.email;
		services.email = new EmailService(
			{
				enabled: true,
				fromEmail: 'warnings@example.com',
				fromName: 'Warning test',
				appBaseUrl: 'https://app.example.com',
				apiBaseUrl: 'https://api.example.com',
				appealsEmail: 'appeals@example.com',
				privacyEmail: null,
				safetyEmail: null,
				supportEmail: 'support@example.com',
				productName: 'Warning test',
				marketingBaseUrl: 'https://example.com',
				staticCdnBaseUrl: 'https://static.example.com',
				legalAddress: null,
				brandLogo: false,
			},
			provider,
		);
		smtpCleanups.add(async () => {
			smtp.release();
			try {
				await Promise.all(pendingDecisions);
			} finally {
				services.email = previous;
				try {
					await provider.close();
				} finally {
					await smtp.close();
				}
			}
		});
		return smtp;
	}

	async function backdateReview(
		disputeId: string,
		field: 'warning_deadline' | 'closed_at',
		agoMs: number,
	): Promise<void> {
		const review = await disputes.findReview(disputeId);
		expect(review).not.toBeNull();
		await disputes.updateReview(review!, {[field]: Db.set(new Date(Date.now() - agoMs))});
	}

	async function passWarningDate(disputeId: string): Promise<void> {
		await backdateReview(disputeId, 'warning_deadline', DAY_MS);
	}

	let eventCounter = 0;

	function signedWebhook(eventData: StripeWebhookEventData): {payload: string; signature: string} {
		eventCounter += 1;
		const {payload, timestamp} = createMockWebhookPayload({id: `evt_dispute_test_${eventCounter}`, ...eventData});
		const signed = crypto
			.createHmac('sha256', Config.stripe.webhookSecret!)
			.update(`${timestamp}.${payload}`)
			.digest('hex');
		return {payload, signature: `t=${timestamp},v1=${signed}`};
	}

	async function sendWebhook(eventData: StripeWebhookEventData, expectStripeError = false): Promise<void> {
		const {payload, signature} = signedWebhook(eventData);
		const request = createBuilder(harness, '')
			.post('/stripe/webhook')
			.header('stripe-signature', signature)
			.header('content-type', 'application/json')
			.body(payload);
		if (expectStripeError) {
			await request.expect(400, APIErrorCodes.STRIPE_ERROR).execute();
			return;
		}
		await request.execute();
	}

	async function sendDispute(
		type: 'charge.dispute.created' | 'charge.dispute.closed' | 'charge.dispute.updated',
		paymentIntentId: string,
		options: {
			status: string;
			reason?: string;
			daysAfterPayment?: number;
			dueInDays?: number;
			charge?: unknown;
			id?: string;
		},
	): Promise<void> {
		await sendWebhook({
			type,
			data: {
				object: {
					id: options.id ?? `dp_${paymentIntentId}`,
					object: 'dispute',
					amount: 2500,
					currency: 'usd',
					charge: options.charge ?? `ch_${paymentIntentId}`,
					created: Math.floor((Date.now() + (options.daysAfterPayment ?? 20) * DAY_MS) / 1000),
					evidence: {},
					evidence_details: {
						has_evidence: false,
						due_by: Math.floor((Date.now() + (options.dueInDays ?? 10) * DAY_MS) / 1000),
					},
					payment_intent: paymentIntentId,
					reason: options.reason ?? 'general',
					status: options.status,
					balance_transactions: [],
				},
			},
		});
	}

	async function openDispute(
		paymentIntentId: string,
		options: {reason?: string; daysAfterPayment?: number; dueInDays?: number} = {},
	): Promise<void> {
		await sendDispute('charge.dispute.created', paymentIntentId, {status: 'needs_response', ...options});
	}

	async function closeDispute(
		paymentIntentId: string,
		status: 'won' | 'lost' | 'warning_closed',
		options: {reason?: string; daysAfterPayment?: number} = {},
	): Promise<void> {
		await sendDispute('charge.dispute.closed', paymentIntentId, {status, ...options});
	}

	async function loseDispute(
		paymentIntentId: string,
		options: {reason?: string; daysAfterPayment?: number} = {},
	): Promise<void> {
		await openDispute(paymentIntentId, options);
		await closeDispute(paymentIntentId, 'lost', options);
	}

	function idOf(account: TestAccount): UserID {
		return createUserID(BigInt(account.userId));
	}

	async function subscriber(
		paymentIntentId: string,
		options: {subscriptionId?: string; productType?: string; isGift?: boolean; status?: string} = {},
	): Promise<TestAccount> {
		const subscriptionId = options.subscriptionId ?? `sub_${paymentIntentId}`;
		const account = await createTestUserWithPremium(harness, UserPremiumTypes.SUBSCRIPTION, {
			premiumUntil: new Date(Date.now() + 30 * DAY_MS),
			stripeCustomerId: `cus_${paymentIntentId}`,
			stripeSubscriptionId: subscriptionId,
		});
		await createTestPayment(harness, idOf(account), `cs_${paymentIntentId}`, {
			paymentIntentId,
			status: options.status ?? 'completed',
			stripeCustomerId: `cus_${paymentIntentId}`,
			productType: options.productType,
			isGift: options.isGift,
		});
		return account;
	}

	async function gift(
		buyer: TestAccount,
		paymentIntentId: string,
		options: {redeemer?: TestAccount; code?: string} = {},
	): Promise<string> {
		const code = options.code ?? `GIFT_${paymentIntentId.toUpperCase()}`;
		await userRepository.createGiftCode({
			code,
			duration_months: null,
			duration_type: 'months',
			duration_quantity: 1,
			created_at: new Date(),
			created_by_user_id: idOf(buyer),
			redeemed_at: options.redeemer ? new Date() : null,
			redeemed_by_user_id: options.redeemer ? idOf(options.redeemer) : null,
			stripe_payment_intent_id: paymentIntentId,
			visionary_sequence_number: null,
			checkout_session_id: `cs_${paymentIntentId}`,
			version: 1,
		});
		if (options.redeemer) {
			await createBuilder(harness, options.redeemer.token)
				.post(`/test/users/${options.redeemer.userId}/premium`)
				.body({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: new Date(Date.now() + 30 * DAY_MS).toISOString(),
				})
				.execute();
		}
		return code;
	}

	async function premiumType(account: TestAccount): Promise<number> {
		const me = await createBuilder<{premium_type: number}>(harness, account.token).get('/users/@me').execute();
		return me.premium_type;
	}

	async function expectAccountUntouched(account: TestAccount): Promise<void> {
		const user = await userRepository.findUnique(idOf(account));
		expect((user?.flags ?? 0n) & (UserFlags.DELETED | UserFlags.DISABLED)).toBe(0n);
		expect(user?.pendingDeletionAt ?? null).toBeNull();
		expect(user?.tempBannedUntil ?? null).toBeNull();
		expect(user?.lastRefundAt ?? null).toBeNull();
		expect(user?.enforcementDecisionId ?? null).toBeNull();
		const emails = await listTestEmails(harness, {recipient: account.email});
		expect(findLastTestEmail(emails, 'enforcement_notice')).toBeNull();
	}

	async function expectPurchasesBlocked(account: TestAccount, blocked: boolean): Promise<void> {
		const state = await createBuilder<{effective: {payment_dispute_open: boolean}}>(harness, account.token)
			.get('/premium/state')
			.execute();
		expect(state.effective.payment_dispute_open).toBe(blocked);
		if (blocked) {
			const {json} = await createBuilder<{code: string; reason: string}>(harness, account.token)
				.post('/stripe/checkout/gift')
				.body({price_id: GIFT_PRICE})
				.expect(403, APIErrorCodes.PREMIUM_PURCHASE_BLOCKED)
				.executeWithResponse();
			expect(json.reason).toBe('payment_dispute_open');
		}
	}

	async function staff(): Promise<TestAccount> {
		return setUserACLs(harness, await createTestAccount(harness), [AdminACLs.WILDCARD]);
	}

	async function reviews(admin: TestAccount): Promise<Array<ReviewResponse>> {
		const list = await createBuilder<{reviews: Array<ReviewResponse>}>(harness, admin.token)
			.get('/admin/payment-dispute-reviews')
			.execute();
		return list.reviews;
	}

	async function decide(
		admin: TestAccount,
		disputeId: string,
		decision: 'justified' | 'unjustified',
		note: string | null = null,
	): Promise<ReviewResponse> {
		return createBuilder<ReviewResponse>(harness, admin.token)
			.post(`/admin/payment-dispute-reviews/${disputeId}/decision`)
			.body({decision, note})
			.execute();
	}

	async function refuseUnjustified(admin: TestAccount, disputeId: string): Promise<Array<string>> {
		const refused = await createBuilder<{reasons: Array<string>}>(harness, admin.token)
			.post(`/admin/payment-dispute-reviews/${disputeId}/decision`)
			.body({decision: 'unjustified', note: null})
			.expect(409, APIErrorCodes.PAYMENT_DISPUTE_EXEMPT)
			.execute();
		return refused.reasons;
	}

	async function systemDms(account: TestAccount): Promise<Array<string>> {
		const channels = await createBuilder<Array<{id: string; recipients?: Array<{id: string}>}>>(harness, account.token)
			.get('/users/@me/channels')
			.execute();
		const systemChannels = channels.filter((channel) => channel.recipients?.some((recipient) => recipient.id === '0'));
		const messages = await Promise.all(
			systemChannels.map((channel) =>
				createBuilder<Array<{content: string | null}>>(harness, account.token)
					.get(`/channels/${channel.id}/messages?limit=50`)
					.execute(),
			),
		);
		return messages.flat().map((message) => message.content ?? '');
	}

	async function scheduleLegacyDisputeDeletion(account: TestAccount, scheduledBy: UserID): Promise<void> {
		const user = await userRepository.findUniqueAssert(idOf(account));
		await userRepository.updateDeletionSchedule(user, {
			flags: (user.flags | UserFlags.DELETED) & ~UserFlags.SELF_DELETED,
			deletion_reason_code: DeletionReasons.BILLING_DISPUTE_OR_ABUSE,
			pending_deletion_at: new Date(Date.now() + 30 * DAY_MS),
			deletion_scheduled_by: scheduledBy,
			deletion_scheduled_at: new Date(),
		});
	}

	describe('while a dispute is open', () => {
		test('a subscription dispute opens a review and pauses purchases while Plutonium keeps working', async () => {
			const buyer = await subscriber('pi_open_subscription');
			await openDispute('pi_open_subscription');
			await expectAccountUntouched(buyer);
			expect(await premiumType(buyer)).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect((await userRepository.findUnique(idOf(buyer)))?.stripeSubscriptionId).toBe('sub_pi_open_subscription');
			expect(stripe.spies.cancelledSubscriptions).toEqual([]);
			expect(stripe.spies.updatedSubscriptions).toEqual([]);
			expect(await disputes.findReview('dp_pi_open_subscription')).toMatchObject({
				product: 'plutonium',
				order_id: 'cs_pi_open_subscription',
				purchaser_id: idOf(buyer),
				status: 'open',
				closed_at: null,
			});
			await expectPurchasesBlocked(buyer, true);
			expect(stripe.spies.createdCheckoutSessions).toEqual([]);
		});

		test('a new review emails support once', async () => {
			await subscriber('pi_open_alert');
			await openDispute('pi_open_alert');
			await sendDispute('charge.dispute.updated', 'pi_open_alert', {status: 'needs_response', dueInDays: 12});
			const alerts = (await listTestEmails(harness, {recipient: 'support@fluxer.com'})).filter(
				(email) => email.type === 'payment_dispute_review_alert' && email.metadata.dispute_id === 'dp_pi_open_alert',
			);
			expect(alerts).toHaveLength(1);
			expect(alerts[0]?.metadata).toMatchObject({product: 'plutonium', reason: 'general'});
		});

		test('changing the billing cycle, reactivating and moving to the list price are refused too', async () => {
			const buyer = await subscriber('pi_open_changes');
			const user = await userRepository.findUniqueAssert(idOf(buyer));
			await userRepository.patchUpsert(user.id, {premium_will_cancel: true}, user.toRow());
			await openDispute('pi_open_changes');
			const requests: Array<[string, Record<string, unknown>]> = [
				['/premium/change-subscription', {billing_cycle: 'yearly'}],
				['/premium/reactivate-subscription', {}],
				['/premium/switch-to-list-price', {}],
			];
			for (const [path, body] of requests) {
				const {json} = await createBuilder<{reason: string}>(harness, buyer.token)
					.post(path)
					.body(body)
					.expect(403, APIErrorCodes.PREMIUM_PURCHASE_BLOCKED)
					.executeWithResponse();
				expect(json.reason).toBe('payment_dispute_open');
			}
			expect(stripe.spies.updatedSubscriptions).toEqual([]);
		});

		test('an inquiry pauses purchases too', async () => {
			const buyer = await subscriber('pi_open_inquiry');
			await sendDispute('charge.dispute.created', 'pi_open_inquiry', {status: 'warning_needs_response'});
			await expectAccountUntouched(buyer);
			expect(await premiumType(buyer)).toBe(UserPremiumTypes.SUBSCRIPTION);
			await expectPurchasesBlocked(buyer, true);
		});

		test('a fraudulent dispute is reported and only its payment method is blocked at Stripe', async () => {
			const chargeId = 'ch_pi_open_fraud';
			stripe = createStripeApiHandlers({
				charges: {
					[chargeId]: {
						billing_details: {email: 'fraudster@example.com'},
						customer: 'cus_pi_open_fraud',
						payment_intent: 'pi_open_fraud',
						payment_method_details: {card: {fingerprint: 'fp_open_fraud'}},
					},
				},
			});
			server.use(...stripe.handlers);
			const buyer = await subscriber('pi_open_fraud');
			await sendWebhook({
				type: 'charge.dispute.created',
				data: {
					object: {
						id: 'dp_pi_open_fraud',
						amount: 2500,
						currency: 'usd',
						charge: chargeId,
						created: Math.floor(Date.now() / 1000),
						evidence: {customer_email_address: 'fraudster@example.com', customer_purchase_ip: '203.0.113.42'},
						evidence_details: {has_evidence: false, due_by: Math.floor((Date.now() + 10 * DAY_MS) / 1000)},
						payment_intent: 'pi_open_fraud',
						reason: 'fraudulent',
						status: 'needs_response',
					},
				},
			});
			expect(stripe.spies.updatedCharges).toEqual([
				{id: chargeId, params: {fraud_details: {user_report: 'fraudulent'}}},
			]);
			expect(stripe.spies.createdValueLists.map((list) => list.itemType)).toEqual(['card_fingerprint']);
			expect(stripe.spies.createdValueListItems.map((item) => item.value)).toEqual(['fp_open_fraud']);
			await expectAccountUntouched(buyer);
			expect(stripe.spies.cancelledSubscriptions).toEqual([]);
			const admin = await staff();
			expect(await refuseUnjustified(admin, 'dp_pi_open_fraud')).toEqual(['exempt_reason', 'during_withdrawal']);
		});

		test('a gift dispute leaves the recipient alone and pauses the buyer', async () => {
			const buyer = await createTestAccount(harness);
			const recipient = await createTestAccount(harness);
			await gift(buyer, 'pi_open_gift', {redeemer: recipient});
			await openDispute('pi_open_gift');
			expect(await premiumType(recipient)).toBe(UserPremiumTypes.SUBSCRIPTION);
			await expectAccountUntouched(buyer);
			await expectPurchasesBlocked(buyer, true);
			await expectPurchasesBlocked(recipient, false);
			const emails = await listTestEmails(harness, {recipient: recipient.email});
			expect(findLastTestEmail(emails, 'gift_chargeback_notification')).toBeNull();
		});

		test('an unredeemed gift code keeps working while the dispute is open', async () => {
			const buyer = await createTestAccount(harness);
			const code = await gift(buyer, 'pi_open_unredeemed');
			await openDispute('pi_open_unredeemed');
			expect((await userRepository.findGiftCode(code))?.revokedAt).toBeNull();
			await expectAccountUntouched(buyer);
		});

		test('a renewal dispute finds the buyer through the Stripe customer', async () => {
			const buyer = await createTestUserWithPremium(harness, UserPremiumTypes.SUBSCRIPTION, {
				premiumUntil: new Date(Date.now() + 30 * DAY_MS),
				stripeCustomerId: 'cus_renewal',
				stripeSubscriptionId: 'sub_renewal',
			});
			await sendDispute('charge.dispute.created', 'pi_renewal', {
				status: 'needs_response',
				charge: {id: 'ch_pi_renewal', object: 'charge', customer: 'cus_renewal'},
			});
			await expectAccountUntouched(buyer);
			expect(await disputes.findReview('dp_pi_renewal')).toMatchObject({
				order_id: 'pi_renewal',
				purchaser_id: idOf(buyer),
			});
		});

		test('an updated dispute moves the evidence deadline of an open review', async () => {
			await subscriber('pi_open_updated');
			await openDispute('pi_open_updated', {dueInDays: 10});
			const first = await disputes.findReview('dp_pi_open_updated');
			await sendDispute('charge.dispute.updated', 'pi_open_updated', {status: 'needs_response', dueInDays: 12});
			const updated = await disputes.findReview('dp_pi_open_updated');
			expect(updated?.evidence_due_by?.getTime()).toBeGreaterThan(first?.evidence_due_by?.getTime() ?? 0);
		});

		test('a dispute for an unknown payment is retried', async () => {
			await sendWebhook(
				{
					type: 'charge.dispute.created',
					data: {object: {id: 'dp_unknown', payment_intent: 'pi_unknown', status: 'needs_response'}},
				},
				true,
			);
		});
	});

	describe('when a dispute closes without the money being returned', () => {
		test.each(['won', 'warning_closed'] as const)(
			'%s lifts the purchase pause and cancels a deletion the old chargeback flow scheduled',
			async (status) => {
				const buyer = await subscriber(`pi_restore_${status}`);
				await openDispute(`pi_restore_${status}`);
				await scheduleLegacyDisputeDeletion(buyer, SYSTEM_USER_ID);
				await closeDispute(`pi_restore_${status}`, status);
				const user = await userRepository.findUnique(idOf(buyer));
				expect((user?.flags ?? 0n) & UserFlags.DELETED).toBe(0n);
				expect(user?.deletionReasonCode).toBeNull();
				expect(user?.pendingDeletionAt).toBeNull();
				expect(user?.lastRefundAt).toBeNull();
				const emails = await listTestEmails(harness, {recipient: buyer.email});
				expect(findLastTestEmail(emails, 'account_deletion_cancelled')).not.toBeNull();
				expect(await disputes.findReview(`dp_pi_restore_${status}`)).toMatchObject({outcome: status});
				expect(stripe.spies.updatedSubscriptions).toEqual([]);
				expect(stripe.spies.cancelledSubscriptions).toEqual([]);
				await expectPurchasesBlocked(buyer, false);
			},
		);

		test('a lost dispute leaves a deletion the old chargeback flow scheduled in place', async () => {
			const buyer = await subscriber('pi_legacy_lost');
			await openDispute('pi_legacy_lost');
			await scheduleLegacyDisputeDeletion(buyer, SYSTEM_USER_ID);
			await closeDispute('pi_legacy_lost', 'lost');
			const user = await userRepository.findUnique(idOf(buyer));
			expect((user?.flags ?? 0n) & UserFlags.DELETED).toBe(UserFlags.DELETED);
			expect(user?.pendingDeletionAt).not.toBeNull();
			const emails = await listTestEmails(harness, {recipient: buyer.email});
			expect(findLastTestEmail(emails, 'account_deletion_cancelled')).toBeNull();
		});

		test('a won dispute leaves a deletion staff scheduled in place', async () => {
			const buyer = await subscriber('pi_restore_staff');
			const admin = await staff();
			await scheduleLegacyDisputeDeletion(buyer, idOf(admin));
			await closeDispute('pi_restore_staff', 'won');
			const user = await userRepository.findUnique(idOf(buyer));
			expect((user?.flags ?? 0n) & UserFlags.DELETED).toBe(UserFlags.DELETED);
			expect(user?.pendingDeletionAt).not.toBeNull();
		});

		test('a won gift dispute keeps the gift with its recipient', async () => {
			const buyer = await createTestAccount(harness);
			const recipient = await createTestAccount(harness);
			await gift(buyer, 'pi_won_gift', {redeemer: recipient});
			await openDispute('pi_won_gift');
			await closeDispute('pi_won_gift', 'won');
			expect(await premiumType(recipient)).toBe(UserPremiumTypes.SUBSCRIPTION);
			await expectAccountUntouched(buyer);
		});

		test('purchases stay paused while another dispute is still open', async () => {
			const buyer = await subscriber('pi_two_first');
			await createTestPayment(harness, idOf(buyer), 'cs_pi_two_second', {
				paymentIntentId: 'pi_two_second',
				status: 'completed',
				stripeCustomerId: 'cus_pi_two_first',
			});
			await openDispute('pi_two_first');
			await openDispute('pi_two_second');
			await closeDispute('pi_two_first', 'won');
			await expectPurchasesBlocked(buyer, true);
			await closeDispute('pi_two_second', 'won');
			await expectPurchasesBlocked(buyer, false);
		});
	});

	describe('when a dispute is lost', () => {
		test('a subscription ends right away, the account is untouched and purchases come back', async () => {
			const buyer = await subscriber('pi_lost_subscription');
			await loseDispute('pi_lost_subscription');
			await expectAccountUntouched(buyer);
			expect(stripe.spies.cancelledSubscriptions).toEqual(['sub_pi_lost_subscription']);
			const user = await userRepository.findUnique(idOf(buyer));
			expect(user?.stripeSubscriptionId).toBeNull();
			expect(await premiumType(buyer)).toBe(UserPremiumTypes.NONE);
			expect(await disputes.findReview('dp_pi_lost_subscription')).toMatchObject({
				product: 'plutonium',
				order_id: 'cs_pi_lost_subscription',
				purchaser_id: idOf(buyer),
				is_gift: false,
				reason: 'general',
				status: 'open',
				outcome: 'lost',
			});
			await expectPurchasesBlocked(buyer, false);
		});

		test('a gift payment whose code was never issued leaves the subscription alone', async () => {
			const buyer = await subscriber('pi_lost_one_time', {productType: 'gift_1_month'});
			await loseDispute('pi_lost_one_time');
			expect(stripe.spies.cancelledSubscriptions).toEqual([]);
			expect(stripe.spies.updatedSubscriptions).toEqual([]);
			await expectAccountUntouched(buyer);
		});

		test('a redeemed gift is taken back from its recipient, who is told', async () => {
			const buyer = await createTestAccount(harness);
			const recipient = await createTestAccount(harness);
			await gift(buyer, 'pi_lost_gift', {redeemer: recipient});
			await loseDispute('pi_lost_gift');
			expect(await premiumType(recipient)).toBe(UserPremiumTypes.NONE);
			const emails = await listTestEmails(harness, {recipient: recipient.email});
			expect(findLastTestEmail(emails, 'gift_chargeback_notification')).not.toBeNull();
			await expectAccountUntouched(buyer);
			expect(await disputes.findReview('dp_pi_lost_gift')).toMatchObject({
				product: 'plutonium',
				is_gift: true,
				purchaser_id: idOf(buyer),
				order_id: 'cs_pi_lost_gift',
			});
		});

		test('an unredeemed gift code stops working', async () => {
			const buyer = await createTestAccount(harness);
			const code = await gift(buyer, 'pi_lost_unredeemed');
			await loseDispute('pi_lost_unredeemed');
			expect((await userRepository.findGiftCode(code))?.revokedAt).toBeInstanceOf(Date);
			await expectAccountUntouched(buyer);
		});

		test('a lifetime recipient keeps lifetime when a gift is taken back', async () => {
			const buyer = await createTestAccount(harness);
			const recipient = await createTestAccount(harness);
			await gift(buyer, 'pi_lost_lifetime', {redeemer: recipient});
			await createBuilder(harness, recipient.token)
				.post(`/test/users/${recipient.userId}/premium`)
				.body({premium_type: UserPremiumTypes.LIFETIME})
				.execute();
			await loseDispute('pi_lost_lifetime');
			expect(await premiumType(recipient)).toBe(UserPremiumTypes.LIFETIME);
		});

		test('a lost dispute that opened before the review existed still takes back what it bought', async () => {
			const buyer = await subscriber('pi_lost_no_review');
			await closeDispute('pi_lost_no_review', 'lost');
			expect(stripe.spies.cancelledSubscriptions).toEqual(['sub_pi_lost_no_review']);
			expect(await disputes.findReview('dp_pi_lost_no_review')).toMatchObject({status: 'open', outcome: 'lost'});
			await expectAccountUntouched(buyer);
			await expectPurchasesBlocked(buyer, false);
		});
	});

	describe('review', () => {
		test.each(EXEMPT_REASONS)('a %s dispute can never be found unjustified', async (reason) => {
			const buyer = await subscriber(`pi_reason_${reason}`);
			await openDispute(`pi_reason_${reason}`, {reason});
			const admin = await staff();
			expect(await refuseUnjustified(admin, `dp_pi_reason_${reason}`)).toEqual(['exempt_reason']);
			await expectAccountUntouched(buyer);
			expect((await disputes.findReview(`dp_pi_reason_${reason}`))?.status).toBe('open');
		});

		test.each(REVIEWABLE_REASONS)('a %s dispute after the withdrawal period is warnable', async (reason) => {
			await subscriber(`pi_reviewable_${reason}`);
			await openDispute(`pi_reviewable_${reason}`, {reason});
			const admin = await staff();
			expect(await reviews(admin)).toEqual([
				expect.objectContaining({
					dispute_id: `dp_pi_reviewable_${reason}`,
					product: 'plutonium',
					reason,
					blockers: [],
					warnable: true,
					closed_at: null,
				}),
			]);
		});

		test('a Plutonium dispute stays out of reach until the closure terms take effect', async () => {
			Config.paymentDisputes.closureEffectiveAt = null;
			try {
				const buyer = await subscriber('pi_before_terms');
				await openDispute('pi_before_terms', {reason: 'general'});
				const admin = await staff();
				expect(await refuseUnjustified(admin, 'dp_pi_before_terms')).toEqual(['before_terms_effective']);
				await loseDispute('pi_before_terms');
				await expectAccountUntouched(buyer);
				const emails = await listTestEmails(harness, {recipient: buyer.email});
				expect(findLastTestEmail(emails, 'payment_dispute_warning')).toBeNull();
				expect((await disputes.findReview('dp_pi_before_terms'))?.status).toBe('open');
			} finally {
				Config.paymentDisputes.closureEffectiveAt = new Date('2000-01-01T00:00:00Z');
			}
		});

		test('a dispute inside the withdrawal period is never unjustified', async () => {
			const buyer = await subscriber('pi_withdrawal_window');
			await openDispute('pi_withdrawal_window', {daysAfterPayment: 13});
			const admin = await staff();
			expect(await refuseUnjustified(admin, 'dp_pi_withdrawal_window')).toEqual(['during_withdrawal']);
			await expectAccountUntouched(buyer);
		});

		test('the withdrawal period runs to the end of the 14th day, past 14 times 24 hours', async () => {
			await subscriber('pi_withdrawal_last_day');
			await openDispute('pi_withdrawal_last_day', {daysAfterPayment: 14 + 1 / 1440});
			const admin = await staff();
			expect(await refuseUnjustified(admin, 'dp_pi_withdrawal_last_day')).toEqual(['during_withdrawal']);
		});

		test('a dispute on a payment refunded before the dispute is never unjustified', async () => {
			await subscriber('pi_refunded_first', {status: 'refunded'});
			await openDispute('pi_refunded_first');
			const admin = await staff();
			expect(await refuseUnjustified(admin, 'dp_pi_refunded_first')).toEqual(['refunded_before_dispute']);
		});

		test('a gift payment whose code was never issued is never unjustified', async () => {
			await subscriber('pi_gift_never_issued', {isGift: true, productType: 'gift_1_month'});
			await openDispute('pi_gift_never_issued');
			const admin = await staff();
			expect(await refuseUnjustified(admin, 'dp_pi_gift_never_issued')).toEqual(['perks_not_provided']);
		});

		test('justified is final and sends nothing', async () => {
			const buyer = await subscriber('pi_justified');
			await openDispute('pi_justified');
			const admin = await staff();
			const decided = await decide(admin, 'dp_pi_justified', 'justified', 'The card was shared with a partner.');
			expect(decided).toMatchObject({status: 'justified', warned_at: null, warning_deadline: null});
			expect(await decide(admin, 'dp_pi_justified', 'unjustified')).toMatchObject({status: 'justified'});
			expect(await systemDms(buyer)).toEqual([]);
			expect(
				findLastTestEmail(await listTestEmails(harness, {recipient: buyer.email}), 'payment_dispute_warning'),
			).toBeNull();
		});

		test('a closed dispute can no longer be decided', async () => {
			await subscriber('pi_closed_decision');
			await loseDispute('pi_closed_decision');
			const admin = await staff();
			await createBuilder(harness, admin.token)
				.post('/admin/payment-dispute-reviews/dp_pi_closed_decision/decision')
				.body({decision: 'unjustified', note: null})
				.expect(409, APIErrorCodes.PAYMENT_DISPUTE_REVIEW_CLOSED)
				.execute();
		});

		test('a warning date less than 2 days away is refused', async () => {
			await subscriber('pi_too_late');
			await openDispute('pi_too_late', {dueInDays: 4});
			const admin = await staff();
			expect((await reviews(admin))[0]).toMatchObject({warnable: false, warning_deadline: null});
			await createBuilder(harness, admin.token)
				.post('/admin/payment-dispute-reviews/dp_pi_too_late/decision')
				.body({decision: 'unjustified', note: null})
				.expect(409, APIErrorCodes.PAYMENT_DISPUTE_WARNING_TOO_LATE)
				.execute();
		});

		test('an unknown review is a 404', async () => {
			const admin = await staff();
			await createBuilder(harness, admin.token)
				.post('/admin/payment-dispute-reviews/dp_missing/decision')
				.body({decision: 'justified', note: null})
				.expect(404, APIErrorCodes.UNKNOWN_PAYMENT_DISPUTE_REVIEW)
				.execute();
		});
	});

	describe('warning and deletion', () => {
		test('unjustified warns by email and system message with a date 3 days before the evidence deadline', async () => {
			const buyer = await subscriber('pi_warn');
			await openDispute('pi_warn', {dueInDays: 10});
			const review = await disputes.findReview('dp_pi_warn');
			const admin = await staff();
			const decided = await decide(admin, 'dp_pi_warn', 'unjustified', 'Plutonium was used for three weeks.');
			const expectedDeadline = (review?.evidence_due_by?.getTime() ?? 0) - 3 * DAY_MS;
			expect(decided.status).toBe('unjustified');
			expect(new Date(decided.warning_deadline ?? 0).getTime()).toBe(expectedDeadline);
			expect(decided.warning_email_sent_at).not.toBeNull();
			expect(decided.warning_dm_sent_at).not.toBeNull();
			expect(decided.warned_at).not.toBeNull();
			const email = findLastTestEmail(
				await listTestEmails(harness, {recipient: buyer.email}),
				'payment_dispute_warning',
			);
			expect(email?.metadata).toMatchObject({
				product: 'plutonium',
				invoice_number: '',
				warning_date: new Date(expectedDeadline).toISOString(),
			});
			const dms = await systemDms(buyer);
			expect(dms).toHaveLength(1);
			expect(dms[0]).toContain(`If the dispute isn't withdrawn before ${longDate(decided.warning_deadline!)}`);
			expect(dms[0]).toContain("we'll permanently delete your");
			expect(dms[0]).toContain("you can't buy anything");
			expect(dms[0]).toContain('copy of your data');
			expect(dms[0]).toContain('Plutonium was used for three weeks.');
			await expectAccountUntouched(buyer);
			await expectPurchasesBlocked(buyer, true);
		});

		async function warningEmails(account: TestAccount): Promise<number> {
			const emails = await listTestEmails(harness, {recipient: account.email});
			return emails.filter((email) => email.type === 'payment_dispute_warning').length;
		}

		test('a warning whose email failed counts only after a retry sends the email alone', async () => {
			const buyer = await subscriber('pi_warn_email_retry');
			await openDispute('pi_warn_email_retry');
			const admin = await staff();
			const smtp = await warningSmtp('reject');
			const first = await decide(admin, 'dp_pi_warn_email_retry', 'unjustified', 'Plutonium was used.');
			expect(first.status).toBe('unjustified');
			expect(first.warning_email_sent_at).toBeNull();
			expect(first.warning_dm_sent_at).not.toBeNull();
			expect(first.warned_at).toBeNull();
			expect(smtp.attempts).toHaveLength(1);
			expect(smtp.attempts[0]).toMatchObject({from: '<warnings@example.com>', to: `<${buyer.email}>`});
			expect(smtp.delivered).toEqual([]);
			expect(await systemDms(buyer)).toHaveLength(1);

			const retried = await decide(admin, 'dp_pi_warn_email_retry', 'unjustified');
			expect(retried.warning_email_sent_at).not.toBeNull();
			expect(retried.warning_dm_sent_at).toBe(first.warning_dm_sent_at);
			expect(retried.warned_at).not.toBeNull();
			expect(retried.warning_deadline).toBe(first.warning_deadline);
			expect(smtp.attempts).toHaveLength(2);
			expect(smtp.delivered).toHaveLength(1);
			expect(smtp.delivered[0]).toMatchObject({from: '<warnings@example.com>', to: `<${buyer.email}>`});
			expect(await systemDms(buyer)).toHaveLength(1);

			const again = await decide(admin, 'dp_pi_warn_email_retry', 'unjustified');
			expect(again.warned_at).toBe(retried.warned_at);
			expect(smtp.attempts).toHaveLength(2);
			expect(smtp.delivered).toHaveLength(1);
			expect(await systemDms(buyer)).toHaveLength(1);
		});

		test('with DMs off on the instance the email alone completes the warning', async () => {
			const buyer = await subscriber('pi_warn_dms_off');
			await openDispute('pi_warn_dms_off');
			await getInstanceConfigRepository().setInstancePolicyConfig({direct_messages_disabled: true});
			const admin = await staff();
			const decided = await decide(admin, 'dp_pi_warn_dms_off', 'unjustified');
			expect(decided.warning_email_sent_at).not.toBeNull();
			expect(decided.warning_dm_sent_at).toBeNull();
			expect(decided.warned_at).not.toBeNull();
			expect(await warningEmails(buyer)).toBe(1);
			expect(await systemDms(buyer)).toHaveLength(0);
		});

		test('a second decision while the warning is being sent is refused and sends nothing', async () => {
			const buyer = await subscriber('pi_warn_concurrent');
			await openDispute('pi_warn_concurrent');
			const admin = await staff();
			const smtp = await warningSmtp('hold');
			const first = decide(admin, 'dp_pi_warn_concurrent', 'unjustified');
			pendingDecisions.add(first);
			await smtp.entered;
			expect(smtp.attempts).toHaveLength(1);
			expect(smtp.attempts[0]).toMatchObject({from: '<warnings@example.com>', to: `<${buyer.email}>`});
			expect(smtp.delivered).toEqual([]);
			await createBuilder(harness, admin.token)
				.post('/admin/payment-dispute-reviews/dp_pi_warn_concurrent/decision')
				.body({decision: 'unjustified', note: null})
				.expect(409, APIErrorCodes.CONFLICT)
				.execute();
			expect(smtp.attempts).toHaveLength(1);
			smtp.release();
			expect((await first).warned_at).not.toBeNull();
			expect(smtp.delivered).toHaveLength(1);
			expect(await systemDms(buyer)).toHaveLength(1);
		});

		test('an incomplete warning never leads to a closure', async () => {
			const buyer = await subscriber('pi_warn_incomplete');
			await openDispute('pi_warn_incomplete');
			const admin = await staff();
			const smtp = await warningSmtp('reject');
			await decide(admin, 'dp_pi_warn_incomplete', 'unjustified');
			expect(smtp.attempts).toHaveLength(1);
			expect(smtp.attempts[0]).toMatchObject({from: '<warnings@example.com>', to: `<${buyer.email}>`});
			expect(smtp.delivered).toEqual([]);
			await passWarningDate('dp_pi_warn_incomplete');
			await closeDispute('pi_warn_incomplete', 'lost');
			const user = await userRepository.findUniqueAssert(idOf(buyer));
			expect(user.flags & UserFlags.DELETED).toBe(0n);
			expect((await disputes.findReview('dp_pi_warn_incomplete'))?.enforcement_decision_id ?? null).toBeNull();
		});

		test('a withdrawn dispute after a warning leads to nothing and lifts the pause', async () => {
			const buyer = await subscriber('pi_warn_withdrawn');
			await openDispute('pi_warn_withdrawn');
			const admin = await staff();
			await decide(admin, 'dp_pi_warn_withdrawn', 'unjustified');
			await closeDispute('pi_warn_withdrawn', 'won');
			await expectAccountUntouched(buyer);
			expect(await premiumType(buyer)).toBe(UserPremiumTypes.SUBSCRIPTION);
			expect(stripe.spies.cancelledSubscriptions).toEqual([]);
			await expectPurchasesBlocked(buyer, false);
		});

		test('a lost dispute after a warning takes back Plutonium and closes the account by an enforcement decision', async () => {
			const buyer = await subscriber('pi_warn_lost');
			await openDispute('pi_warn_lost');
			const admin = await staff();
			await decide(admin, 'dp_pi_warn_lost', 'unjustified', 'Plutonium was used for three weeks.');
			await passWarningDate('dp_pi_warn_lost');
			await closeDispute('pi_warn_lost', 'lost');
			expect(stripe.spies.cancelledSubscriptions).toEqual(['sub_pi_warn_lost']);
			const review = await disputes.findReview('dp_pi_warn_lost');
			const decisionId = review?.enforcement_decision_id ?? null;
			expect(decisionId).not.toBeNull();
			const user = await userRepository.findUniqueAssert(idOf(buyer));
			expect(user.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
			expect(user.deletionReasonCode).toBe(DeletionReasons.BILLING_DISPUTE_OR_ABUSE);
			expect(user.deletionScheduledBy).toBe(idOf(admin));
			expect(user.enforcementDecisionId).toBe(decisionId);
			const daysUntilDeletion = ((user.pendingDeletionAt?.getTime() ?? 0) - Date.now()) / DAY_MS;
			expect(daysUntilDeletion).toBeGreaterThan(58.9);
			expect(daysUntilDeletion).toBeLessThan(60.1);
			const decision = await createBuilder<EnforcementDecisionResponse>(harness, admin.token)
				.get(`/admin/enforcement-decisions/${decisionId}`)
				.execute();
			expect(decision).toMatchObject({
				kind: 'closure',
				reason_code: DeletionReasons.BILLING_DISPUTE_OR_ABUSE,
				source: 'own_review',
				automation: 'none',
				decided_by: admin.userId,
				statement: 'Plutonium was used for three weeks.',
				deletion_at: user.pendingDeletionAt?.toISOString(),
				notice_status: 'sent',
			});
			expect(decision.notice.rule?.url).toBe(`${Config.endpoints.marketing}/terms#4-4-failed-payments-and-disputes`);
			expect(decision.notes[0]?.body).toContain('dp_pi_warn_lost');
			const emails = await listTestEmails(harness, {recipient: buyer.email});
			expect(findLastTestEmail(emails, 'enforcement_notice')?.metadata).toMatchObject({
				decision_id: String(decisionId),
				kind_label: 'closure',
				statement: 'Plutonium was used for three weeks.',
			});
			expect(findLastTestEmail(emails, 'payment_dispute_warning')).not.toBeNull();
			expect(await new AdminRepository().isEmailBanned(buyer.email)).toBe(true);
			const reviewed = (await reviews(admin)).find((entry) => entry.dispute_id === 'dp_pi_warn_lost');
			expect(reviewed?.enforcement_decision_id).toBe(String(decisionId));
			await createBuilder(harness, admin.token)
				.delete(`/admin/users/${buyer.userId}/deletion`)
				.body({expected_pending_deletion_at: user.pendingDeletionAt?.toISOString()})
				.expect(409, APIErrorCodes.ENFORCEMENT_DECISION_OWNS_STATE)
				.execute();
			await createBuilder(harness, admin.token)
				.put(`/admin/users/${buyer.userId}/deletion`)
				.body({
					basis: {type: 'holder_request'},
					days_until_deletion: 90,
					replace_pending_deletion_at: user.pendingDeletionAt?.toISOString(),
				})
				.expect(409, APIErrorCodes.ENFORCEMENT_DECISION_OWNS_STATE)
				.execute();
			const unchanged = await userRepository.findUniqueAssert(idOf(buyer));
			expect(unchanged.pendingDeletionAt?.getTime()).toBe(user.pendingDeletionAt?.getTime());
		});

		test('a redelivered lost dispute records no second decision', async () => {
			const buyer = await subscriber('pi_warn_lost_twice');
			await openDispute('pi_warn_lost_twice');
			const admin = await staff();
			await decide(admin, 'dp_pi_warn_lost_twice', 'unjustified');
			await passWarningDate('dp_pi_warn_lost_twice');
			await closeDispute('pi_warn_lost_twice', 'lost');
			await closeDispute('pi_warn_lost_twice', 'lost');
			const emails = await listTestEmails(harness, {recipient: buyer.email});
			expect(emails.filter((email) => email.type === 'enforcement_notice')).toHaveLength(1);
		});

		test('a closure keeps an earlier deletion the account holder scheduled', async () => {
			const buyer = await subscriber('pi_warn_holder');
			await openDispute('pi_warn_holder');
			const admin = await staff();
			await decide(admin, 'dp_pi_warn_holder', 'unjustified');
			const holderDeletionAt = new Date(Date.now() + 20 * DAY_MS);
			const user = await userRepository.findUniqueAssert(idOf(buyer));
			await userRepository.updateDeletionSchedule(user, {
				flags: user.flags | UserFlags.SELF_DELETED,
				pending_deletion_at: holderDeletionAt,
				deletion_reason_code: DeletionReasons.USER_REQUESTED,
			});
			await passWarningDate('dp_pi_warn_holder');
			await closeDispute('pi_warn_holder', 'lost');
			const closed = await userRepository.findUniqueAssert(idOf(buyer));
			expect(closed.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
			expect(closed.pendingDeletionAt?.getTime()).toBe(holderDeletionAt.getTime());
			const decision = await createBuilder<EnforcementDecisionResponse>(harness, admin.token)
				.get(`/admin/enforcement-decisions/${closed.enforcementDecisionId}`)
				.execute();
			expect(decision.deletion_at).toBe(holderDeletionAt.toISOString());
			expect(decision.notes[0]?.body).toContain(
				`The account holder had already scheduled the account for deletion on ${holderDeletionAt.toISOString()}`,
			);
		});

		test('a second lost dispute joins the closure and a reversal keeps it until both are reversed', async () => {
			const buyer = await subscriber('pi_two_lost_a');
			await createTestPayment(harness, idOf(buyer), 'cs_pi_two_lost_b', {
				paymentIntentId: 'pi_two_lost_b',
				status: 'completed',
				stripeCustomerId: 'cus_pi_two_lost_a',
			});
			await openDispute('pi_two_lost_a');
			await openDispute('pi_two_lost_b');
			const admin = await staff();
			await decide(admin, 'dp_pi_two_lost_a', 'unjustified');
			await decide(admin, 'dp_pi_two_lost_b', 'unjustified');
			await passWarningDate('dp_pi_two_lost_a');
			await passWarningDate('dp_pi_two_lost_b');
			await closeDispute('pi_two_lost_a', 'lost');
			await closeDispute('pi_two_lost_b', 'lost');
			const decisionId = (await disputes.findReview('dp_pi_two_lost_a'))?.enforcement_decision_id ?? null;
			expect(decisionId).not.toBeNull();
			expect((await disputes.findReview('dp_pi_two_lost_b'))?.enforcement_decision_id).toBe(decisionId);
			let emails = await listTestEmails(harness, {recipient: buyer.email});
			expect(emails.filter((email) => email.type === 'enforcement_notice')).toHaveLength(1);
			await createBuilder(harness, admin.token)
				.post('/admin/payment-dispute-reviews/dp_pi_two_lost_a/reversal')
				.body({note: 'The first charge was taken twice.'})
				.execute();
			const stillClosed = await userRepository.findUniqueAssert(idOf(buyer));
			expect(stillClosed.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
			expect(stillClosed.enforcementDecisionId).toBe(decisionId);
			const kept = await createBuilder<EnforcementDecisionResponse>(harness, admin.token)
				.get(`/admin/enforcement-decisions/${decisionId}`)
				.execute();
			expect(kept.status).toBe('active');
			expect(kept.notes.some((note) => note.body.includes('dp_pi_two_lost_b'))).toBe(true);
			emails = await listTestEmails(harness, {recipient: buyer.email});
			expect(findLastTestEmail(emails, 'enforcement_ended')).toBeNull();
			await createBuilder(harness, admin.token)
				.post('/admin/payment-dispute-reviews/dp_pi_two_lost_b/reversal')
				.body({note: 'The second charge was taken twice too.'})
				.execute();
			const restored = await userRepository.findUniqueAssert(idOf(buyer));
			expect(restored.flags & UserFlags.DELETED).toBe(0n);
			expect(restored.enforcementDecisionId).toBeNull();
		});

		test('a closure for another rule makes the dispute closure wait until that decision is lifted', async () => {
			const buyer = await subscriber('pi_warn_waits');
			await openDispute('pi_warn_waits');
			const admin = await staff();
			await decide(admin, 'dp_pi_warn_waits', 'unjustified', 'Plutonium was used for three weeks.');
			const spamDecisionId = await close(harness, admin, buyer.userId);
			await passWarningDate('dp_pi_warn_waits');
			await closeDispute('pi_warn_waits', 'lost');
			expect((await disputes.findReview('dp_pi_warn_waits'))?.enforcement_decision_id ?? null).toBeNull();
			const waiting = (await reviews(admin)).find((entry) => entry.dispute_id === 'dp_pi_warn_waits');
			expect(waiting).toMatchObject({enforcement_decision_id: null, awaited_decision_id: spamDecisionId});

			await endDecision(harness, admin, spamDecisionId).execute();
			const decisionId = (await disputes.findReview('dp_pi_warn_waits'))?.enforcement_decision_id ?? null;
			expect(decisionId).not.toBeNull();
			expect(String(decisionId)).not.toBe(spamDecisionId);
			const user = await userRepository.findUniqueAssert(idOf(buyer));
			expect(user.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
			expect(user.enforcementDecisionId).toBe(decisionId);
			expect(user.deletionReasonCode).toBe(DeletionReasons.BILLING_DISPUTE_OR_ABUSE);
			expect(await new AdminRepository().isEmailBanned(buyer.email)).toBe(true);
			const decision = await createBuilder<EnforcementDecisionResponse>(harness, admin.token)
				.get(`/admin/enforcement-decisions/${decisionId}`)
				.execute();
			expect(decision).toMatchObject({
				kind: 'closure',
				reason_code: DeletionReasons.BILLING_DISPUTE_OR_ABUSE,
				statement: 'Plutonium was used for three weeks.',
				notice_status: 'sent',
			});
			const settled = (await reviews(admin)).find((entry) => entry.dispute_id === 'dp_pi_warn_waits');
			expect(settled).toMatchObject({
				enforcement_decision_id: String(decisionId),
				awaited_decision_id: null,
			});
			const emails = await listTestEmails(harness, {recipient: buyer.email});
			expect(emails.filter((email) => email.type === 'enforcement_notice')).toHaveLength(2);

			await endDecision(harness, admin, String(decisionId)).execute();
			expect((await disputes.findReview('dp_pi_warn_waits'))?.enforcement_decision_id).toBe(decisionId);
			const restored = await userRepository.findUniqueAssert(idOf(buyer));
			expect(restored.flags & UserFlags.DELETED).toBe(0n);
		});

		test('the hourly sweep records a waiting dispute closure that the end of the other decision missed', async () => {
			const buyer = await subscriber('pi_warn_swept');
			await openDispute('pi_warn_swept');
			const admin = await staff();
			await decide(admin, 'dp_pi_warn_swept', 'unjustified');
			const spamDecisionId = await close(harness, admin, buyer.userId);
			await passWarningDate('dp_pi_warn_swept');
			await closeDispute('pi_warn_swept', 'lost');
			setInjectedWorkerService(new NoopWorkerService());
			try {
				await endDecision(harness, admin, spamDecisionId).execute();
			} finally {
				setupSyncStripeWebhookWorker();
			}
			expect((await disputes.findReview('dp_pi_warn_swept'))?.enforcement_decision_id ?? null).toBeNull();
			const missed = (await reviews(admin)).find((entry) => entry.dispute_id === 'dp_pi_warn_swept');
			expect(missed).toMatchObject({enforcement_decision_id: null, awaited_decision_id: null});

			await closePaymentDisputeAccounts({}, createTestWorkerHelpers());
			expect((await disputes.findReview('dp_pi_warn_swept'))?.enforcement_decision_id ?? null).toBeNull();

			await backdateReview('dp_pi_warn_swept', 'closed_at', 11 * 60 * 1000);
			await closePaymentDisputeAccounts({}, createTestWorkerHelpers());
			const decisionId = (await disputes.findReview('dp_pi_warn_swept'))?.enforcement_decision_id ?? null;
			expect(decisionId).not.toBeNull();
			const user = await userRepository.findUniqueAssert(idOf(buyer));
			expect(user.flags & UserFlags.DELETED).toBe(UserFlags.DELETED);
			expect(user.enforcementDecisionId).toBe(decisionId);

			await closePaymentDisputeAccounts({}, createTestWorkerHelpers());
			expect((await disputes.findReview('dp_pi_warn_swept'))?.enforcement_decision_id).toBe(decisionId);
		});

		test('a dispute lost before the warning date takes back Plutonium and closes nothing', async () => {
			const buyer = await subscriber('pi_warn_early');
			await openDispute('pi_warn_early');
			const admin = await staff();
			await decide(admin, 'dp_pi_warn_early', 'unjustified');
			await closeDispute('pi_warn_early', 'lost');
			expect(stripe.spies.cancelledSubscriptions).toEqual(['sub_pi_warn_early']);
			await expectAccountUntouched(buyer);
			expect((await disputes.findReview('dp_pi_warn_early'))?.enforcement_decision_id).toBeNull();
			await expectPurchasesBlocked(buyer, false);
		});

		test('a reversal after an appeal ends the closure decision and sends the ended notice', async () => {
			const buyer = await subscriber('pi_warn_reversed');
			await openDispute('pi_warn_reversed');
			const admin = await staff();
			await decide(admin, 'dp_pi_warn_reversed', 'unjustified');
			await passWarningDate('dp_pi_warn_reversed');
			await closeDispute('pi_warn_reversed', 'lost');
			const decisionId = String((await disputes.findReview('dp_pi_warn_reversed'))?.enforcement_decision_id);
			const reversed = await createBuilder<ReviewResponse & {reversal_note: string}>(harness, admin.token)
				.post('/admin/payment-dispute-reviews/dp_pi_warn_reversed/reversal')
				.body({note: 'The bank statement shows the charge was taken twice.'})
				.execute();
			expect(reversed).toMatchObject({
				status: 'justified',
				reversal_note: 'The bank statement shows the charge was taken twice.',
				enforcement_decision_id: decisionId,
			});
			const user = await userRepository.findUniqueAssert(idOf(buyer));
			expect(user.flags & UserFlags.DELETED).toBe(0n);
			expect(user.pendingDeletionAt).toBeNull();
			expect(user.enforcementDecisionId).toBeNull();
			expect(await new AdminRepository().isEmailBanned(buyer.email)).toBe(false);
			const decision = await createBuilder<EnforcementDecisionResponse>(harness, admin.token)
				.get(`/admin/enforcement-decisions/${decisionId}`)
				.execute();
			expect(decision).toMatchObject({status: 'ended', end_outcome: 'reversed', ended_by: admin.userId});
			const emails = await listTestEmails(harness, {recipient: buyer.email});
			expect(findLastTestEmail(emails, 'enforcement_ended')?.metadata).toMatchObject({
				outcome: 'reversed',
				decision_id: decisionId,
				restores_access: 'yes',
			});
			expect(findLastTestEmail(emails, 'account_deletion_cancelled')).toBeNull();
			await createBuilder(harness, admin.token)
				.post('/admin/payment-dispute-reviews/dp_pi_warn_reversed/reversal')
				.body({note: 'Again.'})
				.expect(409, APIErrorCodes.CONFLICT)
				.execute();
		});

		test('a reversal after staff already ended the decision leaves it as it is', async () => {
			const buyer = await subscriber('pi_warn_ended');
			await openDispute('pi_warn_ended');
			const admin = await staff();
			await decide(admin, 'dp_pi_warn_ended', 'unjustified');
			await passWarningDate('dp_pi_warn_ended');
			await closeDispute('pi_warn_ended', 'lost');
			const decisionId = String((await disputes.findReview('dp_pi_warn_ended'))?.enforcement_decision_id);
			await createBuilder(harness, admin.token)
				.post(`/admin/enforcement-decisions/${decisionId}/end`)
				.header('X-Audit-Log-Reason', 'Lifted on appeal')
				.body({outcome: 'lifted'})
				.execute();
			const reversed = await createBuilder<ReviewResponse>(harness, admin.token)
				.post('/admin/payment-dispute-reviews/dp_pi_warn_ended/reversal')
				.body({note: 'The appeal showed a duplicate charge.'})
				.execute();
			expect(reversed.status).toBe('justified');
			const emails = await listTestEmails(harness, {recipient: buyer.email});
			expect(emails.filter((email) => email.type === 'enforcement_ended')).toHaveLength(1);
		});

		test('a lost justified dispute only takes back what the payment bought', async () => {
			const buyer = await subscriber('pi_justified_lost');
			await openDispute('pi_justified_lost');
			const admin = await staff();
			await decide(admin, 'dp_pi_justified_lost', 'justified');
			await closeDispute('pi_justified_lost', 'lost');
			expect(stripe.spies.cancelledSubscriptions).toEqual(['sub_pi_justified_lost']);
			await expectAccountUntouched(buyer);
		});
	});
});

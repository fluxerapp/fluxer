// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {addGiftCodeDuration} from '@app/api/models/GiftCode';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {HttpResponse, http} from 'msw';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const DAY_SECONDS = 24 * 60 * 60;

describe('gift redemption during payment grace', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		stripeHandlers = createStripeApiHandlers();
	});

	beforeEach(async () => {
		await harness.resetData();
		stripeHandlers.reset();
		server.use(...stripeHandlers.handlers);
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	interface Renewal {
		account: TestAccount;
		userId: UserID;
		subscriptionId: string;
		invoiceId: string;
		periodStart: number;
		periodEnd: number;
	}

	async function setUpRenewal(options: {
		subscriptionStatus: 'active' | 'canceled' | 'past_due';
		invoiceStatus: 'open' | 'paid' | 'void';
		daysSincePeriodStart: number;
		graceDays: number | null;
		premiumUntilPeriodEnd?: boolean;
	}): Promise<Renewal> {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const suffix = randomUUID().replaceAll('-', '').slice(0, 12);
		const subscriptionId = `sub_test_${suffix}`;
		const invoiceId = `in_test_${suffix}`;
		const periodStart = Math.floor(Date.now() / 1000) - options.daysSincePeriodStart * DAY_SECONDS;
		const periodEnd = periodStart + 30 * DAY_SECONDS;
		stripeHandlers.put.invoice({
			id: invoiceId,
			object: 'invoice',
			amount_due: 2500,
			amount_paid: options.invoiceStatus === 'paid' ? 2500 : 0,
			billing_reason: 'subscription_cycle',
			created: periodStart,
			currency: 'usd',
			customer: 'cus_test_1',
			lines: {object: 'list', data: [{period: {start: periodStart, end: periodEnd}}]},
			payments: {object: 'list', data: [], has_more: false, url: `/v1/invoices/${invoiceId}/payments`},
			status: options.invoiceStatus,
			parent: {type: 'subscription_details', subscription_details: {subscription: subscriptionId}},
		});
		stripeHandlers.put.subscription(subscriptionId, {
			status: options.subscriptionStatus,
			latest_invoice: invoiceId,
			current_period_start: periodStart,
			current_period_end: periodEnd,
			trial_end: null,
			metadata: {},
		});
		const user = await getUserRepository().findUniqueAssert(userId);
		await getUserRepository().patchUpsert(
			userId,
			{
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_since: new Date((periodStart - 60 * DAY_SECONDS) * 1000),
				premium_until: new Date((options.premiumUntilPeriodEnd ? periodEnd : periodStart) * 1000),
				premium_grace_ends_at:
					options.graceDays === null ? null : new Date((periodStart + options.graceDays * DAY_SECONDS) * 1000),
				premium_will_cancel: options.subscriptionStatus !== 'active',
				premium_billing_cycle: 'monthly',
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
			},
			user.toRow(),
		);
		return {account, userId, subscriptionId, invoiceId, periodStart, periodEnd};
	}

	async function createMonthGift(): Promise<string> {
		const creator = await createTestAccount(harness);
		const code = `gracegift${randomUUID().replaceAll('-', '').slice(0, 16)}`;
		await getUserRepository().createGiftCode({
			code,
			duration_months: null,
			duration_type: 'months',
			duration_quantity: 1,
			created_at: new Date(),
			created_by_user_id: createUserID(BigInt(creator.userId)),
			redeemed_at: null,
			redeemed_by_user_id: null,
			stripe_payment_intent_id: null,
			visionary_sequence_number: null,
			checkout_session_id: null,
			version: 1,
		});
		return code;
	}

	async function redeem(renewal: Renewal, code: string): Promise<void> {
		await createBuilder(harness, renewal.account.token)
			.post(`/gifts/${code}/redeem`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	}

	function oneMonthAfter(unix: number): number {
		return Math.floor(addGiftCodeDuration(new Date(unix * 1000), 'months', 1)!.getTime() / 1000);
	}

	it('voids the open renewal invoice and covers the unpaid month from its start', async () => {
		const renewal = await setUpRenewal({
			subscriptionStatus: 'past_due',
			invoiceStatus: 'open',
			daysSincePeriodStart: 3,
			graceDays: 7,
		});
		const code = await createMonthGift();

		await redeem(renewal, code);

		expect(stripeHandlers.spies.voidedInvoices).toEqual([
			{id: renewal.invoiceId, idempotencyKey: `gift_void_invoice:${renewal.userId}:${code}`},
		]);
		expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
		const update = stripeHandlers.spies.updatedSubscriptions[0]!;
		expect(update.id).toBe(renewal.subscriptionId);
		expect(Number(update.params.trial_end)).toBe(oneMonthAfter(renewal.periodStart));
		expect(update.params.proration_behavior).toBe('none');
		const user = await getUserRepository().findUniqueAssert(renewal.userId);
		expect(user.stripeSubscriptionId).toBe(renewal.subscriptionId);
		expect(user.premiumGraceEndsAt).toBeNull();
		expect(user.premiumGiftExtensionEndsAt?.getTime()).toBe(oneMonthAfter(renewal.periodStart) * 1000);
	});

	it('stacks a gift on an active subscription after the paid period without voiding anything', async () => {
		const renewal = await setUpRenewal({
			subscriptionStatus: 'active',
			invoiceStatus: 'paid',
			daysSincePeriodStart: 3,
			graceDays: null,
			premiumUntilPeriodEnd: true,
		});
		const code = await createMonthGift();

		await redeem(renewal, code);

		expect(stripeHandlers.spies.voidedInvoices).toHaveLength(0);
		expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
		expect(Number(stripeHandlers.spies.updatedSubscriptions[0]!.params.trial_end)).toBe(
			oneMonthAfter(renewal.periodEnd),
		);
	});

	it('grants the gift on the account once payment grace has ended', async () => {
		const renewal = await setUpRenewal({
			subscriptionStatus: 'past_due',
			invoiceStatus: 'open',
			daysSincePeriodStart: 20,
			graceDays: 7,
		});
		const code = await createMonthGift();

		await redeem(renewal, code);

		expect(stripeHandlers.spies.retrievedSubscriptions).toHaveLength(0);
		expect(stripeHandlers.spies.voidedInvoices).toHaveLength(0);
		expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
		const user = await getUserRepository().findUniqueAssert(renewal.userId);
		expect(user.premiumGiftExtensionEndsAt!.getTime()).toBeGreaterThan(Date.now());
	});

	it('grants the gift on the account when the subscription was canceled during grace', async () => {
		const renewal = await setUpRenewal({
			subscriptionStatus: 'canceled',
			invoiceStatus: 'open',
			daysSincePeriodStart: 3,
			graceDays: 7,
		});
		server.use(
			http.post(`https://api.stripe.com/v1/subscriptions/${renewal.subscriptionId}`, () =>
				HttpResponse.json(
					{error: {type: 'invalid_request_error', message: 'You cannot update a canceled subscription.'}},
					{status: 400},
				),
			),
		);
		const code = await createMonthGift();

		await redeem(renewal, code);

		expect(stripeHandlers.spies.voidedInvoices).toHaveLength(0);
		const user = await getUserRepository().findUniqueAssert(renewal.userId);
		expect(user.stripeSubscriptionId).toBeNull();
		expect(user.premiumGiftExtensionEndsAt!.getTime()).toBeGreaterThan(Date.now());
	});

	it('stacks after the period end when the renewal invoice is paid before the void lands', async () => {
		const renewal = await setUpRenewal({
			subscriptionStatus: 'past_due',
			invoiceStatus: 'open',
			daysSincePeriodStart: 3,
			graceDays: 7,
		});
		server.use(
			http.post(`https://api.stripe.com/v1/invoices/${renewal.invoiceId}/void`, () => {
				stripeHandlers.put.invoice({
					id: renewal.invoiceId,
					object: 'invoice',
					amount_due: 2500,
					amount_paid: 2500,
					billing_reason: 'subscription_cycle',
					created: renewal.periodStart,
					currency: 'usd',
					customer: 'cus_test_1',
					lines: {object: 'list', data: [{period: {start: renewal.periodStart, end: renewal.periodEnd}}]},
					payments: {object: 'list', data: [], has_more: false, url: `/v1/invoices/${renewal.invoiceId}/payments`},
					status: 'paid',
					parent: {type: 'subscription_details', subscription_details: {subscription: renewal.subscriptionId}},
				});
				return HttpResponse.json(
					{error: {type: 'invalid_request_error', message: 'You can only void an open invoice.'}},
					{status: 400},
				);
			}),
		);
		const code = await createMonthGift();

		await redeem(renewal, code);

		expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
		expect(Number(stripeHandlers.spies.updatedSubscriptions[0]!.params.trial_end)).toBe(
			oneMonthAfter(renewal.periodEnd),
		);
	});

	it('does not void or extend again when the same gift is applied twice', async () => {
		const renewal = await setUpRenewal({
			subscriptionStatus: 'past_due',
			invoiceStatus: 'open',
			daysSincePeriodStart: 3,
			graceDays: 7,
		});
		const extend = () =>
			createBuilder(harness, renewal.account.token)
				.post(`/test/users/${renewal.userId}/extend-subscription-trial`)
				.body({duration_type: 'months', duration_quantity: 1, idempotency_key: 'gift_test_rerun'})
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();

		await extend();
		await extend();

		expect(stripeHandlers.spies.voidedInvoices).toHaveLength(1);
		expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
	});

	it('still covers the unpaid month on a retry after the void landed but the trial update failed', async () => {
		const renewal = await setUpRenewal({
			subscriptionStatus: 'active',
			invoiceStatus: 'void',
			daysSincePeriodStart: 3,
			graceDays: 7,
		});

		await createBuilder(harness, renewal.account.token)
			.post(`/test/users/${renewal.userId}/extend-subscription-trial`)
			.body({duration_type: 'months', duration_quantity: 1, idempotency_key: 'gift_test_retry'})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		expect(stripeHandlers.spies.voidedInvoices).toHaveLength(0);
		expect(Number(stripeHandlers.spies.updatedSubscriptions[0]!.params.trial_end)).toBe(
			oneMonthAfter(renewal.periodStart),
		);
	});
});

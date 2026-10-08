// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {UserRow} from '@app/api/database/types/UserTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {GuildService} from '@app/api/guild/services/GuildService';
import {getCacheService, getPremiumStateReconciliationQueueService} from '@app/api/middleware/ServiceSingletons';
import {findUser} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {getStripeClient} from '@app/api/stripe/StripeClient';
import {StripeGiftReversalHandler} from '@app/api/stripe/services/StripeGiftReversalHandler';
import {StripePremiumService} from '@app/api/stripe/services/StripePremiumService';
import {StripeSubscriptionService} from '@app/api/stripe/services/StripeSubscriptionService';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createStripeApiHandlers, type StripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import type {GeoipResult} from '@pkgs/geoip/src/GeoipLookup';
import {ms} from 'itty-time';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

const {lookupGeoipMock} = vi.hoisted(() => ({
	lookupGeoipMock: vi.fn(),
}));

vi.mock('@app/api/utils/IpUtils', async (importOriginal) => ({
	...(await importOriginal<typeof import('@app/api/utils/IpUtils')>()),
	lookupGeoip: lookupGeoipMock,
}));

const MOCK_PRICES = {
	monthlyUsd: 'price_shift_monthly_usd',
	yearlyUsd: 'price_shift_yearly_usd',
	gift1MonthUsd: 'price_shift_gift_1_month_usd',
	gift1YearUsd: 'price_shift_gift_1_year_usd',
};

const MOCK_PRICE_SEEDS = {
	[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month' as const},
	[MOCK_PRICES.yearlyUsd]: {unit_amount: 4999, currency: 'usd', interval: 'year' as const},
};

const CLOCK_TOLERANCE_MS = ms('10 seconds');

type WriteOrder = 'checkout first' | 'webhook first';

const WRITE_ORDERS: Array<WriteOrder> = ['checkout first', 'webhook first'];

function geoipCountry(countryCode: string | null): GeoipResult {
	return {countryCode, normalizedIp: '203.0.113.10', city: null, region: null, countryName: null};
}

function giftTrialMetadata(code: string, days: number, paidUntilUnix?: number): Record<string, string> {
	return {
		[`gtrial_${code}`]: String(days * 24 * 60 * 60),
		...(paidUntilUnix ? {gtrial_paid_until: String(paidUntilUnix)} : {}),
	};
}

function expectNear(actual: Date | null | undefined, expectedMs: number): void {
	expect(actual).not.toBeNull();
	expect(Math.abs(actual!.getTime() - expectedMs)).toBeLessThanOrEqual(CLOCK_TOLERANCE_MS);
}

describe('gift time around a Stripe subscription', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let originalPrices: typeof Config.stripe.prices | undefined;
	const users = new UserRepository();
	const premiumService = new StripePremiumService(
		users,
		new NoopGatewayService(),
		{} as IGuildRepositoryAggregate,
		{} as GuildService,
	);

	beforeAll(async () => {
		originalPrices = Config.stripe.prices;
		Config.stripe.prices = MOCK_PRICES;
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.prices = originalPrices;
	});
	beforeEach(async () => {
		await harness.resetData();
		Config.stripe.prices = MOCK_PRICES;
		lookupGeoipMock.mockReset();
		lookupGeoipMock.mockResolvedValue(geoipCountry(null));
		useStripe();
	});
	afterEach(() => {
		server.resetHandlers();
	});

	function useStripe(config: Parameters<typeof createStripeApiHandlers>[0] = {}): void {
		stripeHandlers = createStripeApiHandlers({prices: MOCK_PRICE_SEEDS, ...config});
		server.use(...stripeHandlers.handlers);
	}

	async function createUser(patch: Partial<UserRow>): Promise<{account: TestAccount; userId: UserID}> {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const user = await findUser(account.userId);
		await users.patchUpsert(userId, patch, user.toRow());
		return {account, userId};
	}

	async function checkoutWrite(userId: UserID, periodEnd: Date, periodStart: Date | null = null): Promise<void> {
		await premiumService.setPremiumFromSubscriptionPeriod(
			userId,
			UserPremiumTypes.SUBSCRIPTION,
			periodEnd,
			'monthly',
			true,
			null,
			periodStart,
		);
	}

	async function webhookWrite(userId: UserID, periodEnd: Date, periodStart: Date | null = null): Promise<void> {
		await users.updateSubscriptionStatus(userId, {
			premiumWillCancel: false,
			computedPremiumUntil: periodEnd,
			periodStart,
		});
	}

	async function writeBoth(
		order: WriteOrder,
		userId: UserID,
		periodEnd: Date,
		periodStart: Date | null = null,
	): Promise<void> {
		if (order === 'checkout first') {
			await checkoutWrite(userId, periodEnd, periodStart);
			await webhookWrite(userId, periodEnd, periodStart);
			return;
		}
		await webhookWrite(userId, periodEnd, periodStart);
		await checkoutWrite(userId, periodEnd, periodStart);
	}

	function newGiftCode(): string {
		return `giftshift${randomUUID().replaceAll('-', '').slice(0, 20)}`;
	}

	async function createRedeemedGift(
		redeemerId: UserID,
		redeemedAt: Date,
		duration: {type: 'days' | 'months'; quantity: number},
		code: string = newGiftCode(),
	): Promise<string> {
		const gifter = await createTestAccount(harness);
		await users.createGiftCode({
			code,
			duration_months: null,
			duration_type: duration.type,
			duration_quantity: duration.quantity,
			created_at: redeemedAt,
			created_by_user_id: createUserID(BigInt(gifter.userId)),
			redeemed_at: redeemedAt,
			redeemed_by_user_id: redeemerId,
			stripe_payment_intent_id: `pi_${code}`,
			visionary_sequence_number: null,
			checkout_session_id: null,
			version: 1,
		});
		return code;
	}

	async function reverseGift(code: string): Promise<void> {
		const handler = new StripeGiftReversalHandler(
			users,
			new NoopGatewayService(),
			getPremiumStateReconciliationQueueService(),
		);
		const gift = await users.findGiftCode(code);
		await handler.handleGiftPremiumReversal(gift!, {reason: 'gift_chargeback'});
	}

	describe('subscribing while gift time is left', () => {
		test.each(WRITE_ORDERS)(
			'moves an active gift past the paid period when premium_until is empty (%s)',
			async (order) => {
				const giftEnd = new Date(Date.now() + ms('10 days'));
				const {account, userId} = await createUser({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: null,
					premium_gift_extension_ends_at: giftEnd,
				});
				const periodEnd = new Date(Date.now() + ms('30 days'));

				await writeBoth(order, userId, periodEnd);

				const after = await findUser(account.userId);
				expect(after.premiumUntil?.getTime()).toBe(periodEnd.getTime());
				expectNear(after.premiumGiftExtensionEndsAt, periodEnd.getTime() + ms('10 days'));
			},
		);

		test.each(WRITE_ORDERS)('anchors at now when premium_until is a stale past date (%s)', async (order) => {
			const giftEnd = new Date(Date.now() + ms('10 days'));
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: new Date(Date.now() - ms('60 days')),
				premium_gift_extension_ends_at: giftEnd,
			});
			const periodEnd = new Date(Date.now() + ms('30 days'));

			await writeBoth(order, userId, periodEnd);

			const after = await findUser(account.userId);
			expect(after.premiumUntil?.getTime()).toBe(periodEnd.getTime());
			expectNear(after.premiumGiftExtensionEndsAt, periodEnd.getTime() + ms('10 days'));
		});

		test.each(WRITE_ORDERS)('leaves an expired gift alone when premium_until is stale (%s)', async (order) => {
			const giftEnd = new Date(Date.now() - ms('5 days'));
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: new Date(Date.now() - ms('60 days')),
				premium_gift_extension_ends_at: giftEnd,
			});
			const periodEnd = new Date(Date.now() + ms('30 days'));

			await writeBoth(order, userId, periodEnd);

			const after = await findUser(account.userId);
			expect(after.premiumUntil?.getTime()).toBe(periodEnd.getTime());
			expect(after.premiumGiftExtensionEndsAt?.getTime()).toBe(giftEnd.getTime());
		});

		test('shifts the gift once when checkout.session.completed is replayed', async () => {
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_gift_extension_ends_at: new Date(Date.now() + ms('10 days')),
			});
			const periodEnd = new Date(Date.now() + ms('30 days'));

			await checkoutWrite(userId, periodEnd);
			const first = await findUser(account.userId);
			await checkoutWrite(userId, periodEnd);

			const replayed = await findUser(account.userId);
			expect(replayed.premiumGiftExtensionEndsAt?.getTime()).toBe(first.premiumGiftExtensionEndsAt?.getTime());
		});

		test.each(WRITE_ORDERS)('moves a stacked gift along on renewal (%s)', async (order) => {
			const premiumUntil = new Date(Date.now() + ms('1 hour'));
			const giftEnd = new Date(premiumUntil.getTime() + ms('10 days'));
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: premiumUntil,
				premium_gift_extension_ends_at: giftEnd,
			});
			const renewedUntil = new Date(premiumUntil.getTime() + ms('30 days'));

			await writeBoth(order, userId, renewedUntil);

			const after = await findUser(account.userId);
			expect(after.premiumUntil?.getTime()).toBe(renewedUntil.getTime());
			expect(after.premiumGiftExtensionEndsAt?.getTime()).toBe(giftEnd.getTime() + ms('30 days'));
		});

		test.each(WRITE_ORDERS)('keeps every gift day when a renewal is paid late (%s)', async (order) => {
			const premiumUntil = new Date(Date.now() - ms('6 days'));
			const giftEnd = new Date(premiumUntil.getTime() + ms('60 days'));
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: premiumUntil,
				premium_gift_extension_ends_at: giftEnd,
				premium_grace_ends_at: new Date(premiumUntil.getTime() + ms('14 days')),
			});
			const renewedUntil = new Date(premiumUntil.getTime() + ms('30 days'));

			await writeBoth(order, userId, renewedUntil, premiumUntil);

			const after = await findUser(account.userId);
			expect(after.premiumUntil?.getTime()).toBe(renewedUntil.getTime());
			expect(after.premiumGiftExtensionEndsAt?.getTime()).toBe(giftEnd.getTime() + ms('30 days'));
		});

		test.each(WRITE_ORDERS)(
			'moves the gift by the whole period when the first payment lands late (%s)',
			async (order) => {
				const periodStart = new Date(Date.now() - ms('2 days'));
				const giftEnd = new Date(Date.now() + ms('10 days'));
				const {account, userId} = await createUser({
					premium_type: UserPremiumTypes.SUBSCRIPTION,
					premium_until: null,
					premium_gift_extension_ends_at: giftEnd,
				});
				const periodEnd = new Date(periodStart.getTime() + ms('30 days'));

				await writeBoth(order, userId, periodEnd, periodStart);

				const after = await findUser(account.userId);
				expect(after.premiumUntil?.getTime()).toBe(periodEnd.getTime());
				expect(after.premiumGiftExtensionEndsAt?.getTime()).toBe(giftEnd.getTime() + ms('30 days'));
			},
		);

		test('lets a user with gift time left start a subscription checkout', async () => {
			const {account} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: new Date(Date.now() - ms('60 days')),
				premium_gift_extension_ends_at: new Date(Date.now() + ms('10 days')),
			});
			useStripe({subscriptionsListEmpty: true});
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/security-flags`)
				.body({email_verified: true})
				.execute();

			await createBuilder(harness, account.token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlyUsd})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(1);
		});

		test('still refuses a recurring checkout for a Visionary', async () => {
			const {account} = await createUser({premium_type: UserPremiumTypes.LIFETIME, premium_lifetime_sequence: 7});
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/security-flags`)
				.body({email_verified: true})
				.execute();

			await createBuilder(harness, account.token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlyUsd})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.PREMIUM_PURCHASE_BLOCKED)
				.execute();
		});

		test('still refuses a recurring checkout while a past_due subscription exists', async () => {
			const {account} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_customer_id: 'cus_shift_past_due',
				premium_gift_extension_ends_at: new Date(Date.now() + ms('10 days')),
			});
			useStripe({subscriptions: {sub_shift_past_due: {customer: 'cus_shift_past_due', status: 'past_due'}}});
			await createBuilder(harness, account.token)
				.post(`/test/users/${account.userId}/security-flags`)
				.body({email_verified: true})
				.execute();

			const error = await createBuilder<{data?: {reason?: string}}>(harness, account.token)
				.post('/stripe/checkout/subscription')
				.body({price_id: MOCK_PRICES.monthlyUsd})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.PREMIUM_PURCHASE_BLOCKED)
				.execute();

			expect(JSON.stringify(error)).toContain('existing_subscription');
			expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		});
	});

	describe('refunds', () => {
		test('a refund keeps only the gift part of a gift trial', async () => {
			const subscriptionId = 'sub_shift_refund_trial';
			const paidUntil = Math.floor((Date.now() + ms('28 days')) / 1000);
			const trialEnd = paidUntil + 30 * 24 * 60 * 60;
			useStripe({
				subscriptions: {
					[subscriptionId]: {
						trial_end: trialEnd,
						status: 'trialing',
						metadata: giftTrialMetadata(newGiftCode(), 30, paidUntil),
					},
				},
			});
			const {account} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_billing_cycle: 'monthly',
				premium_until: new Date(trialEnd * 1000),
				premium_gift_extension_ends_at: new Date(trialEnd * 1000),
			});
			const subscriptionService = new StripeSubscriptionService(
				getStripeClient(),
				users,
				{getRecurringSubscriptionPriceId: () => null, getProduct: () => null},
				getCacheService(),
				new NoopGatewayService(),
			);

			await subscriptionService.cancelSubscriptionImmediately(
				createUserID(BigInt(account.userId)),
				'self_serve_refund',
			);

			const after = await findUser(account.userId);
			expectNear(after.premiumUntil, Date.now());
			expectNear(after.premiumGiftExtensionEndsAt, Date.now() + ms('30 days'));
		});

		test('a refund right after subscribing keeps only the real gift remainder', async () => {
			const subscriptionId = 'sub_shift_refund';
			const premiumUntil = new Date(Date.now() + ms('28 days'));
			const {account} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_billing_cycle: 'monthly',
				premium_until: premiumUntil,
				premium_gift_extension_ends_at: new Date(premiumUntil.getTime() + ms('10 days')),
			});
			const subscriptionService = new StripeSubscriptionService(
				getStripeClient(),
				users,
				{getRecurringSubscriptionPriceId: () => null, getProduct: () => null},
				getCacheService(),
				new NoopGatewayService(),
			);

			await subscriptionService.cancelSubscriptionImmediately(
				createUserID(BigInt(account.userId)),
				'self_serve_refund',
			);

			const after = await findUser(account.userId);
			expect(stripeHandlers.spies.cancelledSubscriptions).toContain(subscriptionId);
			expect(after.premiumType).toBe(UserPremiumTypes.SUBSCRIPTION);
			expectNear(after.premiumUntil, Date.now());
			expectNear(after.premiumGiftExtensionEndsAt, Date.now() + ms('10 days'));
		});
	});

	describe('gift reversals', () => {
		test('removes a gift that was moved past the paid period', async () => {
			const premiumUntil = new Date(Date.now() + ms('30 days'));
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: 'sub_shift_charge_now',
				stripe_customer_id: 'cus_test_1',
				premium_until: premiumUntil,
				premium_gift_extension_ends_at: new Date(premiumUntil.getTime() + ms('20 days')),
			});
			const code = await createRedeemedGift(userId, new Date(Date.now() - ms('10 days')), {type: 'days', quantity: 30});

			await reverseGift(code);

			const after = await findUser(account.userId);
			expect(after.premiumUntil?.getTime()).toBe(premiumUntil.getTime());
			expect(after.premiumGiftExtensionEndsAt).toBeNull();
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
		});

		test('shortens the Stripe trial by the reversed gift once', async () => {
			const subscriptionId = 'sub_shift_gift_trial';
			const trialEnd = Math.floor((Date.now() + ms('50 days')) / 1000);
			const code = newGiftCode();
			useStripe({
				subscriptions: {
					[subscriptionId]: {trial_end: trialEnd, status: 'trialing', metadata: giftTrialMetadata(code, 30)},
				},
			});
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_until: new Date(trialEnd * 1000),
				premium_gift_extension_ends_at: new Date(trialEnd * 1000),
			});
			await createRedeemedGift(userId, new Date(Date.now() - ms('1 hour')), {type: 'days', quantity: 30}, code);

			await reverseGift(code);
			await reverseGift(code);

			const shortenedEnd = trialEnd - 30 * 24 * 60 * 60;
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			expect(stripeHandlers.spies.updatedSubscriptions[0]?.id).toBe(subscriptionId);
			expect(Number(stripeHandlers.spies.updatedSubscriptions[0]?.params.trial_end)).toBe(shortenedEnd);
			expect(stripeHandlers.spies.updatedSubscriptions[0]?.params.metadata).toEqual({[`gtrial_${code}`]: ''});
			const after = await findUser(account.userId);
			expect(after.premiumUntil?.getTime()).toBe(shortenedEnd * 1000);
			expect(after.premiumGiftExtensionEndsAt).toBeNull();
			const gift = await users.findGiftCode(code);
			expect(gift?.premiumReversedSeconds).toBe(30 * 24 * 60 * 60);
		});

		test('ends the trial now when the reversed gift is already being used', async () => {
			const subscriptionId = 'sub_shift_gift_trial_used';
			const trialEnd = Math.floor((Date.now() + ms('10 days')) / 1000);
			const code = newGiftCode();
			useStripe({
				subscriptions: {
					[subscriptionId]: {trial_end: trialEnd, status: 'trialing', metadata: giftTrialMetadata(code, 30)},
				},
			});
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_until: new Date(trialEnd * 1000),
				premium_gift_extension_ends_at: new Date(trialEnd * 1000),
			});
			await createRedeemedGift(userId, new Date(Date.now() - ms('20 days')), {type: 'days', quantity: 30}, code);

			await reverseGift(code);

			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(1);
			expect(stripeHandlers.spies.updatedSubscriptions[0]?.params.trial_end).toBe('now');
			expect(stripeHandlers.spies.cancelledSubscriptions).toHaveLength(0);
			const after = await findUser(account.userId);
			expectNear(after.premiumUntil, Date.now());
		});

		test('cancels a cancelling subscription instead of charging when the gift is already being used', async () => {
			const subscriptionId = 'sub_shift_gift_trial_cancelling';
			const trialEnd = Math.floor((Date.now() + ms('10 days')) / 1000);
			const code = newGiftCode();
			useStripe({
				subscriptions: {
					[subscriptionId]: {
						trial_end: trialEnd,
						status: 'trialing',
						cancel_at_period_end: true,
						metadata: giftTrialMetadata(code, 30),
					},
				},
			});
			const {userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_will_cancel: true,
				premium_until: new Date(trialEnd * 1000),
				premium_gift_extension_ends_at: new Date(trialEnd * 1000),
			});
			await createRedeemedGift(userId, new Date(Date.now() - ms('20 days')), {type: 'days', quantity: 30}, code);

			await reverseGift(code);

			expect(stripeHandlers.spies.cancelledSubscriptions).toEqual([subscriptionId]);
			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
		});

		test('releases the reversal claim when Stripe refuses the trial change', async () => {
			const subscriptionId = 'sub_shift_gift_trial_fail';
			const trialEnd = Math.floor((Date.now() + ms('50 days')) / 1000);
			const code = newGiftCode();
			useStripe({
				subscriptions: {
					[subscriptionId]: {trial_end: trialEnd, status: 'trialing', metadata: giftTrialMetadata(code, 30)},
				},
			});
			server.use(
				http.post('https://api.stripe.com/v1/subscriptions/:id', () =>
					HttpResponse.json(
						{error: {type: 'invalid_request_error', message: 'Mock trial update failure'}},
						{status: 400},
					),
				),
			);
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_until: new Date(trialEnd * 1000),
				premium_gift_extension_ends_at: new Date(trialEnd * 1000),
			});
			await createRedeemedGift(userId, new Date(Date.now() - ms('1 hour')), {type: 'days', quantity: 30}, code);

			await expect(reverseGift(code)).rejects.toThrow();

			expect((await users.findGiftCode(code))?.premiumReversedSeconds).toBeNull();
			expect((await findUser(account.userId)).premiumUntil?.getTime()).toBe(trialEnd * 1000);
		});

		test('cuts a month gift back to exactly the paid period end', async () => {
			const subscriptionId = 'sub_shift_gift_trial_month';
			const paidUntil = Math.floor((Date.now() + ms('15 days')) / 1000);
			const trialEnd = paidUntil + 28 * 24 * 60 * 60;
			const code = newGiftCode();
			useStripe({
				subscriptions: {
					[subscriptionId]: {
						trial_end: trialEnd,
						status: 'trialing',
						metadata: giftTrialMetadata(code, 28, paidUntil),
					},
				},
			});
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_until: new Date(trialEnd * 1000),
				premium_gift_extension_ends_at: new Date(trialEnd * 1000),
			});
			await createRedeemedGift(userId, new Date(Date.now() - ms('1 hour')), {type: 'months', quantity: 1}, code);

			await reverseGift(code);

			expect(Number(stripeHandlers.spies.updatedSubscriptions[0]?.params.trial_end)).toBe(paidUntil);
			const after = await findUser(account.userId);
			expect(after.premiumUntil?.getTime()).toBe(paidUntil * 1000);
			expect(after.premiumGiftExtensionEndsAt).toBeNull();
		});

		test('never cuts the trial below the paid period end', async () => {
			const subscriptionId = 'sub_shift_gift_trial_floor';
			const paidUntil = Math.floor((Date.now() + ms('15 days')) / 1000);
			const trialEnd = paidUntil + 20 * 24 * 60 * 60;
			const code = newGiftCode();
			useStripe({
				subscriptions: {
					[subscriptionId]: {
						trial_end: trialEnd,
						status: 'trialing',
						metadata: giftTrialMetadata(code, 30, paidUntil),
					},
				},
			});
			const {userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_until: new Date(trialEnd * 1000),
				premium_gift_extension_ends_at: new Date(trialEnd * 1000),
			});
			await createRedeemedGift(userId, new Date(Date.now() - ms('1 hour')), {type: 'days', quantity: 30}, code);

			await reverseGift(code);

			expect(Number(stripeHandlers.spies.updatedSubscriptions[0]?.params.trial_end)).toBe(paidUntil);
		});

		test('leaves the trial alone when the reversed gift was not stacked onto it', async () => {
			const subscriptionId = 'sub_shift_gift_trial_other';
			const paidUntil = Math.floor((Date.now() + ms('10 days')) / 1000);
			const trialEnd = paidUntil + 30 * 24 * 60 * 60;
			const stackedCode = newGiftCode();
			useStripe({
				subscriptions: {
					[subscriptionId]: {
						trial_end: trialEnd,
						status: 'trialing',
						metadata: giftTrialMetadata(stackedCode, 30, paidUntil),
					},
				},
			});
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_until: new Date(trialEnd * 1000),
				premium_gift_extension_ends_at: new Date(Date.now() + ms('55 days')),
			});
			const plainCode = await createRedeemedGift(userId, new Date(Date.now() - ms('5 days')), {
				type: 'days',
				quantity: 30,
			});
			await createRedeemedGift(userId, new Date(Date.now() - ms('1 hour')), {type: 'days', quantity: 30}, stackedCode);

			await reverseGift(plainCode);

			expect(stripeHandlers.spies.updatedSubscriptions).toHaveLength(0);
			expect(stripeHandlers.spies.cancelledSubscriptions).toHaveLength(0);
			const after = await findUser(account.userId);
			expect(after.premiumUntil?.getTime()).toBe(trialEnd * 1000);
			expect(after.premiumGiftExtensionEndsAt!.getTime()).toBeLessThanOrEqual(trialEnd * 1000);
		});

		test("keeps the paid shift of the user's other gifts", async () => {
			const subscriptionId = 'sub_shift_gift_trial_keep_shift';
			const now = Date.now();
			const paidUntil = Math.floor((now + ms('10 days')) / 1000);
			const trialEnd = paidUntil + 30 * 24 * 60 * 60;
			const stackedCode = newGiftCode();
			useStripe({
				subscriptions: {
					[subscriptionId]: {
						trial_end: trialEnd,
						status: 'trialing',
						metadata: giftTrialMetadata(stackedCode, 30, paidUntil),
					},
				},
			});
			const firstRedeemedAt = new Date(now - ms('150 days'));
			const firstGiftBaseEnd = firstRedeemedAt.getTime() + ms('365 days');
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_until: new Date(trialEnd * 1000),
				premium_gift_extension_ends_at: new Date(firstGiftBaseEnd + ms('120 days') + ms('30 days')),
			});
			await createRedeemedGift(userId, firstRedeemedAt, {type: 'days', quantity: 365});
			await createRedeemedGift(userId, new Date(now - ms('1 hour')), {type: 'days', quantity: 30}, stackedCode);

			await reverseGift(stackedCode);

			expect(Number(stripeHandlers.spies.updatedSubscriptions[0]?.params.trial_end)).toBe(paidUntil);
			const after = await findUser(account.userId);
			expect(after.premiumUntil?.getTime()).toBe(paidUntil * 1000);
			expect(after.premiumGiftExtensionEndsAt?.getTime()).toBe(firstGiftBaseEnd + ms('120 days'));
		});
	});

	describe('redeeming a gift onto a running subscription', () => {
		test('records the stacked trial once whichever write lands first', async () => {
			const subscriptionId = 'sub_shift_stack';
			const periodEnd = Math.floor((Date.now() + ms('20 days')) / 1000);
			useStripe({
				subscriptions: {
					[subscriptionId]: {current_period_start: periodEnd - 30 * 24 * 60 * 60, current_period_end: periodEnd},
				},
			});
			const {account, userId} = await createUser({
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				stripe_subscription_id: subscriptionId,
				stripe_customer_id: 'cus_test_1',
				premium_billing_cycle: 'monthly',
				premium_until: new Date(periodEnd * 1000),
			});
			const gifter = await createTestAccount(harness);
			await createBuilder(harness, gifter.token)
				.post('/test/gifts/SHIFTSTACKGIFT')
				.body({duration_type: 'days', duration_quantity: 30, created_by_user_id: gifter.userId})
				.execute();

			await createBuilder(harness, account.token).post('/gifts/SHIFTSTACKGIFT/redeem').expect(204).execute();

			const trialEnd = new Date((periodEnd + 30 * 24 * 60 * 60) * 1000);
			expect(stripeHandlers.spies.updatedSubscriptions[0]?.params.metadata).toEqual({
				gtrial_SHIFTSTACKGIFT: String(30 * 24 * 60 * 60),
				gtrial_paid_until: String(periodEnd),
			});
			const redeemed = await findUser(account.userId);
			expect(redeemed.premiumUntil?.getTime()).toBe(trialEnd.getTime());
			expect(redeemed.premiumGiftExtensionEndsAt?.getTime()).toBe(trialEnd.getTime());
			await webhookWrite(userId, trialEnd);
			const afterWebhook = await findUser(account.userId);
			expect(afterWebhook.premiumGiftExtensionEndsAt?.getTime()).toBe(trialEnd.getTime());
		});
	});
});

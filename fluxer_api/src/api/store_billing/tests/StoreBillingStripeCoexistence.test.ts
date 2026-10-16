// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {
	getPremiumStateReconciliationQueueService,
	getStoreBillingRepository,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import type {StoreEntitlementService} from '@app/api/store_billing/StoreEntitlementService';
import {
	createTestStoreEntitlementService,
	findUser,
	seedStorePurchase,
} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {StripeGiftReversalHandler} from '@app/api/stripe/services/StripeGiftReversalHandler';
import {sendStripeWebhook, setupSyncStripeWebhookWorker} from '@app/api/stripe/tests/StripeWebhookTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	buildFakeSubscriptionPurchase,
	createGooglePlayDeveloperApiHandlers,
	type FakeGooglePlayDeveloperApi,
} from '@app/api/test/msw/handlers/GooglePlayDeveloperApiHandlers';
import {
	createStripeApiHandlers,
	createSubscriptionDeletedEvent,
	type StripeApiHandlers,
} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	deleteAccount,
	setPendingDeletionAt,
	triggerDeletionWorker,
	waitForDeletionCompletion,
} from '@app/api/user/tests/UserTestUtils';
import {checkHasActivePaidPremium} from '@app/api/user/UserHelpers';
import {getWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {ms} from 'itty-time';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const MOCK_PRICES = {
	monthlyUsd: 'price_store_coexist_monthly_usd',
	yearlyUsd: 'price_store_coexist_yearly_usd',
	gift1MonthUsd: 'price_store_coexist_gift_1_month_usd',
	gift1YearUsd: 'price_store_coexist_gift_1_year_usd',
};

const MOCK_PRICE_SEEDS = {
	[MOCK_PRICES.monthlyUsd]: {unit_amount: 499, currency: 'usd', interval: 'month' as const},
	[MOCK_PRICES.yearlyUsd]: {unit_amount: 4999, currency: 'usd', interval: 'year' as const},
};

describe('Store purchases alongside Stripe billing', () => {
	let harness: ApiTestHarness;
	let stripeHandlers: StripeApiHandlers;
	let google: FakeGooglePlayDeveloperApi;
	let service: StoreEntitlementService;
	let originalWebhookSecret: string | undefined;
	let originalPrices: typeof Config.stripe.prices;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		originalWebhookSecret = Config.stripe.webhookSecret;
		originalPrices = Config.stripe.prices;
		google = createGooglePlayDeveloperApiHandlers();
	});

	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.webhookSecret = originalWebhookSecret;
		Config.stripe.prices = originalPrices;
	});

	beforeEach(async () => {
		await harness.resetData();
		Config.stripe.webhookSecret = 'whsec_store_coexist';
		Config.stripe.prices = MOCK_PRICES;
		stripeHandlers = createStripeApiHandlers({prices: MOCK_PRICE_SEEDS, subscriptionsListEmpty: true});
		google.reset();
		server.use(...stripeHandlers.handlers, ...google.handlers);
		service = createTestStoreEntitlementService();
		setupSyncStripeWebhookWorker();
		setWorkerDependenciesForTest({...getWorkerDependencies(), storeEntitlementService: service});
	});

	afterEach(() => {
		server.resetHandlers();
	});

	async function createStoreSubscriber(): Promise<{account: TestAccount; userId: UserID; expiresAt: Date}> {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const expiresAt = new Date(Date.now() + ms('20 days'));
		await seedStorePurchase(userId, {expires_at: expiresAt});
		await service.applyStoreEntitlementToUser(userId);
		return {account, userId, expiresAt};
	}

	it('keeps an App Store subscription after an old Stripe subscription is deleted', async () => {
		const {account, userId, expiresAt} = await createStoreSubscriber();
		const user = await findUser(account.userId);
		await getUserRepository().patchUpsert(
			userId,
			{stripe_subscription_id: 'sub_store_coexist_old', stripe_customer_id: 'cus_store_coexist'},
			user.toRow(),
		);

		await sendStripeWebhook(
			harness,
			createSubscriptionDeletedEvent({
				subscriptionId: 'sub_store_coexist_old',
				customerId: 'cus_store_coexist',
				endedAt: Math.floor(Date.now() / 1000) - 60,
			}),
		);

		const after = await findUser(account.userId);
		expect(after.stripeSubscriptionId).toBeNull();
		expect(after.premiumType).toBe(UserPremiumTypes.SUBSCRIPTION);
		expect(after.premiumUntil?.getTime()).toBe(expiresAt.getTime());
		expect(after.premiumGraceEndsAt).toBeNull();
		expect(checkHasActivePaidPremium(after)).toBe(true);
	});

	it('keeps a store subscriber premium when a Stripe gift they redeemed is reversed', async () => {
		const {account, userId, expiresAt} = await createStoreSubscriber();
		const users = getUserRepository();
		const gifter = await createTestAccount(harness);
		const code = `storecoexist${randomUUID().replaceAll('-', '').slice(0, 20)}`;
		await users.createGiftCode({
			code,
			duration_months: null,
			duration_type: 'days',
			duration_quantity: 30,
			created_at: new Date(),
			created_by_user_id: createUserID(BigInt(gifter.userId)),
			redeemed_at: null,
			redeemed_by_user_id: null,
			stripe_payment_intent_id: 'pi_store_coexist',
			visionary_sequence_number: null,
			checkout_session_id: null,
			version: 1,
		});
		await users.redeemGiftCode(code, userId);
		const redeemer = await findUser(account.userId);
		await users.patchUpsert(
			userId,
			{premium_gift_extension_ends_at: new Date(expiresAt.getTime() + ms('30 days'))},
			redeemer.toRow(),
		);
		const handler = new StripeGiftReversalHandler(
			users,
			getGatewayService(),
			getPremiumStateReconciliationQueueService(),
			service,
		);

		const gift = await users.findGiftCode(code);
		await handler.handleGiftPremiumReversal(gift!, {reason: 'charge_dispute'});

		const after = await findUser(account.userId);
		expect(after.premiumType).toBe(UserPremiumTypes.SUBSCRIPTION);
		expect(after.premiumUntil?.getTime()).toBe(expiresAt.getTime());
		expect(after.premiumGiftExtensionEndsAt).toBeNull();
		expect(after.premiumWillCancel).toBe(false);
		expect(checkHasActivePaidPremium(after)).toBe(true);
	});

	it('takes a reversed Stripe gift off a store subscriber once across repeated reversals', async () => {
		const {account, userId, expiresAt} = await createStoreSubscriber();
		const users = getUserRepository();
		const gifter = await createTestAccount(harness);
		const code = `storecoexist${randomUUID().replaceAll('-', '').slice(0, 20)}`;
		await users.createGiftCode({
			code,
			duration_months: null,
			duration_type: 'days',
			duration_quantity: 30,
			created_at: new Date(),
			created_by_user_id: createUserID(BigInt(gifter.userId)),
			redeemed_at: null,
			redeemed_by_user_id: null,
			stripe_payment_intent_id: 'pi_store_coexist_repeat',
			visionary_sequence_number: null,
			checkout_session_id: null,
			version: 1,
		});
		await users.redeemGiftCode(code, userId);
		const stackedEnd = new Date(expiresAt.getTime() + ms('60 days'));
		const redeemer = await findUser(account.userId);
		await users.patchUpsert(
			userId,
			{stripe_customer_id: 'cus_store_coexist_repeat', premium_gift_extension_ends_at: stackedEnd},
			redeemer.toRow(),
		);
		const handler = new StripeGiftReversalHandler(
			users,
			getGatewayService(),
			getPremiumStateReconciliationQueueService(),
			service,
		);

		const gift = await users.findGiftCode(code);
		await handler.handleGiftPremiumReversal(gift!, {reason: 'gift_refund'});
		await handler.handleGiftPremiumReversal(gift!, {reason: 'gift_chargeback'});

		const after = await findUser(account.userId);
		expect(after.premiumGiftExtensionEndsAt?.getTime()).toBe(stackedEnd.getTime() - ms('30 days'));
		expect(after.premiumUntil?.getTime()).toBe(expiresAt.getTime());
	});

	it('blocks a recurring Stripe checkout while a store subscription is active', async () => {
		const {account} = await createStoreSubscriber();
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({email_verified: true})
			.execute();

		const {json} = await createBuilder<{code: string; reason: string; provider: string}>(harness, account.token)
			.post('/stripe/checkout/subscription')
			.body({price_id: MOCK_PRICES.monthlyUsd})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.PREMIUM_PURCHASE_BLOCKED)
			.executeWithResponse();

		expect(json).toMatchObject({reason: 'existing_subscription', provider: 'app_store'});
		expect(stripeHandlers.spies.createdCheckoutSessions).toHaveLength(0);
		const response = await createBuilder<{url: string}>(harness, account.token)
			.post('/stripe/checkout/gift')
			.body({price_id: MOCK_PRICES.gift1MonthUsd})
			.execute();
		expect(response.url).toContain('checkout.stripe.com');
	});

	it('asks Google to stop billing on self deletion and still deletes when Google fails', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const purchaseToken = `token-${randomUUID()}`;
		google.setSubscription(
			'com.fluxer',
			purchaseToken,
			buildFakeSubscriptionPurchase({subscriptionState: 'SUBSCRIPTION_STATE_ACTIVE'}),
		);
		await seedStorePurchase(userId, {
			store_key: `google_play:${purchaseToken}`,
			provider: 'google_play',
			app_id: 'com.fluxer',
			product_id: 'plutonium',
			base_plan_id: 'monthly',
			store_reference: purchaseToken,
			store_state: 'SUBSCRIPTION_STATE_ACTIVE',
		});
		google.failNext('subscriptions.cancel', {status: 503, reason: 'backendError'});

		await deleteAccount(harness, account.token, account.password);
		await setPendingDeletionAt(harness, account.userId, new Date(Date.now() - 60_000));
		const result = await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, account.userId);

		expect(result.scheduled).toBe(1);
		expect(google.spies.apiRequests.filter((request) => request.operation === 'subscriptions.cancel')).toHaveLength(1);
		expect(google.spies.canceled).toEqual([]);
	});

	it('does not credit gift time again when Stripe lowers premium_until and the store raises it', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		const stripeEnd = new Date(Date.now() + ms('10 days'));
		const giftEnd = new Date(Date.now() + ms('40 days'));
		const storeExpiry = new Date(Date.now() + ms('20 days'));
		const users = getUserRepository();
		await users.patchUpsert(
			userId,
			{
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: stripeEnd,
				premium_billing_cycle: 'monthly',
				premium_gift_extension_ends_at: giftEnd,
				stripe_subscription_id: 'sub_store_coexist_live',
				stripe_customer_id: 'cus_store_coexist_live',
			},
			(await findUser(account.userId)).toRow(),
		);
		await seedStorePurchase(userId, {expires_at: storeExpiry});
		for (let cycle = 0; cycle < 3; cycle++) {
			await service.applyStoreEntitlementToUser(userId);
			const raised = await findUser(account.userId);
			expect(raised.premiumUntil?.getTime()).toBe(storeExpiry.getTime());
			expect(raised.premiumGiftExtensionEndsAt?.getTime()).toBe(giftEnd.getTime());
			await users.patchUpsert(userId, {premium_until: stripeEnd}, raised.toRow());
		}
	});

	it('lets the store own the premium fields when the Stripe subscription id is stale', async () => {
		const account = await createTestAccount(harness);
		const userId = createUserID(BigInt(account.userId));
		await getUserRepository().patchUpsert(
			userId,
			{
				premium_type: UserPremiumTypes.SUBSCRIPTION,
				premium_until: new Date(Date.now() - ms('5 days')),
				premium_billing_cycle: null,
				stripe_subscription_id: 'sub_store_coexist_stale',
				stripe_customer_id: 'cus_store_coexist_stale',
			},
			(await findUser(account.userId)).toRow(),
		);
		const expiresAt = new Date(Date.now() + ms('20 days'));
		const row = await seedStorePurchase(userId, {expires_at: expiresAt});
		await service.applyStoreEntitlementToUser(userId);
		expect(await findUser(account.userId)).toMatchObject({premiumUntil: expiresAt, premiumWillCancel: false});
		const repository = getStoreBillingRepository();
		const canceled = await repository.updatePurchase(row, {state: 'canceled', auto_renew: false});
		await service.applyStoreEntitlementToUser(userId);
		expect((await findUser(account.userId)).premiumWillCancel).toBe(true);
		const revokedAt = new Date(Date.now() - ms('1 minute'));
		await repository.updatePurchase(canceled!, {state: 'revoked', entitled: false, revoked_at: revokedAt});
		await service.applyStoreEntitlementToUser(userId);
		const cut = await findUser(account.userId);
		expect(cut.premiumUntil?.getTime()).toBe(revokedAt.getTime());
		expect(cut.premiumGraceEndsAt?.getTime()).toBe(revokedAt.getTime());
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import {getStoreBillingRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {buildGooglePlayStoreKey} from '@app/api/store_billing/StoreBillingTypes';
import {
	findUser,
	getStoreContext,
	installStoreBillingWorker,
	uninstallStoreBillingWorker,
} from '@app/api/store_billing/tests/StoreBillingTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {
	buildFakeProductPurchase,
	buildFakeSubscriptionPurchase,
	createGooglePlayDeveloperApiHandlers,
	type FakeGooglePlayDeveloperApi,
} from '@app/api/test/msw/handlers/GooglePlayDeveloperApiHandlers';
import {createStripeApiHandlers} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {server} from '@app/api/test/msw/server';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {
	PremiumStateResponse,
	SelfServeRefundEligibilityResponse,
	SelfServeRefundResponse,
} from '@fluxer/schema/src/domains/premium/PremiumSchemas';
import type {StorePurchaseClaimResponse} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';
import {ms} from 'itty-time';
import {HttpResponse, http} from 'msw';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const PACKAGE_NAME = 'com.fluxer';
const SUBSCRIPTION_PRODUCT = 'plutonium';
const STRIPE_CUSTOMER_ID = 'cus_play_refund';

describe('Google Play self-service refunds', () => {
	let harness: ApiTestHarness;
	let fake: FakeGooglePlayDeveloperApi;
	const configured = Config.googlePlay.selfServiceRefunds;

	beforeAll(async () => {
		harness = await createApiTestHarness();
		fake = createGooglePlayDeveloperApiHandlers();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	beforeEach(async () => {
		await harness.resetData();
		fake.reset();
		server.use(...createStripeApiHandlers().handlers, ...fake.handlers);
		installStoreBillingWorker();
		Config.googlePlay.selfServiceRefunds = true;
	});

	afterEach(() => {
		Config.googlePlay.selfServiceRefunds = configured;
		uninstallStoreBillingWorker();
		server.resetHandlers();
	});

	async function claimSubscription(
		account: TestAccount,
		options: {startedAgo: number; expiresIn: number},
	): Promise<{purchaseToken: string; orderId: string}> {
		const purchaseToken = `token-${randomUUID()}`;
		const orderId = `GPA.${randomUUID()}`;
		const accountToken = (await getStoreContext(harness, account.token)).app_account_token;
		fake.setSubscription(
			PACKAGE_NAME,
			purchaseToken,
			buildFakeSubscriptionPurchase({
				startTime: new Date(Date.now() - options.startedAgo).toISOString(),
				externalAccountIdentifiers: {obfuscatedExternalAccountId: accountToken},
				lineItems: [
					{
						productId: SUBSCRIPTION_PRODUCT,
						expiryTime: new Date(Date.now() + options.expiresIn).toISOString(),
						latestSuccessfulOrderId: orderId,
						autoRenewingPlan: {autoRenewEnabled: true},
						offerDetails: {basePlanId: 'monthly'},
					},
				],
			}),
		);
		await createBuilder<StorePurchaseClaimResponse>(harness, account.token)
			.post('/premium/store/google-play/purchases')
			.body({purchase_token: purchaseToken, product_id: SUBSCRIPTION_PRODUCT, package_name: PACKAGE_NAME})
			.execute();
		return {purchaseToken, orderId};
	}

	async function claimGift(account: TestAccount): Promise<{orderId: string; giftCode: string}> {
		const purchaseToken = `token-${randomUUID()}`;
		const orderId = `GPA.${randomUUID()}`;
		fake.setProduct(PACKAGE_NAME, purchaseToken, buildFakeProductPurchase({orderId}));
		const result = await createBuilder<StorePurchaseClaimResponse>(harness, account.token)
			.post('/premium/store/google-play/purchases')
			.body({purchase_token: purchaseToken, product_id: 'gift_1_month', package_name: PACKAGE_NAME})
			.execute();
		return {orderId, giftCode: result.gift_code!};
	}

	function eligibility(account: TestAccount): Promise<SelfServeRefundEligibilityResponse> {
		return createBuilder<SelfServeRefundEligibilityResponse>(harness, account.token)
			.get('/premium/refund-eligibility')
			.execute();
	}

	function refund(account: TestAccount): Promise<SelfServeRefundResponse> {
		return createBuilder<SelfServeRefundResponse>(harness, account.token).post('/premium/refund-latest').execute();
	}

	it('refunds a new Google Play subscription and ends access right away', async () => {
		const account = await createTestAccount(harness);
		const {purchaseToken, orderId} = await claimSubscription(account, {
			startedAgo: ms('1 hour'),
			expiresIn: ms('29 days'),
		});
		expect((await findUser(account.userId)).isPremium()).toBe(true);

		const before = await eligibility(account);
		expect(before).toMatchObject({
			eligible: true,
			reason: null,
			provider: 'google_play',
			invoice_id: orderId,
			cancels_subscription: true,
		});
		const state = await createBuilder<PremiumStateResponse>(harness, account.token).get('/premium/state').execute();
		expect(state.billing.refund_eligibility).toMatchObject({eligible: true, provider: 'google_play'});

		const result = await refund(account);

		expect(result).toMatchObject({provider: 'google_play', invoice_id: orderId, status: 'succeeded'});
		expect(fake.spies.refunded).toEqual([{packageName: PACKAGE_NAME, orderId, revoke: true}]);
		const user = await findUser(account.userId);
		const row = await getStoreBillingRepository().findPurchase(buildGooglePlayStoreKey(purchaseToken));
		expect(row).toMatchObject({entitled: false, state: 'revoked'});
		expect(user.premiumUntil).toEqual(row!.revoked_at);
		expect(user.premiumGraceEndsAt).toEqual(row!.revoked_at);
		expect(user.premiumUntil!.getTime()).toBeLessThanOrEqual(Date.now());
		expect(user.lastRefundAt).not.toBeNull();
		expect((await eligibility(account)).eligible).toBe(false);
	});

	it('does not refund a Google Play subscription charged more than three days ago', async () => {
		const account = await createTestAccount(harness);
		await claimSubscription(account, {startedAgo: ms('10 days'), expiresIn: ms('20 days')});

		expect(await eligibility(account)).toMatchObject({eligible: false, reason: 'outside_refund_window'});
		await createBuilder(harness, account.token)
			.post('/premium/refund-latest')
			.expect(403, APIErrorCodes.STRIPE_REFUND_OUTSIDE_WINDOW)
			.execute();
		expect(fake.spies.refunded).toEqual([]);
	});

	it('refunds an unredeemed Google Play gift and revokes its code', async () => {
		const account = await createTestAccount(harness);
		const {orderId, giftCode} = await claimGift(account);

		expect(await eligibility(account)).toMatchObject({eligible: true, cancels_subscription: false});
		await refund(account);

		expect(fake.spies.refunded).toEqual([{packageName: PACKAGE_NAME, orderId, revoke: true}]);
		expect((await getUserRepository().findGiftCode(giftCode))?.revokedAt).not.toBeNull();
	});

	it('does not refund a Google Play gift that was already redeemed', async () => {
		const account = await createTestAccount(harness);
		const friend = await createTestAccount(harness);
		const {giftCode} = await claimGift(account);
		await createBuilder(harness, friend.token).post(`/gifts/${giftCode}/redeem`).expect(204).execute();

		expect(await eligibility(account)).toMatchObject({eligible: false, reason: 'no_refundable_purchase'});
		await createBuilder(harness, account.token)
			.post('/premium/refund-latest')
			.expect(400, APIErrorCodes.STRIPE_NO_PURCHASE_HISTORY)
			.execute();
		expect(fake.spies.refunded).toEqual([]);
	});

	it('offers no Google Play refund while the setting is off', async () => {
		Config.googlePlay.selfServiceRefunds = false;
		const account = await createTestAccount(harness);
		await claimSubscription(account, {startedAgo: ms('1 hour'), expiresIn: ms('29 days')});

		expect((await eligibility(account)).eligible).toBe(false);
		await createBuilder(harness, account.token)
			.post('/premium/refund-latest')
			.expect(400, APIErrorCodes.STRIPE_NO_PURCHASE_HISTORY)
			.execute();
		expect(fake.spies.refunded).toEqual([]);
	});

	it('refunds both a Stripe invoice and a newer Google Play purchase made in the last three days', async () => {
		const account = await createTestAccount(harness);
		const stripeHandlers = createStripeApiHandlers();
		server.use(...stripeHandlers.handlers);
		await createBuilder(harness, account.token)
			.post(`/test/users/${account.userId}/premium`)
			.body({stripe_customer_id: STRIPE_CUSTOMER_ID})
			.execute();
		const paidAt = Math.floor((Date.now() - ms('2 days')) / 1000);
		server.use(
			http.get('https://api.stripe.com/v1/invoices', () =>
				HttpResponse.json({
					object: 'list',
					has_more: false,
					url: '/v1/invoices',
					data: [
						{
							id: 'in_older',
							object: 'invoice',
							customer: STRIPE_CUSTOMER_ID,
							parent: null,
							amount_due: 499,
							amount_paid: 499,
							currency: 'usd',
							status: 'paid',
							created: paidAt,
							payments: {
								object: 'list',
								has_more: false,
								url: '/v1/invoices/in_older/payments',
								data: [
									{
										id: 'inpay_older',
										invoice: 'in_older',
										object: 'invoice_payment',
										status: 'paid',
										status_transitions: {paid_at: paidAt},
										payment: {charge: 'ch_older', payment_intent: 'pi_older', type: 'payment_intent'},
									},
								],
							},
						},
					],
				}),
			),
		);
		const {orderId} = await claimSubscription(account, {startedAgo: ms('1 hour'), expiresIn: ms('29 days')});

		expect(await eligibility(account)).toMatchObject({eligible: true, provider: 'stripe', invoice_id: 'in_older'});

		const result = await refund(account);

		expect(result).toMatchObject({provider: 'stripe', invoice_id: 'in_older'});
		expect(stripeHandlers.spies.createdRefunds).toHaveLength(1);
		expect(fake.spies.refunded).toEqual([{packageName: PACKAGE_NAME, orderId, revoke: true}]);
	});
});

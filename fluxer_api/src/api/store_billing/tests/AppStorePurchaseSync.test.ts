import {setInjectedAppleRootCertificates} from '@app/api/store_billing/app_store/AppleRootCertificates';
import {
	type AppStorePurchaseLookup,
	AppStorePurchaseSync,
	AppStorePurchaseSyncError,
	type AppStorePurchaseSyncFailureReason,
} from '@app/api/store_billing/app_store/AppStorePurchaseSync';
import {
	AppStoreServerApiClient,
	AppStoreServerApiError,
} from '@app/api/store_billing/app_store/AppStoreServerApiClient';
import {type AppleTestPki, createAppleTestPki} from '@app/api/store_billing/tests/AppleTestPki';
import {
	type AppStoreFakePayload,
	type AppStoreServerApiFake,
	createAppStoreServerApiFake,
} from '@app/api/test/msw/handlers/AppStoreServerApiHandlers';
import {server} from '@app/api/test/msw/server';
import {ms} from 'itty-time';
import {afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const MONTHLY = 'com.fluxer.plutonium.monthly';
const YEARLY = 'com.fluxer.plutonium.yearly';
const GIFT_MONTH = 'com.fluxer.gift.1month';
const GIFT_YEAR = 'com.fluxer.gift.1year';
const ORIGINAL_ID = '2000000100000001';
const RENEWAL_ID = '2000000100000009';
const TOKEN_UPPER = '7E3FB20B-4CDB-47CC-936D-99D65F608138';
const TOKEN = TOKEN_UPPER.toLowerCase();

interface SubscriptionSeed {
	environment?: 'production' | 'sandbox';
	status: number;
	productId?: string;
	transaction?: AppStoreFakePayload;
	renewalInfo?: AppStoreFakePayload | null;
}

async function expectSyncFailure(promise: Promise<unknown>, reason: AppStorePurchaseSyncFailureReason): Promise<void> {
	const error = await promise.then(
		() => null,
		(caught: unknown) => caught,
	);
	expect(error).toBeInstanceOf(AppStorePurchaseSyncError);
	expect((error as AppStorePurchaseSyncError).reason).toBe(reason);
}

describe('AppStorePurchaseSync', () => {
	let pki: AppleTestPki;
	let fake: AppStoreServerApiFake;
	let sync: AppStorePurchaseSync;
	let now: number;

	beforeAll(() => {
		pki = createAppleTestPki();
	});

	beforeEach(() => {
		now = Date.now();
		setInjectedAppleRootCertificates([pki.root]);
		fake = createAppStoreServerApiFake({sign: (payload) => pki.signJws(payload)});
		server.use(...fake.handlers);
		sync = new AppStorePurchaseSync(new AppStoreServerApiClient());
	});

	afterEach(() => {
		setInjectedAppleRootCertificates(undefined);
	});

	function seedSubscription(seed: SubscriptionSeed): void {
		const environment = seed.environment ?? 'production';
		fake.addTransaction(environment, {
			transactionId: ORIGINAL_ID,
			originalTransactionId: ORIGINAL_ID,
			productId: MONTHLY,
			purchaseDate: now - ms('60 days'),
			expiresDate: now - ms('30 days'),
		});
		fake.addTransaction(environment, {
			transactionId: RENEWAL_ID,
			originalTransactionId: ORIGINAL_ID,
			productId: seed.productId ?? MONTHLY,
			purchaseDate: now - ms('1 day'),
			originalPurchaseDate: now - ms('400 days'),
			expiresDate: now + ms('29 days'),
			appAccountToken: TOKEN_UPPER,
			appTransactionId: '704400000000000001',
			storefront: 'SWE',
			...seed.transaction,
		});
		fake.putSubscription(environment, {
			originalTransactionId: ORIGINAL_ID,
			status: seed.status,
			latestTransactionId: RENEWAL_ID,
			renewalInfo:
				seed.renewalInfo === null
					? null
					: {
							recentSubscriptionStartDate: now - ms('60 days'),
							appAccountToken: TOKEN_UPPER,
							...seed.renewalInfo,
						},
		});
	}

	function lookup(overrides: Partial<AppStorePurchaseLookup> = {}): AppStorePurchaseLookup {
		return {
			environment: 'production',
			bundleId: 'com.fluxer',
			productId: MONTHLY,
			transactionId: RENEWAL_ID,
			originalTransactionId: ORIGINAL_ID,
			...overrides,
		};
	}

	describe('subscriptions', () => {
		it('maps an active subscription from Get All Subscription Statuses', async () => {
			seedSubscription({status: 1});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({
				storeKey: `app_store:production:${ORIGINAL_ID}`,
				provider: 'app_store',
				kind: 'subscription',
				slot: 'monthly',
				environment: 'production',
				appId: 'com.fluxer',
				productId: MONTHLY,
				basePlanId: null,
				storeReference: ORIGINAL_ID,
				latestTransactionId: RENEWAL_ID,
				appTransactionId: '704400000000000001',
				accountToken: TOKEN,
				ownershipType: 'PURCHASED',
				state: 'active',
				storeState: '1',
				providerEntitled: true,
				graceEndsAt: null,
				autoRenew: true,
				autoRenewProductId: MONTHLY,
				revokedAt: null,
				revocationReason: null,
				linkedStoreKey: null,
				acknowledged: true,
				region: 'SWE',
			});
			expect(snapshot.expiresAt?.getTime()).toBe(now + ms('29 days'));
			expect(snapshot.startedAt?.getTime()).toBe(now - ms('60 days'));
			expect(snapshot.purchasedAt?.getTime()).toBe(now - ms('1 day'));
			expect(snapshot.lastEventAt.getTime()).toBeGreaterThanOrEqual(now);
			expect(fake.requests.map((request) => request.path)).toEqual([`/inApps/v1/subscriptions/${ORIGINAL_ID}`]);
		});

		it('maps auto-renew off to canceled while still entitled', async () => {
			seedSubscription({status: 1, renewalInfo: {autoRenewStatus: 0, expirationIntent: 1}});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({state: 'canceled', providerEntitled: true, autoRenew: false});
		});

		it('maps billing grace to grace until the grace expiry', async () => {
			const graceEnd = now + ms('16 days');
			seedSubscription({
				status: 4,
				transaction: {expiresDate: now - ms('1 day')},
				renewalInfo: {gracePeriodExpiresDate: graceEnd, isInBillingRetryPeriod: true},
			});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({state: 'grace', storeState: '4', providerEntitled: true});
			expect(snapshot.graceEndsAt?.getTime()).toBe(graceEnd);
			expect(snapshot.expiresAt?.getTime()).toBe(now - ms('1 day'));
		});

		it('maps billing retry without grace to not entitled', async () => {
			seedSubscription({
				status: 3,
				transaction: {expiresDate: now - ms('1 day')},
				renewalInfo: {isInBillingRetryPeriod: true},
			});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({state: 'billing_retry', providerEntitled: false, graceEndsAt: null});
		});

		it('maps expired to not entitled', async () => {
			seedSubscription({status: 2, renewalInfo: {autoRenewStatus: 0, expirationIntent: 1}});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({state: 'expired', storeState: '2', providerEntitled: false});
		});

		it('maps revoked with the revocation details', async () => {
			const revokedAt = now - ms('2 hours');
			seedSubscription({
				status: 5,
				transaction: {revocationDate: revokedAt, revocationReason: 0, revocationType: 'REFUND_FULL'},
			});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({state: 'revoked', providerEntitled: false, revocationReason: 'REFUND_FULL'});
			expect(snapshot.revokedAt?.getTime()).toBe(revokedAt);
		});

		it('treats a revocation date as revoked whatever the status', async () => {
			seedSubscription({status: 1, transaction: {revocationDate: now - ms('1 hour'), revocationReason: 1}});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({state: 'revoked', providerEntitled: false, revocationReason: '1'});
		});

		it('keeps an unknown status out of entitlement', async () => {
			seedSubscription({status: 9});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({state: 'pending', storeState: '9', providerEntitled: false});
		});

		it('takes the slot from the latest transaction after a crossgrade', async () => {
			seedSubscription({status: 1, productId: YEARLY, transaction: {expiresDate: now + ms('364 days')}});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({slot: 'yearly', productId: YEARLY, autoRenewProductId: YEARLY});
		});

		it('picks the item for the original transaction among other subscriptions', async () => {
			seedSubscription({status: 1});
			fake.addTransaction('production', {
				transactionId: '2000000200000001',
				originalTransactionId: '2000000200000001',
				productId: YEARLY,
				expiresDate: now + ms('300 days'),
			});
			fake.putSubscription('production', {
				originalTransactionId: '2000000200000001',
				status: 2,
				latestTransactionId: '2000000200000001',
				customerId: ORIGINAL_ID,
				subscriptionGroupIdentifier: '21000009',
			});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({storeReference: ORIGINAL_ID, state: 'active', slot: 'monthly'});
			const statuses = await new AppStoreServerApiClient().getAllSubscriptionStatuses({
				environment: 'production',
				bundleId: 'com.fluxer',
				transactionId: ORIGINAL_ID,
			});
			expect(statuses.groups.flatMap((group) => group.lastTransactions)).toHaveLength(2);
		});

		it('falls back to the transaction token and purchase date without renewal info', async () => {
			seedSubscription({status: 1, renewalInfo: null});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot).toMatchObject({accountToken: TOKEN, autoRenew: null, autoRenewProductId: null, state: 'active'});
			expect(snapshot.startedAt?.getTime()).toBe(now - ms('1 day'));
		});

		it('prefers the renewal token after an account token change', async () => {
			seedSubscription({status: 1, renewalInfo: {appAccountToken: '11111111-2222-4333-8444-555555555555'}});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot.accountToken).toBe('11111111-2222-4333-8444-555555555555');
		});

		it('keys sandbox purchases apart and routes an unknown environment', async () => {
			seedSubscription({environment: 'sandbox', status: 1});
			const snapshot = await sync.loadPurchase(lookup({environment: null}));
			expect(snapshot).toMatchObject({storeKey: `app_store:sandbox:${ORIGINAL_ID}`, environment: 'sandbox'});
			expect(fake.requests.map((request) => request.environment)).toEqual(['production', 'sandbox']);
		});

		it('reports family shared ownership', async () => {
			seedSubscription({status: 1, transaction: {inAppOwnershipType: 'FAMILY_SHARED'}});
			const snapshot = await sync.loadPurchase(lookup());
			expect(snapshot.ownershipType).toBe('FAMILY_SHARED');
		});

		it('fails when the statuses do not include the subscription', async () => {
			fake.addTransaction('production', {
				transactionId: ORIGINAL_ID,
				originalTransactionId: ORIGINAL_ID,
				productId: MONTHLY,
				expiresDate: now + ms('10 days'),
			});
			await expectSyncFailure(sync.loadPurchase(lookup()), 'not_found');
		});

		it('fails when the latest transaction has no expiry', async () => {
			seedSubscription({status: 1, transaction: {expiresDate: null}});
			await expectSyncFailure(sync.loadPurchase(lookup()), 'missing_expiry');
		});

		it('fails when the product moved to one that is not configured', async () => {
			seedSubscription({status: 1, productId: 'com.fluxer.unknown'});
			await expectSyncFailure(sync.loadPurchase(lookup()), 'unknown_product');
		});

		it('fails when a subscription slot points at a one-time product', async () => {
			seedSubscription({status: 1, productId: GIFT_MONTH});
			await expectSyncFailure(sync.loadPurchase(lookup()), 'mismatch');
		});

		it('passes retryable API errors through', async () => {
			seedSubscription({status: 1});
			fake.failNext({}, {status: 500, errorCode: 5000001});
			const error = await sync.loadPurchase(lookup()).then(
				() => null,
				(caught: unknown) => caught,
			);
			expect(error).toBeInstanceOf(AppStoreServerApiError);
			expect((error as AppStoreServerApiError).retryable).toBe(true);
		});
	});

	describe('gifts', () => {
		const GIFT_ID = '2000000300000001';

		function seedGift(overrides: AppStoreFakePayload = {}): void {
			fake.addTransaction('production', {
				transactionId: GIFT_ID,
				originalTransactionId: GIFT_ID,
				productId: GIFT_YEAR,
				type: 'Consumable',
				purchaseDate: now - ms('5 minutes'),
				appAccountToken: TOKEN_UPPER,
				storefront: 'BRA',
				...overrides,
			});
		}

		function giftLookup(): AppStorePurchaseLookup {
			return lookup({productId: GIFT_YEAR, transactionId: GIFT_ID, originalTransactionId: GIFT_ID});
		}

		it('maps a purchased gift from Get Transaction Info', async () => {
			seedGift();
			const snapshot = await sync.loadPurchase(giftLookup());
			expect(snapshot).toMatchObject({
				storeKey: `app_store:production:${GIFT_ID}`,
				kind: 'gift',
				slot: 'gift_1_year',
				state: 'purchased',
				providerEntitled: true,
				accountToken: TOKEN,
				expiresAt: null,
				graceEndsAt: null,
				autoRenew: null,
				storeState: null,
				region: 'BRA',
				latestTransactionId: GIFT_ID,
			});
			expect(snapshot.purchasedAt?.getTime()).toBe(now - ms('5 minutes'));
			expect(fake.requests.map((request) => request.path)).toEqual([`/inApps/v1/transactions/${GIFT_ID}`]);
		});

		it('maps a refunded gift', async () => {
			const revokedAt = now - ms('1 minute');
			seedGift({revocationDate: revokedAt, revocationReason: 0, revocationType: 'REFUND_FULL'});
			const snapshot = await sync.loadPurchase(giftLookup());
			expect(snapshot).toMatchObject({
				state: 'refunded',
				providerEntitled: false,
				storeState: 'REFUND_FULL',
				revocationReason: 'REFUND_FULL',
			});
			expect(snapshot.revokedAt?.getTime()).toBe(revokedAt);
		});

		it('fails when a gift product is sold as a subscription', async () => {
			seedGift({type: 'Auto-Renewable Subscription'});
			await expectSyncFailure(sync.loadPurchase(giftLookup()), 'mismatch');
		});

		it('fails when the transaction belongs to another original transaction', async () => {
			seedGift({originalTransactionId: '2000000399999999'});
			await expectSyncFailure(sync.loadPurchase(giftLookup()), 'mismatch');
		});
	});

	it('refuses a product that is not configured without calling Apple', async () => {
		await expectSyncFailure(sync.loadPurchase(lookup({productId: 'com.fluxer.unknown'})), 'unknown_product');
		expect(fake.requests).toHaveLength(0);
	});
});

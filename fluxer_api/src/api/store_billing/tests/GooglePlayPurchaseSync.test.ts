// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildGooglePlayProductSnapshot,
	buildGooglePlaySubscriptionSnapshot,
} from '@app/api/store_billing/google_play/GooglePlayPurchaseSync';
import type {StorePurchaseState} from '@app/api/store_billing/StoreBillingTypes';
import {
	buildFakeProductPurchase,
	buildFakeSubscriptionPurchase,
} from '@app/api/test/msw/handlers/GooglePlayDeveloperApiHandlers';
import {describe, expect, it} from 'vitest';

const NOW = new Date('2026-09-29T12:00:00.000Z');
const FUTURE = '2026-10-29T12:00:00.000Z';
const PAST = '2026-09-28T12:00:00.000Z';
const PACKAGE = 'com.fluxer';
const TOKEN = 'purchase-token-1';

function subscriptionSnapshot(overrides: Parameters<typeof buildFakeSubscriptionPurchase>[0] = {}) {
	return buildGooglePlaySubscriptionSnapshot({
		packageName: PACKAGE,
		purchaseToken: TOKEN,
		purchase: buildFakeSubscriptionPurchase({startTime: '2026-09-01T00:00:00.000Z', ...overrides}),
		now: NOW,
	});
}

function lineItem(productId: string, basePlanId: string | undefined, expiryTime: string, autoRenewEnabled = true) {
	return {
		productId,
		expiryTime,
		latestSuccessfulOrderId: `GPA.${productId}.${basePlanId ?? 'none'}`,
		autoRenewingPlan: {autoRenewEnabled},
		...(basePlanId ? {offerDetails: {basePlanId}} : {}),
	};
}

describe('buildGooglePlaySubscriptionSnapshot', () => {
	it('builds a full snapshot for an active purchase', () => {
		expect(
			subscriptionSnapshot({
				regionCode: 'BR',
				externalAccountIdentifiers: {obfuscatedExternalAccountId: '0f1e2d3c-4b5a-4968-8776-655443322110'},
				lineItems: [lineItem('plutonium', 'yearly', FUTURE)],
			}),
		).toEqual({
			storeKey: `google_play:${TOKEN}`,
			provider: 'google_play',
			kind: 'subscription',
			slot: 'yearly',
			environment: 'production',
			appId: PACKAGE,
			productId: 'plutonium',
			basePlanId: 'yearly',
			storeReference: TOKEN,
			latestTransactionId: 'GPA.plutonium.yearly',
			appTransactionId: null,
			accountToken: '0f1e2d3c-4b5a-4968-8776-655443322110',
			ownershipType: null,
			state: 'active',
			storeState: 'SUBSCRIPTION_STATE_ACTIVE',
			providerEntitled: true,
			expiresAt: new Date(FUTURE),
			graceEndsAt: null,
			autoRenew: true,
			autoRenewProductId: 'plutonium',
			startedAt: new Date('2026-09-01T00:00:00.000Z'),
			purchasedAt: new Date('2026-09-01T00:00:00.000Z'),
			revokedAt: null,
			revocationReason: null,
			linkedStoreKey: null,
			expiredStoreKey: null,
			expiredAccountToken: null,
			acknowledged: false,
			quantity: 1,
			region: 'BR',
			lastEventAt: NOW,
		});
	});

	const stateCases: Array<{
		subscriptionState: string;
		expiryTime: string;
		state: StorePurchaseState;
		entitled: boolean;
		graceEndsAt: string | null;
	}> = [
		{
			subscriptionState: 'SUBSCRIPTION_STATE_ACTIVE',
			expiryTime: FUTURE,
			state: 'active',
			entitled: true,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_ACTIVE',
			expiryTime: PAST,
			state: 'active',
			entitled: true,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_IN_GRACE_PERIOD',
			expiryTime: FUTURE,
			state: 'grace',
			entitled: true,
			graceEndsAt: FUTURE,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_IN_GRACE_PERIOD',
			expiryTime: PAST,
			state: 'grace',
			entitled: false,
			graceEndsAt: PAST,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_CANCELED',
			expiryTime: FUTURE,
			state: 'canceled',
			entitled: true,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_CANCELED',
			expiryTime: PAST,
			state: 'expired',
			entitled: false,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_ON_HOLD',
			expiryTime: PAST,
			state: 'on_hold',
			entitled: false,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_ON_HOLD',
			expiryTime: FUTURE,
			state: 'on_hold',
			entitled: false,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_PAUSED',
			expiryTime: FUTURE,
			state: 'paused',
			entitled: false,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_EXPIRED',
			expiryTime: PAST,
			state: 'expired',
			entitled: false,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_EXPIRED',
			expiryTime: FUTURE,
			state: 'expired',
			entitled: false,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_PENDING',
			expiryTime: FUTURE,
			state: 'pending',
			entitled: false,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_PENDING_PURCHASE_CANCELED',
			expiryTime: FUTURE,
			state: 'expired',
			entitled: false,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_UNSPECIFIED',
			expiryTime: FUTURE,
			state: 'pending',
			entitled: false,
			graceEndsAt: null,
		},
		{
			subscriptionState: 'SUBSCRIPTION_STATE_INTRODUCED_LATER',
			expiryTime: FUTURE,
			state: 'pending',
			entitled: false,
			graceEndsAt: null,
		},
	];

	it.each(stateCases)(
		'maps $subscriptionState expiring $expiryTime to $state',
		({subscriptionState, expiryTime, state, entitled, graceEndsAt}) => {
			const snapshot = subscriptionSnapshot({
				subscriptionState,
				lineItems: [lineItem('plutonium', 'monthly', expiryTime)],
			});
			expect(snapshot?.state).toBe(state);
			expect(snapshot?.storeState).toBe(subscriptionState);
			expect(snapshot?.providerEntitled).toBe(entitled);
			expect(snapshot?.expiresAt).toEqual(new Date(expiryTime));
			expect(snapshot?.graceEndsAt).toEqual(graceEndsAt === null ? null : new Date(graceEndsAt));
		},
	);

	it('treats an active purchase past its expiry as silent grace with no explicit grace end', () => {
		const snapshot = subscriptionSnapshot({lineItems: [lineItem('plutonium', 'monthly', PAST)]});
		expect(snapshot).toMatchObject({state: 'active', providerEntitled: true, graceEndsAt: null});
		expect(snapshot?.expiresAt).toEqual(new Date(PAST));
	});

	it('treats a missing subscription state as pending', () => {
		const snapshot = subscriptionSnapshot({subscriptionState: undefined});
		expect(snapshot).toMatchObject({
			state: 'pending',
			storeState: 'SUBSCRIPTION_STATE_UNSPECIFIED',
			providerEntitled: false,
		});
	});

	it('never entitles an active purchase without an expiry', () => {
		const snapshot = subscriptionSnapshot({
			lineItems: [{productId: 'plutonium', offerDetails: {basePlanId: 'monthly'}}],
		});
		expect(snapshot).toMatchObject({state: 'active', providerEntitled: false, expiresAt: null});
	});

	it('ignores line items for products that are not configured', () => {
		const snapshot = subscriptionSnapshot({
			lineItems: [
				lineItem('addon', 'monthly', '2027-09-29T12:00:00.000Z'),
				lineItem('plutonium', 'weekly', '2027-09-29T12:00:00.000Z'),
				lineItem('plutonium', 'monthly', FUTURE),
			],
		});
		expect(snapshot).toMatchObject({productId: 'plutonium', basePlanId: 'monthly', slot: 'monthly'});
		expect(snapshot?.expiresAt).toEqual(new Date(FUTURE));
	});

	it('picks the configured line item with the latest expiry', () => {
		const snapshot = subscriptionSnapshot({
			lineItems: [
				lineItem('plutonium', 'monthly', FUTURE),
				lineItem('plutonium', 'yearly', '2027-09-29T12:00:00.000Z'),
			],
		});
		expect(snapshot).toMatchObject({slot: 'yearly', latestTransactionId: 'GPA.plutonium.yearly'});
	});

	it('returns null when no line item is a configured subscription', () => {
		expect(subscriptionSnapshot({lineItems: [lineItem('addon', 'monthly', FUTURE)]})).toBeNull();
		expect(subscriptionSnapshot({lineItems: [lineItem('plutonium', undefined, FUTURE)]})).toBeNull();
		expect(subscriptionSnapshot({lineItems: [lineItem('gift_1_month', 'monthly', FUTURE)]})).toBeNull();
		expect(subscriptionSnapshot({lineItems: []})).toBeNull();
		expect(subscriptionSnapshot({lineItems: undefined})).toBeNull();
	});

	it('records the linked token of an upgrade or resubscribe', () => {
		expect(subscriptionSnapshot({linkedPurchaseToken: 'old-token'})?.linkedStoreKey).toBe('google_play:old-token');
	});

	it('records the out-of-app resubscribe context', () => {
		expect(
			subscriptionSnapshot({
				outOfAppPurchaseContext: {
					expiredPurchaseToken: 'expired-token',
					expiredExternalAccountIdentifiers: {obfuscatedExternalAccountId: 'aaaa-account'},
				},
			}),
		).toMatchObject({
			expiredStoreKey: 'google_play:expired-token',
			expiredAccountToken: 'aaaa-account',
			accountToken: null,
		});
	});

	it('reads the out-of-app account id under its documented example name too', () => {
		expect(
			subscriptionSnapshot({
				outOfAppPurchaseContext: {expiredExternalAccountIdentifiers: {obfuscatedAccountId: 'bbbb-account'}},
			}),
		).toMatchObject({expiredStoreKey: null, expiredAccountToken: 'bbbb-account'});
	});

	it('marks license test purchases as sandbox', () => {
		expect(subscriptionSnapshot({testPurchase: {}})?.environment).toBe('sandbox');
		expect(subscriptionSnapshot()?.environment).toBe('production');
	});

	it('reads the acknowledgement state', () => {
		expect(subscriptionSnapshot({acknowledgementState: 'ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED'})?.acknowledged).toBe(true);
		expect(subscriptionSnapshot({acknowledgementState: 'ACKNOWLEDGEMENT_STATE_PENDING'})?.acknowledged).toBe(false);
		expect(subscriptionSnapshot({acknowledgementState: undefined})?.acknowledged).toBe(false);
	});

	it('reads auto renewal from the plan type', () => {
		const base = {productId: 'plutonium', expiryTime: FUTURE, offerDetails: {basePlanId: 'monthly'}};
		expect(subscriptionSnapshot({lineItems: [{...base, autoRenewingPlan: {}}]})?.autoRenew).toBe(false);
		expect(subscriptionSnapshot({lineItems: [{...base, autoRenewingPlan: {autoRenewEnabled: false}}]})?.autoRenew).toBe(
			false,
		);
		expect(subscriptionSnapshot({lineItems: [{...base, prepaidPlan: {}}]})?.autoRenew).toBe(false);
		expect(subscriptionSnapshot({lineItems: [base]})?.autoRenew).toBeNull();
	});

	it('reports the product a deferred replacement renews into', () => {
		const snapshot = subscriptionSnapshot({
			lineItems: [
				{...lineItem('plutonium', 'monthly', FUTURE), deferredItemReplacement: {productId: 'plutonium_plus'}},
			],
		});
		expect(snapshot?.autoRenewProductId).toBe('plutonium_plus');
	});

	it('falls back to the deprecated latest order id', () => {
		const snapshot = subscriptionSnapshot({
			latestOrderId: 'GPA.legacy',
			lineItems: [{productId: 'plutonium', expiryTime: FUTURE, offerDetails: {basePlanId: 'monthly'}}],
		});
		expect(snapshot?.latestTransactionId).toBe('GPA.legacy');
	});

	it('leaves start dates empty when Google omits or garbles them', () => {
		expect(subscriptionSnapshot({startTime: 'yesterday'})).toMatchObject({startedAt: null, purchasedAt: null});
	});
});

describe('buildGooglePlayProductSnapshot', () => {
	function productSnapshot(overrides: Parameters<typeof buildFakeProductPurchase>[0] = {}, expectedProductId?: string) {
		return buildGooglePlayProductSnapshot({
			packageName: PACKAGE,
			purchaseToken: TOKEN,
			expectedProductId,
			purchase: buildFakeProductPurchase({purchaseCompletionTime: '2026-09-29T11:00:00.000Z', ...overrides}),
			now: NOW,
		});
	}

	it('builds a full snapshot for a purchased gift', () => {
		expect(
			productSnapshot({
				obfuscatedExternalAccountId: '0f1e2d3c-4b5a-4968-8776-655443322110',
				productLineItem: [{productId: 'gift_1_year', productOfferDetails: {quantity: 1}}],
			}),
		).toEqual({
			storeKey: `google_play:${TOKEN}`,
			provider: 'google_play',
			kind: 'gift',
			slot: 'gift_1_year',
			environment: 'production',
			appId: PACKAGE,
			productId: 'gift_1_year',
			basePlanId: null,
			storeReference: TOKEN,
			latestTransactionId: 'GPA.5555-6666-7777-88888',
			appTransactionId: null,
			accountToken: '0f1e2d3c-4b5a-4968-8776-655443322110',
			ownershipType: null,
			state: 'purchased',
			storeState: 'PURCHASED',
			providerEntitled: true,
			expiresAt: null,
			graceEndsAt: null,
			autoRenew: null,
			autoRenewProductId: null,
			startedAt: new Date('2026-09-29T11:00:00.000Z'),
			purchasedAt: new Date('2026-09-29T11:00:00.000Z'),
			revokedAt: null,
			revocationReason: null,
			linkedStoreKey: null,
			expiredStoreKey: null,
			expiredAccountToken: null,
			acknowledged: false,
			quantity: 1,
			region: 'US',
			lastEventAt: NOW,
		});
	});

	it('maps a consumed purchase to fulfilled and acknowledged', () => {
		expect(
			productSnapshot({
				productLineItem: [
					{productId: 'gift_1_month', productOfferDetails: {consumptionState: 'CONSUMPTION_STATE_CONSUMED'}},
				],
			}),
		).toMatchObject({state: 'fulfilled', providerEntitled: true, acknowledged: true});
	});

	it('maps a cancelled purchase to refunded', () => {
		expect(productSnapshot({purchaseStateContext: {purchaseState: 'CANCELLED'}})).toMatchObject({
			state: 'refunded',
			storeState: 'CANCELLED',
			providerEntitled: false,
		});
	});

	it('never entitles a pending or unknown purchase state', () => {
		expect(productSnapshot({purchaseStateContext: {purchaseState: 'PENDING'}})).toMatchObject({
			state: 'pending',
			providerEntitled: false,
		});
		expect(productSnapshot({purchaseStateContext: {purchaseState: 'PURCHASE_STATE_NEW'}})).toMatchObject({
			state: 'pending',
			providerEntitled: false,
		});
		expect(productSnapshot({purchaseStateContext: undefined})).toMatchObject({
			state: 'pending',
			storeState: 'PURCHASE_STATE_UNSPECIFIED',
			providerEntitled: false,
		});
	});

	it('marks test instrument purchases as sandbox', () => {
		expect(productSnapshot({testPurchaseContext: {fopType: 'TEST'}})?.environment).toBe('sandbox');
		expect(productSnapshot({testPurchaseContext: {}})?.environment).toBe('sandbox');
	});

	it('treats an acknowledged but unconsumed gift as not settled', () => {
		expect(productSnapshot({acknowledgementState: 'ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED'})?.acknowledged).toBe(false);
	});

	it('reads the purchased quantity', () => {
		expect(productSnapshot({})?.quantity).toBe(1);
		expect(
			productSnapshot({productLineItem: [{productId: 'gift_1_month', productOfferDetails: {quantity: 3}}]})?.quantity,
		).toBe(3);
	});

	it('only accepts configured gift products', () => {
		expect(productSnapshot({productLineItem: [{productId: 'gems_100'}]})).toBeNull();
		expect(productSnapshot({productLineItem: [{productId: 'plutonium'}]})).toBeNull();
		expect(productSnapshot({productLineItem: []})).toBeNull();
		expect(productSnapshot({productLineItem: [{productId: 'gems_100'}, {productId: 'gift_1_year'}]})?.slot).toBe(
			'gift_1_year',
		);
	});

	it('requires the product the client claimed', () => {
		expect(productSnapshot({}, 'gift_1_year')).toBeNull();
		expect(productSnapshot({}, 'gift_1_month')?.slot).toBe('gift_1_month');
	});
});

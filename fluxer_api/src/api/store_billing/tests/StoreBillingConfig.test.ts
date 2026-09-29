// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {
	getAppStoreApp,
	getAppStoreConfig,
	getAppStoreProductSlot,
	getGooglePlayConfig,
	getGooglePlayOneTimeProductSlot,
	getGooglePlaySubscriptionSlot,
	isAppStoreConfigured,
	isGooglePlayConfigured,
	isGooglePlayPackageConfigured,
	isGooglePlaySubscriptionProduct,
	isSandboxEntitlementAllowed,
	listAppStoreProducts,
	listGooglePlayProducts,
	STORE_PRODUCT_SLOTS,
} from '@app/api/store_billing/StoreBillingConfig';
import {
	buildAppStoreStoreKey,
	buildGooglePlayStoreKey,
	isTerminalStorePurchaseState,
} from '@app/api/store_billing/StoreBillingTypes';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe('store product slots', () => {
	it('mirrors the Stripe product shapes', () => {
		expect(STORE_PRODUCT_SLOTS).toEqual({
			monthly: {kind: 'subscription', billingCycle: 'monthly', durationMonths: 1},
			yearly: {kind: 'subscription', billingCycle: 'yearly', durationMonths: 12},
			gift_1_month: {kind: 'gift', billingCycle: null, durationMonths: 1},
			gift_1_year: {kind: 'gift', billingCycle: null, durationMonths: 12},
		});
	});
});

describe('store billing config', () => {
	let savedAppStore: typeof Config.appStore;
	let savedGooglePlay: typeof Config.googlePlay;
	let savedStoreBilling: typeof Config.storeBilling;
	let savedSelfHosted: boolean;

	beforeEach(() => {
		savedAppStore = {...Config.appStore};
		savedGooglePlay = {...Config.googlePlay};
		savedStoreBilling = {...Config.storeBilling};
		savedSelfHosted = Config.instance.selfHosted;
	});

	afterEach(() => {
		Object.assign(Config.appStore, savedAppStore);
		Object.assign(Config.googlePlay, savedGooglePlay);
		Object.assign(Config.storeBilling, savedStoreBilling);
		Config.instance.selfHosted = savedSelfHosted;
	});

	it('treats both stores as configured with the test credentials', () => {
		expect(getAppStoreConfig().enabled).toBe(true);
		expect(getGooglePlayConfig().packages).toEqual(['com.fluxer']);
		expect(isAppStoreConfigured()).toBe(true);
		expect(isGooglePlayConfigured()).toBe(true);
	});

	it('treats a store as not configured when disabled', () => {
		Config.appStore.enabled = false;
		Config.googlePlay.enabled = false;

		expect(isAppStoreConfigured()).toBe(false);
		expect(isGooglePlayConfigured()).toBe(false);
	});

	it('treats both stores as not configured on a self-hosted instance', () => {
		Config.instance.selfHosted = true;

		expect(isAppStoreConfigured()).toBe(false);
		expect(isGooglePlayConfigured()).toBe(false);
	});

	it('requires App Store signing credentials, an app and a product', () => {
		Config.appStore.privateKey = undefined;
		expect(isAppStoreConfigured()).toBe(false);
		Config.appStore.privateKeyPath = '/etc/fluxer/keys/app-store.p8';
		expect(isAppStoreConfigured()).toBe(true);
		Config.appStore.keyId = ' ';
		expect(isAppStoreConfigured()).toBe(false);
		Config.appStore.keyId = savedAppStore.keyId;
		Config.appStore.apps = [];
		expect(isAppStoreConfigured()).toBe(false);
		Config.appStore.apps = savedAppStore.apps;
		Config.appStore.products = {};
		expect(isAppStoreConfigured()).toBe(false);
	});

	it('accepts Google Play credentials inline or from a service account file', () => {
		Config.googlePlay.privateKey = undefined;
		expect(isGooglePlayConfigured()).toBe(false);
		Config.googlePlay.clientEmail = undefined;
		Config.googlePlay.serviceAccountJsonPath = '/etc/fluxer/keys/play.json';
		expect(isGooglePlayConfigured()).toBe(true);
		Config.googlePlay.packages = [];
		expect(isGooglePlayConfigured()).toBe(false);
	});

	it('resolves App Store apps and product slots', () => {
		expect(getAppStoreApp('com.fluxer')).toEqual({bundleId: 'com.fluxer', appAppleId: 1234567890});
		expect(getAppStoreApp('com.other')).toBeNull();
		expect(getAppStoreProductSlot('com.fluxer.plutonium.yearly')).toBe('yearly');
		expect(getAppStoreProductSlot('com.fluxer.gift.1month')).toBe('gift_1_month');
		expect(getAppStoreProductSlot('com.fluxer.unknown')).toBeNull();
		expect(getAppStoreProductSlot('toString')).toBeNull();
		expect(listAppStoreProducts()).toContainEqual({productId: 'com.fluxer.plutonium.monthly', slot: 'monthly'});
	});

	it('resolves Google Play subscriptions by base plan and one-time products by id', () => {
		expect(getGooglePlaySubscriptionSlot('plutonium', 'monthly')).toBe('monthly');
		expect(getGooglePlaySubscriptionSlot('plutonium', 'weekly')).toBeNull();
		expect(getGooglePlaySubscriptionSlot('gift_1_month', '')).toBeNull();
		expect(getGooglePlayOneTimeProductSlot('gift_1_year')).toBe('gift_1_year');
		expect(getGooglePlayOneTimeProductSlot('plutonium')).toBeNull();
		expect(getGooglePlayOneTimeProductSlot('constructor')).toBeNull();
		expect(isGooglePlaySubscriptionProduct('plutonium')).toBe(true);
		expect(isGooglePlaySubscriptionProduct('gift_1_month')).toBe(false);
		expect(isGooglePlayPackageConfigured('com.fluxer')).toBe(true);
		expect(isGooglePlayPackageConfigured('com.fluxer.canary')).toBe(false);
		expect(listGooglePlayProducts()).toEqual(
			expect.arrayContaining([
				{productId: 'plutonium', basePlanId: 'yearly', slot: 'yearly'},
				{productId: 'gift_1_month', basePlanId: null, slot: 'gift_1_month'},
			]),
		);
	});

	it('entitles sandbox purchases only for allowed users unless every user is allowed', () => {
		const allowed = createUserID(4242n);
		const other = createUserID(4343n);
		expect(isSandboxEntitlementAllowed(allowed)).toBe(false);

		Config.storeBilling.sandboxUserIds = ['4242'];
		expect(isSandboxEntitlementAllowed(allowed)).toBe(true);
		expect(isSandboxEntitlementAllowed(other)).toBe(false);

		Config.storeBilling.sandboxEntitlesAll = true;
		expect(isSandboxEntitlementAllowed(other)).toBe(true);
	});
});

describe('store purchase keys and states', () => {
	it('builds store keys per provider and environment', () => {
		expect(buildAppStoreStoreKey('sandbox', '2000000000000001')).toBe('app_store:sandbox:2000000000000001');
		expect(buildGooglePlayStoreKey('token.abc')).toBe('google_play:token.abc');
	});

	it('treats only final states as terminal', () => {
		for (const state of ['expired', 'revoked', 'superseded', 'fulfilled', 'refunded'] as const) {
			expect(isTerminalStorePurchaseState(state)).toBe(true);
		}
		for (const state of ['pending', 'active', 'grace', 'billing_retry', 'on_hold', 'paused', 'canceled'] as const) {
			expect(isTerminalStorePurchaseState(state)).toBe(false);
		}
		expect(isTerminalStorePurchaseState('purchased')).toBe(false);
	});
});

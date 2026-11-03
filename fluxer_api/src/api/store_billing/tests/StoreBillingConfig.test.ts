// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {
	isAppStoreConfigured,
	isGooglePlayConfigured,
	isSandboxEntitlementAllowed,
} from '@app/api/store_billing/StoreBillingConfig';
import {isTerminalStorePurchaseState} from '@app/api/store_billing/StoreBillingTypes';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

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

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {AppStoreAppConfig} from '@app/api/config/APIConfig';
import type {StorePurchaseKind, StoreSlot} from '@app/api/store_billing/StoreBillingTypes';
import type {RecurringBillingCycle} from '@app/api/stripe/ProductRegistry';

interface StoreProductSlotShape {
	kind: StorePurchaseKind;
	billingCycle: RecurringBillingCycle | null;
	durationMonths: number;
}

export const STORE_PRODUCT_SLOTS: Record<StoreSlot, StoreProductSlotShape> = {
	monthly: {kind: 'subscription', billingCycle: 'monthly', durationMonths: 1},
	yearly: {kind: 'subscription', billingCycle: 'yearly', durationMonths: 12},
	gift_1_month: {kind: 'gift', billingCycle: null, durationMonths: 1},
	gift_1_year: {kind: 'gift', billingCycle: null, durationMonths: 12},
};

export interface GooglePlayProductEntry {
	productId: string;
	basePlanId: string | null;
	slot: StoreSlot;
}

function hasText(value: string | undefined): boolean {
	return value !== undefined && value.trim().length > 0;
}

export function isAppStoreConfigured(): boolean {
	const cfg = Config.appStore;
	return (
		!Config.instance.selfHosted &&
		cfg.enabled &&
		hasText(cfg.issuerId) &&
		hasText(cfg.keyId) &&
		(hasText(cfg.privateKey) || hasText(cfg.privateKeyPath)) &&
		cfg.apps.length > 0 &&
		Object.keys(cfg.products).length > 0
	);
}

export function isGooglePlayConfigured(): boolean {
	const cfg = Config.googlePlay;
	const hasCredentials =
		hasText(cfg.serviceAccountJsonPath) ||
		(hasText(cfg.clientEmail) && (hasText(cfg.privateKey) || hasText(cfg.privateKeyPath)));
	return (
		!Config.instance.selfHosted &&
		cfg.enabled &&
		hasCredentials &&
		cfg.packages.length > 0 &&
		Object.keys(cfg.products).length > 0
	);
}

export function getAppStoreApp(bundleId: string): AppStoreAppConfig | null {
	return Config.appStore.apps.find((app) => app.bundleId === bundleId) ?? null;
}

export function getAppStoreProductSlot(productId: string): StoreSlot | null {
	return Object.hasOwn(Config.appStore.products, productId) ? Config.appStore.products[productId] : null;
}

export function listAppStoreProducts(): Array<{productId: string; slot: StoreSlot}> {
	return Object.entries(Config.appStore.products).map(([productId, slot]) => ({productId, slot}));
}

export function isGooglePlayPackageConfigured(packageName: string): boolean {
	return Config.googlePlay.packages.includes(packageName);
}

export function listGooglePlayProducts(): Array<GooglePlayProductEntry> {
	return Object.entries(Config.googlePlay.products).map(([key, slot]) => {
		const separator = key.indexOf(':');
		if (separator === -1) {
			return {productId: key, basePlanId: null, slot};
		}
		return {productId: key.slice(0, separator), basePlanId: key.slice(separator + 1), slot};
	});
}

export function getGooglePlaySubscriptionSlot(productId: string, basePlanId: string): StoreSlot | null {
	const key = `${productId}:${basePlanId}`;
	if (!Object.hasOwn(Config.googlePlay.products, key)) {
		return null;
	}
	const slot = Config.googlePlay.products[key];
	return STORE_PRODUCT_SLOTS[slot].kind === 'subscription' ? slot : null;
}

export function getGooglePlayOneTimeProductSlot(productId: string): StoreSlot | null {
	if (!Object.hasOwn(Config.googlePlay.products, productId)) {
		return null;
	}
	const slot = Config.googlePlay.products[productId];
	return STORE_PRODUCT_SLOTS[slot].kind === 'gift' ? slot : null;
}

export function isGooglePlaySubscriptionProduct(productId: string): boolean {
	return listGooglePlayProducts().some(
		(entry) => entry.productId === productId && STORE_PRODUCT_SLOTS[entry.slot].kind === 'subscription',
	);
}

export function isSandboxEntitlementAllowed(userId: UserID): boolean {
	const cfg = Config.storeBilling;
	return cfg.sandboxEntitlesAll || cfg.sandboxUserIds.includes(userId.toString());
}

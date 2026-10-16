// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	ProductPurchaseV2,
	SubscriptionPurchaseLineItem,
	SubscriptionPurchaseV2,
} from '@app/api/store_billing/google_play/GooglePlayDeveloperApiClient';
import {
	getGooglePlayOneTimeProductSlot,
	getGooglePlaySubscriptionSlot,
} from '@app/api/store_billing/StoreBillingConfig';
import {
	buildGooglePlayStoreKey,
	type StorePurchaseSnapshot,
	type StorePurchaseState,
	type StoreSlot,
} from '@app/api/store_billing/StoreBillingTypes';

const ENTITLING_SUBSCRIPTION_STATES: ReadonlySet<string> = new Set([
	'SUBSCRIPTION_STATE_ACTIVE',
	'SUBSCRIPTION_STATE_IN_GRACE_PERIOD',
	'SUBSCRIPTION_STATE_CANCELED',
]);

interface SyncGooglePlaySubscriptionParams {
	packageName: string;
	purchaseToken: string;
	purchase: SubscriptionPurchaseV2;
	now: Date;
}

interface SyncGooglePlayProductParams {
	packageName: string;
	purchaseToken: string;
	expectedProductId?: string;
	purchase: ProductPurchaseV2;
	now: Date;
}

interface ConfiguredLineItem {
	lineItem: SubscriptionPurchaseLineItem;
	basePlanId: string;
	slot: StoreSlot;
	expiresAt: Date | null;
}

function parseTime(value: string | undefined): Date | null {
	if (!value) {
		return null;
	}
	const parsed = Date.parse(value);
	return Number.isNaN(parsed) ? null : new Date(parsed);
}

function nonEmpty(value: string | undefined): string | null {
	return value && value.length > 0 ? value : null;
}

function pickConfiguredLineItem(lineItems: Array<SubscriptionPurchaseLineItem>): ConfiguredLineItem | null {
	let best: ConfiguredLineItem | null = null;
	for (const lineItem of lineItems) {
		const basePlanId = lineItem.offerDetails?.basePlanId;
		if (!basePlanId) {
			continue;
		}
		const slot = getGooglePlaySubscriptionSlot(lineItem.productId, basePlanId);
		if (!slot) {
			continue;
		}
		const candidate: ConfiguredLineItem = {lineItem, basePlanId, slot, expiresAt: parseTime(lineItem.expiryTime)};
		if (!best || (candidate.expiresAt?.getTime() ?? 0) > (best.expiresAt?.getTime() ?? 0)) {
			best = candidate;
		}
	}
	return best;
}

function mapSubscriptionState(subscriptionState: string, expiresAt: Date | null, now: Date): StorePurchaseState {
	switch (subscriptionState) {
		case 'SUBSCRIPTION_STATE_ACTIVE':
			return 'active';
		case 'SUBSCRIPTION_STATE_IN_GRACE_PERIOD':
			return 'grace';
		case 'SUBSCRIPTION_STATE_CANCELED':
			return expiresAt && expiresAt.getTime() > now.getTime() ? 'canceled' : 'expired';
		case 'SUBSCRIPTION_STATE_ON_HOLD':
			return 'on_hold';
		case 'SUBSCRIPTION_STATE_PAUSED':
			return 'paused';
		case 'SUBSCRIPTION_STATE_EXPIRED':
		case 'SUBSCRIPTION_STATE_PENDING_PURCHASE_CANCELED':
			return 'expired';
		default:
			return 'pending';
	}
}

function isSubscriptionEntitled(subscriptionState: string, expiresAt: Date | null, now: Date): boolean {
	if (!ENTITLING_SUBSCRIPTION_STATES.has(subscriptionState) || !expiresAt) {
		return false;
	}
	if (subscriptionState === 'SUBSCRIPTION_STATE_ACTIVE') {
		return true;
	}
	return expiresAt.getTime() > now.getTime();
}

function resolveAutoRenew(lineItem: SubscriptionPurchaseLineItem): boolean | null {
	if (lineItem.autoRenewingPlan) {
		return lineItem.autoRenewingPlan.autoRenewEnabled === true;
	}
	if (lineItem.prepaidPlan) {
		return false;
	}
	return null;
}

export function buildGooglePlaySubscriptionSnapshot(
	params: SyncGooglePlaySubscriptionParams,
): StorePurchaseSnapshot | null {
	const {purchase, now} = params;
	const configured = pickConfiguredLineItem(purchase.lineItems ?? []);
	if (!configured) {
		return null;
	}
	const subscriptionState = purchase.subscriptionState ?? 'SUBSCRIPTION_STATE_UNSPECIFIED';
	const expiresAt = configured.expiresAt;
	const state = mapSubscriptionState(subscriptionState, expiresAt, now);
	const startedAt = parseTime(purchase.startTime);
	const outOfApp = purchase.outOfAppPurchaseContext;
	const expiredIdentifiers = outOfApp?.expiredExternalAccountIdentifiers;
	return {
		storeKey: buildGooglePlayStoreKey(params.purchaseToken),
		provider: 'google_play',
		kind: 'subscription',
		slot: configured.slot,
		environment: purchase.testPurchase ? 'sandbox' : 'production',
		appId: params.packageName,
		productId: configured.lineItem.productId,
		basePlanId: configured.basePlanId,
		storeReference: params.purchaseToken,
		latestTransactionId:
			nonEmpty(configured.lineItem.latestSuccessfulOrderId) ?? nonEmpty(purchase.latestOrderId) ?? null,
		appTransactionId: null,
		accountToken: nonEmpty(purchase.externalAccountIdentifiers?.obfuscatedExternalAccountId),
		ownershipType: null,
		state,
		storeState: subscriptionState,
		providerEntitled: isSubscriptionEntitled(subscriptionState, expiresAt, now),
		expiresAt,
		graceEndsAt: state === 'grace' ? expiresAt : null,
		autoRenew: resolveAutoRenew(configured.lineItem),
		autoRenewProductId:
			nonEmpty(configured.lineItem.deferredItemReplacement?.productId) ?? configured.lineItem.productId,
		startedAt,
		purchasedAt: startedAt,
		revokedAt: null,
		revocationReason: null,
		linkedStoreKey: purchase.linkedPurchaseToken ? buildGooglePlayStoreKey(purchase.linkedPurchaseToken) : null,
		expiredStoreKey: outOfApp?.expiredPurchaseToken ? buildGooglePlayStoreKey(outOfApp.expiredPurchaseToken) : null,
		expiredAccountToken:
			nonEmpty(expiredIdentifiers?.obfuscatedExternalAccountId) ??
			nonEmpty(expiredIdentifiers?.obfuscatedAccountId) ??
			null,
		acknowledged: purchase.acknowledgementState === 'ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED',
		quantity: 1,
		region: nonEmpty(purchase.regionCode),
		lastEventAt: now,
	};
}

function mapProductState(purchaseState: string, consumed: boolean): StorePurchaseState {
	switch (purchaseState) {
		case 'PURCHASED':
			return consumed ? 'fulfilled' : 'purchased';
		case 'CANCELLED':
			return 'refunded';
		default:
			return 'pending';
	}
}

export function buildGooglePlayProductSnapshot(params: SyncGooglePlayProductParams): StorePurchaseSnapshot | null {
	const {purchase, now} = params;
	const lineItem = (purchase.productLineItem ?? []).find(
		(item) =>
			(params.expectedProductId === undefined || item.productId === params.expectedProductId) &&
			getGooglePlayOneTimeProductSlot(item.productId) !== null,
	);
	if (!lineItem) {
		return null;
	}
	const slot = getGooglePlayOneTimeProductSlot(lineItem.productId);
	if (!slot) {
		return null;
	}
	const purchaseState = purchase.purchaseStateContext?.purchaseState ?? 'PURCHASE_STATE_UNSPECIFIED';
	const consumed = lineItem.productOfferDetails?.consumptionState === 'CONSUMPTION_STATE_CONSUMED';
	const state = mapProductState(purchaseState, consumed);
	const purchasedAt = parseTime(purchase.purchaseCompletionTime);
	return {
		storeKey: buildGooglePlayStoreKey(params.purchaseToken),
		provider: 'google_play',
		kind: 'gift',
		slot,
		environment: purchase.testPurchaseContext ? 'sandbox' : 'production',
		appId: params.packageName,
		productId: lineItem.productId,
		basePlanId: null,
		storeReference: params.purchaseToken,
		latestTransactionId: nonEmpty(purchase.orderId),
		appTransactionId: null,
		accountToken: nonEmpty(purchase.obfuscatedExternalAccountId),
		ownershipType: null,
		state,
		storeState: purchaseState,
		providerEntitled: purchaseState === 'PURCHASED',
		expiresAt: null,
		graceEndsAt: null,
		autoRenew: null,
		autoRenewProductId: null,
		startedAt: purchasedAt,
		purchasedAt,
		revokedAt: null,
		revocationReason: null,
		linkedStoreKey: null,
		expiredStoreKey: null,
		expiredAccountToken: null,
		acknowledged: consumed,
		quantity: lineItem.productOfferDetails?.quantity ?? 1,
		region: nonEmpty(purchase.regionCode),
		lastEventAt: now,
	};
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AppStoreRenewalInfoPayload,
	type AppStoreTransactionPayload,
	toStoreEnvironment,
} from '@app/api/store_billing/app_store/AppStoreJwsVerifier';
import type {
	AppStoreLastTransaction,
	AppStoreServerApiClient,
} from '@app/api/store_billing/app_store/AppStoreServerApiClient';
import {getAppStoreProductSlot, STORE_PRODUCT_SLOTS} from '@app/api/store_billing/StoreBillingConfig';
import {
	buildAppStoreStoreKey,
	type StoreEnvironment,
	type StorePurchaseSnapshot,
	type StorePurchaseState,
	type StoreSlot,
} from '@app/api/store_billing/StoreBillingTypes';

const APP_STORE_SUBSCRIPTION_STATUS_ACTIVE = 1;
const APP_STORE_SUBSCRIPTION_STATUS_EXPIRED = 2;
const APP_STORE_SUBSCRIPTION_STATUS_BILLING_RETRY = 3;
const APP_STORE_SUBSCRIPTION_STATUS_GRACE = 4;
const APP_STORE_SUBSCRIPTION_STATUS_REVOKED = 5;
const APP_STORE_AUTO_RENEWABLE_TYPE = 'Auto-Renewable Subscription';
const APP_STORE_AUTO_RENEW_OFF = 0;
const APP_STORE_AUTO_RENEW_ON = 1;

export type AppStorePurchaseSyncFailureReason = 'unknown_product' | 'not_found' | 'mismatch' | 'missing_expiry';

export class AppStorePurchaseSyncError extends Error {
	constructor(readonly reason: AppStorePurchaseSyncFailureReason) {
		super(`App Store purchase could not be synced: ${reason}`);
		this.name = 'AppStorePurchaseSyncError';
	}
}

export interface AppStorePurchaseLookup {
	environment: StoreEnvironment | null;
	bundleId: string;
	productId: string;
	transactionId: string;
	originalTransactionId: string;
}

function toDate(value: number | null | undefined): Date | null {
	return value === null || value === undefined ? null : new Date(value);
}

function resolveSlot(productId: string): StoreSlot {
	const slot = getAppStoreProductSlot(productId);
	if (!slot) {
		throw new AppStorePurchaseSyncError('unknown_product');
	}
	return slot;
}

function normalizeAccountToken(token: string | null | undefined): string | null {
	return token ? token.toLowerCase() : null;
}

function revocationReason(transaction: AppStoreTransactionPayload): string | null {
	if (transaction.revocationType) {
		return transaction.revocationType;
	}
	return transaction.revocationReason === null || transaction.revocationReason === undefined
		? null
		: String(transaction.revocationReason);
}

function resolveAppStoreSubscriptionState(
	status: number,
	transaction: AppStoreTransactionPayload,
	renewalInfo: AppStoreRenewalInfoPayload | null,
): StorePurchaseState {
	if (status === APP_STORE_SUBSCRIPTION_STATUS_REVOKED || transaction.revocationDate) {
		return 'revoked';
	}
	switch (status) {
		case APP_STORE_SUBSCRIPTION_STATUS_ACTIVE:
			return renewalInfo?.autoRenewStatus === APP_STORE_AUTO_RENEW_OFF ? 'canceled' : 'active';
		case APP_STORE_SUBSCRIPTION_STATUS_GRACE:
			return 'grace';
		case APP_STORE_SUBSCRIPTION_STATUS_BILLING_RETRY:
			return 'billing_retry';
		case APP_STORE_SUBSCRIPTION_STATUS_EXPIRED:
			return 'expired';
		default:
			return 'pending';
	}
}

function buildAppStoreSubscriptionSnapshot(item: AppStoreLastTransaction): StorePurchaseSnapshot {
	const {transaction, renewalInfo, status} = item;
	if (transaction.originalTransactionId !== item.originalTransactionId) {
		throw new AppStorePurchaseSyncError('mismatch');
	}
	if (transaction.type !== APP_STORE_AUTO_RENEWABLE_TYPE) {
		throw new AppStorePurchaseSyncError('mismatch');
	}
	const slot = resolveSlot(transaction.productId);
	if (STORE_PRODUCT_SLOTS[slot].kind !== 'subscription') {
		throw new AppStorePurchaseSyncError('mismatch');
	}
	if (transaction.expiresDate === null || transaction.expiresDate === undefined) {
		throw new AppStorePurchaseSyncError('missing_expiry');
	}
	const environment = toStoreEnvironment(transaction.environment);
	const state = resolveAppStoreSubscriptionState(status, transaction, renewalInfo);
	const graceEndsAt =
		state === 'grace' ? new Date(renewalInfo?.gracePeriodExpiresDate ?? transaction.expiresDate) : null;
	const autoRenewStatus = renewalInfo?.autoRenewStatus;
	const lastEventMs = Math.max(transaction.signedDate, renewalInfo?.signedDate ?? 0);
	return {
		storeKey: buildAppStoreStoreKey(environment, item.originalTransactionId),
		provider: 'app_store',
		kind: 'subscription',
		slot,
		environment,
		appId: transaction.bundleId,
		productId: transaction.productId,
		basePlanId: null,
		storeReference: item.originalTransactionId,
		latestTransactionId: transaction.transactionId,
		appTransactionId: transaction.appTransactionId ?? renewalInfo?.appTransactionId ?? null,
		accountToken: normalizeAccountToken(renewalInfo?.appAccountToken ?? transaction.appAccountToken),
		ownershipType: transaction.inAppOwnershipType ?? null,
		state,
		storeState: String(status),
		providerEntitled: state === 'active' || state === 'canceled' || state === 'grace',
		expiresAt: new Date(transaction.expiresDate),
		graceEndsAt,
		autoRenew:
			autoRenewStatus === null || autoRenewStatus === undefined ? null : autoRenewStatus === APP_STORE_AUTO_RENEW_ON,
		autoRenewProductId: renewalInfo?.autoRenewProductId ?? null,
		startedAt: new Date(renewalInfo?.recentSubscriptionStartDate ?? transaction.purchaseDate),
		purchasedAt: new Date(transaction.purchaseDate),
		revokedAt: toDate(transaction.revocationDate),
		revocationReason: revocationReason(transaction),
		linkedStoreKey: null,
		expiredStoreKey: null,
		expiredAccountToken: null,
		acknowledged: true,
		quantity: 1,
		region: transaction.storefront ?? null,
		lastEventAt: new Date(lastEventMs),
	};
}

function buildAppStoreGiftSnapshot(transaction: AppStoreTransactionPayload): StorePurchaseSnapshot {
	if (transaction.type === APP_STORE_AUTO_RENEWABLE_TYPE) {
		throw new AppStorePurchaseSyncError('mismatch');
	}
	const slot = resolveSlot(transaction.productId);
	if (STORE_PRODUCT_SLOTS[slot].kind !== 'gift') {
		throw new AppStorePurchaseSyncError('mismatch');
	}
	const environment = toStoreEnvironment(transaction.environment);
	const revoked = Boolean(transaction.revocationDate);
	return {
		storeKey: buildAppStoreStoreKey(environment, transaction.originalTransactionId),
		provider: 'app_store',
		kind: 'gift',
		slot,
		environment,
		appId: transaction.bundleId,
		productId: transaction.productId,
		basePlanId: null,
		storeReference: transaction.originalTransactionId,
		latestTransactionId: transaction.transactionId,
		appTransactionId: transaction.appTransactionId ?? null,
		accountToken: normalizeAccountToken(transaction.appAccountToken),
		ownershipType: transaction.inAppOwnershipType ?? null,
		state: revoked ? 'refunded' : 'purchased',
		storeState: transaction.revocationType ?? null,
		providerEntitled: !revoked,
		expiresAt: null,
		graceEndsAt: null,
		autoRenew: null,
		autoRenewProductId: null,
		startedAt: new Date(transaction.purchaseDate),
		purchasedAt: new Date(transaction.purchaseDate),
		revokedAt: toDate(transaction.revocationDate),
		revocationReason: revocationReason(transaction),
		linkedStoreKey: null,
		expiredStoreKey: null,
		expiredAccountToken: null,
		acknowledged: true,
		quantity: transaction.quantity ?? 1,
		region: transaction.storefront ?? null,
		lastEventAt: new Date(transaction.signedDate),
	};
}

function pickLastTransaction(
	items: ReadonlyArray<AppStoreLastTransaction>,
	originalTransactionId: string,
): AppStoreLastTransaction | null {
	let best: AppStoreLastTransaction | null = null;
	for (const item of items) {
		if (item.originalTransactionId !== originalTransactionId) {
			continue;
		}
		if (!best || (item.transaction.expiresDate ?? 0) > (best.transaction.expiresDate ?? 0)) {
			best = item;
		}
	}
	return best;
}

export class AppStorePurchaseSync {
	constructor(private readonly client: AppStoreServerApiClient) {}

	async loadPurchase(lookup: AppStorePurchaseLookup): Promise<StorePurchaseSnapshot> {
		const slot = resolveSlot(lookup.productId);
		if (STORE_PRODUCT_SLOTS[slot].kind === 'subscription') {
			return await this.loadSubscription(lookup);
		}
		return await this.loadGift(lookup);
	}

	private async loadSubscription(lookup: AppStorePurchaseLookup): Promise<StorePurchaseSnapshot> {
		const statuses = await this.client.getAllSubscriptionStatuses({
			environment: lookup.environment,
			bundleId: lookup.bundleId,
			transactionId: lookup.originalTransactionId,
		});
		const item = pickLastTransaction(
			statuses.groups.flatMap((group) => group.lastTransactions),
			lookup.originalTransactionId,
		);
		if (!item) {
			throw new AppStorePurchaseSyncError('not_found');
		}
		if (item.transaction.bundleId !== lookup.bundleId) {
			throw new AppStorePurchaseSyncError('mismatch');
		}
		return buildAppStoreSubscriptionSnapshot(item);
	}

	private async loadGift(lookup: AppStorePurchaseLookup): Promise<StorePurchaseSnapshot> {
		const {transaction} = await this.client.getTransactionInfo({
			environment: lookup.environment,
			bundleId: lookup.bundleId,
			transactionId: lookup.transactionId,
		});
		if (transaction.originalTransactionId !== lookup.originalTransactionId) {
			throw new AppStorePurchaseSyncError('mismatch');
		}
		return buildAppStoreGiftSnapshot(transaction);
	}
}

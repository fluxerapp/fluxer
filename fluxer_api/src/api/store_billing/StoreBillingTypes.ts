// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import type {StoreProductSlotName} from '@fluxer/config/src/MasterConfig';

export type StoreProvider = 'app_store' | 'google_play';

export type StorePurchaseKind = 'subscription' | 'gift';

export type StoreSlot = StoreProductSlotName;

export type StoreEnvironment = 'production' | 'sandbox';

export type StorePurchaseState =
	| 'pending'
	| 'active'
	| 'grace'
	| 'billing_retry'
	| 'on_hold'
	| 'paused'
	| 'canceled'
	| 'expired'
	| 'revoked'
	| 'superseded'
	| 'purchased'
	| 'fulfilled'
	| 'refunded';

const TERMINAL_STORE_PURCHASE_STATES: ReadonlySet<StorePurchaseState> = new Set([
	'expired',
	'revoked',
	'superseded',
	'fulfilled',
	'refunded',
]);

export const PAID_SUBSCRIPTION_STATES: ReadonlySet<StorePurchaseState> = new Set(['active', 'canceled', 'grace']);

export const LIFETIME_REFUND_REASON = 'lifetime_refund';

export const UNSUPPORTED_QUANTITY_REASON = 'unsupported_quantity';

export const GOOGLE_PLAY_VOIDED_REASON = 'google_play_voided';

export function isTerminalStorePurchaseState(state: StorePurchaseState): boolean {
	return TERMINAL_STORE_PURCHASE_STATES.has(state);
}

export function isVoidedGooglePlaySubscriptionStillRenewing(row: StorePurchaseRow): boolean {
	return (
		row.provider === 'google_play' &&
		row.kind === 'subscription' &&
		row.state === 'revoked' &&
		row.revocation_reason === GOOGLE_PLAY_VOIDED_REASON &&
		row.auto_renew === true
	);
}

export function buildAppStoreStoreKey(environment: StoreEnvironment, originalTransactionId: string): string {
	return `app_store:${environment}:${originalTransactionId}`;
}

export function buildGooglePlayStoreKey(purchaseToken: string): string {
	return `google_play:${purchaseToken}`;
}

export function redactStoreKey(storeKey: string): string {
	if (!storeKey.startsWith('google_play:')) {
		return storeKey;
	}
	return `google_play:${createHash('sha256').update(storeKey).digest('hex').slice(0, 16)}`;
}

export interface StorePurchaseSnapshot {
	storeKey: string;
	provider: StoreProvider;
	kind: StorePurchaseKind;
	slot: StoreSlot;
	environment: StoreEnvironment;
	appId: string;
	productId: string;
	basePlanId: string | null;
	storeReference: string;
	latestTransactionId: string | null;
	appTransactionId: string | null;
	accountToken: string | null;
	ownershipType: string | null;
	state: StorePurchaseState;
	storeState: string | null;
	providerEntitled: boolean;
	expiresAt: Date | null;
	graceEndsAt: Date | null;
	autoRenew: boolean | null;
	autoRenewProductId: string | null;
	startedAt: Date | null;
	purchasedAt: Date | null;
	revokedAt: Date | null;
	revocationReason: string | null;
	linkedStoreKey: string | null;
	expiredStoreKey: string | null;
	expiredAccountToken: string | null;
	acknowledged: boolean;
	quantity: number;
	region: string | null;
	lastEventAt: Date;
}

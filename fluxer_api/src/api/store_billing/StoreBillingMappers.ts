// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import {STORE_PRODUCT_SLOTS} from '@app/api/store_billing/StoreBillingConfig';
import {PREMIUM_GRACE_PERIOD_MS} from '@app/api/user/UserHelpers';
import type {AdminStorePurchaseResponse} from '@fluxer/schema/src/domains/admin/AdminStoreBillingSchemas';
import type {
	PremiumStoreSubscriptionState,
	StorePurchaseResponse,
} from '@fluxer/schema/src/domains/premium/StoreBillingSchemas';

const APP_STORE_MANAGE_URL = 'https://apps.apple.com/account/subscriptions';
const GOOGLE_PLAY_MANAGE_URL = 'https://play.google.com/store/account/subscriptions';

function toIso(value: Date | null | undefined): string | null {
	return value ? value.toISOString() : null;
}

export function resolveStoreAccessEnd(row: StorePurchaseRow): Date | null {
	if (row.state === 'grace' && row.grace_ends_at) {
		return row.grace_ends_at;
	}
	return row.expires_at ?? null;
}

function isActiveStoreSubscription(row: StorePurchaseRow, now: Date): boolean {
	if (row.kind !== 'subscription' || !row.entitled || row.user_id === null) {
		return false;
	}
	const accessEnd = resolveStoreAccessEnd(row);
	if (!accessEnd) {
		return false;
	}
	if (row.state === 'grace') {
		return accessEnd.getTime() >= now.getTime();
	}
	return accessEnd.getTime() + PREMIUM_GRACE_PERIOD_MS >= now.getTime();
}

export function selectActiveStoreSubscription(
	rows: ReadonlyArray<StorePurchaseRow>,
	now: Date,
): StorePurchaseRow | null {
	let best: StorePurchaseRow | null = null;
	for (const row of rows) {
		if (!isActiveStoreSubscription(row, now)) {
			continue;
		}
		if (!best || (resolveStoreAccessEnd(row)?.getTime() ?? 0) > (resolveStoreAccessEnd(best)?.getTime() ?? 0)) {
			best = row;
		}
	}
	return best;
}

export function mapStorePurchaseToResponse(row: StorePurchaseRow): StorePurchaseResponse {
	const isSubscription = row.kind === 'subscription';
	return {
		id: row.id.toString(),
		provider: row.provider,
		kind: row.kind,
		slot: row.slot,
		product_id: row.product_id,
		environment: row.environment,
		state: row.state,
		entitled: row.entitled,
		expires_at: isSubscription ? toIso(row.expires_at) : null,
		entitled_until: isSubscription && row.entitled ? toIso(resolveStoreAccessEnd(row)) : null,
		will_renew: isSubscription ? (row.auto_renew ?? null) : null,
		gift_code: row.gift_code ?? null,
		created_at: row.created_at.toISOString(),
	};
}

export function mapStorePurchaseToAdminResponse(row: StorePurchaseRow): AdminStorePurchaseResponse {
	return {
		id: row.id.toString(),
		user_id: row.user_id?.toString() ?? null,
		provider: row.provider,
		kind: row.kind,
		slot: row.slot,
		environment: row.environment,
		app_id: row.app_id,
		product_id: row.product_id,
		base_plan_id: row.base_plan_id ?? null,
		latest_transaction_id: row.latest_transaction_id ?? null,
		ownership_type: row.ownership_type ?? null,
		state: row.state,
		store_state: row.store_state ?? null,
		entitled: row.entitled,
		expires_at: toIso(row.expires_at),
		grace_ends_at: toIso(row.grace_ends_at),
		auto_renew: row.auto_renew ?? null,
		started_at: toIso(row.started_at),
		purchased_at: toIso(row.purchased_at),
		revoked_at: toIso(row.revoked_at),
		revocation_reason: row.revocation_reason ?? null,
		superseded: row.superseded_by_store_key != null,
		acknowledged: row.acknowledged,
		gift_code: row.gift_code ?? null,
		bound_at: toIso(row.bound_at),
		last_event_at: toIso(row.last_event_at),
		synced_at: toIso(row.synced_at),
		created_at: row.created_at.toISOString(),
		updated_at: row.updated_at.toISOString(),
	};
}

function buildManageUrl(row: StorePurchaseRow): string {
	if (row.provider === 'app_store') {
		return APP_STORE_MANAGE_URL;
	}
	const params = new URLSearchParams({sku: row.product_id, package: row.app_id});
	return `${GOOGLE_PLAY_MANAGE_URL}?${params.toString()}`;
}

export function mapPremiumStoreSubscriptionState(row: StorePurchaseRow): PremiumStoreSubscriptionState | null {
	const billingCycle = STORE_PRODUCT_SLOTS[row.slot].billingCycle;
	if (!billingCycle || !row.expires_at || (row.slot !== 'monthly' && row.slot !== 'yearly')) {
		return null;
	}
	return {
		provider: row.provider,
		purchase_id: row.id.toString(),
		slot: row.slot,
		billing_cycle: billingCycle,
		state: row.state,
		expires_at: row.expires_at.toISOString(),
		grace_ends_at: row.state === 'grace' ? toIso(row.grace_ends_at) : null,
		will_renew: row.auto_renew === true,
		manage_url: buildManageUrl(row),
		environment: row.environment,
	};
}

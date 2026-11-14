// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import {
	isTerminalStorePurchaseState,
	isVoidedGooglePlaySubscriptionStillRenewing,
	LIFETIME_REFUND_REASON,
	PAID_SUBSCRIPTION_STATES,
	type StorePurchaseState,
	UNSUPPORTED_QUANTITY_REASON,
} from '@app/api/store_billing/StoreBillingTypes';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {ms} from 'itty-time';

export const STORE_PURCHASE_REFRESH_QUEUE_KEY = 'store:purchase:refresh';

export const REFRESH_WATCH_DELAY_MS = ms('6 hours');

const REFRESH_AFTER_EXPIRY_MS = ms('10 minutes');
const REFRESH_MAX_DELAY_MS = ms('7 days');
const REFRESH_MIN_DELAY_MS = ms('5 minutes');
const REFRESH_UNSETTLED_DELAY_MS = ms('1 hour');
const REFRESH_UNSETTLED_SANDBOX_DELAY_MS = ms('1 minute');
const REFRESH_LAPSED_DELAY_MS = ms('1 hour');
const WATCHED_STATES: ReadonlySet<StorePurchaseState> = new Set([
	'grace',
	'billing_retry',
	'on_hold',
	'paused',
	'pending',
]);

function needsGooglePlaySettle(row: StorePurchaseRow): boolean {
	if (row.provider !== 'google_play') {
		return false;
	}
	if (row.kind === 'gift') {
		if (row.revocation_reason === UNSUPPORTED_QUANTITY_REASON) {
			return !row.revoked_at && row.state !== 'refunded';
		}
		return (
			!row.acknowledged &&
			row.user_id !== null &&
			row.entitled &&
			(row.state === 'purchased' || row.state === 'fulfilled')
		);
	}
	return (
		!row.acknowledged &&
		row.user_id !== null &&
		row.revocation_reason !== LIFETIME_REFUND_REASON &&
		PAID_SUBSCRIPTION_STATES.has(row.state)
	);
}

export async function scheduleStorePurchaseRefresh(kv: IKVProvider, row: StorePurchaseRow): Promise<void> {
	if (Config.instance.selfHosted) {
		return;
	}
	const needsSettle = needsGooglePlaySettle(row);
	const keepWatching =
		needsSettle ||
		(row.kind === 'subscription'
			? !isTerminalStorePurchaseState(row.state) || isVoidedGooglePlaySubscriptionStillRenewing(row)
			: row.state === 'pending');
	if (!keepWatching) {
		await kv.zrem(STORE_PURCHASE_REFRESH_QUEUE_KEY, row.store_key);
		return;
	}
	const now = Date.now();
	let dueAt = now + REFRESH_WATCH_DELAY_MS;
	if (needsSettle) {
		dueAt = now + (row.environment === 'sandbox' ? REFRESH_UNSETTLED_SANDBOX_DELAY_MS : REFRESH_UNSETTLED_DELAY_MS);
	} else if (!WATCHED_STATES.has(row.state) && row.expires_at) {
		if (row.expires_at.getTime() <= now) {
			dueAt = now + REFRESH_LAPSED_DELAY_MS;
		} else {
			dueAt = Math.min(row.expires_at.getTime() + REFRESH_AFTER_EXPIRY_MS, now + REFRESH_MAX_DELAY_MS);
			dueAt = Math.max(dueAt, now + REFRESH_MIN_DELAY_MS);
		}
	}
	await kv.zadd(STORE_PURCHASE_REFRESH_QUEUE_KEY, dueAt, row.store_key);
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import type {GiftCode} from '@app/api/models/GiftCode';
import {
	isGooglePlayConfigured,
	isGooglePlayPackageConfigured,
	STORE_PRODUCT_SLOTS,
} from '@app/api/store_billing/StoreBillingConfig';
import {PAID_SUBSCRIPTION_STATES} from '@app/api/store_billing/StoreBillingTypes';

export interface GooglePlayRefundTarget {
	row: StorePurchaseRow;
	orderId: string;
	paidAt: Date;
}

type FindGiftCode = (code: string) => Promise<GiftCode | null>;

export function isGooglePlaySelfServiceRefundEnabled(): boolean {
	return Config.googlePlay.selfServiceRefunds && isGooglePlayConfigured();
}

function lastChargedAt(row: StorePurchaseRow, now: Date): Date | null {
	const startedAt = row.started_at ?? row.purchased_at;
	if (row.kind === 'gift' || !row.expires_at) {
		return startedAt;
	}
	const periodStart = new Date(row.expires_at);
	periodStart.setUTCMonth(periodStart.getUTCMonth() - STORE_PRODUCT_SLOTS[row.slot].durationMonths);
	const chargedAt = Math.max(periodStart.getTime(), startedAt?.getTime() ?? Number.NEGATIVE_INFINITY);
	return new Date(Math.min(chargedAt, now.getTime()));
}

async function isRefundable(row: StorePurchaseRow, findGiftCode: FindGiftCode): Promise<boolean> {
	if (
		row.provider !== 'google_play' ||
		row.revoked_at !== null ||
		!row.latest_transaction_id ||
		!isGooglePlayPackageConfigured(row.app_id)
	) {
		return false;
	}
	if (row.kind === 'subscription') {
		return row.entitled && PAID_SUBSCRIPTION_STATES.has(row.state);
	}
	if (row.state !== 'purchased' && row.state !== 'fulfilled') {
		return false;
	}
	if (!row.gift_code) {
		return true;
	}
	const gift = await findGiftCode(row.gift_code);
	return gift === null || (gift.redeemedByUserId === null && gift.revokedAt === null);
}

export async function findGooglePlayRefundTarget(
	rows: ReadonlyArray<StorePurchaseRow>,
	findGiftCode: FindGiftCode,
	now = new Date(),
): Promise<GooglePlayRefundTarget | null> {
	if (!isGooglePlaySelfServiceRefundEnabled()) {
		return null;
	}
	let latest: GooglePlayRefundTarget | null = null;
	for (const row of rows) {
		const paidAt = lastChargedAt(row, now);
		if (!paidAt || (latest && latest.paidAt.getTime() >= paidAt.getTime())) {
			continue;
		}
		if (await isRefundable(row, findGiftCode)) {
			latest = {row, orderId: row.latest_transaction_id!, paidAt};
		}
	}
	return latest;
}

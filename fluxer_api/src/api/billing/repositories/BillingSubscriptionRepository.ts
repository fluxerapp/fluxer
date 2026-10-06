// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type MapStripeSubscriptionHints,
	mapStripeSubscriptionToRow,
	normalizeBillingSubscriptionRow,
	type StripeSubscriptionPayload,
} from '@app/api/billing/mappers/StripeToBillingMapper';
import {
	buildPatchFromRow,
	executeBillingVersionedUpdate,
	isExistingNewer,
	rowsEquivalent,
} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {BillingSubscriptionRow} from '@app/api/database/types/BillingTypes';
import {BILLING_SUBSCRIPTION_COLUMNS} from '@app/api/database/types/BillingTypes';
import {BillingSubscriptions, BillingSubscriptionsByCustomer, BillingSubscriptionsByUser} from '@app/api/Tables';

const FETCH_BY_ID = BillingSubscriptions.selectCql({
	where: BillingSubscriptions.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_CUSTOMER = BillingSubscriptionsByCustomer.selectCql({
	where: BillingSubscriptionsByCustomer.where.eq('customer_id'),
});
const FETCH_BY_USER = BillingSubscriptionsByUser.selectCql({
	where: BillingSubscriptionsByUser.where.eq('user_id'),
});
const FETCH_BY_PROVIDER_IDS = BillingSubscriptions.selectCql({
	where: BillingSubscriptions.where.in('provider_id', 'provider_ids'),
});
const ACTIVE_SUBSCRIPTION_STATUSES = new Set<BillingSubscriptionRow['status']>(['active', 'trialing']);

function getSubscriptionEndMs(row: BillingSubscriptionRow): number {
	return row.current_period_end?.getTime() ?? 0;
}

export class BillingSubscriptionRepository {
	async findById(providerId: string): Promise<BillingSubscriptionRow | null> {
		const row = await fetchOne<BillingSubscriptionRow>(FETCH_BY_ID, {provider_id: providerId});
		return row ? normalizeBillingSubscriptionRow(row) : null;
	}

	async listByCustomer(customerId: string): Promise<Array<BillingSubscriptionRow>> {
		const refs = await fetchMany<{
			provider_id: string;
		}>(FETCH_BY_CUSTOMER, {customer_id: customerId});
		if (refs.length === 0) return [];
		const ids = refs.map((r) => r.provider_id);
		const rows = await fetchMany<BillingSubscriptionRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		return rows.map(normalizeBillingSubscriptionRow);
	}

	async listByUser(userId: bigint): Promise<Array<BillingSubscriptionRow>> {
		const refs = await fetchMany<{
			provider_id: string;
		}>(FETCH_BY_USER, {user_id: userId});
		if (refs.length === 0) return [];
		const ids = refs.map((r) => r.provider_id);
		const rows = await fetchMany<BillingSubscriptionRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		return rows.map(normalizeBillingSubscriptionRow);
	}

	async findActiveForUser(userId: bigint): Promise<BillingSubscriptionRow | null> {
		const subs = await this.listByUser(userId);
		return (
			subs
				.filter((subscription) => ACTIVE_SUBSCRIPTION_STATUSES.has(subscription.status))
				.sort((left, right) => getSubscriptionEndMs(right) - getSubscriptionEndMs(left))[0] ?? null
		);
	}

	async upsertFromStripe(
		s: StripeSubscriptionPayload,
		hints?: MapStripeSubscriptionHints,
	): Promise<{
		changed: boolean;
		row: BillingSubscriptionRow;
	}> {
		const mapped = mapStripeSubscriptionToRow(s, hints);
		const existing = await this.findById(mapped.primary.provider_id);
		if (isExistingNewer(existing, mapped.primary)) {
			return {changed: false, row: existing!};
		}
		if (existing && rowsEquivalent(existing, mapped.primary, ['mirrored_at', 'version'])) {
			return {changed: false, row: existing};
		}
		const result = await executeBillingVersionedUpdate<BillingSubscriptionRow, 'provider_id'>(
			async () => existing,
			(current) => ({
				pk: {provider_id: mapped.primary.provider_id},
				patch: buildPatchFromRow(mapped.primary, current, BILLING_SUBSCRIPTION_COLUMNS, ['provider_id']),
			}),
			BillingSubscriptions,
			{initialData: existing},
		);
		await upsertOne(BillingSubscriptionsByCustomer.upsertAll(mapped.byCustomer));
		if (mapped.byUser) {
			await upsertOne(BillingSubscriptionsByUser.upsertAll(mapped.byUser));
		}
		return {changed: true, row: {...mapped.primary, version: result.finalVersion}};
	}
}

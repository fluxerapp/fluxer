// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type MapStripeSubscriptionHints,
	mapStripeSubscriptionToRow,
	normalizeBillingSubscriptionRow,
	type StripeSubscriptionPayload,
} from '@app/api/billing/mappers/StripeToBillingMapper';
import {hydrateByRefs, upsertVersionedMirrorRow} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
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

export class BillingSubscriptionRepository {
	async findById(providerId: string): Promise<BillingSubscriptionRow | null> {
		const row = await fetchOne<BillingSubscriptionRow>(FETCH_BY_ID, {provider_id: providerId});
		return row ? normalizeBillingSubscriptionRow(row) : null;
	}

	async listByCustomer(customerId: string): Promise<Array<BillingSubscriptionRow>> {
		const rows = await hydrateByRefs<BillingSubscriptionRow>(
			FETCH_BY_CUSTOMER,
			{customer_id: customerId},
			FETCH_BY_PROVIDER_IDS,
		);
		return rows.map(normalizeBillingSubscriptionRow);
	}

	async listByUser(userId: bigint): Promise<Array<BillingSubscriptionRow>> {
		const rows = await hydrateByRefs<BillingSubscriptionRow>(FETCH_BY_USER, {user_id: userId}, FETCH_BY_PROVIDER_IDS);
		return rows.map(normalizeBillingSubscriptionRow);
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
		const result = await upsertVersionedMirrorRow(
			BillingSubscriptions,
			BILLING_SUBSCRIPTION_COLUMNS,
			existing,
			mapped.primary,
		);
		if (result.changed) {
			await upsertOne(BillingSubscriptionsByCustomer.upsertAll(mapped.byCustomer));
			if (mapped.byUser) await upsertOne(BillingSubscriptionsByUser.upsertAll(mapped.byUser));
		}
		return result;
	}
}

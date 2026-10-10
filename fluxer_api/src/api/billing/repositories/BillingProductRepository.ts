// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripeProductToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {
	buildPatchFromRow,
	executeBillingVersionedUpdate,
	isExistingNewer,
	rowsEquivalent,
} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {BillingProductRow} from '@app/api/database/types/BillingTypes';
import {BILLING_PRODUCT_COLUMNS} from '@app/api/database/types/BillingTypes';
import {BillingProducts} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingProducts.selectCql({
	where: BillingProducts.where.eq('provider_id'),
	limit: 1,
});

export class BillingProductRepository {
	async findById(providerId: string): Promise<BillingProductRow | null> {
		return fetchOne<BillingProductRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async upsertFromStripe(p: Stripe.Product): Promise<{
		changed: boolean;
		row: BillingProductRow;
	}> {
		const mapped = mapStripeProductToRow(p);
		const existing = await this.findById(mapped.provider_id);
		if (isExistingNewer(existing, mapped)) {
			return {changed: false, row: existing!};
		}
		if (existing && rowsEquivalent(existing, mapped, ['mirrored_at', 'version'])) {
			return {changed: false, row: existing};
		}
		const result = await executeBillingVersionedUpdate<BillingProductRow, 'provider_id'>(
			async () => existing,
			(current) => ({
				pk: {provider_id: mapped.provider_id},
				patch: buildPatchFromRow(mapped, current, BILLING_PRODUCT_COLUMNS, ['provider_id']),
			}),
			BillingProducts,
			{initialData: existing},
		);
		return {changed: true, row: {...mapped, version: result.finalVersion}};
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripePriceToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {
	buildPatchFromRow,
	executeBillingVersionedUpdate,
	isExistingNewer,
	rowsEquivalent,
} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {BillingPriceRow} from '@app/api/database/types/BillingTypes';
import {BILLING_PRICE_COLUMNS} from '@app/api/database/types/BillingTypes';
import {BillingPrices} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingPrices.selectCql({
	where: BillingPrices.where.eq('provider_id'),
	limit: 1,
});

export class BillingPriceRepository {
	async findById(providerId: string): Promise<BillingPriceRow | null> {
		return fetchOne<BillingPriceRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async upsertFromStripe(p: Stripe.Price): Promise<{
		changed: boolean;
		row: BillingPriceRow;
	}> {
		const mapped = mapStripePriceToRow(p);
		const existing = await this.findById(mapped.provider_id);
		if (isExistingNewer(existing, mapped)) {
			return {changed: false, row: existing!};
		}
		if (existing && rowsEquivalent(existing, mapped, ['mirrored_at', 'version'])) {
			return {changed: false, row: existing};
		}
		const result = await executeBillingVersionedUpdate<BillingPriceRow, 'provider_id'>(
			async () => existing,
			(current) => ({
				pk: {provider_id: mapped.provider_id},
				patch: buildPatchFromRow(mapped, current, BILLING_PRICE_COLUMNS, ['provider_id']),
			}),
			BillingPrices,
			{initialData: existing},
		);
		return {changed: true, row: {...mapped, version: result.finalVersion}};
	}
}

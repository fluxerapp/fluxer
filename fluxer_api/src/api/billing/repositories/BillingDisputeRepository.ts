// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripeDisputeToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {isExistingNewer} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {BillingDisputeRow} from '@app/api/database/types/BillingTypes';
import {BillingDisputes, BillingDisputesByCharge} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingDisputes.selectCql({
	where: BillingDisputes.where.eq('provider_id'),
	limit: 1,
});

export class BillingDisputeRepository {
	async findById(providerId: string): Promise<BillingDisputeRow | null> {
		return fetchOne<BillingDisputeRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async upsertFromStripe(
		d: Stripe.Dispute,
		hints?: {
			customerId?: string;
			userId?: bigint;
		},
	): Promise<{
		changed: boolean;
		row: BillingDisputeRow;
	}> {
		const mapped = mapStripeDisputeToRow(d, hints);
		const existing = await this.findById(mapped.primary.provider_id);
		if (isExistingNewer(existing, mapped.primary)) {
			return {changed: false, row: existing!};
		}
		await upsertOne(BillingDisputes.upsertAll(mapped.primary));
		if (mapped.byCharge) {
			await upsertOne(BillingDisputesByCharge.upsertAll(mapped.byCharge));
		}
		return {changed: true, row: mapped.primary};
	}
}

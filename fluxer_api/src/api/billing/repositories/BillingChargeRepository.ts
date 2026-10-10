// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripeChargeToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {isExistingNewer} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {BillingChargeRow} from '@app/api/database/types/BillingTypes';
import {BillingCharges, BillingChargesByCustomer} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingCharges.selectCql({
	where: BillingCharges.where.eq('provider_id'),
	limit: 1,
});

export class BillingChargeRepository {
	async findById(providerId: string): Promise<BillingChargeRow | null> {
		return fetchOne<BillingChargeRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async upsertFromStripe(c: Stripe.Charge): Promise<{
		changed: boolean;
		row: BillingChargeRow;
	}> {
		const mapped = mapStripeChargeToRow(c);
		const existing = await this.findById(mapped.primary.provider_id);
		if (isExistingNewer(existing, mapped.primary)) {
			return {changed: false, row: existing!};
		}
		await upsertOne(BillingCharges.upsertAll(mapped.primary));
		if (mapped.byCustomer) {
			await upsertOne(BillingChargesByCustomer.upsertAll(mapped.byCustomer));
		}
		return {changed: true, row: mapped.primary};
	}
}

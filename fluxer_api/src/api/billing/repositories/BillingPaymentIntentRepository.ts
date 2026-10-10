// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripePaymentIntentToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {isExistingNewer} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {BillingPaymentIntentRow} from '@app/api/database/types/BillingTypes';
import {BillingPaymentIntents, BillingPaymentIntentsByCustomer} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingPaymentIntents.selectCql({
	where: BillingPaymentIntents.where.eq('provider_id'),
	limit: 1,
});

export class BillingPaymentIntentRepository {
	async findById(providerId: string): Promise<BillingPaymentIntentRow | null> {
		return fetchOne<BillingPaymentIntentRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async upsertFromStripe(pi: Stripe.PaymentIntent): Promise<{
		changed: boolean;
		row: BillingPaymentIntentRow;
	}> {
		const mapped = mapStripePaymentIntentToRow(pi);
		const existing = await this.findById(mapped.primary.provider_id);
		if (isExistingNewer(existing, mapped.primary)) {
			return {changed: false, row: existing!};
		}
		await upsertOne(BillingPaymentIntents.upsertAll(mapped.primary));
		if (mapped.byCustomer) {
			await upsertOne(BillingPaymentIntentsByCustomer.upsertAll(mapped.byCustomer));
		}
		return {changed: true, row: mapped.primary};
	}
}

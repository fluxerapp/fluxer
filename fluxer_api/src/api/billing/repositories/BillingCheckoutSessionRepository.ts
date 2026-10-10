// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripeCheckoutSessionToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {isExistingNewer} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {BillingCheckoutSessionRow} from '@app/api/database/types/BillingTypes';
import {BillingCheckoutSessions, BillingCheckoutSessionsByCustomer} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingCheckoutSessions.selectCql({
	where: BillingCheckoutSessions.where.eq('provider_id'),
	limit: 1,
});

export class BillingCheckoutSessionRepository {
	async findById(providerId: string): Promise<BillingCheckoutSessionRow | null> {
		return fetchOne<BillingCheckoutSessionRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async upsertFromStripe(
		cs: Stripe.Checkout.Session,
		hints?: {
			knownUserId?: bigint;
			eventCreated?: number;
		},
	): Promise<{
		changed: boolean;
		row: BillingCheckoutSessionRow;
	}> {
		const mapped = mapStripeCheckoutSessionToRow(cs, hints);
		const existing = await this.findById(mapped.primary.provider_id);
		if (isExistingNewer(existing, mapped.primary)) {
			return {changed: false, row: existing!};
		}
		await upsertOne(BillingCheckoutSessions.upsertAll(mapped.primary));
		if (mapped.byCustomer) {
			await upsertOne(BillingCheckoutSessionsByCustomer.upsertAll(mapped.byCustomer));
		}
		return {changed: true, row: mapped.primary};
	}
}

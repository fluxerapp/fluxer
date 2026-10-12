// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripeCustomerToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {hydrateByRefs, upsertVersionedMirrorRow} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import {executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import type {BillingCustomerRow} from '@app/api/database/types/BillingTypes';
import {BILLING_CUSTOMER_COLUMNS} from '@app/api/database/types/BillingTypes';
import {BillingCustomers, BillingCustomersByUserId} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingCustomers.selectCql({
	where: BillingCustomers.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_USER_ID = BillingCustomersByUserId.selectCql({
	where: BillingCustomersByUserId.where.eq('user_id'),
});
const FETCH_BY_PROVIDER_IDS = BillingCustomers.selectCql({
	where: BillingCustomers.where.in('provider_id', 'provider_ids'),
});

export class BillingCustomerRepository {
	async findById(providerId: string): Promise<BillingCustomerRow | null> {
		return fetchOne<BillingCustomerRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async findByUserId(userId: bigint): Promise<Array<BillingCustomerRow>> {
		return hydrateByRefs<BillingCustomerRow>(FETCH_BY_USER_ID, {user_id: userId}, FETCH_BY_PROVIDER_IDS);
	}

	async upsertFromStripe(
		c: Stripe.Customer | Stripe.DeletedCustomer,
		hints?: {
			knownUserId?: bigint;
		},
	): Promise<{
		changed: boolean;
		row: BillingCustomerRow;
	}> {
		const mapped = mapStripeCustomerToRow(c, hints);
		const existing = await this.findById(mapped.primary.provider_id);
		const result = await upsertVersionedMirrorRow(BillingCustomers, BILLING_CUSTOMER_COLUMNS, existing, mapped.primary);
		if (result.changed) {
			if (mapped.byUserId) await upsertOne(BillingCustomersByUserId.upsertAll(mapped.byUserId));
		}
		return result;
	}

	async markDeleted(providerId: string, deletedAt: Date): Promise<void> {
		await executeVersionedUpdate<BillingCustomerRow, 'provider_id'>(
			async () => this.findById(providerId),
			() => ({
				pk: {provider_id: providerId},
				patch: {
					deleted: Db.set(true),
					stripe_updated_at: Db.set(deletedAt),
					mirrored_at: Db.set(new Date()),
				},
			}),
			BillingCustomers,
			{bumpVersion: (current) => (current ?? 0n) + 1n},
		);
	}
}

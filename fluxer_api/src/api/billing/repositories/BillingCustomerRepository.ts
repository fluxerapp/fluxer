// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripeCustomerToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {
	buildPatchFromRow,
	executeBillingVersionedUpdate,
	isExistingNewer,
	rowsEquivalent,
} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
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
		const refs = await fetchMany<{
			provider_id: string;
		}>(FETCH_BY_USER_ID, {user_id: userId});
		if (refs.length === 0) return [];
		const ids = refs.map((r) => r.provider_id);
		return fetchMany<BillingCustomerRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
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
		if (isExistingNewer(existing, mapped.primary)) {
			return {changed: false, row: existing!};
		}
		if (existing && rowsEquivalent(existing, mapped.primary, ['mirrored_at', 'version'])) {
			return {changed: false, row: existing};
		}
		const result = await executeBillingVersionedUpdate<BillingCustomerRow, 'provider_id'>(
			async () => existing,
			(current) => ({
				pk: {provider_id: mapped.primary.provider_id},
				patch: buildPatchFromRow(mapped.primary, current, BILLING_CUSTOMER_COLUMNS, ['provider_id']),
			}),
			BillingCustomers,
			{initialData: existing},
		);
		if (mapped.byUserId) {
			await upsertOne(BillingCustomersByUserId.upsertAll(mapped.byUserId));
		}
		return {
			changed: true,
			row: {...mapped.primary, version: result.finalVersion},
		};
	}

	async markDeleted(providerId: string, deletedAt: Date): Promise<void> {
		await executeBillingVersionedUpdate<BillingCustomerRow, 'provider_id'>(
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
		);
	}
}

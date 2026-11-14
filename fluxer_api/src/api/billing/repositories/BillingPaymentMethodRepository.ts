// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripePaymentMethodToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {hydrateByRefs, upsertVersionedMirrorRow} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
import {executeVersionedUpdate} from '@app/api/database/CassandraVersionedUpdate';
import type {BillingPaymentMethodRow} from '@app/api/database/types/BillingTypes';
import {BILLING_PAYMENT_METHOD_COLUMNS} from '@app/api/database/types/BillingTypes';
import {BillingPaymentMethods, BillingPaymentMethodsByCustomer} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingPaymentMethods.selectCql({
	where: BillingPaymentMethods.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_CUSTOMER = BillingPaymentMethodsByCustomer.selectCql({
	where: BillingPaymentMethodsByCustomer.where.eq('customer_id'),
});
const FETCH_BY_PROVIDER_IDS = BillingPaymentMethods.selectCql({
	where: BillingPaymentMethods.where.in('provider_id', 'provider_ids'),
});

export class BillingPaymentMethodRepository {
	async findById(providerId: string): Promise<BillingPaymentMethodRow | null> {
		return fetchOne<BillingPaymentMethodRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async listByCustomer(customerId: string): Promise<Array<BillingPaymentMethodRow>> {
		const rows = await hydrateByRefs<BillingPaymentMethodRow>(
			FETCH_BY_CUSTOMER,
			{customer_id: customerId},
			FETCH_BY_PROVIDER_IDS,
		);
		return rows.filter((row) => row.customer_id === customerId);
	}

	async upsertFromStripe(
		pm: Stripe.PaymentMethod,
		hints?: {
			isDefault?: boolean;
		},
	): Promise<{
		changed: boolean;
		row: BillingPaymentMethodRow;
	}> {
		const mapped = mapStripePaymentMethodToRow(pm, hints);
		const existing = await this.findById(mapped.primary.provider_id);
		const result = await upsertVersionedMirrorRow(
			BillingPaymentMethods,
			BILLING_PAYMENT_METHOD_COLUMNS,
			existing,
			mapped.primary,
		);
		if (result.changed) {
			if (mapped.byCustomer) await upsertOne(BillingPaymentMethodsByCustomer.upsertAll(mapped.byCustomer));
		}
		return result;
	}

	async markDetached(providerId: string, detachedAt: Date): Promise<void> {
		await executeVersionedUpdate<BillingPaymentMethodRow, 'provider_id'>(
			async () => this.findById(providerId),
			() => ({
				pk: {provider_id: providerId},
				patch: {
					customer_id: Db.clear(),
					stripe_updated_at: Db.set(detachedAt),
					mirrored_at: Db.set(new Date()),
				},
			}),
			BillingPaymentMethods,
			{bumpVersion: (current) => (current ?? 0n) + 1n},
		);
	}
}

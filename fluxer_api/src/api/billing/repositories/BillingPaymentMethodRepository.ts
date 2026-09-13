// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripePaymentMethodToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {
	buildPatchFromRow,
	executeBillingVersionedUpdate,
	isExistingNewer,
	rowsEquivalent,
} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db} from '@app/api/database/CassandraTypes';
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
		const refs = await fetchMany<{
			provider_id: string;
		}>(FETCH_BY_CUSTOMER, {customer_id: customerId});
		if (refs.length === 0) return [];
		const ids = refs.map((r) => r.provider_id);
		const rows = await fetchMany<BillingPaymentMethodRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		return rows.filter((row) => row.customer_id === customerId);
	}

	async findDefaultForCustomer(customerId: string): Promise<BillingPaymentMethodRow | null> {
		const list = await this.listByCustomer(customerId);
		return list.find((r) => r.is_default === true) ?? null;
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
		if (isExistingNewer(existing, mapped.primary)) {
			return {changed: false, row: existing!};
		}
		if (existing && rowsEquivalent(existing, mapped.primary, ['mirrored_at', 'version'])) {
			return {changed: false, row: existing};
		}
		const result = await executeBillingVersionedUpdate<BillingPaymentMethodRow, 'provider_id'>(
			async () => existing,
			(current) => ({
				pk: {provider_id: mapped.primary.provider_id},
				patch: buildPatchFromRow(mapped.primary, current, BILLING_PAYMENT_METHOD_COLUMNS, ['provider_id']),
			}),
			BillingPaymentMethods,
			{initialData: existing},
		);
		if (mapped.byCustomer) {
			await upsertOne(BillingPaymentMethodsByCustomer.upsertAll(mapped.byCustomer));
		}
		return {changed: true, row: {...mapped.primary, version: result.finalVersion}};
	}

	async markDetached(providerId: string, detachedAt: Date): Promise<void> {
		await executeBillingVersionedUpdate<BillingPaymentMethodRow, 'provider_id'>(
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
		);
	}
}

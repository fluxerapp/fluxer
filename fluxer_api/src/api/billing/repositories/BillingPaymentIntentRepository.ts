// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripePaymentIntentToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {isExistingNewer} from '@app/api/billing/repositories/BillingRepoHelpers';
import {
	fetchMany,
	fetchOne,
	fetchPage,
	type PagedQueryResult,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import type {BillingPaymentIntentRow} from '@app/api/database/types/BillingTypes';
import {BillingPaymentIntents, BillingPaymentIntentsByCustomer} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingPaymentIntents.selectCql({
	where: BillingPaymentIntents.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_CUSTOMER = BillingPaymentIntentsByCustomer.selectCql({
	where: BillingPaymentIntentsByCustomer.where.eq('customer_id'),
});
const FETCH_BY_PROVIDER_IDS = BillingPaymentIntents.selectCql({
	where: BillingPaymentIntents.where.in('provider_id', 'provider_ids'),
});

export class BillingPaymentIntentRepository {
	async findById(providerId: string): Promise<BillingPaymentIntentRow | null> {
		return fetchOne<BillingPaymentIntentRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async listByCustomer(
		customerId: string,
		page?: {
			pageSize: number;
			pageState?: string | null;
		},
	): Promise<PagedQueryResult<BillingPaymentIntentRow>> {
		const refsPage = await fetchPage<{
			provider_id: string;
		}>(
			FETCH_BY_CUSTOMER,
			{customer_id: customerId},
			{pageSize: page?.pageSize ?? 50, pageState: page?.pageState ?? null},
		);
		if (refsPage.rows.length === 0) {
			return {rows: [], pageState: refsPage.pageState};
		}
		const ids = refsPage.rows.map((r) => r.provider_id);
		const rows = await fetchMany<BillingPaymentIntentRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		return {rows, pageState: refsPage.pageState};
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

// SPDX-License-Identifier: AGPL-3.0-or-later

import type Stripe from 'stripe';
import {fetchMany, fetchOne, fetchPage, type PagedQueryResult, upsertOne} from '../../database/CassandraQueryExecution';
import type {BillingChargeRow} from '../../database/types/BillingTypes';
import {BillingCharges, BillingChargesByCustomer} from '../../Tables';
import {mapStripeChargeToRow} from '../mappers/StripeToBillingMapper';
import {BILLING_REVERSE_CHRONOLOGICAL_ORDER, isExistingNewer, restoreReferenceOrder} from './BillingRepoHelpers';

const FETCH_BY_ID = BillingCharges.selectCql({
	where: BillingCharges.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_CUSTOMER = BillingChargesByCustomer.selectCql({
	where: BillingChargesByCustomer.where.eq('customer_id'),
	orderBy: BILLING_REVERSE_CHRONOLOGICAL_ORDER,
});
const FETCH_BY_PROVIDER_IDS = BillingCharges.selectCql({
	where: BillingCharges.where.in('provider_id', 'provider_ids'),
});

export class BillingChargeRepository {
	async findById(providerId: string): Promise<BillingChargeRow | null> {
		return fetchOne<BillingChargeRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async listByCustomer(
		customerId: string,
		page?: {
			pageSize: number;
			pageState?: string | null;
		},
	): Promise<PagedQueryResult<BillingChargeRow>> {
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
		const rows = await fetchMany<BillingChargeRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		return {rows: restoreReferenceOrder(refsPage.rows, rows), pageState: refsPage.pageState};
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

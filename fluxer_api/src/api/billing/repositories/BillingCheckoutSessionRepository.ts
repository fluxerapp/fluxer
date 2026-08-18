// SPDX-License-Identifier: AGPL-3.0-or-later

import type Stripe from 'stripe';
import {fetchMany, fetchOne, fetchPage, type PagedQueryResult, upsertOne} from '../../database/CassandraQueryExecution';
import type {BillingCheckoutSessionRow} from '../../database/types/BillingTypes';
import {BillingCheckoutSessions, BillingCheckoutSessionsByCustomer} from '../../Tables';
import {mapStripeCheckoutSessionToRow} from '../mappers/StripeToBillingMapper';
import {isExistingNewer} from './BillingRepoHelpers';

const FETCH_BY_ID = BillingCheckoutSessions.selectCql({
	where: BillingCheckoutSessions.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_CUSTOMER = BillingCheckoutSessionsByCustomer.selectCql({
	where: BillingCheckoutSessionsByCustomer.where.eq('customer_id'),
});
const FETCH_BY_PROVIDER_IDS = BillingCheckoutSessions.selectCql({
	where: BillingCheckoutSessions.where.in('provider_id', 'provider_ids'),
});

export class BillingCheckoutSessionRepository {
	async findById(providerId: string): Promise<BillingCheckoutSessionRow | null> {
		return fetchOne<BillingCheckoutSessionRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async listByCustomer(
		customerId: string,
		page?: {
			pageSize: number;
			pageState?: string | null;
		},
	): Promise<PagedQueryResult<BillingCheckoutSessionRow>> {
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
		const rows = await fetchMany<BillingCheckoutSessionRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		return {rows, pageState: refsPage.pageState};
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

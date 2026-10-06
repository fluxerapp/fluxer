// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripeInvoiceToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import type {BillingPaymentRepository} from '@app/api/billing/repositories/BillingPaymentRepository';
import {
	buildPatchFromRow,
	executeBillingVersionedUpdate,
	isExistingNewer,
	rowsEquivalent,
} from '@app/api/billing/repositories/BillingRepoHelpers';
import {
	fetchMany,
	fetchOne,
	fetchPage,
	type PagedQueryResult,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import type {BillingInvoiceRow} from '@app/api/database/types/BillingTypes';
import {BILLING_INVOICE_COLUMNS} from '@app/api/database/types/BillingTypes';
import {
	BillingCustomersByUserId,
	BillingInvoices,
	BillingInvoicesByCustomer,
	BillingInvoicesBySubscription,
} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingInvoices.selectCql({
	where: BillingInvoices.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_CUSTOMER_PARTITION = BillingInvoicesByCustomer.selectCql({
	where: BillingInvoicesByCustomer.where.eq('customer_id'),
});
const FETCH_BY_SUBSCRIPTION_PARTITION = BillingInvoicesBySubscription.selectCql({
	where: BillingInvoicesBySubscription.where.eq('subscription_id'),
});
const FETCH_BY_PROVIDER_IDS = BillingInvoices.selectCql({
	where: BillingInvoices.where.in('provider_id', 'provider_ids'),
});
const FETCH_CUSTOMERS_BY_USER = BillingCustomersByUserId.selectCql({
	where: BillingCustomersByUserId.where.eq('user_id'),
});

export class BillingInvoiceRepository {
	constructor(private paymentsRepo: BillingPaymentRepository) {}

	async findById(providerId: string): Promise<BillingInvoiceRow | null> {
		return fetchOne<BillingInvoiceRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async listByCustomer(
		customerId: string,
		page?: {
			pageSize: number;
			pageState?: string | null;
		},
	): Promise<PagedQueryResult<BillingInvoiceRow>> {
		const refsPage = await fetchPage<{
			provider_id: string;
		}>(
			FETCH_BY_CUSTOMER_PARTITION,
			{customer_id: customerId},
			{pageSize: page?.pageSize ?? 50, pageState: page?.pageState ?? null},
		);
		if (refsPage.rows.length === 0) {
			return {rows: [], pageState: refsPage.pageState};
		}
		const ids = refsPage.rows.map((r) => r.provider_id);
		const rows = await fetchMany<BillingInvoiceRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		return {rows, pageState: refsPage.pageState};
	}

	async listBySubscription(
		subscriptionId: string,
		page?: {
			pageSize: number;
			pageState?: string | null;
		},
	): Promise<PagedQueryResult<BillingInvoiceRow>> {
		const refsPage = await fetchPage<{
			provider_id: string;
		}>(
			FETCH_BY_SUBSCRIPTION_PARTITION,
			{subscription_id: subscriptionId},
			{pageSize: page?.pageSize ?? 50, pageState: page?.pageState ?? null},
		);
		if (refsPage.rows.length === 0) {
			return {rows: [], pageState: refsPage.pageState};
		}
		const ids = refsPage.rows.map((r) => r.provider_id);
		const rows = await fetchMany<BillingInvoiceRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
		return {rows, pageState: refsPage.pageState};
	}

	async listByUser(
		userId: bigint,
		page?: {
			pageSize: number;
			pageState?: string | null;
		},
	): Promise<PagedQueryResult<BillingInvoiceRow>> {
		const customerRefs = await fetchMany<{
			provider_id: string;
		}>(FETCH_CUSTOMERS_BY_USER, {user_id: userId});
		if (customerRefs.length === 0) {
			return {rows: [], pageState: null};
		}
		const aggregated: Array<BillingInvoiceRow> = [];
		let lastPageState: string | null = null;
		for (const ref of customerRefs) {
			const result = await this.listByCustomer(ref.provider_id, page);
			aggregated.push(...result.rows);
			lastPageState = result.pageState;
		}
		return {rows: aggregated, pageState: lastPageState};
	}

	async upsertFromStripe(
		inv: Stripe.Invoice,
		hints?: {
			knownUserId?: bigint;
		},
	): Promise<{
		changed: boolean;
		row: BillingInvoiceRow;
	}> {
		const mapped = mapStripeInvoiceToRow(inv, hints);
		const existing = await this.findById(mapped.primary.provider_id);
		if (isExistingNewer(existing, mapped.primary)) {
			for (const p of mapped.payments) {
				await this.paymentsRepo.upsertFromStripeMapped(p);
			}
			return {changed: false, row: existing!};
		}
		if (existing && rowsEquivalent(existing, mapped.primary, ['mirrored_at', 'version'])) {
			for (const p of mapped.payments) {
				await this.paymentsRepo.upsertFromStripeMapped(p);
			}
			return {changed: false, row: existing};
		}
		const result = await executeBillingVersionedUpdate<BillingInvoiceRow, 'provider_id'>(
			async () => existing,
			(current) => ({
				pk: {provider_id: mapped.primary.provider_id},
				patch: buildPatchFromRow(mapped.primary, current, BILLING_INVOICE_COLUMNS, ['provider_id']),
			}),
			BillingInvoices,
			{initialData: existing},
		);
		await upsertOne(BillingInvoicesByCustomer.upsertAll(mapped.byCustomer));
		if (mapped.bySubscription) {
			await upsertOne(BillingInvoicesBySubscription.upsertAll(mapped.bySubscription));
		}
		for (const p of mapped.payments) {
			await this.paymentsRepo.upsertFromStripeMapped(p);
		}
		return {changed: true, row: {...mapped.primary, version: result.finalVersion}};
	}
}

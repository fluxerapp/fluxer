// SPDX-License-Identifier: AGPL-3.0-or-later

import {isExistingNewer} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {BillingPaymentByInvoiceRow, BillingPaymentRow} from '@app/api/database/types/BillingTypes';
import {BillingPayments, BillingPaymentsByInvoice} from '@app/api/Tables';

const FETCH_BY_ID = BillingPayments.selectCql({
	where: BillingPayments.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_INVOICE = BillingPaymentsByInvoice.selectCql({
	where: BillingPaymentsByInvoice.where.eq('invoice_id'),
});
const FETCH_BY_PROVIDER_IDS = BillingPayments.selectCql({
	where: BillingPayments.where.in('provider_id', 'provider_ids'),
});

export class BillingPaymentRepository {
	async findById(providerId: string): Promise<BillingPaymentRow | null> {
		return fetchOne<BillingPaymentRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async listByInvoice(invoiceId: string): Promise<Array<BillingPaymentRow>> {
		const refs = await fetchMany<{
			provider_id: string;
		}>(FETCH_BY_INVOICE, {invoice_id: invoiceId});
		if (refs.length === 0) return [];
		const ids = refs.map((r) => r.provider_id);
		return fetchMany<BillingPaymentRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
	}

	async findPrimaryForInvoice(invoiceId: string): Promise<BillingPaymentRow | null> {
		const list = await this.listByInvoice(invoiceId);
		return list.find((p) => p.is_default === true) ?? null;
	}

	async upsertFromStripeMapped(mapped: {primary: BillingPaymentRow; byInvoice: BillingPaymentByInvoiceRow}): Promise<{
		changed: boolean;
		row: BillingPaymentRow;
	}> {
		const existing = await this.findById(mapped.primary.provider_id);
		if (isExistingNewer(existing, mapped.primary)) {
			return {changed: false, row: existing!};
		}
		await upsertOne(BillingPayments.upsertAll(mapped.primary));
		if (mapped.byInvoice.invoice_id) {
			await upsertOne(BillingPaymentsByInvoice.upsertAll(mapped.byInvoice));
		}
		return {changed: true, row: mapped.primary};
	}

	async upsertFromStripe(
		p: BillingPaymentRow,
		hints?: {
			byInvoice?: BillingPaymentByInvoiceRow;
		},
	): Promise<{
		changed: boolean;
		row: BillingPaymentRow;
	}> {
		const existing = await this.findById(p.provider_id);
		if (isExistingNewer(existing, p)) {
			return {changed: false, row: existing!};
		}
		await upsertOne(BillingPayments.upsertAll(p));
		if (hints?.byInvoice?.invoice_id) {
			await upsertOne(BillingPaymentsByInvoice.upsertAll(hints.byInvoice));
		}
		return {changed: true, row: p};
	}
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {hydrateByRefs, upsertMirrorRow} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne} from '@app/api/database/CassandraQueryExecution';
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
		return hydrateByRefs<BillingPaymentRow>(FETCH_BY_INVOICE, {invoice_id: invoiceId}, FETCH_BY_PROVIDER_IDS);
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
		return upsertMirrorRow(BillingPayments, existing, mapped.primary, [
			[BillingPaymentsByInvoice, mapped.byInvoice.invoice_id ? mapped.byInvoice : null],
		]);
	}
}

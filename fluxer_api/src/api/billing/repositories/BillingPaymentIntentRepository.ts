// SPDX-License-Identifier: AGPL-3.0-or-later

import type Stripe from 'stripe';
import {fetchMany, fetchOne, fetchPage, type PagedQueryResult, upsertOne} from '../../database/CassandraQueryExecution';
import type {BillingPaymentIntentRow} from '../../database/types/BillingTypes';
import {
	BillingPaymentIntents,
	BillingPaymentIntentsByCustomer,
	BillingPayments,
	BillingPaymentsByInvoice,
} from '../../Tables';
import {mapStripePaymentIntentToRow} from '../mappers/StripeToBillingMapper';
import {BILLING_REVERSE_CHRONOLOGICAL_ORDER, isExistingNewer, restoreReferenceOrder} from './BillingRepoHelpers';

const FETCH_BY_ID = BillingPaymentIntents.selectCql({
	where: BillingPaymentIntents.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_CUSTOMER = BillingPaymentIntentsByCustomer.selectCql({
	where: BillingPaymentIntentsByCustomer.where.eq('customer_id'),
	orderBy: BILLING_REVERSE_CHRONOLOGICAL_ORDER,
});
const FETCH_BY_PROVIDER_IDS = BillingPaymentIntents.selectCql({
	where: BillingPaymentIntents.where.in('provider_id', 'provider_ids'),
});
const FETCH_PAYMENT_INTENTS_BY_INVOICE_VIA_PAYMENTS = BillingPayments.selectCql({
	columns: ['payment_intent_id'],
	where: BillingPayments.where.eq('provider_id'),
	limit: 1,
});
const FETCH_PAYMENTS_BY_INVOICE = BillingPaymentsByInvoice.select({
	columns: ['provider_id', 'payment_intent_id'],
	where: BillingPaymentsByInvoice.where.eq('invoice_id'),
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
		return {rows: restoreReferenceOrder(refsPage.rows, rows), pageState: refsPage.pageState};
	}

	async findByInvoiceId(invoiceId: string): Promise<BillingPaymentIntentRow | null> {
		const refs = await fetchMany<{
			provider_id: string;
			payment_intent_id: string | null;
		}>(
			FETCH_PAYMENTS_BY_INVOICE.bind({
				invoice_id: invoiceId,
			}),
		);
		if (refs.length === 0) return null;
		for (const ref of refs) {
			const payment = await fetchOne<{
				is_default: boolean | null;
				payment_intent_id: string | null;
			}>(FETCH_PAYMENT_INTENTS_BY_INVOICE_VIA_PAYMENTS, {provider_id: ref.provider_id});
			if (payment?.is_default === true && payment.payment_intent_id) {
				return this.findById(payment.payment_intent_id);
			}
		}
		const firstWithIntent = refs.find((r) => r.payment_intent_id !== null);
		if (firstWithIntent?.payment_intent_id) {
			return this.findById(firstWithIntent.payment_intent_id);
		}
		return null;
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

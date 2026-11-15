// SPDX-License-Identifier: AGPL-3.0-or-later

import {mapStripeRefundToRow} from '@app/api/billing/mappers/StripeToBillingMapper';
import {hydrateByRefs, upsertMirrorRow} from '@app/api/billing/repositories/BillingRepoHelpers';
import {fetchOne} from '@app/api/database/CassandraQueryExecution';
import type {BillingRefundRow} from '@app/api/database/types/BillingTypes';
import {
	BillingRefunds,
	BillingRefundsByCharge,
	BillingRefundsByInvoice,
	BillingRefundsByPaymentIntent,
} from '@app/api/Tables';
import type Stripe from 'stripe';

const FETCH_BY_ID = BillingRefunds.selectCql({
	where: BillingRefunds.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_PAYMENT_INTENT = BillingRefundsByPaymentIntent.selectCql({
	where: BillingRefundsByPaymentIntent.where.eq('payment_intent_id'),
});
const FETCH_BY_INVOICE = BillingRefundsByInvoice.selectCql({
	where: BillingRefundsByInvoice.where.eq('invoice_id'),
});
const FETCH_BY_PROVIDER_IDS = BillingRefunds.selectCql({
	where: BillingRefunds.where.in('provider_id', 'provider_ids'),
});

const HINT_COLUMNS: ReadonlySet<string> = new Set(['invoice_id', 'customer_id', 'user_id', 'livemode']);

function withoutUnknownHints(row: BillingRefundRow): BillingRefundRow {
	return Object.fromEntries(
		Object.entries(row).filter(([column, value]) => value !== null || !HINT_COLUMNS.has(column)),
	) as unknown as BillingRefundRow;
}

export class BillingRefundRepository {
	async findById(providerId: string): Promise<BillingRefundRow | null> {
		return fetchOne<BillingRefundRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async listByPaymentIntent(paymentIntentId: string): Promise<Array<BillingRefundRow>> {
		return hydrateByRefs<BillingRefundRow>(
			FETCH_BY_PAYMENT_INTENT,
			{payment_intent_id: paymentIntentId},
			FETCH_BY_PROVIDER_IDS,
		);
	}

	async listByInvoice(invoiceId: string): Promise<Array<BillingRefundRow>> {
		return hydrateByRefs<BillingRefundRow>(FETCH_BY_INVOICE, {invoice_id: invoiceId}, FETCH_BY_PROVIDER_IDS);
	}

	async upsertFromStripe(
		r: Stripe.Refund,
		hints?: {
			invoiceId?: string;
			customerId?: string;
			userId?: bigint;
			livemode?: boolean;
		},
	): Promise<{
		changed: boolean;
		row: BillingRefundRow;
	}> {
		const existing = r.id ? await this.findById(r.id) : null;
		const mapped = mapStripeRefundToRow(r, {
			invoiceId: hints?.invoiceId ?? existing?.invoice_id ?? undefined,
			customerId: hints?.customerId ?? existing?.customer_id ?? undefined,
			userId: hints?.userId ?? existing?.user_id ?? undefined,
			livemode: hints?.livemode ?? existing?.livemode ?? undefined,
		});
		const result = await upsertMirrorRow(BillingRefunds, existing, withoutUnknownHints(mapped.primary), [
			[BillingRefundsByCharge, mapped.byCharge],
			[BillingRefundsByPaymentIntent, mapped.byPaymentIntent],
			[BillingRefundsByInvoice, mapped.byInvoice],
		]);
		return result.changed ? {changed: true, row: mapped.primary} : result;
	}
}

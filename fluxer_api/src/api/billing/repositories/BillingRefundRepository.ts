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
		const mapped = mapStripeRefundToRow(r, hints);
		const existing = await this.findById(mapped.primary.provider_id);
		return upsertMirrorRow(BillingRefunds, existing, mapped.primary, [
			[BillingRefundsByCharge, mapped.byCharge],
			[BillingRefundsByPaymentIntent, mapped.byPaymentIntent],
			[BillingRefundsByInvoice, mapped.byInvoice],
		]);
	}
}

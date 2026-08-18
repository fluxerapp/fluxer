// SPDX-License-Identifier: AGPL-3.0-or-later

import type Stripe from 'stripe';
import {fetchMany, fetchOne, upsertOne} from '../../database/CassandraQueryExecution';
import type {BillingRefundRow} from '../../database/types/BillingTypes';
import {
	BillingRefunds,
	BillingRefundsByCharge,
	BillingRefundsByInvoice,
	BillingRefundsByPaymentIntent,
} from '../../Tables';
import {mapStripeRefundToRow} from '../mappers/StripeToBillingMapper';
import {isExistingNewer} from './BillingRepoHelpers';

const FETCH_BY_ID = BillingRefunds.selectCql({
	where: BillingRefunds.where.eq('provider_id'),
	limit: 1,
});
const FETCH_BY_CHARGE = BillingRefundsByCharge.selectCql({
	where: BillingRefundsByCharge.where.eq('charge_id'),
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

async function hydrate(
	refs: Array<{
		provider_id: string;
	}>,
): Promise<Array<BillingRefundRow>> {
	if (refs.length === 0) return [];
	const ids = refs.map((r) => r.provider_id);
	return fetchMany<BillingRefundRow>(FETCH_BY_PROVIDER_IDS, {provider_ids: ids});
}

export class BillingRefundRepository {
	async findById(providerId: string): Promise<BillingRefundRow | null> {
		return fetchOne<BillingRefundRow>(FETCH_BY_ID, {provider_id: providerId});
	}

	async listByCharge(chargeId: string): Promise<Array<BillingRefundRow>> {
		const refs = await fetchMany<{
			provider_id: string;
		}>(FETCH_BY_CHARGE, {charge_id: chargeId});
		return hydrate(refs);
	}

	async listByPaymentIntent(paymentIntentId: string): Promise<Array<BillingRefundRow>> {
		const refs = await fetchMany<{
			provider_id: string;
		}>(FETCH_BY_PAYMENT_INTENT, {payment_intent_id: paymentIntentId});
		return hydrate(refs);
	}

	async listByInvoice(invoiceId: string): Promise<Array<BillingRefundRow>> {
		const refs = await fetchMany<{
			provider_id: string;
		}>(FETCH_BY_INVOICE, {invoice_id: invoiceId});
		return hydrate(refs);
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
		if (isExistingNewer(existing, mapped.primary)) {
			return {changed: false, row: existing!};
		}
		await upsertOne(BillingRefunds.upsertAll(mapped.primary));
		if (mapped.byCharge) {
			await upsertOne(BillingRefundsByCharge.upsertAll(mapped.byCharge));
		}
		if (mapped.byPaymentIntent) {
			await upsertOne(BillingRefundsByPaymentIntent.upsertAll(mapped.byPaymentIntent));
		}
		if (mapped.byInvoice) {
			await upsertOne(BillingRefundsByInvoice.upsertAll(mapped.byInvoice));
		}
		return {changed: true, row: mapped.primary};
	}
}

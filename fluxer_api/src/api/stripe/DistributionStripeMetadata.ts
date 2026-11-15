// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/api/Logger';
import {
	distributionMetadata,
	findChannelPrice,
	getDistributionPricingConfig,
	type ProductDistribution,
	pickDistributionMetadata,
} from '@app/api/stripe/DistributionPricing';
import {extractId} from '@app/api/stripe/StripeUtils';
import type Stripe from 'stripe';

function hasChannelPrices(): boolean {
	return getDistributionPricingConfig().rules.some((rule) => Object.keys(rule.prices).length > 0);
}

function invoiceDistribution(invoice: Stripe.Invoice): ProductDistribution | null {
	for (const line of invoice.lines?.data ?? []) {
		if (line.amount <= 0) continue;
		const priceId = extractId(line.pricing?.price_details?.price ?? null);
		const distribution = priceId ? findChannelPrice(priceId) : null;
		if (distribution) return distribution;
	}
	return null;
}

function invoiceDistributionMetadata(
	invoice: Stripe.Invoice,
	distribution: ProductDistribution,
): Record<string, string> {
	const subscriptionMetadata = invoice.parent?.subscription_details?.metadata ?? null;
	const purchaseRef =
		invoice.billing_reason === 'subscription_create' ? subscriptionMetadata?.['distribution_purchase_ref'] : undefined;
	return {
		...distributionMetadata(distribution),
		...(purchaseRef ? {distribution_purchase_ref: purchaseRef} : {}),
	};
}

export async function tagInvoiceDistribution(stripe: Stripe, invoice: Stripe.Invoice): Promise<void> {
	const distribution = invoiceDistribution(invoice);
	if (!distribution || !invoice.id) return;
	const metadata = invoiceDistributionMetadata(invoice, distribution);
	const payments = await stripe.invoicePayments.list({invoice: invoice.id, limit: 10});
	for (const payment of payments.data) {
		const paymentIntentId = extractId(payment.payment?.payment_intent ?? null);
		const chargeId = extractId(payment.payment?.charge ?? null);
		if (paymentIntentId) {
			const paymentIntent = await stripe.paymentIntents.update(paymentIntentId, {metadata});
			const latestChargeId = extractId(paymentIntent.latest_charge);
			if (latestChargeId) await stripe.charges.update(latestChargeId, {metadata});
		} else if (chargeId) {
			await stripe.charges.update(chargeId, {metadata});
		}
	}
}

export async function tagChargeDistribution(stripe: Stripe, charge: Stripe.Charge): Promise<void> {
	if (charge.metadata?.['distribution'] || !hasChannelPrices()) return;
	const paymentIntentId = extractId(charge.payment_intent);
	if (!paymentIntentId) return;
	const paymentIntent = await stripe.paymentIntents.retrieve(paymentIntentId);
	const metadata = pickDistributionMetadata(paymentIntent.metadata);
	if (Object.keys(metadata).length > 0) {
		await stripe.charges.update(charge.id, {metadata});
	}
}

export async function distributionRefundMetadata(
	stripe: Stripe,
	target: {payment_intent?: string | null; charge?: string | null},
): Promise<Record<string, string>> {
	if (!hasChannelPrices()) return {};
	try {
		if (target.payment_intent) {
			const paymentIntent = await stripe.paymentIntents.retrieve(target.payment_intent);
			return pickDistributionMetadata(paymentIntent.metadata);
		}
		if (!target.charge) return {};
		const charge = await stripe.charges.retrieve(target.charge);
		const fromCharge = pickDistributionMetadata(charge.metadata);
		if (Object.keys(fromCharge).length > 0) return fromCharge;
		const paymentIntentId = extractId(charge.payment_intent);
		if (!paymentIntentId) return {};
		const paymentIntent = await stripe.paymentIntents.retrieve(paymentIntentId);
		return pickDistributionMetadata(paymentIntent.metadata);
	} catch (error) {
		Logger.warn({error, target}, 'Failed to read distribution metadata for refund');
		return {};
	}
}

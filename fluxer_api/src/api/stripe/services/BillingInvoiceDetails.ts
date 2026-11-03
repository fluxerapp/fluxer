// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {ProductInfo, ProductRegistry} from '@app/api/stripe/ProductRegistry';
import {getPrimarySubscriptionItem} from '@app/api/stripe/StripeSubscriptionPeriod';
import {extractId} from '@app/api/stripe/StripeUtils';
import {isProRataWithdrawal, plutoniumWithdrawalPeriod} from '@app/api/stripe/services/PlutoniumWithdrawal';
import type {BillingTotals, PlutoniumPurchaseTerms} from '@pkgs/email/src/IEmailService';
import type Stripe from 'stripe';

export interface ChargedLine {
	product: ProductInfo;
	period: {start: Date; end: Date} | null;
	unitAmountMinor: number | null;
}

export function unixToDate(value: number | null | undefined): Date | null {
	return value ? new Date(value * 1000) : null;
}

export function invoicePaidAt(invoice: Stripe.Invoice): Date {
	return unixToDate(invoice.status_transitions?.paid_at) ?? new Date(invoice.created * 1000);
}

function isProration(line: Stripe.InvoiceLineItem): boolean {
	const parent = line.parent;
	return Boolean(parent?.subscription_item_details?.proration || parent?.invoice_item_details?.proration);
}

export function findChargedLine(invoice: Stripe.Invoice, productRegistry: ProductRegistry): ChargedLine | null {
	const candidates = (invoice.lines?.data ?? []).flatMap((line) => {
		const priceId = extractId(line.pricing?.price_details?.price ?? null);
		const product = priceId ? productRegistry.getProduct(priceId) : null;
		return product && line.amount > 0 ? [{line, product}] : [];
	});
	const regular = candidates.filter(({line}) => !isProration(line));
	const pool = regular.length > 0 ? regular : candidates;
	const chosen = pool.reduce<(typeof pool)[number] | null>(
		(best, candidate) => (best && best.line.amount >= candidate.line.amount ? best : candidate),
		null,
	);
	if (!chosen) {
		return null;
	}
	const start = unixToDate(chosen.line.period?.start);
	const end = unixToDate(chosen.line.period?.end);
	const unitAmount = Number(chosen.line.pricing?.unit_amount_decimal ?? Number.NaN);
	return {
		product: chosen.product,
		period: start && end && end > start ? {start, end} : null,
		unitAmountMinor: Number.isFinite(unitAmount) ? Math.round(unitAmount) : null,
	};
}

export function billingSellerShown(): boolean {
	return !Config.instance.selfHosted;
}

export function plainTotals(amountMinor: number, currency: string): BillingTotals {
	return {totalMinor: amountMinor, paidMinor: amountMinor, currency, tax: null, reverseCharge: false};
}

export function invoiceTotals(invoice: Stripe.Invoice): BillingTotals {
	const taxes = invoice.total_taxes ?? [];
	const charged = taxes.filter((tax) => tax.amount > 0);
	const taxAmount = charged.reduce((sum, tax) => sum + tax.amount, 0);
	return {
		totalMinor: invoice.total,
		paidMinor: invoice.amount_paid,
		currency: invoice.currency,
		tax:
			taxAmount > 0
				? {
						amountMinor: taxAmount,
						behavior: charged.every((tax) => tax.tax_behavior === 'inclusive') ? 'inclusive' : 'exclusive',
					}
				: null,
		reverseCharge: taxes.some((tax) => tax.taxability_reason === 'reverse_charge'),
	};
}

export function isDonationSubscription(subscription: Stripe.Subscription): boolean {
	return subscription.metadata?.is_donation === 'true';
}

export function subscriptionBillingProduct(
	subscription: Stripe.Subscription,
	productRegistry: ProductRegistry,
): 'plutonium' | 'donation' | null {
	if (isDonationSubscription(subscription)) {
		return 'donation';
	}
	const priceId = getPrimarySubscriptionItem(subscription)?.price?.id;
	const product = priceId ? productRegistry.getProduct(priceId) : null;
	return product && !product.isGift && product.billingCycle ? 'plutonium' : null;
}

export function primaryInvoiceLine(invoice: Stripe.Invoice): Stripe.InvoiceLineItem | null {
	return (invoice.lines?.data ?? [])
		.filter((line) => line.amount > 0 && !isProration(line))
		.reduce<Stripe.InvoiceLineItem | null>((best, line) => (best && best.amount >= line.amount ? best : line), null);
}

export function isDonationInvoice(invoice: Stripe.Invoice): boolean {
	return invoice.parent?.subscription_details?.metadata?.is_donation === 'true';
}

function checkoutMetadata(invoice: Stripe.Invoice): Record<string, string> {
	return {...invoice.metadata, ...invoice.parent?.subscription_details?.metadata};
}

function renewalTax(tax: BillingTotals['tax']): 'included' | 'added' | 'none' {
	if (!tax) {
		return 'none';
	}
	return tax.behavior === 'inclusive' ? 'included' : 'added';
}

export function purchaseTermsFromMetadata(
	metadata: Record<string, string>,
	paidAt: Date,
	renewal: PlutoniumPurchaseTerms['renewal'],
): PlutoniumPurchaseTerms {
	const period = plutoniumWithdrawalPeriod(metadata, paidAt);
	const proRata = period !== null && period.rule !== 'brazil' && isProRataWithdrawal(metadata);
	const recordedConsentAt =
		metadata.eu_withdrawal_waiver_accepted === 'true' && metadata.eu_withdrawal_waiver_accepted_at
			? new Date(metadata.eu_withdrawal_waiver_accepted_at)
			: null;
	const consentAt = recordedConsentAt ?? (proRata ? paidAt : null);
	return {
		renewal,
		consentAt: consentAt && !Number.isNaN(consentAt.getTime()) ? consentAt : null,
		withdrawalEndsOn: period?.endsOn ?? null,
		withdrawalRule: period?.rule ?? null,
		withdrawalProRata: proRata,
		seller: billingSellerShown(),
	};
}

export function plutoniumPurchaseTerms(invoice: Stripe.Invoice, line: ChargedLine): PlutoniumPurchaseTerms | null {
	if (!line.product.isGift && invoice.billing_reason !== 'subscription_create') {
		return null;
	}
	const tax = invoiceTotals(invoice).tax;
	const interval = line.product.billingCycle === 'yearly' ? 'year' : 'month';
	return purchaseTermsFromMetadata(
		checkoutMetadata(invoice),
		invoicePaidAt(invoice),
		line.product.isGift || line.unitAmountMinor == null
			? null
			: {interval, amountMinor: line.unitAmountMinor, tax: renewalTax(tax)},
	);
}

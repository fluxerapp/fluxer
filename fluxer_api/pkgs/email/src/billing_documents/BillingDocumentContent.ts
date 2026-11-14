// SPDX-License-Identifier: AGPL-3.0-or-later

export const BILLING_DOCUMENT_RENDER_VERSION = '2';

export const BILLING_DOCUMENT_SERIES = ['OP', 'PL', 'GC', 'DN', 'KR', 'KG'] as const;
export type BillingDocumentSeries = (typeof BILLING_DOCUMENT_SERIES)[number];

export type BillingDocumentLanguage = 'en' | 'sv-SE' | 'pt-BR';
export type BillingDocumentKind = 'invoice' | 'receipt' | 'donation_receipt' | 'credit_note' | 'refund_note';
export type BillingDocumentFlow = 'operator_pass' | 'plutonium' | 'gift' | 'donation';
export type BillingDocumentLayout = 'full' | 'simplified';
export type BillingDocumentVatTreatment =
	| 'charged'
	| 'reverse_charge_eu'
	| 'outside_scope'
	| 'not_charged'
	| 'voucher'
	| 'donation';
export type BillingDocumentCreditReason = 'withdrawal' | 'refund' | 'duplicate' | 'fraud' | 'dispute';
export type BillingDocumentVatIdVerification = 'verified' | 'unverified' | 'pending' | 'unavailable';

export interface BillingDocumentAddress {
	line1: string | null;
	line2: string | null;
	postalCode: string | null;
	city: string | null;
	state: string | null;
	country: string | null;
}

export interface BillingDocumentBuyer {
	business: boolean;
	name: string | null;
	businessName: string | null;
	email: string | null;
	address: BillingDocumentAddress | null;
	country: string | null;
	vatId: string | null;
	vatIdVerification: BillingDocumentVatIdVerification | null;
}

export interface BillingDocumentSeller {
	name: string;
	organisationNumber: string;
	registeredOffice: string;
	address: string;
	email: string;
	vatNumbers: ReadonlyArray<string>;
}

export interface BillingDocumentTaxLine {
	country: string | null;
	jurisdiction: string | null;
	ratePercent: number | null;
	taxType: string | null;
	taxabilityReason: string | null;
	taxableMinor: number;
	taxMinor: number;
}

export interface BillingDocumentLine {
	description: string;
	quantity: number;
	unitAmountExclMinor: number;
	amountExclMinor: number;
	taxMinor: number;
	ratePercent: number | null;
	periodStart: string | null;
	periodEnd: string | null;
}

export interface BillingDocumentSekTax {
	amountMinor: number;
	rate: string;
	rateDate: string;
	source: 'ECB';
}

export interface BillingDocumentOriginal {
	number: string;
	issuedAt: string;
	documentId: string | null;
}

export interface BillingDocumentReferences {
	checkoutSessionId: string | null;
	paymentIntentId: string | null;
	chargeId: string | null;
	stripeInvoiceId: string | null;
	refundId: string | null;
	disputeId: string | null;
}

export interface BillingDocumentContent {
	renderVersion: string;
	documentId: string;
	number: string;
	series: BillingDocumentSeries;
	kind: BillingDocumentKind;
	flow: BillingDocumentFlow;
	layout: BillingDocumentLayout;
	language: BillingDocumentLanguage;
	issuedAt: string;
	supplyDate: string | null;
	periodStart: string | null;
	periodEnd: string | null;
	currency: string;
	lines: ReadonlyArray<BillingDocumentLine>;
	taxLines: ReadonlyArray<BillingDocumentTaxLine>;
	subtotalExclMinor: number;
	taxMinor: number;
	totalMinor: number;
	vatTreatment: BillingDocumentVatTreatment;
	sekTax: BillingDocumentSekTax | null;
	seller: BillingDocumentSeller;
	buyer: BillingDocumentBuyer;
	payment: {at: string; method: string | null} | null;
	original: BillingDocumentOriginal | null;
	reason: BillingDocumentCreditReason | null;
	references: BillingDocumentReferences;
}

const STOCKHOLM_DATE = new Intl.DateTimeFormat('en-CA', {
	timeZone: 'Europe/Stockholm',
	year: 'numeric',
	month: '2-digit',
	day: '2-digit',
});

export function stockholmDate(iso: string): string {
	if (/^\d{4}-\d{2}-\d{2}$/.test(iso)) {
		return iso;
	}
	const parts = STOCKHOLM_DATE.formatToParts(new Date(iso));
	const part = (type: string) => parts.find((entry) => entry.type === type)?.value ?? '';
	return `${part('year')}-${part('month')}-${part('day')}`;
}

export function formatBillingDocumentNumber(series: BillingDocumentSeries, number: bigint | number): string {
	return `${series}-${String(number).padStart(6, '0')}`;
}

export function billingDocumentFilename(number: string): string {
	return `Fluxer-${number.replace(/[^\w-]/g, '')}.pdf`;
}

export function isCreditDocument(kind: BillingDocumentKind): boolean {
	return kind === 'credit_note' || kind === 'refund_note';
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {stripeCurrencyExponent} from '@fluxer/number_utils/src/PriceFormatting';
import {
	BILLING_DOCUMENT_RENDER_VERSION,
	type BillingDocumentBuyer,
	type BillingDocumentContent,
	type BillingDocumentCreditReason,
	type BillingDocumentFlow,
	type BillingDocumentKind,
	type BillingDocumentLanguage,
	type BillingDocumentLayout,
	type BillingDocumentLine,
	type BillingDocumentOriginal,
	type BillingDocumentReferences,
	type BillingDocumentSekTax,
	type BillingDocumentSeller,
	type BillingDocumentSeries,
	type BillingDocumentTaxLine,
	type BillingDocumentVatTreatment,
} from '@pkgs/email/src/billing_documents/BillingDocumentContent';
import {
	getOperatorPassSellerAddress,
	OPERATOR_PASS_TRADER_EMAIL,
} from '@pkgs/email/src/operator_pass_terms/OperatorPassTerms';

const SWEDISH_VAT_NUMBER = 'SE559537399301';
const REGISTERED_OFFICE: Record<BillingDocumentLanguage, string> = {
	en: 'Haninge, Sweden',
	'sv-SE': 'Haninge',
	'pt-BR': 'Haninge, Suécia',
};
const UK_VAT_NUMBER = 'GB518363191';
const NORWAY_VOEC_NUMBER = 'VOEC 3058085';

const EU_COUNTRIES = new Set([
	'AT',
	'BE',
	'BG',
	'CY',
	'CZ',
	'DE',
	'DK',
	'EE',
	'ES',
	'FI',
	'FR',
	'GR',
	'HR',
	'HU',
	'IE',
	'IT',
	'LT',
	'LU',
	'LV',
	'MT',
	'NL',
	'PL',
	'PT',
	'RO',
	'SE',
	'SI',
	'SK',
]);

export interface SaleLine {
	description: string;
	quantity: number;
	amountMinor: number;
	taxMinor: number;
	ratePercent: number | null;
	periodStart: string | null;
	periodEnd: string | null;
}

export interface SaleData {
	flow: BillingDocumentFlow;
	currency: string;
	totalMinor: number;
	lines: ReadonlyArray<SaleLine>;
	taxLines: ReadonlyArray<BillingDocumentTaxLine>;
	buyer: BillingDocumentBuyer;
	supplyDate: string | null;
	periodStart: string | null;
	periodEnd: string | null;
	payment: {at: string; method: string | null} | null;
	references: BillingDocumentReferences;
}

export interface DocumentHeader {
	documentId: string;
	number: string;
	issuedAt: string;
	language: BillingDocumentLanguage;
}

export interface CreditOriginal {
	sale: SaleData;
	language: BillingDocumentLanguage | null;
	reference: BillingDocumentOriginal;
	layout: BillingDocumentLayout;
	vatTreatment: BillingDocumentVatTreatment;
	sekRate: {rate: string; rateDate: string} | null;
}

export interface CreditRequest {
	amountMinor: number;
	creditedBeforeMinor: number;
	creditedQuantity?: number | null;
	reason: BillingDocumentCreditReason;
	at: string;
	references: BillingDocumentReferences;
}

export function billingDocumentLanguage(locale: string | null | undefined): BillingDocumentLanguage {
	const normalized = locale?.toLowerCase() ?? '';
	if (normalized.startsWith('sv')) return 'sv-SE';
	if (normalized.startsWith('pt')) return 'pt-BR';
	return 'en';
}

export function saleSeries(flow: BillingDocumentFlow): BillingDocumentSeries {
	switch (flow) {
		case 'operator_pass':
			return 'OP';
		case 'plutonium':
			return 'PL';
		case 'gift':
			return 'GC';
		case 'donation':
			return 'DN';
	}
}

export function creditSeries(original: Pick<CreditOriginal, 'vatTreatment'>): BillingDocumentSeries {
	return creditsNoVat(original) ? 'KG' : 'KR';
}

function creditsNoVat(original: Pick<CreditOriginal, 'vatTreatment'>): boolean {
	return original.vatTreatment === 'voucher' || original.vatTreatment === 'donation';
}

function saleKind(flow: BillingDocumentFlow): BillingDocumentKind {
	switch (flow) {
		case 'gift':
			return 'receipt';
		case 'donation':
			return 'donation_receipt';
		default:
			return 'invoice';
	}
}

function noVatFlow(flow: BillingDocumentFlow): boolean {
	return flow === 'gift' || flow === 'donation';
}

export function chargedTax(sale: Pick<SaleData, 'taxLines'>): number {
	return sale.taxLines.reduce((sum, line) => sum + line.taxMinor, 0);
}

export function vatTreatmentOf(sale: Pick<SaleData, 'flow' | 'taxLines' | 'buyer'>): BillingDocumentVatTreatment {
	if (sale.flow === 'gift') return 'voucher';
	if (sale.flow === 'donation') return 'donation';
	const country = sale.buyer.country?.toUpperCase() ?? null;
	const reverseCharged = sale.taxLines.some((line) => line.taxabilityReason === 'reverse_charge');
	if (chargedTax(sale) > 0) return 'charged';
	if (reverseCharged) {
		return country && EU_COUNTRIES.has(country) && country !== 'SE' ? 'reverse_charge_eu' : 'outside_scope';
	}
	if (sale.buyer.business && country && country !== 'SE') {
		return EU_COUNTRIES.has(country) ? 'reverse_charge_eu' : 'outside_scope';
	}
	return 'not_charged';
}

export function layoutOf(buyer: BillingDocumentBuyer): BillingDocumentLayout {
	return buyer.business ? 'full' : 'simplified';
}

function billingDocumentSeller(
	language: BillingDocumentLanguage,
	taxLines: ReadonlyArray<BillingDocumentTaxLine>,
): BillingDocumentSeller {
	const charged = (country: string) => taxLines.some((line) => line.country === country && line.taxMinor !== 0);
	return {
		name: 'Fluxer Platform AB',
		organisationNumber: '559537-3993',
		registeredOffice: REGISTERED_OFFICE[language],
		address: getOperatorPassSellerAddress(language),
		email: OPERATOR_PASS_TRADER_EMAIL,
		vatNumbers: [
			SWEDISH_VAT_NUMBER,
			...(charged('GB') ? [UK_VAT_NUMBER] : []),
			...(charged('NO') ? [NORWAY_VOEC_NUMBER] : []),
		],
	};
}

function documentLine(line: SaleLine, noVat: boolean): BillingDocumentLine {
	const taxMinor = noVat ? 0 : line.taxMinor;
	const amountExclMinor = line.amountMinor - taxMinor;
	const quantity = Math.max(1, line.quantity);
	return {
		description: line.description,
		quantity,
		unitAmountExclMinor: Math.round(amountExclMinor / quantity),
		amountExclMinor,
		taxMinor,
		ratePercent: noVat ? null : line.ratePercent,
		periodStart: line.periodStart,
		periodEnd: line.periodEnd,
	};
}

export function sekMinor(amountMinor: number, currency: string, rate: string): number {
	return Math.round((amountMinor / 10 ** stripeCurrencyExponent(currency)) * Number(rate) * 100);
}

export function swedishTaxMinor(taxLines: ReadonlyArray<BillingDocumentTaxLine>): number {
	return taxLines.filter((line) => line.country === 'SE').reduce((sum, line) => sum + line.taxMinor, 0);
}

export function buildSaleContent(
	sale: SaleData,
	header: DocumentHeader,
	sekTax: BillingDocumentSekTax | null,
): BillingDocumentContent {
	const series = saleSeries(sale.flow);
	const noVat = noVatFlow(sale.flow);
	const taxLines = noVat
		? []
		: sale.taxLines.filter((line) => line.taxMinor !== 0 || line.taxabilityReason === 'reverse_charge');
	const taxMinor = taxLines.reduce((sum, line) => sum + line.taxMinor, 0);
	const buyer = noVat && sale.flow === 'gift' ? {...sale.buyer, vatId: null, vatIdVerification: null} : sale.buyer;
	return {
		renderVersion: BILLING_DOCUMENT_RENDER_VERSION,
		documentId: header.documentId,
		number: header.number,
		series,
		kind: saleKind(sale.flow),
		flow: sale.flow,
		layout: layoutOf(buyer),
		language: header.language,
		issuedAt: header.issuedAt,
		supplyDate: sale.supplyDate,
		periodStart: sale.periodStart,
		periodEnd: sale.periodEnd,
		currency: sale.currency.toUpperCase(),
		lines: sale.lines.map((line) => documentLine(line, noVat)),
		taxLines,
		subtotalExclMinor: sale.totalMinor - taxMinor,
		taxMinor,
		totalMinor: sale.totalMinor,
		vatTreatment: vatTreatmentOf({flow: sale.flow, taxLines, buyer}),
		sekTax: noVat ? null : sekTax,
		seller: billingDocumentSeller(header.language, taxLines),
		buyer,
		payment: sale.payment,
		original: null,
		reason: null,
		references: sale.references,
	};
}

function cumulativeShare(value: number, total: number, credited: number): number {
	if (total <= 0) return 0;
	return Math.round((value * Math.min(Math.max(credited, 0), total)) / total);
}

function creditShare(value: number, total: number, before: number, now: number): number {
	return cumulativeShare(value, total, before + now) - cumulativeShare(value, total, before);
}

function creditableAmount(
	original: CreditOriginal,
	request: Pick<CreditRequest, 'amountMinor' | 'creditedBeforeMinor'>,
): number {
	return Math.max(0, Math.min(request.amountMinor, original.sale.totalMinor - request.creditedBeforeMinor));
}

function creditedQuantity(line: SaleLine, creditedMinor: number, requested: number | null | undefined): number {
	const quantity = Math.max(1, line.quantity);
	if (requested != null && requested >= 1 && requested <= quantity) {
		return requested;
	}
	if (creditedMinor >= line.amountMinor) {
		return quantity;
	}
	const unitMinor = line.amountMinor / quantity;
	const units = creditedMinor / unitMinor;
	const whole = Math.round(units);
	return whole >= 1 && Math.abs(creditedMinor - Math.round(whole * unitMinor)) <= whole ? whole : 1;
}

export function buildCreditContent(
	original: CreditOriginal,
	request: CreditRequest,
	header: DocumentHeader,
): BillingDocumentContent {
	const sale = original.sale;
	const total = sale.totalMinor;
	const amount = creditableAmount(original, request);
	const before = request.creditedBeforeMinor;
	const noVat = creditsNoVat(original);
	const taxLines: Array<BillingDocumentTaxLine> = noVat
		? []
		: sale.taxLines.map((line) => ({
				...line,
				taxableMinor: -creditShare(line.taxableMinor, total, before, amount),
				taxMinor: -creditShare(line.taxMinor, total, before, amount),
			}));
	const taxMinor = taxLines.reduce((sum, line) => sum + line.taxMinor, 0);
	const lines = sale.lines.map((line) => {
		const lineAmount = creditShare(line.amountMinor, total, before, amount);
		const lineTax = noVat ? 0 : creditShare(line.taxMinor, total, before, amount);
		const quantity = creditedQuantity(line, lineAmount, sale.lines.length === 1 ? request.creditedQuantity : null);
		return {
			description: line.description,
			quantity,
			unitAmountExclMinor: -Math.round((lineAmount - lineTax) / quantity),
			amountExclMinor: -(lineAmount - lineTax),
			taxMinor: -lineTax,
			ratePercent: noVat ? null : line.ratePercent,
			periodStart: line.periodStart,
			periodEnd: line.periodEnd,
		};
	});
	const last = lines.at(-1);
	if (last) {
		const linesTotal = lines.reduce((sum, line) => sum + line.amountExclMinor + line.taxMinor, 0);
		const linesTax = lines.reduce((sum, line) => sum + line.taxMinor, 0);
		last.taxMinor += taxMinor - linesTax;
		last.amountExclMinor += -amount - linesTotal - (taxMinor - linesTax);
		last.unitAmountExclMinor = Math.round(last.amountExclMinor / last.quantity);
	}
	const seTax = swedishTaxMinor(taxLines);
	const sekTax =
		original.sekRate && seTax !== 0
			? {
					amountMinor: sekMinor(seTax, sale.currency, original.sekRate.rate),
					rate: original.sekRate.rate,
					rateDate: original.sekRate.rateDate,
					source: 'ECB' as const,
				}
			: null;
	return {
		renderVersion: BILLING_DOCUMENT_RENDER_VERSION,
		documentId: header.documentId,
		number: header.number,
		series: creditSeries(original),
		kind: noVat ? 'refund_note' : 'credit_note',
		flow: sale.flow,
		layout: original.layout,
		language: header.language,
		issuedAt: header.issuedAt,
		supplyDate: null,
		periodStart: null,
		periodEnd: null,
		currency: sale.currency.toUpperCase(),
		lines,
		taxLines: taxLines.filter((line) => line.taxMinor !== 0 || line.taxabilityReason === 'reverse_charge'),
		subtotalExclMinor: -amount - taxMinor,
		taxMinor,
		totalMinor: -amount,
		vatTreatment: original.vatTreatment,
		sekTax,
		seller: billingDocumentSeller(header.language, sale.taxLines),
		buyer: sale.buyer,
		payment: {at: request.at, method: null},
		original: original.reference,
		reason: request.reason,
		references: request.references,
	};
}

export function creditOriginalFromContent(content: BillingDocumentContent): CreditOriginal {
	return {
		sale: {
			flow: content.flow,
			currency: content.currency,
			totalMinor: content.totalMinor,
			lines: content.lines.map((line) => ({
				description: line.description,
				quantity: line.quantity,
				amountMinor: line.amountExclMinor + line.taxMinor,
				taxMinor: line.taxMinor,
				ratePercent: line.ratePercent,
				periodStart: line.periodStart,
				periodEnd: line.periodEnd,
			})),
			taxLines: content.taxLines,
			buyer: content.buyer,
			supplyDate: content.supplyDate,
			periodStart: content.periodStart,
			periodEnd: content.periodEnd,
			payment: content.payment,
			references: content.references,
		},
		language: content.language,
		reference: {number: content.number, issuedAt: content.issuedAt, documentId: content.documentId},
		layout: content.layout,
		vatTreatment: content.vatTreatment,
		sekRate: content.sekTax ? {rate: content.sekTax.rate, rateDate: content.sekTax.rateDate} : null,
	};
}

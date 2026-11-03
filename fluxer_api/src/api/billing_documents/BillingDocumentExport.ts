// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import {billingDocumentContent} from '@app/api/billing_documents/BillingDocumentService';
import type {BillingDocumentRow} from '@app/api/database/types/BillingDocumentTypes';
import {BILLING_DOCUMENT_SERIES} from '@pkgs/email/src/billing_documents/BillingDocumentContent';

const COLUMNS = [
	'document_id',
	'number',
	'series',
	'kind',
	'flow',
	'layout',
	'issued_at',
	'supply_date',
	'period_start',
	'period_end',
	'currency',
	'total_minor',
	'tax_lines',
	'sek_tax_minor',
	'fx_rate',
	'fx_date',
	'buyer_country',
	'is_business',
	'buyer_vat_id',
	'vat_id_verification',
	'reverse_charge',
	'original_number',
	'original_document_id',
	'checkout_session_id',
	'payment_intent_id',
	'charge_id',
	'stripe_invoice_id',
	'refund_id',
	'dispute_id',
	'pdf_file',
	'pdf_sha256',
	'content_sha256',
] as const;

type Column = (typeof COLUMNS)[number];

export interface BillingDocumentExport {
	month: string;
	csvFilename: string;
	csv: string;
	documents: ReadonlyArray<BillingDocumentRow>;
}

export class BillingDocumentSeriesGapError extends Error {
	constructor(readonly gaps: ReadonlyArray<string>) {
		super(
			`Claimed document numbers without a document: ${gaps.join(', ')}. Re-issue each source with POST /admin/billing-documents/reissue`,
		);
		this.name = 'BillingDocumentSeriesGapError';
	}
}

function csvField(value: string): string {
	return /[",\r\n]/u.test(value) ? `"${value.replaceAll('"', '""')}"` : value;
}

export function billingDocumentPdfPath(row: BillingDocumentRow): string {
	return `pdf/${row.display_number}.pdf`;
}

function exportRow(row: BillingDocumentRow): Record<Column, string> {
	const content = billingDocumentContent(row);
	const references = content.references;
	return {
		document_id: row.document_id.toString(),
		number: row.display_number,
		series: row.series,
		kind: row.kind,
		flow: row.flow,
		layout: row.layout,
		issued_at: row.issued_at.toISOString(),
		supply_date: row.supply_date ?? '',
		period_start: row.period_start?.toISOString() ?? '',
		period_end: row.period_end?.toISOString() ?? '',
		currency: row.currency,
		total_minor: row.total_minor.toString(),
		tax_lines: JSON.stringify(
			content.taxLines.map((line) => ({
				country: line.country,
				jurisdiction: line.jurisdiction,
				rate: line.ratePercent,
				taxable_minor: line.taxableMinor,
				tax_minor: line.taxMinor,
				taxability_reason: line.taxabilityReason,
			})),
		),
		sek_tax_minor: row.sek_tax_minor?.toString() ?? '',
		fx_rate: row.fx_rate ?? '',
		fx_date: row.fx_date ?? '',
		buyer_country: content.buyer.country ?? '',
		is_business: String(content.buyer.business),
		buyer_vat_id: content.buyer.vatId ?? '',
		vat_id_verification: content.buyer.vatIdVerification ?? '',
		reverse_charge: String(content.vatTreatment === 'reverse_charge_eu'),
		original_number: row.original_number ?? '',
		original_document_id: row.original_document_id?.toString() ?? '',
		checkout_session_id: references.checkoutSessionId ?? '',
		payment_intent_id: references.paymentIntentId ?? '',
		charge_id: references.chargeId ?? '',
		stripe_invoice_id: references.stripeInvoiceId ?? '',
		refund_id: references.refundId ?? '',
		dispute_id: references.disputeId ?? '',
		pdf_file: billingDocumentPdfPath(row),
		pdf_sha256: row.pdf_sha256,
		content_sha256: row.content_sha256,
	};
}

async function findSeriesGaps(
	repository: Pick<BillingDocumentRepository, 'listSeriesClaims' | 'findDocument'>,
): Promise<Array<string>> {
	const gaps: Array<string> = [];
	for (const series of BILLING_DOCUMENT_SERIES) {
		const claims = await repository.listSeriesClaims(series);
		const numbers = claims.map((claim) => claim.number).sort((left, right) => (left < right ? -1 : 1));
		numbers.forEach((number, index) => {
			if (number !== BigInt(index + 1)) {
				gaps.push(`${series}-${index + 1}`);
			}
		});
		for (const claim of claims) {
			if (!claim.document_id || !(await repository.findDocument(claim.document_id))) {
				gaps.push(`${series}-${claim.number} (${claim.source_key ?? 'no source'})`);
			}
		}
	}
	return gaps;
}

export async function buildBillingDocumentExport(
	repository: Pick<BillingDocumentRepository, 'listMonthDocuments' | 'listSeriesClaims' | 'findDocument'>,
	month: string,
): Promise<BillingDocumentExport> {
	const gaps = await findSeriesGaps(repository);
	if (gaps.length > 0) {
		throw new BillingDocumentSeriesGapError(gaps);
	}
	const documents = await repository.listMonthDocuments(month);
	const lines = [COLUMNS.join(',')];
	for (const row of documents) {
		const values = exportRow(row);
		lines.push(COLUMNS.map((column) => csvField(values[column])).join(','));
	}
	return {month, csvFilename: `fluxer-dokument-${month}.csv`, csv: `${lines.join('\r\n')}\r\n`, documents};
}

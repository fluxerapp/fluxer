// SPDX-License-Identifier: AGPL-3.0-or-later

export interface BillingDocumentNumberRow {
	series: string;
	number: bigint;
	last_number: bigint | null;
	source_key: string | null;
	document_id: bigint | null;
}

export interface BillingDocumentRow {
	document_id: bigint;
	series: string;
	number: bigint;
	display_number: string;
	kind: string;
	flow: string;
	layout: string;
	issued_at: Date;
	supply_date: string | null;
	period_start: Date | null;
	period_end: Date | null;
	currency: string;
	total_minor: bigint;
	tax_minor: bigint;
	tax_lines: string;
	sek_tax_minor: bigint | null;
	fx_rate: string | null;
	fx_date: string | null;
	buyer: string;
	seller: string;
	language: string;
	original_document_id: bigint | null;
	original_number: string | null;
	reason: string | null;
	source_key: string;
	stripe_refs: string;
	user_id: bigint | null;
	donor_email_hash: string | null;
	content: string;
	content_sha256: string;
	pdf: Buffer;
	pdf_sha256: string;
	render_version: string;
	emailed_at: Date | null;
	archived_at: Date | null;
}

export interface BillingDocumentBySourceRow {
	source_key: string;
	document_id: bigint;
	series: string | null;
	number: bigint | null;
}

export interface BillingDocumentByUserRow {
	user_id: bigint;
	issued_at: Date;
	document_id: bigint;
}

export interface BillingDocumentByMonthRow {
	month: string;
	document_id: bigint;
}

export const BILLING_DOCUMENT_NUMBER_COLUMNS = [
	'series',
	'number',
	'last_number',
	'source_key',
	'document_id',
] as const satisfies ReadonlyArray<keyof BillingDocumentNumberRow>;

export const BILLING_DOCUMENT_COLUMNS = [
	'document_id',
	'series',
	'number',
	'display_number',
	'kind',
	'flow',
	'layout',
	'issued_at',
	'supply_date',
	'period_start',
	'period_end',
	'currency',
	'total_minor',
	'tax_minor',
	'tax_lines',
	'sek_tax_minor',
	'fx_rate',
	'fx_date',
	'buyer',
	'seller',
	'language',
	'original_document_id',
	'original_number',
	'reason',
	'source_key',
	'stripe_refs',
	'user_id',
	'donor_email_hash',
	'content',
	'content_sha256',
	'pdf',
	'pdf_sha256',
	'render_version',
	'emailed_at',
	'archived_at',
] as const satisfies ReadonlyArray<keyof BillingDocumentRow>;

export const BILLING_DOCUMENT_BY_SOURCE_COLUMNS = [
	'source_key',
	'document_id',
	'series',
	'number',
] as const satisfies ReadonlyArray<keyof BillingDocumentBySourceRow>;

export const BILLING_DOCUMENT_BY_USER_COLUMNS = [
	'user_id',
	'issued_at',
	'document_id',
] as const satisfies ReadonlyArray<keyof BillingDocumentByUserRow>;

export const BILLING_DOCUMENT_BY_MONTH_COLUMNS = ['month', 'document_id'] as const satisfies ReadonlyArray<
	keyof BillingDocumentByMonthRow
>;

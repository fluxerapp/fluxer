// SPDX-License-Identifier: AGPL-3.0-or-later

import {SnowflakeStringType, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {z} from 'zod';

export const BillingDocumentKind = z
	.enum(['invoice', 'receipt', 'donation_receipt', 'credit_note', 'refund_note', 'stripe_invoice'])
	.describe(
		'What the document is. Credit and refund notes correct an earlier document. stripe_invoice is an invoice Stripe issued before Fluxer issued its own documents, kept read-only as Stripe issued it',
	);

export const BillingDocumentResponse = z.object({
	id: SnowflakeStringType.describe('The document ID'),
	number: z.string().describe('The document number, for example OP-000123'),
	kind: BillingDocumentKind,
	flow: z.enum(['operator_pass', 'plutonium', 'gift', 'donation']).describe('What was bought or given'),
	issued_at: z.string().describe('When the document was issued, in ISO 8601'),
	total: z.number().int().describe('Total in minor units of the currency, negative for credit and refund notes'),
	currency: z.string().describe('Three-letter ISO currency code'),
	original_number: z
		.string()
		.nullable()
		.describe('For a credit or refund note, the number of the document it corrects'),
});

export type BillingDocumentResponse = z.infer<typeof BillingDocumentResponse>;

export const BillingDocumentListResponse = z.array(BillingDocumentResponse);

export const BillingDocumentListQuery = z.object({
	before: z.iso.datetime().optional().describe('Only documents issued before this time'),
	limit: z.coerce.number().int().min(1).max(100).default(50).describe('How many documents to return'),
});

export const BillingDocumentIdParam = z.object({
	document_id: SnowflakeType.describe('The document ID'),
});

export const BillingDocumentFileResponse = z.file().describe('The stored PDF of the document, byte for byte as issued');

export const AdminBillingDocumentExportQuery = z.object({
	month: z
		.string()
		.regex(/^\d{4}-(0[1-9]|1[0-2])$/)
		.describe('The month to export, as YYYY-MM in Stockholm time'),
});

export const AdminBillingDocumentExportFileResponse = z
	.file()
	.describe('A zip with fluxer-dokument-YYYY-MM.csv and pdf/<number>.pdf for every document issued in the month');

export const AdminBillingDocumentReissueRequest = z.object({
	source_key: z
		.string()
		.regex(/^(cs|in|re|du):[A-Za-z0-9_]+$/)
		.describe(
			'The Stripe source to issue the document for: cs:<checkout session>, in:<invoice>, re:<refund> or du:<lost dispute>',
		),
	user_id: SnowflakeType.optional().describe(
		'The account the document belongs to, when Stripe does not say. Defaults to the account Stripe or the original document names',
	),
});

export type AdminBillingDocumentReissueRequest = z.infer<typeof AdminBillingDocumentReissueRequest>;

export const AdminBillingDocumentLegacyImportRequest = z.object({
	before: z.iso
		.datetime()
		.describe(
			'The cutover time. Only Stripe invoices created before it are imported, so later Stripe invoices that Fluxer documents itself are left out',
		),
	dry_run: z.boolean().describe('Count what would be imported without downloading or storing anything'),
	resume: z
		.boolean()
		.default(false)
		.describe(
			'Continue the last run with the same before and dry_run from where it stopped instead of starting from the newest invoice',
		),
});

export type AdminBillingDocumentLegacyImportRequest = z.infer<typeof AdminBillingDocumentLegacyImportRequest>;

export const AdminBillingDocumentLegacyImportResponse = z.object({
	run: z.string().describe('When the run was started or last resumed, in ISO 8601'),
	before: z.string().describe('The cutover time of the run, in ISO 8601'),
	dry_run: z.boolean().describe('Whether the run only counts'),
	cursor: z.string().nullable().describe('The last Stripe invoice the run has gone through'),
	done: z.boolean().describe('Whether the run has gone through every invoice'),
	updated_at: z.string().describe('When the run last finished a page, in ISO 8601'),
	scanned: z.number().int().describe('Paid Stripe invoices with a PDF the run has looked at'),
	imported: z.number().int().describe('Invoices stored as legacy documents by this run'),
	already_imported: z.number().int().describe('Invoices an earlier run already stored'),
	would_import: z.number().int().describe('Invoices a dry run would store'),
	no_account: z.number().int().describe('Invoices that belong to no Fluxer account, such as donations, left out'),
	fluxer_document: z.number().int().describe('Invoices Fluxer already issued its own document for, left out'),
	failed: z.number().int().describe('Invoices that could not be imported; running the import again retries them'),
});

export type AdminBillingDocumentLegacyImportResponse = z.infer<typeof AdminBillingDocumentLegacyImportResponse>;

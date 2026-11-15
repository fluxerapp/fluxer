// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {alertSupport} from '@app/api/billing/BillingSupportAlert';
import type {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import type {BillingDocumentRow} from '@app/api/database/types/BillingDocumentTypes';
import {Logger} from '@app/api/Logger';
import {extractId} from '@app/api/stripe/StripeUtils';
import type {BillingDocumentFlow} from '@pkgs/email/src/billing_documents/BillingDocumentContent';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type Stripe from 'stripe';
import {z} from 'zod';

export const LEGACY_IMPORT_TASK = 'importLegacyBillingDocuments';
const LEGACY_IMPORT_PAGE_SIZE = 20;
const LEGACY_DOCUMENT_KIND = 'stripe_invoice';
const LEGACY_SERIES = 'STRIPE';
const PROGRESS_KEY = 'billing_documents:legacy_import:progress';
const PDF_MAX_BYTES = 10 * 1024 * 1024;
const PDF_TIMEOUT_MS = 30_000;

export const LegacyImportPayloadSchema = z.object({
	run: z.string(),
	before: z.number().int(),
	dry_run: z.boolean(),
	starting_after: z.string().nullable(),
});

export type LegacyImportPayload = z.infer<typeof LegacyImportPayloadSchema>;

export interface LegacyImportCounts {
	scanned: number;
	imported: number;
	already_imported: number;
	would_import: number;
	no_account: number;
	fluxer_document: number;
	failed: number;
}

export interface LegacyImportProgress extends LegacyImportCounts {
	run: string;
	before: string;
	dry_run: boolean;
	cursor: string | null;
	done: boolean;
	updated_at: string;
}

export interface LegacyImportDeps {
	stripe: Stripe;
	repository: Pick<BillingDocumentRepository, 'findSource' | 'claimSource' | 'findDocument' | 'createDocument'>;
	findUserIdByCustomer: (customerId: string) => Promise<bigint | null>;
	nextId: () => Promise<bigint>;
	fetchPdf?: (url: string) => Promise<Buffer>;
}

export function emptyLegacyImportCounts(): LegacyImportCounts {
	return {scanned: 0, imported: 0, already_imported: 0, would_import: 0, no_account: 0, fluxer_document: 0, failed: 0};
}

function sha256(value: string | Uint8Array): string {
	return createHash('sha256').update(value).digest('hex');
}

function numericId(value: string | null | undefined): bigint | null {
	return value && /^\d+$/.test(value) ? BigInt(value) : null;
}

function invoiceMetadata(invoice: Stripe.Invoice): Record<string, string> {
	return {...invoice.metadata, ...invoice.parent?.subscription_details?.metadata};
}

function legacyInvoiceFlow(invoice: Stripe.Invoice): BillingDocumentFlow {
	const metadata = invoiceMetadata(invoice);
	if (metadata.operator_pass === 'true') return 'operator_pass';
	if (metadata.is_donation === 'true') return 'donation';
	if (metadata.is_gift === 'true') return 'gift';
	return 'plutonium';
}

async function downloadStripePdf(url: string): Promise<Buffer> {
	const response = await fetch(url, {signal: AbortSignal.timeout(PDF_TIMEOUT_MS)});
	if (!response.ok) {
		throw new Error(`Stripe invoice PDF download failed with ${response.status}`);
	}
	const bytes = Buffer.from(await response.arrayBuffer());
	if (bytes.byteLength > PDF_MAX_BYTES || bytes.subarray(0, 5).toString('latin1') !== '%PDF-') {
		throw new Error('Stripe invoice download is not a PDF');
	}
	return bytes;
}

async function resolveUserId(invoice: Stripe.Invoice, deps: LegacyImportDeps): Promise<bigint | null> {
	const metadata = invoiceMetadata(invoice);
	const fromMetadata = numericId(metadata.user_id) ?? numericId(metadata.operator_pass_purchaser_id);
	if (fromMetadata != null) {
		return fromMetadata;
	}
	const customerId = extractId(invoice.customer);
	return customerId ? deps.findUserIdByCustomer(customerId) : null;
}

function legacyRow(
	invoice: Stripe.Invoice & {id: string},
	documentId: bigint,
	userId: bigint,
	pdf: Buffer,
): BillingDocumentRow {
	const issuedAt = new Date((invoice.status_transitions?.finalized_at ?? invoice.created) * 1000);
	const number = invoice.number ?? invoice.id;
	const references = {stripeInvoiceId: invoice.id, customerId: extractId(invoice.customer)};
	const content = JSON.stringify({
		kind: LEGACY_DOCUMENT_KIND,
		number,
		issuedAt: issuedAt.toISOString(),
		currency: invoice.currency.toUpperCase(),
		totalMinor: invoice.total,
		references,
	});
	return {
		document_id: documentId,
		series: LEGACY_SERIES,
		number: 0n,
		display_number: number,
		kind: LEGACY_DOCUMENT_KIND,
		flow: legacyInvoiceFlow(invoice),
		layout: 'stripe',
		issued_at: issuedAt,
		supply_date: null,
		period_start: null,
		period_end: null,
		currency: invoice.currency.toUpperCase(),
		total_minor: BigInt(invoice.total),
		tax_minor: BigInt((invoice.total_taxes ?? []).reduce((sum, tax) => sum + tax.amount, 0)),
		tax_lines: '[]',
		sek_tax_minor: null,
		fx_rate: null,
		fx_date: null,
		buyer: JSON.stringify({email: invoice.customer_email ?? null, name: invoice.customer_name ?? null}),
		seller: '{}',
		language: 'en',
		original_document_id: null,
		original_number: null,
		reason: null,
		source_key: `li:${invoice.id}`,
		stripe_refs: JSON.stringify(references),
		user_id: userId,
		donor_email_hash: null,
		content,
		content_sha256: sha256(content),
		pdf,
		pdf_sha256: sha256(pdf),
		render_version: 'stripe',
		emailed_at: null,
		archived_at: null,
	};
}

async function importInvoice(
	invoice: Stripe.Invoice,
	dryRun: boolean,
	deps: LegacyImportDeps,
	counts: LegacyImportCounts,
): Promise<void> {
	const invoiceId = invoice.id;
	if (!invoiceId || invoice.status !== 'paid' || invoice.total <= 0 || !invoice.invoice_pdf) {
		return;
	}
	counts.scanned++;
	if (await deps.repository.findSource(`in:${invoiceId}`)) {
		counts.fluxer_document++;
		return;
	}
	const existing = await deps.repository.findSource(`li:${invoiceId}`);
	if (existing && (await deps.repository.findDocument(existing.document_id))) {
		counts.already_imported++;
		return;
	}
	const userId = await resolveUserId(invoice, deps);
	if (userId == null) {
		counts.no_account++;
		return;
	}
	if (dryRun) {
		counts.would_import++;
		return;
	}
	const pdf = await (deps.fetchPdf ?? downloadStripePdf)(invoice.invoice_pdf);
	const claimed = await deps.repository.claimSource(`li:${invoiceId}`, await deps.nextId());
	await deps.repository.createDocument(legacyRow({...invoice, id: invoiceId}, claimed.document_id, userId, pdf), {
		indexMonth: false,
	});
	counts.imported++;
}

export async function importLegacyInvoicePage(
	payload: LegacyImportPayload,
	deps: LegacyImportDeps,
): Promise<{counts: LegacyImportCounts; cursor: string | null; hasMore: boolean}> {
	const page = await deps.stripe.invoices.list({
		status: 'paid',
		created: {lt: payload.before},
		limit: LEGACY_IMPORT_PAGE_SIZE,
		...(payload.starting_after ? {starting_after: payload.starting_after} : {}),
	});
	const counts = emptyLegacyImportCounts();
	for (const invoice of page.data) {
		try {
			await importInvoice(invoice, payload.dry_run, deps, counts);
		} catch (error) {
			counts.failed++;
			Logger.error(
				{error, invoiceId: invoice.id, billingDocumentAlert: true},
				'Could not import a Stripe invoice PDF into billing history; run the import again to retry it',
			);
			await alertSupport({
				dedupeKey: `legacy-import:${payload.run}`,
				subject: 'Legacy Stripe invoice import had failures',
				summary: `At least one Stripe invoice PDF could not be imported into billing history in import run ${payload.run}. The import progress shows how many failed. Run the import again to retry them.`,
				facts: {invoiceId: invoice.id, error},
			});
		}
	}
	return {counts, cursor: page.data.at(-1)?.id ?? payload.starting_after, hasMore: page.has_more};
}

export async function readLegacyImportProgress(kv: Pick<IKVProvider, 'get'>): Promise<LegacyImportProgress | null> {
	const raw = await kv.get(PROGRESS_KEY);
	return raw ? (JSON.parse(raw) as LegacyImportProgress) : null;
}

export async function writeLegacyImportProgress(
	kv: Pick<IKVProvider, 'set'>,
	progress: LegacyImportProgress,
): Promise<void> {
	await kv.set(PROGRESS_KEY, JSON.stringify(progress));
}

export function addLegacyImportCounts(
	progress: LegacyImportProgress,
	counts: LegacyImportCounts,
): LegacyImportProgress {
	const next = {...progress};
	for (const key of Object.keys(counts) as Array<keyof LegacyImportCounts>) {
		next[key] = progress[key] + counts[key];
	}
	return next;
}

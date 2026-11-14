// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditCoverageCase} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import {emptyLegacyImportCounts, writeLegacyImportProgress} from '@app/api/billing_documents/LegacyStripeInvoiceImport';
import type {BillingDocumentRow} from '@app/api/database/types/BillingDocumentTypes';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';

async function seedDocument(documentId: bigint, userId: bigint | null): Promise<BillingDocumentRow> {
	return new BillingDocumentRepository().createDocument({
		document_id: documentId,
		series: 'PL',
		number: documentId,
		display_number: `PL-${documentId}`,
		kind: 'invoice',
		flow: 'plutonium',
		layout: 'simplified',
		issued_at: new Date('2026-11-05T10:00:00Z'),
		supply_date: null,
		period_start: null,
		period_end: null,
		currency: 'EUR',
		total_minor: 499n,
		tax_minor: 100n,
		tax_lines: '[]',
		sek_tax_minor: null,
		fx_rate: null,
		fx_date: null,
		buyer: '{}',
		seller: '{}',
		language: 'en',
		original_document_id: null,
		original_number: null,
		reason: null,
		source_key: `in:audit_${documentId}`,
		stripe_refs: '{}',
		user_id: userId,
		donor_email_hash: null,
		content: JSON.stringify({buyer: {}, taxLines: [], references: {}, vatTreatment: 'charged'}),
		content_sha256: 'sha',
		pdf: Buffer.from('%PDF-1.7'),
		pdf_sha256: 'sha',
		render_version: '1',
		emailed_at: null,
		archived_at: null,
	});
}

export const BillingDocumentAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'GET',
		route: '/admin/billing-documents/export',
		async prepare() {
			await seedDocument(910001n, null);
			return {
				request: {path: '/admin/billing-documents/export?month=2026-11'},
				expected: {
					action: 'get_billing_document_export',
					targetType: 'billing_documents',
					targetId: '0',
					metadata: {month: '2026-11', documents: '1'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/billing-documents/reissue',
		async prepare() {
			await seedDocument(910004n, null);
			await new BillingDocumentRepository().claimSource('in:audit_910004', 910004n);
			return {
				request: {path: '/admin/billing-documents/reissue', body: {source_key: 'in:audit_910004'}},
				expected: {
					action: 'reissue_billing_document',
					targetType: 'billing_document',
					targetId: '910004',
					metadata: {source_key: 'in:audit_910004', number: 'PL-910004'},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/billing-documents/legacy-import',
		async prepare() {
			return {
				request: {
					path: '/admin/billing-documents/legacy-import',
					body: {before: '2026-11-01T00:00:00.000Z', dry_run: true},
				},
				expected: {
					action: 'start_billing_document_legacy_import',
					targetType: 'billing_documents',
					targetId: '0',
					metadata: {before: '2026-11-01T00:00:00.000Z', dry_run: 'true', resumed: 'false'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/billing-documents/legacy-import',
		async prepare() {
			await writeLegacyImportProgress(getKVClient(), {
				...emptyLegacyImportCounts(),
				run: '2026-11-01T00:00:00.000Z',
				before: '2026-11-01T00:00:00.000Z',
				dry_run: true,
				cursor: null,
				done: false,
				updated_at: '2026-11-01T00:00:00.000Z',
			});
			return {
				request: {path: '/admin/billing-documents/legacy-import'},
				expected: {
					action: 'get_billing_document_legacy_import',
					targetType: 'billing_documents',
					targetId: '0',
					metadata: {dry_run: 'true', done: 'false', scanned: '0'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/billing-documents/:document_id/pdf',
		async prepare() {
			await seedDocument(910002n, null);
			return {
				request: {path: '/admin/billing-documents/910002/pdf'},
				expected: {
					action: 'get_billing_document',
					targetType: 'billing_document',
					targetId: '910002',
					metadata: {number: 'PL-910002'},
				},
			};
		},
	},
	{
		method: 'GET',
		route: '/admin/users/:user_id/billing-documents',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			await seedDocument(910003n, BigInt(target.userId));
			return {
				request: {path: `/admin/users/${target.userId}/billing-documents`},
				expected: {
					action: 'list_user_billing_documents',
					targetType: 'user',
					targetId: target.userId,
					metadata: {result_count: '1'},
				},
			};
		},
	},
];

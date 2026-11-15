// SPDX-License-Identifier: AGPL-3.0-or-later

import type {BillingDocumentRow} from '@app/api/database/types/BillingDocumentTypes';
import type {BillingDocumentResponse} from '@fluxer/schema/src/domains/billing_documents/BillingDocumentSchemas';
import {billingDocumentFilename} from '@pkgs/email/src/billing_documents/BillingDocumentContent';

export function mapBillingDocument(row: BillingDocumentRow): BillingDocumentResponse {
	return {
		id: row.document_id.toString(),
		number: row.display_number,
		kind: row.kind as BillingDocumentResponse['kind'],
		flow: row.flow as BillingDocumentResponse['flow'],
		issued_at: row.issued_at.toISOString(),
		total: Number(row.total_minor),
		currency: row.currency,
		original_number: row.original_number,
	};
}

export function billingDocumentPdfResponse(row: BillingDocumentRow): Response {
	const bytes = new Uint8Array(row.pdf);
	const headers = new Headers();
	headers.set('Content-Type', 'application/pdf');
	headers.set('Content-Disposition', `attachment; filename="${billingDocumentFilename(row.display_number)}"`);
	headers.set('Content-Length', String(bytes.byteLength));
	headers.set('Cache-Control', 'private, no-store');
	return new Response(bytes, {status: 200, headers});
}

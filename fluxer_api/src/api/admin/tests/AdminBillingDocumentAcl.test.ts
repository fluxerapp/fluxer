// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, test} from 'vitest';

const AUDIT_REASON = 'billing document access test';

async function seedDocument(documentId: bigint, userId: bigint): Promise<void> {
	await new BillingDocumentRepository().createDocument({
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
		source_key: `in:acl_${documentId}`,
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

describe('Admin billing document access', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	test('a lookup-only admin can list documents but not download the PDFs that show names and addresses', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP]);
		const buyer = await createTestAccount(harness);
		await seedDocument(920001n, BigInt(buyer.userId));
		await createBuilder(harness, admin.token)
			.get(`/admin/users/${buyer.userId}/billing-documents`)
			.header('X-Audit-Log-Reason', AUDIT_REASON)
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, admin.token)
			.get('/admin/billing-documents/920001/pdf')
			.header('X-Audit-Log-Reason', AUDIT_REASON)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		await createBuilder(harness, admin.token)
			.get('/admin/billing-documents/export?month=2026-11')
			.header('X-Audit-Log-Reason', AUDIT_REASON)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('an admin who may see email addresses can download a document', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP, AdminACLs.USER_VIEW_EMAIL]);
		const buyer = await createTestAccount(harness);
		await seedDocument(920002n, BigInt(buyer.userId));
		await createBuilder(harness, admin.token)
			.get('/admin/billing-documents/920002/pdf')
			.header('X-Audit-Log-Reason', AUDIT_REASON)
			.expect(HTTP_STATUS.OK)
			.executeRaw();
	});
});

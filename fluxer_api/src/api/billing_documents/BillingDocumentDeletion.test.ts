// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {deleteAccount, setPendingDeletionAt} from '@app/api/user/tests/UserTestUtils';
import {afterAll, beforeAll, beforeEach, expect, test} from 'vitest';

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
		source_key: `in:deletion_${documentId}`,
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

test('account deletion removes the user index of billing documents and keeps the documents', async () => {
	const buyer = await createTestAccount(harness);
	await seedDocument(930001n, BigInt(buyer.userId));
	const repository = new BillingDocumentRepository();
	expect(await repository.listUserDocuments(BigInt(buyer.userId), null, 10)).toHaveLength(1);
	await deleteAccount(harness, buyer.token, buyer.password);
	await setPendingDeletionAt(harness, buyer.userId, new Date(Date.now() - 60_000));
	await createBuilderWithoutAuth(harness)
		.post(`/test/worker/process-pending-deletion/${buyer.userId}`)
		.expect(HTTP_STATUS.OK)
		.execute();
	expect(await repository.listUserDocuments(BigInt(buyer.userId), null, 10)).toEqual([]);
	expect((await repository.findDocument(930001n))?.display_number).toBe('PL-930001');
});

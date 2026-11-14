// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {BillingDocumentRepository} from '@app/api/billing_documents/BillingDocumentRepository';
import {
	importLegacyInvoicePage,
	LEGACY_IMPORT_TASK,
	type LegacyImportDeps,
	type LegacyImportPayload,
	readLegacyImportProgress,
} from '@app/api/billing_documents/LegacyStripeInvoiceImport';
import {getKVClient, setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type Stripe from 'stripe';
import {afterAll, beforeAll, expect, test, vi} from 'vitest';

let harness: ApiTestHarness;

beforeAll(async () => {
	harness = await createApiTestHarness();
});

afterAll(async () => {
	await harness.shutdown();
});

const BEFORE = Math.floor(Date.parse('2026-11-01T00:00:00Z') / 1000);

function invoice(id: string, patch: Record<string, unknown> = {}): Stripe.Invoice {
	return {
		id,
		object: 'invoice',
		number: `ABCD-${id}`,
		status: 'paid',
		total: 499,
		currency: 'eur',
		created: BEFORE - 86_400,
		status_transitions: {finalized_at: BEFORE - 86_400},
		customer: `cus_${id}`,
		customer_email: 'buyer@example.com',
		customer_name: null,
		invoice_pdf: `https://pay.stripe.com/invoice/${id}/pdf`,
		metadata: {},
		parent: null,
		total_taxes: [{amount: 100}],
		...patch,
	} as unknown as Stripe.Invoice;
}

function deps(invoices: Array<Stripe.Invoice>, start: bigint, fetchPdf: LegacyImportDeps['fetchPdf']) {
	let nextId = start;
	const list = vi.fn(async () => ({data: invoices, has_more: false}));
	return {
		list,
		deps: {
			stripe: {invoices: {list}} as unknown as Stripe,
			repository: new BillingDocumentRepository(),
			findUserIdByCustomer: async (customerId: string) => (customerId === 'cus_linked' ? 77n : null),
			nextId: async () => nextId++,
			fetchPdf,
		} satisfies LegacyImportDeps,
	};
}

test('imports each account its past Stripe invoice PDFs once, read-only and outside the monthly export', async () => {
	const repository = new BillingDocumentRepository();
	await repository.claimSource('in:in_fluxer', 8_100_999n);
	const invoices = [
		invoice('in_meta', {
			metadata: {user_id: '42'},
			parent: {subscription_details: {metadata: {user_id: '42'}}},
		}),
		invoice('in_customer', {customer: 'cus_linked', metadata: {is_gift: 'true'}}),
		invoice('in_donor', {metadata: {is_donation: 'true'}}),
		invoice('in_pass', {metadata: {operator_pass: 'true', operator_pass_purchaser_id: '43'}}),
		invoice('in_fluxer', {metadata: {user_id: '42'}}),
		invoice('in_unpaid', {status: 'open', metadata: {user_id: '42'}}),
	];
	const pdf = Buffer.from('%PDF-1.4 stripe invoice');
	const fetchPdf = vi.fn(async () => pdf);
	const payload: LegacyImportPayload = {run: 'r1', before: BEFORE, dry_run: true, starting_after: null};

	const dry = deps(invoices, 8_100_000n, fetchPdf);
	const dryRun = await importLegacyInvoicePage(payload, dry.deps);
	expect(dryRun.counts).toMatchObject({scanned: 5, would_import: 3, imported: 0, no_account: 1, fluxer_document: 1});
	expect(fetchPdf).not.toHaveBeenCalled();
	expect(await repository.findSource('li:in_meta')).toBeNull();
	expect(dry.list).toHaveBeenCalledWith(expect.objectContaining({status: 'paid', created: {lt: BEFORE}}));

	const run = deps(invoices, 8_100_000n, fetchPdf);
	const first = await importLegacyInvoicePage({...payload, dry_run: false}, run.deps);
	expect(first.counts).toMatchObject({imported: 3, no_account: 1, fluxer_document: 1, failed: 0});
	expect(first.cursor).toBe('in_unpaid');

	const listed = await repository.listUserDocuments(42n, null, 10);
	expect(listed.map((row) => row.display_number)).toEqual(['ABCD-in_meta']);
	expect(listed[0]).toMatchObject({kind: 'stripe_invoice', flow: 'plutonium', currency: 'EUR', total_minor: 499n});
	expect(Buffer.compare(listed[0]!.pdf, pdf)).toBe(0);
	expect((await repository.listUserDocuments(77n, null, 10))[0]).toMatchObject({flow: 'gift'});
	expect((await repository.listUserDocuments(43n, null, 10))[0]).toMatchObject({flow: 'operator_pass'});
	expect(await repository.listMonthDocuments('2026-10')).toEqual([]);

	fetchPdf.mockClear();
	const again = await importLegacyInvoicePage({...payload, dry_run: false}, deps(invoices, 8_100_100n, fetchPdf).deps);
	expect(again.counts).toMatchObject({imported: 0, already_imported: 3});
	expect(fetchPdf).not.toHaveBeenCalled();
	expect(await repository.listUserDocuments(42n, null, 10)).toHaveLength(1);
});

test('an invoice whose PDF cannot be fetched is counted as failed and imported by the next run', async () => {
	const repository = new BillingDocumentRepository();
	const invoices = [invoice('in_flaky', {metadata: {user_id: '91'}})];
	const payload: LegacyImportPayload = {run: 'r2', before: BEFORE, dry_run: false, starting_after: null};
	const failing = await importLegacyInvoicePage(
		payload,
		deps(invoices, 8_200_000n, async () => {
			throw new Error('timeout');
		}).deps,
	);
	expect(failing.counts).toMatchObject({failed: 1, imported: 0});
	expect(await repository.findSource('li:in_flaky')).toBeNull();
	const retried = await importLegacyInvoicePage(
		payload,
		deps(invoices, 8_200_100n, async () => Buffer.from('%PDF-1.4 retry')).deps,
	);
	expect(retried.counts).toMatchObject({imported: 1, failed: 0});
	expect(await repository.listUserDocuments(91n, null, 10)).toHaveLength(1);
});

test('a cutover with milliseconds starts a run the worker accepts', async () => {
	const worker = new RecordingWorkerService();
	setInjectedWorkerService(worker);
	const admin = await createTestAccount(harness);
	await setUserACLs(harness, admin, [AdminACLs.AUTHENTICATE, AdminACLs.USER_UPDATE_FLAGS]);
	await createBuilder(harness, admin.token)
		.post('/admin/billing-documents/legacy-import')
		.header('X-Audit-Log-Reason', 'legacy import test')
		.body({before: '2026-11-01T00:00:00.789Z', dry_run: true})
		.expect(HTTP_STATUS.OK)
		.execute();
	const queued = worker.byTask(LEGACY_IMPORT_TASK).at(-1)!.payload as LegacyImportPayload;
	const progress = await readLegacyImportProgress(getKVClient());
	expect(progress?.before).toBe(new Date(queued.before * 1000).toISOString());
});

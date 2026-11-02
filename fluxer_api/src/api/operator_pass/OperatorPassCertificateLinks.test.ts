// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {Db} from '@app/api/database/CassandraTypes';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getCacheService} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassCertificateService} from '@app/api/operator_pass/OperatorPassCertificateService';
import {endOperatorPassesForDeletion} from '@app/api/operator_pass/OperatorPassHoldingService';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {
	OperatorPassCertificateLinkResponse,
	OperatorPassPurchaseListResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {afterAll, beforeAll, expect, test} from 'vitest';

let harness: ApiTestHarness;

beforeAll(async () => {
	harness = await createApiTestHarness();
});

afterAll(async () => {
	await harness.shutdown();
});

const repository = new OperatorPassRepository();

async function buyPasses(names: Array<string>): Promise<{buyer: TestAccount; purchase: OperatorPassPurchaseRow}> {
	const buyer = await createTestAccount(harness);
	const purchaserId = createUserID(BigInt(buyer.userId));
	const paidAt = new Date('2026-11-02T09:41:00Z');
	const purchase = {
		checkout_session_id: `cs_test_links_${randomUUID()}`,
		purchaser_id: purchaserId,
		quantity: names.length,
		status: 'completed',
		phase: 'standard',
		amount_minor: 2900 * names.length,
		currency: 'EUR',
		quote_honored: false,
		created_at: paidAt,
		purchased_at: paidAt,
		delivered_at: paidAt,
		certificate_names: names,
		operator_pass_terms_version: '2026-10-31',
		version: 1,
	} as OperatorPassPurchaseRow;
	await repository.createPurchase(purchase);
	await new OperatorPassCertificateService(repository, getCacheService(), getKVClient()).issueForPurchase(purchase);
	for (let index = 1; index <= names.length; index++) {
		await repository.createUnit({
			pass_id: `${purchase.checkout_session_id}:${index}`,
			purchase_id: purchase.checkout_session_id,
			buyer_id: purchaserId,
			holder_id: purchaserId,
			source: 'purchase',
			status: 'held',
			transferable: true,
			delivered_at: paidAt,
			first_delivered_at: paidAt,
			paused_at: null,
			ended_at: null,
			end_reason: null,
			withdrawal_country: null,
			withdrawal_days: null,
			version: 1,
		});
	}
	return {buyer, purchase};
}

async function tokenOf(passId: string): Promise<string> {
	return (await repository.findLatestCertificate(passId))!.link_token!;
}

function getPage(token: string, suffix = '') {
	return createBuilderWithoutAuth(harness).get(`/operator-pass/certificates/${token}${suffix}`).executeRaw();
}

test('every page gets its own link, which shows only the public details and opens the stored PDF', async () => {
	const {buyer, purchase} = await buyPasses(['Ada Lovelace', 'Grace Hopper']);
	const first = await repository.findLatestCertificate(`${purchase.checkout_session_id}:1`);
	const second = await repository.findLatestCertificate(`${purchase.checkout_session_id}:2`);
	expect(first?.link_token).toMatch(/^[A-Za-z0-9_-]{22}$/);
	expect(second?.link_token).toMatch(/^[A-Za-z0-9_-]{22}$/);
	expect(first?.link_token).not.toBe(second?.link_token);

	const page = await getPage(first!.link_token!);
	expect(page.response.status).toBe(200);
	expect(page.json).toEqual({
		number: first!.number,
		status: 'valid',
		name: 'Ada Lovelace',
		source: 'purchase',
		paid_at: '2026-11-02T09:41:00.000Z',
		issued_at: first!.issued_at.toISOString(),
	});
	expect(page.response.headers.get('cache-control')).toBe('private, no-store');
	expect(page.response.headers.get('x-robots-tag')).toBe('noindex, nofollow');

	const file = await harness.requestJson({path: `/operator-pass/certificates/${first!.link_token}/pdf`});
	expect(file.status).toBe(200);
	expect(file.headers.get('content-type')).toBe('application/pdf');
	expect(file.headers.get('content-disposition')).toBe(
		`inline; filename="fluxer-operator-certificate-${String(first!.number).padStart(6, '0')}.pdf"`,
	);
	expect(file.headers.get('cache-control')).toBe('private, no-store');
	expect(Buffer.from(await file.arrayBuffer()).equals(first!.pdf)).toBe(true);

	const unknown = await getPage('A'.repeat(22));
	expect(unknown.response.status).toBe(404);
	expect(unknown.response.headers.get('cache-control')).toBe('private, no-store');
	expect((await getPage('not-a-token')).response.status).toBe(400);
	const selfHosted = Config.instance.selfHosted;
	Config.instance.selfHosted = true;
	try {
		expect((await getPage(first!.link_token!)).response.status).toBe(404);
	} finally {
		Config.instance.selfHosted = selfHosted;
	}

	const purchases = await createBuilder<OperatorPassPurchaseListResponse>(harness, buyer.token)
		.get('/operator-pass/purchases')
		.execute();
	expect(purchases[0]?.pages.map((listed) => listed.certificate_url)).toEqual([
		`${Config.endpoints.marketing}/pass/c/${first!.link_token}`,
		`${Config.endpoints.marketing}/pass/c/${second!.link_token}`,
	]);
});

test('a cancelled page shows only that it was cancelled, and a lost dispute cancels every page', async () => {
	const {purchase} = await buyPasses(['Ada Lovelace', 'Grace Hopper']);
	const withdrawnId = `${purchase.checkout_session_id}:1`;
	await repository.updateUnit((await repository.findUnit(withdrawnId))!, {
		status: Db.set('withdrawn'),
		end_reason: Db.set('withdrawal'),
		ended_at: Db.set(new Date()),
	});
	const withdrawn = await getPage(await tokenOf(withdrawnId));
	expect(withdrawn.json).toMatchObject({status: 'cancelled', name: null, issued_at: null});
	expect((await getPage(await tokenOf(withdrawnId), '/pdf')).response.status).toBe(404);
	const other = await tokenOf(`${purchase.checkout_session_id}:2`);
	expect((await getPage(other)).json).toMatchObject({status: 'valid', name: 'Grace Hopper'});

	await repository.updatePurchase((await repository.findPurchase(purchase.checkout_session_id))!, {
		status: Db.set('dispute_lost'),
	});
	expect((await getPage(other)).json).toMatchObject({status: 'cancelled', name: null, issued_at: null});
	expect((await getPage(other, '/pdf')).response.status).toBe(404);
});

test('only the buyer can replace a link, the old link stops at once, and the copy keeps its number, date and name', async () => {
	const {buyer, purchase} = await buyPasses(['Ada Lovelace', 'Grace Hopper']);
	const passId = `${purchase.checkout_session_id}:1`;
	const before = (await repository.findLatestCertificate(passId))!;
	const reset = (token: string, id = passId) =>
		createBuilder<OperatorPassCertificateLinkResponse>(harness, token)
			.post(`/operator-pass/passes/${encodeURIComponent(id)}/certificate-link/reset`)
			.body({});

	const stranger = await createTestAccount(harness);
	await reset(stranger.token).expect(404).execute();
	const giftedId = `${purchase.checkout_session_id}:2`;
	await repository.updateUnit((await repository.findUnit(giftedId))!, {
		holder_id: Db.set(createUserID(BigInt(stranger.userId))),
		source: Db.set('transfer'),
	});
	await reset(stranger.token, giftedId).expect(404).execute();

	const {certificate_url: url} = await reset(buyer.token).execute();

	const after = (await repository.findLatestCertificate(passId))!;
	expect(url).toBe(`${Config.endpoints.marketing}/pass/c/${after.link_token}`);
	expect(after.link_token).not.toBe(before.link_token);
	expect(after).toMatchObject({
		version: before.version + 1,
		number: before.number,
		paid_at: before.paid_at,
		holder_name: before.holder_name,
		reissue_reason: 'link',
	});
	expect((await getPage(before.link_token!)).response.status).toBe(404);
	expect((await getPage(before.link_token!, '/pdf')).response.status).toBe(404);
	expect((await getPage(after.link_token!)).json).toMatchObject({status: 'valid', name: 'Ada Lovelace'});

	await repository.updateUnit((await repository.findUnit(passId))!, {
		status: Db.set('withdrawn'),
		end_reason: Db.set('withdrawal'),
		ended_at: Db.set(new Date()),
	});
	await reset(buyer.token).expect(400).execute();
});

test('a name change keeps the link, and deleting the buyer or resetting a preview ends it', async () => {
	const {buyer, purchase} = await buyPasses(['Ada Lovelace', 'Grace Hopper']);
	const passId = `${purchase.checkout_session_id}:1`;
	const token = await tokenOf(passId);
	const certificates = new OperatorPassCertificateService(repository, getCacheService(), getKVClient());
	const copy = await certificates.reissuePage({passId, holderName: 'Ada King', reason: 'name_change'});
	expect(copy.link_token).toBe(token);
	expect((await getPage(token)).json).toMatchObject({name: 'Ada King', issued_at: copy.issued_at.toISOString()});

	const recipient = createUserID(BigInt((await createTestAccount(harness)).userId));
	const giftedId = `${purchase.checkout_session_id}:2`;
	await repository.updateUnit((await repository.findUnit(giftedId))!, {
		holder_id: Db.set(recipient),
		source: Db.set('transfer'),
	});
	await repository.moveUnitHolder(giftedId, purchase.purchaser_id, recipient);
	await endOperatorPassesForDeletion(repository, getCacheService(), recipient);
	expect((await getPage(await tokenOf(giftedId))).json).toMatchObject({status: 'valid'});

	await endOperatorPassesForDeletion(repository, getCacheService(), createUserID(BigInt(buyer.userId)));
	expect((await getPage(token)).response.status).toBe(404);
	expect((await getPage(token, '/pdf')).response.status).toBe(404);

	const preview = await buyPasses(['Ada Lovelace']);
	const previewId = `${preview.purchase.checkout_session_id}:1`;
	const previewToken = await tokenOf(previewId);
	await repository.deleteCertificateRecords(previewId);
	expect(await repository.findCertificateLink(previewToken)).toBeNull();
	expect((await getPage(previewToken)).response.status).toBe(404);
});

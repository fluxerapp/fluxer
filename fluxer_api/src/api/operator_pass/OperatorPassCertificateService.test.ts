// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {createUserID} from '@app/api/BrandedTypes';
import {Db} from '@app/api/database/CassandraTypes';
import type {OperatorPassPurchaseRow} from '@app/api/database/types/OperatorPassTypes';
import {getKVClient} from '@app/api/middleware/ServiceRegistry';
import {getCacheService} from '@app/api/middleware/ServiceSingletons';
import {OperatorPassCertificateService} from '@app/api/operator_pass/OperatorPassCertificateService';
import {OperatorPassRepository} from '@app/api/operator_pass/OperatorPassRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {OperatorPassCertificateNameRejectedError} from '@fluxer/errors/src/domains/operator_pass/OperatorPassCertificateNameRejectedError';
import {combineOperatorPassCertificatePages} from '@pkgs/email/src/operator_pass_certificate/OperatorPassCertificatePdf';
import {PDFDocument} from 'pdf-lib';
import {afterAll, beforeAll, expect, test, vi} from 'vitest';

let harness: ApiTestHarness;

beforeAll(async () => {
	harness = await createApiTestHarness();
});

afterAll(async () => {
	await harness.shutdown();
});

async function holdUnits(repository: OperatorPassRepository, purchase: OperatorPassPurchaseRow): Promise<void> {
	const holderId = createUserID(1n);
	const deliveredAt = new Date('2026-11-01T10:05:00Z');
	for (let index = 1; index <= (purchase.quantity ?? 1); index++) {
		await repository.createUnit({
			pass_id: `${purchase.checkout_session_id}:${index}`,
			purchase_id: purchase.checkout_session_id,
			buyer_id: holderId,
			holder_id: holderId,
			source: 'purchase',
			status: 'held',
			transferable: true,
			delivered_at: deliveredAt,
			first_delivered_at: deliveredAt,
			paused_at: null,
			ended_at: null,
			end_reason: null,
			withdrawal_country: null,
			withdrawal_days: null,
			version: 1,
		});
	}
}

test('a new copy keeps the number and date, and the download serves it', async () => {
	const repository = new OperatorPassRepository();
	const certificates = new OperatorPassCertificateService(repository, getCacheService(), getKVClient());
	const purchase = {
		checkout_session_id: `cs_test_${randomUUID()}`,
		quantity: 1,
		certificate_names: ['Ada Lovelace'],
		purchased_at: new Date('2026-11-01T10:00:00Z'),
		phase: 'early_bird',
		operator_pass_terms_version: '2026-10-31',
	} as OperatorPassPurchaseRow;
	await certificates.issueForPurchase(purchase);
	await holdUnits(repository, purchase);
	const passId = `${purchase.checkout_session_id}:1`;
	const [original] = await repository.listCertificateVersions(passId);

	await expect(
		certificates.reissuePage({passId, holderName: 'Fluxer Team', reason: 'name_change'}),
	).rejects.toBeInstanceOf(OperatorPassCertificateNameRejectedError);

	const copy = await certificates.reissuePage({passId, holderName: 'Ada King', reason: 'name_change'});
	expect(copy).toMatchObject({
		version: 2,
		number: original!.number,
		paid_at: original!.paid_at,
		terms_version: original!.terms_version,
		holder_name: 'Ada King',
		reissue_reason: 'name_change',
	});
	expect((await repository.listCertificateVersions(passId)).map((row) => row.holder_name)).toEqual([
		'Ada Lovelace',
		'Ada King',
	]);
	await certificates.issueForPurchase(purchase);
	expect(await repository.listCertificateVersions(passId)).toHaveLength(2);

	const file = await certificates.loadPurchaseFile(purchase);
	expect(file?.bytes).toEqual(
		await combineOperatorPassCertificatePages([new Uint8Array(copy.pdf)], copy.number, copy.issued_at),
	);
});

test('cancelled pages leave the download, and a fully cancelled purchase has none', async () => {
	const repository = new OperatorPassRepository();
	const certificates = new OperatorPassCertificateService(repository, getCacheService(), getKVClient());
	const purchase = {
		checkout_session_id: `cs_test_${randomUUID()}`,
		quantity: 2,
		certificate_names: ['Ada Lovelace', 'Grace Hopper'],
		purchased_at: new Date('2026-11-01T10:00:00Z'),
		phase: 'standard',
		operator_pass_terms_version: '2026-10-31',
	} as OperatorPassPurchaseRow;
	await certificates.issueForPurchase(purchase);
	await holdUnits(repository, purchase);
	const [first, second] = await repository.listCertificatesByPurchase(purchase.checkout_session_id);

	await repository.markCertificateCancelled(first!.pass_id, new Date('2026-11-02T10:00:00Z'));
	const file = await certificates.loadPurchaseFile(purchase);
	expect(file?.bytes).toEqual(
		await combineOperatorPassCertificatePages([new Uint8Array(second!.pdf)], second!.number, second!.issued_at),
	);

	await repository.markCertificateCancelled(second!.pass_id, new Date('2026-11-02T10:00:00Z'));
	expect(await certificates.loadPurchaseFile(purchase)).toBeNull();
});

test('a page whose pass ended in a refund leaves the download even if the cancellation was never written', async () => {
	const repository = new OperatorPassRepository();
	const certificates = new OperatorPassCertificateService(repository, getCacheService(), getKVClient());
	const purchase = {
		checkout_session_id: `cs_test_${randomUUID()}`,
		quantity: 2,
		certificate_names: ['Ada Lovelace', 'Grace Hopper'],
		purchased_at: new Date('2026-11-01T10:00:00Z'),
		phase: 'standard',
		operator_pass_terms_version: '2026-10-31',
	} as OperatorPassPurchaseRow;
	await certificates.issueForPurchase(purchase);
	const [first, second] = await repository.listCertificatesByPurchase(purchase.checkout_session_id);
	expect(await certificates.loadPurchaseFile(purchase)).toBeNull();

	await holdUnits(repository, purchase);
	const endedAt = new Date('2026-11-02T10:00:00Z');
	const endUnit = async (passId: string) => {
		const unit = (await repository.findUnit(passId))!;
		await repository.updateUnit(unit, {
			status: Db.set('revoked'),
			ended_at: Db.set(endedAt),
			end_reason: Db.set('refund'),
		});
	};

	await endUnit(first!.pass_id);
	const file = await certificates.loadPurchaseFile(purchase);
	expect(file?.bytes).toEqual(
		await combineOperatorPassCertificatePages([new Uint8Array(second!.pdf)], second!.number, second!.issued_at),
	);
	const [healed] = await repository.listCertificateVersions(first!.pass_id);
	expect(healed!.cancelled_at).toEqual(endedAt);

	await endUnit(second!.pass_id);
	expect(await certificates.loadPurchaseFile(purchase)).toBeNull();
});

test('clearing personal data keeps the proof of delivery and blocks a new copy', async () => {
	const repository = new OperatorPassRepository();
	const certificates = new OperatorPassCertificateService(repository, getCacheService(), getKVClient());
	const purchase = {
		checkout_session_id: `cs_test_${randomUUID()}`,
		quantity: 1,
		certificate_names: ['Ada Lovelace'],
		purchased_at: new Date('2026-11-01T10:00:00Z'),
		phase: 'standard',
		operator_pass_terms_version: '2026-10-31',
	} as OperatorPassPurchaseRow;
	await certificates.issueForPurchase(purchase);
	const passId = `${purchase.checkout_session_id}:1`;
	const [original] = await repository.listCertificateVersions(passId);

	await repository.clearCertificatePersonalData(passId);
	const [cleared] = await repository.listCertificateVersions(passId);
	expect(cleared).toMatchObject({
		holder_name: '',
		number: original!.number,
		sha256: original!.sha256,
		paid_at: original!.paid_at,
		issued_at: original!.issued_at,
	});
	expect(cleared!.pdf.length).toBe(0);
	expect(await certificates.loadPurchaseFile(purchase)).toBeNull();
	await expect(certificates.reissuePage({passId, holderName: 'Ada King', reason: 'name_change'})).rejects.toThrow();
});

test('concurrent purchases get distinct numbers with one claim each', async () => {
	const repository = new OperatorPassRepository();
	const certificates = new OperatorPassCertificateService(repository, getCacheService(), getKVClient());
	const purchaseFor = () =>
		({
			checkout_session_id: `cs_test_${randomUUID()}`,
			quantity: 1,
			certificate_names: ['Ada Lovelace'],
			purchased_at: new Date('2026-11-01T10:00:00Z'),
			phase: 'standard',
			operator_pass_terms_version: '2026-10-31',
		}) as OperatorPassPurchaseRow;
	await certificates.issueForPurchase(purchaseFor());

	const claims = vi.spyOn(repository, 'claimCertificateNumber');
	const purchases = Array.from({length: 20}, purchaseFor);
	await Promise.all(purchases.map((purchase) => certificates.issueForPurchase(purchase)));

	const numbers = await Promise.all(
		purchases.map(async (purchase) => {
			const [row] = await repository.listCertificateVersions(`${purchase.checkout_session_id}:1`);
			return row!.number;
		}),
	);
	expect(new Set(numbers).size).toBe(20);
	expect(claims.mock.calls.length).toBeLessThanOrEqual(20);
});

test('a retry after the purchase index write failed repairs the index before every page counts as delivered', async () => {
	const repository = new OperatorPassRepository();
	const certificates = new OperatorPassCertificateService(repository, getCacheService(), getKVClient());
	const purchase = {
		checkout_session_id: `cs_test_${randomUUID()}`,
		quantity: 2,
		status: 'pending',
		certificate_names: ['Ada Lovelace', 'Grace Hopper'],
		purchased_at: new Date('2026-11-01T10:00:00Z'),
		phase: 'standard',
		operator_pass_terms_version: '2026-10-31',
	} as OperatorPassPurchaseRow;
	const index = repository.indexCertificate.bind(repository);
	vi.spyOn(repository, 'indexCertificate')
		.mockImplementationOnce(index)
		.mockRejectedValueOnce(new Error('index write failed'));
	await expect(certificates.issueForPurchase(purchase)).rejects.toThrow('index write failed');
	expect(await repository.listCertificatesByPurchase(purchase.checkout_session_id)).toHaveLength(1);

	await certificates.issueForPurchase(purchase);
	expect(await repository.listCertificatesByPurchase(purchase.checkout_session_id)).toHaveLength(2);
	const file = await certificates.loadPurchaseFile({...purchase, delivered_at: new Date('2026-11-01T10:01:00Z')});
	expect((await PDFDocument.load(file!.bytes)).getPageCount()).toBe(2);
});

test('a delivered purchase can be downloaded before its passes exist, and not once it is refunded', async () => {
	const repository = new OperatorPassRepository();
	const certificates = new OperatorPassCertificateService(repository, getCacheService(), getKVClient());
	const purchase = {
		checkout_session_id: `cs_test_${randomUUID()}`,
		quantity: 1,
		status: 'pending',
		certificate_names: ['Ada Lovelace'],
		purchased_at: new Date('2026-11-01T10:00:00Z'),
		phase: 'standard',
		operator_pass_terms_version: '2026-10-31',
	} as OperatorPassPurchaseRow;
	await certificates.issueForPurchase(purchase);
	const delivered = {...purchase, delivered_at: new Date('2026-11-01T10:01:00Z')};
	expect(await certificates.loadPurchaseFile(delivered)).not.toBeNull();
	const refunded = {...delivered, status: 'refunded'} as OperatorPassPurchaseRow;
	expect(await certificates.loadPurchaseFile(refunded)).toBeNull();
});

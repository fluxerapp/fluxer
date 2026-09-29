// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import type {StorePurchaseRow} from '@app/api/database/types/StoreBillingTypes';
import {StoreBillingRepository} from '@app/api/store_billing/StoreBillingRepository';
import {buildAppStoreStoreKey, buildGooglePlayStoreKey} from '@app/api/store_billing/StoreBillingTypes';
import {describe, expect, it} from 'vitest';

const USER_A = createUserID(1001n);
const USER_B = createUserID(1002n);

function purchaseRow(overrides: Partial<StorePurchaseRow> = {}): StorePurchaseRow {
	const createdAt = new Date('2026-09-01T00:00:00.000Z');
	return {
		store_key: buildAppStoreStoreKey('production', '2000000000000001'),
		id: 5000n,
		provider: 'app_store',
		kind: 'subscription',
		slot: 'monthly',
		environment: 'production',
		app_id: 'com.fluxer',
		product_id: 'com.fluxer.plutonium.monthly',
		base_plan_id: null,
		store_reference: '2000000000000001',
		latest_transaction_id: '2000000000000009',
		app_transaction_id: null,
		user_id: null,
		bound_at: null,
		released_at: null,
		account_token: null,
		ownership_type: 'PURCHASED',
		state: 'active',
		store_state: '1',
		entitled: true,
		expires_at: new Date('2026-10-01T00:00:00.000Z'),
		grace_ends_at: null,
		auto_renew: true,
		auto_renew_product_id: 'com.fluxer.plutonium.monthly',
		started_at: createdAt,
		purchased_at: createdAt,
		revoked_at: null,
		revocation_reason: null,
		linked_store_key: null,
		superseded_by_store_key: null,
		acknowledged: true,
		gift_code: null,
		region: 'SWE',
		last_event_at: createdAt,
		synced_at: createdAt,
		created_at: createdAt,
		updated_at: createdAt,
		version: 1,
		...overrides,
	};
}

describe('StoreBillingRepository purchases', () => {
	it('creates a purchase once and reads it back by store key', async () => {
		const repository = new StoreBillingRepository();
		const row = purchaseRow();

		expect(await repository.insertPurchase(row)).toBe(true);
		expect(await repository.insertPurchase({...row, id: 5001n})).toBe(false);

		const stored = await repository.findPurchase(row.store_key);
		expect(stored?.id).toBe(5000n);
		expect(stored?.expires_at?.toISOString()).toBe('2026-10-01T00:00:00.000Z');
		expect(await repository.findPurchase(buildGooglePlayStoreKey('missing'))).toBeNull();
	});

	it('lists a purchase created with an owner under that owner', async () => {
		const repository = new StoreBillingRepository();
		await repository.insertPurchase(purchaseRow({user_id: USER_A, bound_at: new Date()}));

		const listed = await repository.listPurchasesForUser(USER_A);
		expect(listed.map((row) => row.id)).toEqual([5000n]);
		expect(await repository.listPurchasesForUser(USER_B)).toEqual([]);
	});

	it('applies an update against the current version and bumps it', async () => {
		const repository = new StoreBillingRepository();
		await repository.insertPurchase(purchaseRow());
		const current = await repository.findPurchase(purchaseRow().store_key);
		if (!current) throw new Error('missing purchase');

		const updated = await repository.updatePurchase(current, {state: 'canceled', auto_renew: false});

		expect(updated?.version).toBe(2);
		const stored = await repository.findPurchase(current.store_key);
		expect(stored).toMatchObject({state: 'canceled', auto_renew: false, version: 2});
	});

	it('refuses an update built from a stale version', async () => {
		const repository = new StoreBillingRepository();
		await repository.insertPurchase(purchaseRow());
		const stale = await repository.findPurchase(purchaseRow().store_key);
		if (!stale) throw new Error('missing purchase');
		await repository.updatePurchase(stale, {state: 'grace'});

		expect(await repository.updatePurchase(stale, {state: 'expired', entitled: false})).toBeNull();
		expect(await repository.findPurchase(stale.store_key)).toMatchObject({state: 'grace', entitled: true, version: 2});
	});

	it('writes nothing when an update changes no column', async () => {
		const repository = new StoreBillingRepository();
		await repository.insertPurchase(purchaseRow());
		const current = await repository.findPurchase(purchaseRow().store_key);
		if (!current) throw new Error('missing purchase');

		const result = await repository.updatePurchase(current, {state: 'active', entitled: true});

		expect(result).toBe(current);
		expect((await repository.findPurchase(current.store_key))?.version).toBe(1);
	});

	it('keeps the by-user index in step through bind, rebind and unbind', async () => {
		const repository = new StoreBillingRepository();
		await repository.insertPurchase(purchaseRow());
		const unbound = await repository.findPurchase(purchaseRow().store_key);
		if (!unbound) throw new Error('missing purchase');

		const boundToA = await repository.bindPurchase(unbound, USER_A, new Date('2026-09-02T00:00:00.000Z'));
		if (!boundToA) throw new Error('bind failed');
		expect((await repository.listPurchasesForUser(USER_A)).map((row) => row.store_key)).toEqual([unbound.store_key]);

		const boundToB = await repository.bindPurchase(boundToA, USER_B, new Date('2026-09-03T00:00:00.000Z'));
		if (!boundToB) throw new Error('rebind failed');
		expect(await repository.listPurchasesForUser(USER_A)).toEqual([]);
		expect((await repository.listPurchasesForUser(USER_B)).map((row) => row.user_id)).toEqual([USER_B]);

		const released = await repository.unbindPurchase(boundToB);
		expect(released).toMatchObject({user_id: null, bound_at: null, version: 4});
		expect(await repository.listPurchasesForUser(USER_B)).toEqual([]);
		expect(await repository.findPurchase(unbound.store_key)).toMatchObject({user_id: null, bound_at: null});
	});

	it('does not list a purchase for a user whose bind lost the version race', async () => {
		const repository = new StoreBillingRepository();
		await repository.insertPurchase(purchaseRow());
		const current = await repository.findPurchase(purchaseRow().store_key);
		if (!current) throw new Error('missing purchase');
		await repository.bindPurchase(current, USER_A, new Date());

		expect(await repository.bindPurchase(current, USER_B, new Date())).toBeNull();
		expect(await repository.listPurchasesForUser(USER_B)).toEqual([]);
		expect((await repository.listPurchasesForUser(USER_A)).map((row) => row.user_id)).toEqual([USER_A]);
	});

	it('records a gift code only while none is set', async () => {
		const repository = new StoreBillingRepository();
		const row = purchaseRow({
			store_key: buildGooglePlayStoreKey('gift-token'),
			kind: 'gift',
			slot: 'gift_1_month',
			state: 'purchased',
		});
		await repository.insertPurchase(row);

		expect(await repository.setGiftCode(row.store_key, 'FIRSTCODE')).toBe(true);
		expect(await repository.setGiftCode(row.store_key, 'SECONDCODE')).toBe(false);
		expect(await repository.findPurchase(row.store_key)).toMatchObject({gift_code: 'FIRSTCODE', version: 1});
	});

	it('does not lose a recorded gift code to a later version-guarded update', async () => {
		const repository = new StoreBillingRepository();
		const row = purchaseRow({store_key: buildGooglePlayStoreKey('gift-token-2'), kind: 'gift', slot: 'gift_1_year'});
		await repository.insertPurchase(row);
		const beforeMint = await repository.findPurchase(row.store_key);
		if (!beforeMint) throw new Error('missing purchase');
		await repository.setGiftCode(row.store_key, 'MINTED');

		await repository.updatePurchase(beforeMint, {state: 'fulfilled'});

		expect(await repository.findPurchase(row.store_key)).toMatchObject({gift_code: 'MINTED', state: 'fulfilled'});
	});
});

describe('StoreBillingRepository account tokens', () => {
	it('creates one lowercase UUID token per user and maps it back', async () => {
		const repository = new StoreBillingRepository();

		const token = await repository.getOrCreateAccountToken(USER_A);

		expect(token).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/);
		expect(await repository.getOrCreateAccountToken(USER_A)).toBe(token);
		expect(await repository.findAccountTokenForUser(USER_A)).toBe(token);
		expect(await repository.findUserIdByAccountToken(token)).toBe(USER_A);
		expect(await repository.findUserIdByAccountToken(token.toUpperCase())).toBe(USER_A);
		expect(await repository.getOrCreateAccountToken(USER_B)).not.toBe(token);
	});

	it('settles concurrent creation on a single token', async () => {
		const repository = new StoreBillingRepository();

		const tokens = await Promise.all([
			repository.getOrCreateAccountToken(USER_A),
			repository.getOrCreateAccountToken(USER_A),
			repository.getOrCreateAccountToken(USER_A),
		]);

		expect(new Set(tokens).size).toBe(1);
		expect(await repository.findAccountTokenForUser(USER_A)).toBe(tokens[0]);
		expect(await repository.findUserIdByAccountToken(tokens[0])).toBe(USER_A);
	});

	it('returns no user for an unknown token', async () => {
		expect(
			await new StoreBillingRepository().findUserIdByAccountToken('00000000-0000-4000-8000-000000000000'),
		).toBeNull();
	});
});

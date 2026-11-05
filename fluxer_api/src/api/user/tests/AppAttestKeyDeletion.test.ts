// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID, type UserID} from '@app/api/BrandedTypes';
import {executeConditional, fetchMany, fetchOne} from '@app/api/database/CassandraQueryExecution';
import {AppAttestKeyLookup, AppAttestKeys} from '@app/api/Tables';
import {deleteAllAppAttestKeys} from '@app/api/user/AgeAssurance';
import {describe, expect, it} from 'vitest';

async function registerKey(userId: UserID, keyId: string): Promise<void> {
	expect(
		await executeConditional(
			AppAttestKeys.insertIfNotExists({
				user_id: userId,
				key_id: keyId,
				public_key: Buffer.alloc(65, 4),
				environment: 'production',
				app_id: '3G5837T29K.com.fluxer',
				counter: 0n,
				created_at: new Date(),
				last_used_at: null,
			}),
		),
	).toBe(true);
	expect(await executeConditional(AppAttestKeyLookup.insertIfNotExists({key_id: keyId, user_id: userId}))).toBe(true);
}

async function keyIdsFor(userId: UserID): Promise<Array<string>> {
	const rows = await fetchMany<{key_id: string}>(
		AppAttestKeys.selectCql({columns: ['key_id'], where: AppAttestKeys.where.eq('user_id')}),
		{user_id: userId},
	);
	return rows.map((row) => row.key_id).sort();
}

async function lookupOwner(keyId: string): Promise<UserID | null> {
	const row = await fetchOne<{user_id: UserID}>(
		AppAttestKeyLookup.selectCql({where: AppAttestKeyLookup.where.eq('key_id'), limit: 1}),
		{key_id: keyId},
	);
	return row?.user_id ?? null;
}

describe('deleteAllAppAttestKeys', () => {
	it('removes every key and key lookup of the deleted user and nothing of anyone else', async () => {
		const deleted = createUserID(7_100_000_000_000_001n);
		const other = createUserID(7_100_000_000_000_002n);
		const deletedKeys = [`${'A'.repeat(43)}=`, `${'B'.repeat(43)}=`];
		const otherKey = `${'C'.repeat(43)}=`;
		for (const keyId of deletedKeys) await registerKey(deleted, keyId);
		await registerKey(other, otherKey);

		await deleteAllAppAttestKeys(deleted);

		expect(await keyIdsFor(deleted)).toEqual([]);
		for (const keyId of deletedKeys) expect(await lookupOwner(keyId)).toBeNull();
		expect(await keyIdsFor(other)).toEqual([otherKey]);
		expect(await lookupOwner(otherKey)).toBe(other);
	});
});

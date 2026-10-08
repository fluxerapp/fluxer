// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import type {UserID} from '@app/api/BrandedTypes';
import {
	deleteOneOrMany,
	executeConditional,
	fetchMany,
	fetchOne,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {Db, type DbOp, nextVersion} from '@app/api/database/CassandraTypes';
import {buildPatchFromData} from '@app/api/database/CassandraVersionedUpdate';
import {
	STORE_PURCHASE_COLUMNS,
	type StoreAccountTokenByUserRow,
	type StoreAccountTokenRow,
	type StorePurchaseByUserRow,
	type StorePurchaseRow,
} from '@app/api/database/types/StoreBillingTypes';
import {StoreAccountTokens, StoreAccountTokensByUser, StorePurchases, StorePurchasesByUser} from '@app/api/Tables';

type StorePurchaseChanges = Partial<Omit<StorePurchaseRow, 'store_key' | 'version' | 'created_at'>>;

type StorePurchasePatch = Partial<{
	[K in Exclude<keyof StorePurchaseRow, 'store_key'>]: DbOp<StorePurchaseRow[K]>;
}>;

const ACCOUNT_TOKEN_CREATE_ATTEMPTS = 3;

const FETCH_PURCHASE_QUERY = StorePurchases.selectCql({
	where: StorePurchases.where.eq('store_key'),
	limit: 1,
});
const FETCH_PURCHASE_KEYS_BY_USER_QUERY = StorePurchasesByUser.selectCql({
	where: StorePurchasesByUser.where.eq('user_id'),
});
const FETCH_ACCOUNT_TOKEN_QUERY = StoreAccountTokens.selectCql({
	where: StoreAccountTokens.where.eq('token_'),
	limit: 1,
});
const FETCH_ACCOUNT_TOKEN_BY_USER_QUERY = StoreAccountTokensByUser.selectCql({
	where: StoreAccountTokensByUser.where.eq('user_id'),
	limit: 1,
});

export class StoreBillingRepository {
	async findPurchase(storeKey: string): Promise<StorePurchaseRow | null> {
		return fetchOne<StorePurchaseRow>(FETCH_PURCHASE_QUERY, {store_key: storeKey});
	}

	async listPurchasesForUser(userId: UserID): Promise<Array<StorePurchaseRow>> {
		const refs = await fetchMany<StorePurchaseByUserRow>(FETCH_PURCHASE_KEYS_BY_USER_QUERY, {user_id: userId});
		const rows = await Promise.all(refs.map((ref) => this.findPurchase(ref.store_key)));
		return rows.filter((row): row is StorePurchaseRow => row !== null && row.user_id === userId);
	}

	async insertPurchase(row: StorePurchaseRow): Promise<boolean> {
		if (row.user_id !== null) {
			await this.addUserIndex(row.user_id, row);
		}
		return executeConditional(StorePurchases.insertIfNotExists(row));
	}

	async updatePurchase(current: StorePurchaseRow, changes: StorePurchaseChanges): Promise<StorePurchaseRow | null> {
		const candidate: StorePurchaseRow = {...current, ...changes};
		const patch = buildPatchFromData(candidate, current, STORE_PURCHASE_COLUMNS, ['store_key']) as StorePurchasePatch;
		if (Object.keys(patch).length === 0) {
			return current;
		}
		const updatedAt = changes.updated_at ?? new Date();
		const version = nextVersion(current.version);
		const next: StorePurchaseRow = {...candidate, updated_at: updatedAt, version};
		if (next.user_id !== null && next.user_id !== current.user_id) {
			await this.addUserIndex(next.user_id, next);
		}
		const applied = await executeConditional(
			StorePurchases.conditionalPatchByPk(
				{store_key: current.store_key},
				{...patch, updated_at: Db.set(updatedAt), version: Db.set(version)},
				{version: current.version},
			),
		);
		if (!applied) {
			return null;
		}
		if (current.user_id !== null && current.user_id !== next.user_id) {
			await deleteOneOrMany(StorePurchasesByUser.deleteByPk({user_id: current.user_id, store_key: current.store_key}));
		}
		return next;
	}

	async bindPurchase(current: StorePurchaseRow, userId: UserID, boundAt: Date): Promise<StorePurchaseRow | null> {
		return this.updatePurchase(current, {user_id: userId, bound_at: boundAt});
	}

	async unbindPurchase(current: StorePurchaseRow): Promise<StorePurchaseRow | null> {
		return this.updatePurchase(current, {user_id: null, bound_at: null});
	}

	async releasePurchase(current: StorePurchaseRow): Promise<StorePurchaseRow | null> {
		return this.updatePurchase(current, {user_id: null, bound_at: null, released_at: new Date()});
	}

	async setGiftCode(storeKey: string, giftCode: string): Promise<boolean> {
		return executeConditional(
			StorePurchases.conditionalPatchByPk(
				{store_key: storeKey},
				{gift_code: Db.set(giftCode), updated_at: Db.set(new Date())},
				{kind: 'gift', gift_code: null},
			),
		);
	}

	async findUserIdByAccountToken(token: string): Promise<UserID | null> {
		const row = await fetchOne<StoreAccountTokenRow>(FETCH_ACCOUNT_TOKEN_QUERY, {token_: token.toLowerCase()});
		return row?.user_id ?? null;
	}

	async findAccountTokenForUser(userId: UserID): Promise<string | null> {
		const row = await fetchOne<StoreAccountTokenByUserRow>(FETCH_ACCOUNT_TOKEN_BY_USER_QUERY, {user_id: userId});
		return row?.token_ ?? null;
	}

	async getOrCreateAccountToken(userId: UserID): Promise<string> {
		const existing = await this.findAccountTokenForUser(userId);
		if (existing) {
			return existing;
		}
		for (let attempt = 0; attempt < ACCOUNT_TOKEN_CREATE_ATTEMPTS; attempt++) {
			const token = randomUUID().toLowerCase();
			const createdAt = new Date();
			const claimed = await executeConditional(
				StoreAccountTokens.insertIfNotExists({token_: token, user_id: userId, created_at: createdAt}),
			);
			if (!claimed) {
				continue;
			}
			const assigned = await executeConditional(
				StoreAccountTokensByUser.insertIfNotExists({user_id: userId, token_: token, created_at: createdAt}),
			);
			if (assigned) {
				return token;
			}
			await deleteOneOrMany(StoreAccountTokens.deleteByPk({token_: token}));
			const winner = await this.findAccountTokenForUser(userId);
			if (winner) {
				return winner;
			}
		}
		throw new Error('Could not allocate a store account token');
	}

	private async addUserIndex(userId: UserID, row: StorePurchaseRow): Promise<void> {
		await upsertOne(
			StorePurchasesByUser.upsertAll({user_id: userId, store_key: row.store_key, created_at: row.created_at}),
		);
	}
}

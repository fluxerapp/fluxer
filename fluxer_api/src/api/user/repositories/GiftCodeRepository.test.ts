// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {getKvMeta} from '@app/api/database/CassandraMetaRegistry';
import {
	type CassandraQueryExecutorForTesting,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import type {KvQueryMeta, PreparedQuery} from '@app/api/database/CassandraTypes';
import {createGiftCodeRow} from '@app/api/models/GiftCode';
import {GiftCodesByRedeemer} from '@app/api/Tables';
import {InMemoryCassandraQueryExecutor} from '@app/api/test/InMemoryCassandraQueryExecutor';
import {GiftCodeRepository} from '@app/api/user/repositories/GiftCodeRepository';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

type WriteOutcome = 'timeout_after_apply' | 'reject';

function writesTable(meta: KvQueryMeta | null | undefined, table: string): boolean {
	return meta?.table.name === table && meta.action !== 'select' && meta.action !== 'count';
}

class FlakyWrites implements CassandraQueryExecutorForTesting {
	readonly storage = new InMemoryCassandraQueryExecutor();
	failure: {table: string; outcome: WriteOutcome} | null = null;

	private hits(metas: Array<KvQueryMeta | null | undefined>): WriteOutcome | null {
		const failure = this.failure;
		if (!failure || !metas.some((meta) => writesTable(meta, failure.table))) {
			return null;
		}
		return failure.outcome;
	}

	async executeQuery<T>(query: PreparedQuery): Promise<Array<T>> {
		const outcome = this.hits([query.kvMeta ?? getKvMeta(query.cql)]);
		if (outcome === 'reject') {
			throw new Error('The database write failed');
		}
		const rows = await this.storage.executeQuery<T>(query);
		if (outcome === 'timeout_after_apply') {
			throw new Error('The database write timed out');
		}
		return rows;
	}

	async executeBatch(queries: Array<{query: string; params: object; meta?: KvQueryMeta}>, atomic?: boolean) {
		const outcome = this.hits(queries.map((query) => query.meta ?? getKvMeta(query.query)));
		if (outcome === 'reject') {
			throw new Error('The database batch failed');
		}
		await this.storage.executeBatch(queries, atomic);
		if (outcome === 'timeout_after_apply') {
			throw new Error('The database batch timed out');
		}
	}

	reset(): void {
		this.storage.reset();
	}
}

const CREATOR = createUserID(1940000000000000001n);
const REDEEMER = createUserID(1940000000000000002n);

function giftRow(code: string) {
	return createGiftCodeRow({
		code,
		durationType: 'weeks',
		durationQuantity: 1,
		createdAt: new Date('2026-03-01T07:40:00Z'),
		createdByUserId: CREATOR,
		paymentIntentId: `pi_${code}`,
		checkoutSessionId: `cs_${code}`,
	});
}

describe('GiftCodeRepository.createGiftCode', () => {
	let executor: FlakyWrites;
	let repository: GiftCodeRepository;

	beforeEach(() => {
		executor = new FlakyWrites();
		setCassandraQueryExecutorForTesting(executor);
		repository = new GiftCodeRepository();
	});

	afterEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});

	it('lists a gift for its creator when the gift row write times out after it was applied', async () => {
		executor.failure = {table: 'gift_codes', outcome: 'timeout_after_apply'};
		await expect(repository.createGiftCode(giftRow('timedoutgift'))).rejects.toThrow();
		executor.failure = null;

		expect(await repository.findGiftCode('timedoutgift')).not.toBeNull();
		expect((await repository.findGiftCodesByCreator(CREATOR)).map((gift) => gift.code)).toEqual(['timedoutgift']);
		expect((await repository.findGiftCodeByPaymentIntent('pi_timedoutgift'))?.code).toBe('timedoutgift');
	});

	it('leaves no gift row behind when the creator index write is rejected', async () => {
		executor.failure = {table: 'gift_codes_by_creator', outcome: 'reject'};
		await expect(repository.createGiftCode(giftRow('rejectedgift'))).rejects.toThrow();
		executor.failure = null;

		expect(await repository.findGiftCode('rejectedgift')).toBeNull();
		expect(await repository.findGiftCodesByCreator(CREATOR)).toEqual([]);
	});

	it('writes the gift and every index when nothing fails', async () => {
		await repository.createGiftCode(giftRow('cleangift'));

		expect((await repository.findGiftCodesByCreator(CREATOR)).map((gift) => gift.code)).toEqual(['cleangift']);
		expect((await repository.findGiftCodeByPaymentIntent('pi_cleangift'))?.code).toBe('cleangift');
	});
});

describe('GiftCodeRepository.redeemGiftCode', () => {
	let executor: FlakyWrites;
	let repository: GiftCodeRepository;

	beforeEach(async () => {
		executor = new FlakyWrites();
		setCassandraQueryExecutorForTesting(executor);
		repository = new GiftCodeRepository();
		await repository.createGiftCode(giftRow('redeemgift'));
	});

	afterEach(() => {
		setCassandraQueryExecutorForTesting(new InMemoryCassandraQueryExecutor());
	});

	it('writes the redeemer index with the gift code', async () => {
		await repository.redeemGiftCode('redeemgift', REDEEMER);

		expect((await repository.findGiftCode('redeemgift'))?.redeemedByUserId).toBe(REDEEMER);
		expect((await repository.findGiftCodesByRedeemer(REDEEMER)).map((gift) => gift.code)).toEqual(['redeemgift']);
	});

	it('leaves the gift code unredeemed when the redeemer index write is rejected', async () => {
		executor.failure = {table: GiftCodesByRedeemer.name, outcome: 'reject'};
		await expect(repository.redeemGiftCode('redeemgift', REDEEMER)).rejects.toThrow();
		executor.failure = null;

		const gift = await repository.findGiftCode('redeemgift');
		expect(gift?.redeemedByUserId).toBeNull();
		expect(gift?.redeemedAt).toBeNull();
		expect(await repository.findGiftCodesByRedeemer(REDEEMER)).toEqual([]);
	});
});

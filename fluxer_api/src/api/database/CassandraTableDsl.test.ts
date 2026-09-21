// SPDX-License-Identifier: AGPL-3.0-or-later

import {getTableMetadata} from '@app/api/database/CassandraMetaRegistry';
import {defineTable} from '@app/api/database/CassandraTableDsl';
import {Db, type PreparedQuery} from '@app/api/database/CassandraTypes';
import {describe, expect, it} from 'vitest';

interface TtlHelperTestRow {
	id: string;
	value: string;
}

const TtlHelperTestRows = defineTable<TtlHelperTestRow, 'id'>({
	name: 'ttl_helper_test_rows',
	columns: ['id', 'value'],
	primaryKey: ['id'],
});

function ttlParamNameFromQuery(query: PreparedQuery): string {
	const match = query.cql.match(/USING TTL :([a-z_]+)/);
	if (!match) {
		throw new Error(`Unexpected TTL query shape: ${query.cql}`);
	}
	return match[1]!;
}

function expectBoundTtl(query: PreparedQuery, ttlSeconds: number): void {
	const ttlParamName = ttlParamNameFromQuery(query);
	expect(query.params[ttlParamName]).toBe(ttlSeconds);
	expect(query.cql).not.toContain(`TTL ${ttlSeconds}`);
}

function limitParamNameFromQuery(query: PreparedQuery): string {
	const match = query.cql.match(/LIMIT :([a-z_]+)/);
	if (!match) {
		throw new Error(`Unexpected limit query shape: ${query.cql}`);
	}
	return match[1]!;
}

describe('CassandraTableDsl TTL helpers', () => {
	it('binds TTL values instead of interpolating them into prepared CQL', () => {
		const insertShort = TtlHelperTestRows.insertWithTtl({id: 'insert-short', value: 'a'}, 60);
		const insertLong = TtlHelperTestRows.insertWithTtl({id: 'insert-long', value: 'b'}, 120);
		const patchShort = TtlHelperTestRows.patchByPkWithTtl({id: 'patch-short'}, {value: Db.set('c')}, 60);
		const patchLong = TtlHelperTestRows.patchByPkWithTtl({id: 'patch-long'}, {value: Db.set('d')}, 120);
		const upsertShort = TtlHelperTestRows.upsertAllWithTtl({id: 'upsert-short', value: 'e'}, 60);
		const upsertLong = TtlHelperTestRows.upsertAllWithTtl({id: 'upsert-long', value: 'f'}, 120);
		expect(insertShort.cql).toBe(insertLong.cql);
		expect(patchShort.cql).toBe(patchLong.cql);
		expect(upsertShort.cql).toBe(upsertLong.cql);
		expectBoundTtl(insertShort, 60);
		expectBoundTtl(insertLong, 120);
		expectBoundTtl(patchShort, 60);
		expectBoundTtl(patchLong, 120);
		expectBoundTtl(upsertShort, 60);
		expectBoundTtl(upsertLong, 120);
	});
});

describe('CassandraTableDsl select templates', () => {
	it('binds LIMIT values for prepared query templates', () => {
		const shortQuery = TtlHelperTestRows.select({
			where: TtlHelperTestRows.where.eq('id'),
			limit: 10,
		}).bind({id: 'short'});
		const longQuery = TtlHelperTestRows.select({
			where: TtlHelperTestRows.where.eq('id'),
			limit: 20,
		}).bind({id: 'long'});
		const shortLimitParamName = limitParamNameFromQuery(shortQuery);
		const longLimitParamName = limitParamNameFromQuery(longQuery);
		expect(shortQuery.cql).toBe(longQuery.cql);
		expect(shortQuery.params[shortLimitParamName]).toBe(10);
		expect(longQuery.params[longLimitParamName]).toBe(20);
		expect(shortQuery.cql).not.toContain('LIMIT 10');
		expect(longQuery.cql).not.toContain('LIMIT 20');
	});
});

describe('CassandraTableDsl default TTL', () => {
	it('keeps the CQL of a table with a default TTL free of USING TTL', () => {
		const DefaultTtlRows = defineTable<TtlHelperTestRow, 'id'>({
			name: 'default_ttl_dsl_rows',
			columns: ['id', 'value'],
			primaryKey: ['id'],
			defaultTtlSeconds: 600,
		});
		expect(DefaultTtlRows.defaultTtlSeconds).toBe(600);
		const queries = [
			DefaultTtlRows.insert({id: 'insert', value: 'a'}),
			DefaultTtlRows.upsertAll({id: 'upsert', value: 'b'}),
			DefaultTtlRows.patchByPk({id: 'patch'}, {value: Db.set('c')}),
		];
		for (const query of queries) {
			expect(query.cql).not.toContain('USING TTL');
			expect(query.kvMeta?.table.defaultTtlSeconds).toBe(600);
		}
		expect(getTableMetadata('default_ttl_dsl_rows')?.defaultTtlSeconds).toBe(600);
		expect(TtlHelperTestRows.defaultTtlSeconds).toBeUndefined();
		expect(getTableMetadata('ttl_helper_test_rows')?.defaultTtlSeconds).toBeUndefined();
	});

	it('rejects a default TTL of zero, a fraction or past the maximum', () => {
		for (const defaultTtlSeconds of [0, 1.5, 630_720_001]) {
			expect(() =>
				defineTable<TtlHelperTestRow, 'id'>({
					name: 'default_ttl_dsl_rejected_rows',
					columns: ['id', 'value'],
					primaryKey: ['id'],
					defaultTtlSeconds,
				}),
			).toThrow();
		}
		expect(getTableMetadata('default_ttl_dsl_rejected_rows')).toBeUndefined();
	});
});

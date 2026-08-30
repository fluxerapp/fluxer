// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {getKvMeta, registerKvMeta} from './CassandraMetaRegistry';
import type {KvQueryMeta, KvTableSpec} from './CassandraTypes';

const MetaRegistryTestRows: KvTableSpec = {
	name: 'meta_registry_test_rows',
	columns: ['id', 'value'],
	primaryKey: ['id'],
	partitionKey: ['id'],
};

function selectMeta(limit: number): KvQueryMeta {
	return {action: 'select', table: MetaRegistryTestRows, limit};
}

describe('CassandraMetaRegistry', () => {
	it('resolves lookups whose whitespace differs from the registered statement', () => {
		const meta = selectMeta(10);
		registerKvMeta('SELECT id, value FROM meta_registry_test_rows\n\tWHERE id = :id;', meta);
		expect(getKvMeta('SELECT id, value FROM meta_registry_test_rows WHERE id = :id;')).toBe(meta);
		expect(getKvMeta('  SELECT id,   value FROM meta_registry_test_rows\n WHERE id = :id;  ')).toBe(meta);
	});

	it('keeps the most recently registered metadata for a repeated statement', () => {
		const cql = 'SELECT id, value FROM meta_registry_test_rows WHERE id = :id LIMIT :limit_bind;';
		const first = selectMeta(10);
		const second = selectMeta(20);
		registerKvMeta(cql, first);
		expect(getKvMeta(cql)).toBe(first);
		registerKvMeta(cql, second);
		expect(getKvMeta(cql)).toBe(second);
		expect(getKvMeta(`  ${cql.replace(/ /g, '   ')}\n`)).toBe(second);
	});
});

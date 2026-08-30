// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '../Config';
import type {KvQueryMeta, KvTableSpec} from './CassandraTypes';

export function getIsDev(): boolean {
	return Config.nodeEnv === 'development';
}

interface TableMetadata {
	name: string;
	columns: ReadonlyArray<string>;
	primaryKey: ReadonlyArray<string>;
	partitionKey: ReadonlyArray<string>;
}

const kvMetaRegistry = new Map<string, KvQueryMeta<Record<string, unknown>>>();
const kvMetaKeyCache = new Map<string, string>();
const tableRegistry = new Map<string, TableMetadata>();

export function registerTableSpec<Row extends object>(tableSpec: KvTableSpec<Row>): void {
	const metadata: TableMetadata = {
		name: tableSpec.name,
		columns: tableSpec.columns as ReadonlyArray<string>,
		primaryKey: tableSpec.primaryKey as ReadonlyArray<string>,
		partitionKey: tableSpec.partitionKey as ReadonlyArray<string>,
	};
	tableRegistry.set(tableSpec.name, metadata);
}

function normalizeCqlForRegistry(cql: string): string {
	return cql.replace(/\s+/g, ' ').trim();
}

export function registerKvMeta(cql: string, meta: KvQueryMeta): void {
	let key = kvMetaKeyCache.get(cql);
	if (key === undefined) {
		key = normalizeCqlForRegistry(cql);
		kvMetaKeyCache.set(cql, key);
	}
	kvMetaRegistry.set(key, meta);
}

export function getKvMeta(cql: string): KvQueryMeta | undefined {
	const key = kvMetaKeyCache.get(cql);
	if (key !== undefined) return kvMetaRegistry.get(key);
	return kvMetaRegistry.get(normalizeCqlForRegistry(cql));
}

export function getTableMetadata(tableName: string): TableMetadata | undefined {
	return tableRegistry.get(tableName);
}

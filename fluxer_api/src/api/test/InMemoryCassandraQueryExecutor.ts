// SPDX-License-Identifier: AGPL-3.0-or-later

import {getKvMeta} from '../database/CassandraMetaRegistry';
import type {CassandraQueryExecutorForTesting} from '../database/CassandraQueryExecution';
import type {CassandraParams, KvQueryMeta, PreparedQuery, WhereExpr} from '../database/CassandraTypes';

type Row = Record<string, unknown>;

function normalizeCql(cql: string): string {
	return cql.replace(/\s+/g, ' ').trim();
}

function compareValues(a: unknown, b: unknown): number {
	if (typeof a === 'bigint' || typeof b === 'bigint') {
		const av = typeof a === 'bigint' ? a : BigInt(a as number | string);
		const bv = typeof b === 'bigint' ? b : BigInt(b as number | string);
		if (av === bv) return 0;
		return av < bv ? -1 : 1;
	}
	const av = a instanceof Date ? a.getTime() : typeof a === 'bigint' ? Number(a) : a;
	const bv = b instanceof Date ? b.getTime() : typeof b === 'bigint' ? Number(b) : b;
	if (av === bv) return 0;
	return (av as number | string) < (bv as number | string) ? -1 : 1;
}

function valuesEqual(a: unknown, b: unknown): boolean {
	if (a == null && b == null) return true;
	if (a instanceof Date && b instanceof Date) return a.getTime() === b.getTime();
	if (Buffer.isBuffer(a) && Buffer.isBuffer(b)) return a.equals(b);
	return a === b;
}

function cloneValue<T>(value: T): T {
	if (value instanceof Date) return new Date(value.getTime()) as T;
	if (value instanceof Set) return new Set([...value].map((entry) => cloneValue(entry))) as T;
	if (value instanceof Map) {
		return new Map([...value.entries()].map(([key, entry]) => [cloneValue(key), cloneValue(entry)])) as T;
	}
	if (Array.isArray(value)) return value.map((entry) => cloneValue(entry)) as T;
	if (value && typeof value === 'object' && Object.getPrototypeOf(value) === Object.prototype) {
		return {...(value as object)} as T;
	}
	return value;
}

function cloneRow(row: Row): Row {
	const cloned: Row = {};
	for (const [key, value] of Object.entries(row)) {
		cloned[key] = cloneValue(value);
	}
	return cloned;
}

function pkKey(meta: KvQueryMeta, row: Row): string {
	return meta.table.primaryKey.map((column) => valueKey(row[column])).join('\u0000');
}

function valueKey(value: unknown): string {
	if (Buffer.isBuffer(value)) return `buffer:${value.toString('base64')}`;
	if (value instanceof Date) return `date:${value.toISOString()}`;
	if (typeof value === 'bigint') return `bigint:${value.toString()}`;
	return `${typeof value}:${String(value)}`;
}

function getParam(params: CassandraParams, param: string): unknown {
	return params[param];
}

function matchesWhere(row: Row, where: ReadonlyArray<WhereExpr<Row>> | undefined, params: CassandraParams): boolean {
	for (const clause of where ?? []) {
		switch (clause.kind) {
			case 'eq':
				if (!valuesEqual(row[clause.col], getParam(params, clause.param))) return false;
				break;
			case 'in': {
				const values = getParam(params, clause.param) as ReadonlyArray<unknown> | Set<unknown> | undefined;
				const haystack = values instanceof Set ? [...values] : (values ?? []);
				if (!haystack.some((value) => valuesEqual(row[clause.col], value))) return false;
				break;
			}
			case 'lt':
				if (compareValues(row[clause.col], getParam(params, clause.param)) >= 0) return false;
				break;
			case 'lte':
				if (compareValues(row[clause.col], getParam(params, clause.param)) > 0) return false;
				break;
			case 'gt':
				if (compareValues(row[clause.col], getParam(params, clause.param)) <= 0) return false;
				break;
			case 'gte':
				if (compareValues(row[clause.col], getParam(params, clause.param)) < 0) return false;
				break;
			case 'tokenGt':
				break;
			case 'tupleGt': {
				const left = clause.cols.map((column) => row[column]);
				const right = clause.params.map((param) => getParam(params, param));
				let greater = false;
				for (let i = 0; i < left.length; i += 1) {
					const cmp = compareValues(left[i], right[i]);
					if (cmp > 0) {
						greater = true;
						break;
					}
					if (cmp < 0) break;
				}
				if (!greater) return false;
				break;
			}
		}
	}
	return true;
}

function projectRow(row: Row, columns: ReadonlyArray<string> | undefined): Row {
	if (!columns) return cloneRow(row);
	const projected: Row = {};
	for (const column of columns) {
		projected[column] = cloneValue(row[column]);
	}
	return projected;
}

function parseRawMeta(cql: string): KvQueryMeta<Row> | null {
	const normalized = normalizeCql(cql).replace(/;$/, '');
	const update = /^UPDATE\s+([a-zA-Z0-9_]+)\s+SET\s+(.+?)\s+WHERE\s+(.+)$/i.exec(normalized);
	if (update) {
		const table = update[1]!;
		const patchKeys = update[2]!
			.split(',')
			.map((part) => part.trim().split(/\s*=\s*/)[0])
			.filter(Boolean);
		const where = update[3]!
			.split(/\s+AND\s+/i)
			.map((part) => part.trim().split(/\s*=\s*/)[0])
			.filter(Boolean);
		return {
			action: 'patch',
			table: {name: table, columns: [...new Set([...where, ...patchKeys])], primaryKey: where, partitionKey: where},
			where: where.map((col) => ({kind: 'eq', col, param: col})),
			patchKeys,
			pkColumns: where,
		};
	}
	const insert = /^INSERT\s+INTO\s+([a-zA-Z0-9_]+)\s*\((.+?)\)\s+VALUES/i.exec(normalized);
	if (insert) {
		const columns = insert[2]!.split(',').map((part) => part.trim());
		const pk = columns.includes('event_id') ? ['event_id'] : [columns[0]!];
		return {
			action: 'upsert',
			table: {name: insert[1]!, columns, primaryKey: pk, partitionKey: pk},
		};
	}
	return null;
}

export class InMemoryCassandraQueryExecutor implements CassandraQueryExecutorForTesting {
	private readonly tables = new Map<string, Map<string, Row>>();

	reset(): void {
		this.tables.clear();
	}

	private table(meta: KvQueryMeta): Map<string, Row> {
		let table = this.tables.get(meta.table.name);
		if (!table) {
			table = new Map();
			this.tables.set(meta.table.name, table);
		}
		return table;
	}

	private meta(query: PreparedQuery): KvQueryMeta<Row> {
		const meta = (query.kvMeta ?? getKvMeta(query.cql) ?? parseRawMeta(query.cql)) as KvQueryMeta<Row> | null;
		if (!meta) {
			throw new Error(`In-memory Cassandra does not understand query: ${query.cql}`);
		}
		return meta;
	}

	async executeQuery<T = Row>(query: PreparedQuery): Promise<Array<T>> {
		const meta = this.meta(query);
		switch (meta.action) {
			case 'select':
				return this.select(meta, query.params) as Array<T>;
			case 'count':
				return [{count: this.select(meta, query.params).length}] as Array<T>;
			case 'upsert':
				if (meta.ifNotExists) {
					return [{'[applied]': this.upsert(meta, query.params, true)}] as Array<T>;
				}
				this.upsert(meta, query.params);
				return [];
			case 'patch':
				this.patch(meta, query.params);
				return [];
			case 'delete':
				this.delete(meta, query.params);
				return [];
			case 'batch':
				return [];
		}
		return [];
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		_atomic?: boolean,
	): Promise<void> {
		for (const query of queries) {
			await this.executeQuery({
				cql: query.query,
				params: query.params as CassandraParams,
				kvMeta: query.meta,
			});
		}
	}

	private rowFromParams(meta: KvQueryMeta, params: CassandraParams): Row {
		const row: Row = {};
		for (const column of meta.table.columns) {
			if (column in params) {
				row[column] = cloneValue(params[column]);
			}
		}
		if (meta.nowColumn) {
			row[meta.nowColumn] = new Date();
		}
		return row;
	}

	private keyFromParams(meta: KvQueryMeta, params: CassandraParams): string {
		const row: Row = {};
		for (const column of meta.pkColumns ?? meta.table.primaryKey) {
			row[column] = params[column];
		}
		return pkKey(meta, row);
	}

	private upsert(meta: KvQueryMeta, params: CassandraParams, ifNotExists = false): boolean {
		const table = this.table(meta);
		const next = this.rowFromParams(meta, params);
		const key = pkKey(meta, next);
		if (ifNotExists && table.has(key)) {
			return false;
		}
		table.set(key, {...(table.get(key) ?? {}), ...next});
		return true;
	}

	private patch(meta: KvQueryMeta, params: CassandraParams): void {
		const table = this.table(meta);
		const key = this.keyFromParams(meta, params);
		const row =
			table.get(key) ?? this.rowFromParams({...meta, table: {...meta.table, columns: meta.table.primaryKey}}, params);
		for (const column of meta.patchKeys ?? []) {
			row[column] = column in params ? cloneValue(params[column]) : null;
		}
		table.set(key, row);
	}

	private delete(meta: KvQueryMeta, params: CassandraParams): void {
		const table = this.table(meta);
		for (const [key, row] of table.entries()) {
			if (matchesWhere(row, meta.where, params)) {
				table.delete(key);
			}
		}
	}

	private select(meta: KvQueryMeta, params: CassandraParams): Array<Row> {
		let rows = [...this.table(meta).values()].filter((row) => matchesWhere(row, meta.where, params));
		if (meta.orderBy) {
			const direction = meta.orderBy.direction === 'DESC' ? -1 : 1;
			rows = rows.sort((a, b) => compareValues(a[meta.orderBy!.col], b[meta.orderBy!.col]) * direction);
		}
		if (typeof meta.limit === 'number') {
			rows = rows.slice(0, meta.limit);
		}
		return rows.map((row) => projectRow(row, meta.columns));
	}
}

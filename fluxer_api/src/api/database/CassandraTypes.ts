// SPDX-License-Identifier: AGPL-3.0-or-later

import type cassandra from 'cassandra-driver';

export type DbOp<T> =
	| {
			kind: 'set';
			value: T;
	  }
	| {
			kind: 'clear';
	  };

export const Db = {
	set<T>(value: T): DbOp<T> {
		return {kind: 'set', value};
	},
	clear<T = never>(): DbOp<T> {
		return {kind: 'clear'};
	},
} as const;

export function nextVersion(current: number | null | undefined): number {
	return (current ?? 0) + 1;
}

export type ColumnName<Row> = Extract<keyof Row, string>;
export type RowValue<Row, K extends ColumnName<Row>> = Row[K & keyof Row];
export type CassandraParam =
	| string
	| number
	| bigint
	| boolean
	| Buffer
	| Date
	| cassandra.types.LocalDate
	| Set<unknown>
	| Map<unknown, unknown>
	| Array<unknown>
	| Record<string, unknown>
	| null;
export type CassandraParams = Record<string, CassandraParam>;
export type KvAction = 'select' | 'count' | 'insert' | 'upsert' | 'delete' | 'patch' | 'batch';

export interface KvTableSpec<Row extends object = Record<string, unknown>> {
	name: string;
	columns: ReadonlyArray<ColumnName<Row>>;
	primaryKey: ReadonlyArray<ColumnName<Row>>;
	partitionKey: ReadonlyArray<ColumnName<Row>>;
}

export interface KvCondition<Row extends object = Record<string, unknown>> {
	col: ColumnName<Row>;
	expectedParam: string;
	expectedValue: unknown;
}

export interface KvQueryMeta<Row extends object = Record<string, unknown>> {
	action: KvAction;
	table: KvTableSpec<Row>;
	where?: ReadonlyArray<WhereExpr<Row>>;
	orderBy?: OrderBy<Row> | ReadonlyArray<OrderBy<Row>>;
	limit?: number;
	columns?: ReadonlyArray<ColumnName<Row>>;
	patch?: Partial<Record<ColumnName<Row>, DbOp<unknown>>>;
	patchKeys?: ReadonlyArray<ColumnName<Row>>;
	pkColumns?: ReadonlyArray<ColumnName<Row>>;
	ttlSeconds?: number;
	ttlParamName?: string;
	nowColumn?: ColumnName<Row>;
	condition?: KvCondition<Row>;
	conditions?: ReadonlyArray<KvCondition<Row>>;
	ifNotExists?: boolean;
}

export interface PreparedQuery<P extends CassandraParams = CassandraParams> {
	cql: string;
	params: P;
	kvMeta?: KvQueryMeta;
}

function normalizeBinaryValue(value: unknown): unknown {
	if (value instanceof Uint8Array) return Buffer.from(value);
	if (Array.isArray(value)) return value.map(normalizeBinaryValue);
	if (value instanceof Set) return new Set([...value].map(normalizeBinaryValue));
	if (value instanceof Map) {
		return new Map([...value].map(([key, entry]) => [normalizeBinaryValue(key), normalizeBinaryValue(entry)]));
	}
	if (value !== null && typeof value === 'object' && Object.getPrototypeOf(value) === Object.prototype) {
		return Object.fromEntries(Object.entries(value).map(([key, entry]) => [key, normalizeBinaryValue(entry)]));
	}
	return value;
}

export function normalizeBinaryParams<P extends CassandraParams>(params: P): P {
	return normalizeBinaryValue(params) as P;
}

export function prepared<P extends CassandraParams>(cql: string, params: P, kvMeta?: KvQueryMeta): PreparedQuery<P> {
	return {cql, params: normalizeBinaryParams(params), kvMeta};
}

export interface QueryTemplate<P extends CassandraParams = CassandraParams> {
	cql: string;
	bind(params: P): PreparedQuery<P>;
}

export type WhereExpr<Row extends object> =
	| {
			kind: 'eq';
			col: ColumnName<Row>;
			param: string;
	  }
	| {
			kind: 'in';
			col: ColumnName<Row>;
			param: string;
	  }
	| {
			kind: 'lt';
			col: ColumnName<Row>;
			param: string;
	  }
	| {
			kind: 'gt';
			col: ColumnName<Row>;
			param: string;
	  }
	| {
			kind: 'lte';
			col: ColumnName<Row>;
			param: string;
	  }
	| {
			kind: 'gte';
			col: ColumnName<Row>;
			param: string;
	  }
	| {
			kind: 'tokenGt';
			col: ColumnName<Row>;
			param: string;
	  }
	| {
			kind: 'tupleGt';
			cols: ReadonlyArray<ColumnName<Row>>;
			params: ReadonlyArray<string>;
	  }
	| {
			kind: 'tupleLt';
			cols: ReadonlyArray<ColumnName<Row>>;
			params: ReadonlyArray<string>;
	  };
export type OrderBy<Row extends object> = {
	col: ColumnName<Row>;
	direction?: 'ASC' | 'DESC';
};

export interface Table<Row extends object, PK extends ColumnName<Row>, PartKey extends ColumnName<Row> = PK> {
	name: string;
	columns: ReadonlyArray<ColumnName<Row>>;
	primaryKey: ReadonlyArray<PK>;
	partitionKey: ReadonlyArray<PartKey>;
	selectCql(opts?: {
		columns?: ReadonlyArray<ColumnName<Row>>;
		where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
		orderBy?: OrderBy<Row> | ReadonlyArray<OrderBy<Row>>;
		limit?: number;
	}): string;
	select(opts?: {
		columns?: ReadonlyArray<ColumnName<Row>>;
		where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
		orderBy?: OrderBy<Row> | ReadonlyArray<OrderBy<Row>>;
		limit?: number;
	}): QueryTemplate;
	updateAllCql(): string;
	paramsFromRow(row: Row): CassandraParams;
	upsertAll(row: Row): PreparedQuery;
	patchByPk(
		pk: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
	): PreparedQuery;
	patchByPkIf(
		pk: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		condition:
			| {col: ColumnName<Row>; expected: RowValue<Row, ColumnName<Row>>}
			| ReadonlyArray<{col: ColumnName<Row>; expected: RowValue<Row, ColumnName<Row>>}>,
	): PreparedQuery;
	deleteCql(opts?: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>}): string;
	delete(opts?: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>}): QueryTemplate;
	deleteByPk(pk: Pick<Row, PK>): PreparedQuery;
	deletePartition(pk: Pick<Row, PartKey>): PreparedQuery;
	insertCql(opts?: {ttlParam?: string}): string;
	insert(row: Row): PreparedQuery;
	insertIfNotExists(row: Row): PreparedQuery;
	insertWithTtl(row: Row, ttlSeconds: number): PreparedQuery;
	insertWithTtlParam(row: Row, ttlParamName: string): PreparedQuery;
	selectCountCql(opts?: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>}): string;
	selectCount(opts?: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>}): QueryTemplate;
	insertWithNow<NowCol extends ColumnName<Row>>(row: Omit<Row, NowCol>, nowColumn: NowCol): PreparedQuery;
	patchByPkWithTtl(
		pk: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		ttlSeconds: number,
	): PreparedQuery;
	patchByPkWithTtlIf(
		pk: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		ttlSeconds: number,
		condition:
			| {col: ColumnName<Row>; expected: RowValue<Row, ColumnName<Row>>}
			| ReadonlyArray<{col: ColumnName<Row>; expected: RowValue<Row, ColumnName<Row>>}>,
	): PreparedQuery;
	patchByPkWithTtlParam(
		pk: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		ttlParamName: string,
		ttlValue: number,
	): PreparedQuery;
	upsertAllWithTtl(row: Row, ttlSeconds: number): PreparedQuery;
	upsertAllWithTtlParam(row: Row, ttlParamName: string, ttlValue: number): PreparedQuery;
	where: {
		eq: <K extends ColumnName<Row>>(col: K, param?: string) => WhereExpr<Row>;
		in: <K extends ColumnName<Row>>(col: K, param: string) => WhereExpr<Row>;
		lt: <K extends ColumnName<Row>>(col: K, param?: string) => WhereExpr<Row>;
		gt: <K extends ColumnName<Row>>(col: K, param?: string) => WhereExpr<Row>;
		lte: <K extends ColumnName<Row>>(col: K, param?: string) => WhereExpr<Row>;
		gte: <K extends ColumnName<Row>>(col: K, param?: string) => WhereExpr<Row>;
		tokenGt: <K extends ColumnName<Row>>(col: K, param: string) => WhereExpr<Row>;
		tupleGt: <K extends ColumnName<Row>>(cols: ReadonlyArray<K>, params: ReadonlyArray<string>) => WhereExpr<Row>;
		tupleLt: <K extends ColumnName<Row>>(cols: ReadonlyArray<K>, params: ReadonlyArray<string>) => WhereExpr<Row>;
	};
}

export type PatchObject = {
	[key: string]: DbOp<unknown>;
};

export function normalizeExecuteArgs<P extends CassandraParams>(
	queryOrPrepared: string | PreparedQuery<P>,
	params?: P,
): PreparedQuery<P> {
	if (typeof queryOrPrepared === 'string') {
		if (!params) {
			throw new Error('Missing params object for Cassandra query execution');
		}
		return {cql: queryOrPrepared, params};
	}
	return queryOrPrepared;
}

const IN_PARAM_CACHE = new Map<string, Array<string>>();

function getInParamNames(cql: string): Array<string> {
	const cached = IN_PARAM_CACHE.get(cql);
	if (cached) return cached;
	const regex = /\bIN\s*\(?\s*:([A-Za-z_][A-Za-z0-9_]*)/g;
	const names: Array<string> = [];
	const seen = new Set<string>();
	let match: RegExpExecArray | null;
	while ((match = regex.exec(cql)) !== null) {
		const name = match[1];
		if (!name || seen.has(name)) continue;
		seen.add(name);
		names.push(name);
	}
	IN_PARAM_CACHE.set(cql, names);
	return names;
}

export function normalizeInParams<P extends CassandraParams>(cql: string, params: P): P {
	const inParams = getInParamNames(cql);
	if (inParams.length === 0) return params;
	let changed = false;
	const out: Record<string, CassandraParam> = {...params};
	for (const paramName of inParams) {
		const value = out[paramName];
		if (value instanceof Set) {
			out[paramName] = Array.from(value.values());
			changed = true;
		}
	}
	return (changed ? (out as P) : params) as P;
}

export function isUnsafePreparedStatement(query: string): boolean {
	const tokens = query.trim().split(/\s+/);
	return tokens.length >= 2 && tokens[0].toLowerCase() === 'select' && tokens[1] === '*';
}

function assertNoUndefinedDeep(value: unknown, path: string): void {
	if (value === undefined) {
		throw new Error(
			`Undefined value at "${path}". This project forbids undefined in Cassandra params; use null explicitly or omit the column via PATCH.`,
		);
	}
	if (value === null) return;
	const t = typeof value;
	if (t === 'string' || t === 'number' || t === 'bigint' || t === 'boolean') return;
	if (value instanceof Date) return;
	if (value instanceof Buffer) return;
	if (typeof value === 'object' && value && value.constructor?.name === 'LocalDate') return;
	if (Array.isArray(value)) {
		for (let i = 0; i < value.length; i++) {
			assertNoUndefinedDeep(value[i], `${path}[${i}]`);
		}
		return;
	}
	if (value instanceof Set) {
		let idx = 0;
		for (const v of value.values()) {
			assertNoUndefinedDeep(v, `${path}{set:${idx}}`);
			idx++;
		}
		return;
	}
	if (value instanceof Map) {
		let idx = 0;
		for (const [k, v] of value.entries()) {
			assertNoUndefinedDeep(k, `${path}{mapKey:${idx}}`);
			assertNoUndefinedDeep(v, `${path}{mapVal:${idx}}`);
			idx++;
		}
		return;
	}
	if (typeof value === 'object') {
		for (const [k, v] of Object.entries(value as Record<string, unknown>)) {
			assertNoUndefinedDeep(v, `${path}.${k}`);
		}
	}
}

export function assertNoUndefinedParams(params: Record<string, unknown>): void {
	for (const [k, v] of Object.entries(params)) {
		assertNoUndefinedDeep(v, `:${k}`);
	}
}

export function chunkArray<T>(items: Array<T>, size: number): Array<Array<T>> {
	const chunks: Array<Array<T>> = [];
	if (size <= 0) {
		throw new Error('Chunk size must be greater than 0');
	}
	for (let i = 0; i < items.length; i += size) {
		chunks.push(items.slice(i, i + size));
	}
	return chunks;
}

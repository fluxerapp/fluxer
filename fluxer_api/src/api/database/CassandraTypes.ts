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

export interface KvQueryMeta<Row extends object = Record<string, unknown>> {
	action: KvAction;
	table: KvTableSpec<Row>;
	where?: ReadonlyArray<WhereExpr<Row>>;
	orderBy?: OrderBy<Row>;
	limit?: number;
	columns?: ReadonlyArray<ColumnName<Row>>;
	patch?: Partial<Record<ColumnName<Row>, DbOp<unknown>>>;
	patchKeys?: ReadonlyArray<ColumnName<Row>>;
	pkColumns?: ReadonlyArray<ColumnName<Row>>;
	ttlSeconds?: number;
	ttlParamName?: string;
	nowColumn?: ColumnName<Row>;
	condition?: {
		col: ColumnName<Row>;
		expectedParam: string;
		expectedValue: unknown;
	};
	ifNotExists?: boolean;
}

export interface PreparedQuery<P extends CassandraParams = CassandraParams> {
	cql: string;
	params: P;
	kvMeta?: KvQueryMeta;
}

export function prepared<P extends CassandraParams>(cql: string, params: P, kvMeta?: KvQueryMeta): PreparedQuery<P> {
	return {cql, params, kvMeta};
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
		orderBy?: OrderBy<Row>;
		limit?: number;
	}): string;
	select(opts?: {
		columns?: ReadonlyArray<ColumnName<Row>>;
		where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
		orderBy?: OrderBy<Row>;
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

interface CqlStatementMeta {
	unsafe: boolean;
	type: string;
	inParams: ReadonlyArray<string>;
}

const STATEMENT_META_CACHE = new Map<string, CqlStatementMeta>();

function computeInParamNames(cql: string): Array<string> {
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
	return names;
}

function computeQueryType(trimmed: string): string {
	const upper = trimmed.toUpperCase();
	if (upper.startsWith('SELECT')) return 'SELECT';
	if (upper.startsWith('INSERT')) return 'INSERT';
	if (upper.startsWith('UPDATE')) return 'UPDATE';
	if (upper.startsWith('DELETE')) return 'DELETE';
	if (upper.startsWith('BEGIN BATCH')) return 'BATCH';
	return 'QUERY';
}

function computeStatementMeta(cql: string): CqlStatementMeta {
	const trimmed = cql.trim();
	const tokens = trimmed.split(/\s+/);
	return {
		unsafe: tokens.length >= 2 && tokens[0].toLowerCase() === 'select' && tokens[1] === '*',
		type: computeQueryType(trimmed),
		inParams: computeInParamNames(cql),
	};
}

export function getStatementMeta(cql: string): CqlStatementMeta {
	const cached = STATEMENT_META_CACHE.get(cql);
	if (cached) return cached;
	const meta = computeStatementMeta(cql);
	STATEMENT_META_CACHE.set(cql, meta);
	return meta;
}

export function normalizeInParams<P extends CassandraParams>(meta: CqlStatementMeta, params: P): P {
	if (meta.inParams.length === 0) return params;
	let changed = false;
	const out: Record<string, CassandraParam> = {...params};
	for (const paramName of meta.inParams) {
		const value = out[paramName];
		if (value instanceof Set) {
			out[paramName] = Array.from(value.values());
			changed = true;
		}
	}
	return (changed ? (out as P) : params) as P;
}

function hasUndefinedDeep(value: unknown): boolean {
	if (value === undefined) return true;
	if (value === null) return false;
	const t = typeof value;
	if (t === 'string' || t === 'number' || t === 'bigint' || t === 'boolean') return false;
	if (value instanceof Date) return false;
	if (value instanceof Buffer) return false;
	if (t === 'object' && value.constructor?.name === 'LocalDate') return false;
	if (Array.isArray(value)) {
		for (let i = 0; i < value.length; i++) {
			if (hasUndefinedDeep(value[i])) return true;
		}
		return false;
	}
	if (value instanceof Set) {
		for (const v of value.values()) {
			if (hasUndefinedDeep(v)) return true;
		}
		return false;
	}
	if (value instanceof Map) {
		for (const [k, v] of value.entries()) {
			if (hasUndefinedDeep(k) || hasUndefinedDeep(v)) return true;
		}
		return false;
	}
	if (t === 'object') {
		const keys = Object.keys(value as Record<string, unknown>);
		for (let i = 0; i < keys.length; i++) {
			if (hasUndefinedDeep((value as Record<string, unknown>)[keys[i]!])) return true;
		}
	}
	return false;
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
	const keys = Object.keys(params);
	for (let i = 0; i < keys.length; i++) {
		const k = keys[i]!;
		const v = params[k];
		if (hasUndefinedDeep(v)) {
			assertNoUndefinedDeep(v, `:${k}`);
		}
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

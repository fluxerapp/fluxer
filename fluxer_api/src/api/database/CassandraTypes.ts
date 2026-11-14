// SPDX-License-Identifier: AGPL-3.0-or-later

import {LocalDate} from '@app/api/database/LocalDate';

const MAX_TTL_SECONDS = 630_720_000;

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

export function validateTtlSeconds(value: number): number {
	if (!Number.isInteger(value) || value < 0 || value > MAX_TTL_SECONDS) {
		throw new Error(`Cassandra TTL must be an integer between 0 and ${MAX_TTL_SECONDS} seconds`);
	}
	return value;
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
	| LocalDate
	| Set<unknown>
	| Map<unknown, unknown>
	| Array<unknown>
	| Record<string, unknown>
	| null;
export type CassandraParams = Record<string, CassandraParam>;
type KvAction = 'select' | 'count' | 'insert' | 'upsert' | 'delete' | 'patch' | 'batch';

export interface KvTableSpec<Row extends object = Record<string, unknown>> {
	name: string;
	columns: ReadonlyArray<ColumnName<Row>>;
	primaryKey: ReadonlyArray<ColumnName<Row>>;
	partitionKey: ReadonlyArray<ColumnName<Row>>;
	defaultTtlSeconds?: number;
}

export interface KvColumnParam<Row extends object = Record<string, unknown>> {
	col: ColumnName<Row>;
	param: string;
}

export interface KvQueryCondition<Row extends object = Record<string, unknown>> {
	col: ColumnName<Row>;
	expectedParam: string;
}

interface KvConditionalBatchKey<Row extends object> {
	pk: ReadonlyArray<KvColumnParam<Row>>;
}

export type KvConditionalBatchEntry<Row extends object = Record<string, unknown>> = KvConditionalBatchKey<Row> &
	(
		| {action: 'insert'; values: ReadonlyArray<KvColumnParam<Row>>; ttlParamName?: string}
		| {
				action: 'patch';
				patch: ReadonlyArray<KvColumnParam<Row>>;
				conditions: ReadonlyArray<KvQueryCondition<Row>>;
				ttlParamName?: string;
		  }
		| {action: 'delete'; conditions: ReadonlyArray<KvQueryCondition<Row>>}
	);

export interface KvQueryMeta<Row extends object = Record<string, unknown>> {
	action: KvAction;
	table: KvTableSpec<Row>;
	where?: ReadonlyArray<WhereExpr<Row>>;
	orderBy?: OrderBy<Row>;
	limit?: number;
	unordered?: boolean;
	columns?: ReadonlyArray<ColumnName<Row>>;
	patch?: Partial<Record<ColumnName<Row>, DbOp<unknown>>>;
	patchKeys?: ReadonlyArray<ColumnName<Row>>;
	pkColumns?: ReadonlyArray<ColumnName<Row>>;
	ttlSeconds?: number;
	ttlParamName?: string;
	nowColumn?: ColumnName<Row>;
	conditions?: ReadonlyArray<KvQueryCondition<Row>>;
	batchEntries?: ReadonlyArray<KvConditionalBatchEntry<Row>>;
	ifNotExists?: boolean;
}

export interface KvAdvance {
	table: KvTableSpec;
	pk: CassandraParams;
	guard: {col: string; value: bigint; orEqual?: boolean};
	set: CassandraParams;
	always?: CassandraParams;
	increment?: Record<string, number>;
	insert: CassandraParams | null;
}

export interface PreparedQuery<P extends CassandraParams = CassandraParams> {
	cql: string;
	params: P;
	kvMeta?: KvQueryMeta;
}

export function prepared<P extends CassandraParams>(cql: string, params: P, kvMeta?: KvQueryMeta): PreparedQuery<P> {
	return {cql, params, kvMeta};
}

export function isConditionalQuery(query: PreparedQuery): boolean {
	return (
		query.kvMeta?.conditions !== undefined ||
		query.kvMeta?.batchEntries !== undefined ||
		query.kvMeta?.ifNotExists === true ||
		/\bIF\b/i.test(query.cql)
	);
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
	  };
export type OrderBy<Row extends object> = {
	col: ColumnName<Row>;
	direction?: 'ASC' | 'DESC';
};

export type ConditionalWriteEntry<Row extends object, PK extends ColumnName<Row>> =
	| {action: 'insert'; row: Row; ttlSeconds?: number}
	| {
			action: 'patch';
			pk: Pick<Row, PK>;
			patch: Partial<{
				[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
			}>;
			expected: Partial<Row>;
			ttlSeconds?: number;
	  }
	| {action: 'delete'; pk: Pick<Row, PK>; expected: Partial<Row>};

export interface Table<Row extends object, PK extends ColumnName<Row>, PartKey extends ColumnName<Row> = PK> {
	name: string;
	columns: ReadonlyArray<ColumnName<Row>>;
	primaryKey: ReadonlyArray<PK>;
	partitionKey: ReadonlyArray<PartKey>;
	defaultTtlSeconds: number | undefined;
	selectCql(opts?: {
		columns?: ReadonlyArray<ColumnName<Row>>;
		where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
		orderBy?: OrderBy<Row>;
		limit?: number;
		unordered?: boolean;
	}): string;
	select(opts?: {
		columns?: ReadonlyArray<ColumnName<Row>>;
		where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
		orderBy?: OrderBy<Row>;
		limit?: number;
	}): QueryTemplate;
	upsertAll(row: Row): PreparedQuery;
	patchByPk(
		pk: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
	): PreparedQuery;
	conditionalPatchByPk(
		pk: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		expected: Partial<Row>,
	): PreparedQuery;
	conditionalPatchByPkWithTtl(
		pk: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		expected: Partial<Row>,
		ttlSeconds: number,
	): PreparedQuery;
	conditionalBatch(entries: ReadonlyArray<ConditionalWriteEntry<Row, PK>>): PreparedQuery;
	deleteCql(opts?: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>}): string;
	delete(opts?: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>}): QueryTemplate;
	deleteByPk(pk: Pick<Row, PK>): PreparedQuery;
	conditionalDeleteByPk(pk: Pick<Row, PK>, expected: Partial<Row>): PreparedQuery;
	deletePartition(pk: Pick<Row, PartKey>): PreparedQuery;
	insert(row: Row): PreparedQuery;
	insertIfNotExists(row: Row): PreparedQuery;
	insertIfNotExistsWithTtl(row: Row, ttlSeconds: number): PreparedQuery;
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
	upsertAllWithTtl(row: Row, ttlSeconds: number): PreparedQuery;
	where: {
		eq: <K extends ColumnName<Row>>(col: K, param?: string) => WhereExpr<Row>;
		in: <K extends ColumnName<Row>>(col: K, param: string) => WhereExpr<Row>;
		lt: <K extends ColumnName<Row>>(col: K, param?: string) => WhereExpr<Row>;
		gt: <K extends ColumnName<Row>>(col: K, param?: string) => WhereExpr<Row>;
		lte: <K extends ColumnName<Row>>(col: K, param?: string) => WhereExpr<Row>;
		gte: <K extends ColumnName<Row>>(col: K, param?: string) => WhereExpr<Row>;
		tokenGt: <K extends ColumnName<Row>>(col: K, param: string) => WhereExpr<Row>;
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

function findUndefinedPath(value: unknown): string | null {
	if (value === undefined) return '';
	if (value === null) return null;
	const t = typeof value;
	if (t === 'string' || t === 'number' || t === 'bigint' || t === 'boolean') return null;
	if (value instanceof Date || value instanceof Buffer) return null;
	if (value instanceof LocalDate) return null;
	if (Array.isArray(value)) {
		for (let i = 0; i < value.length; i++) {
			const suffix = findUndefinedPath(value[i]);
			if (suffix !== null) return `[${i}]${suffix}`;
		}
		return null;
	}
	if (value instanceof Set) {
		let index = 0;
		for (const item of value.values()) {
			const suffix = findUndefinedPath(item);
			if (suffix !== null) return `{set:${index}}${suffix}`;
			index++;
		}
		return null;
	}
	if (value instanceof Map) {
		let index = 0;
		for (const [key, item] of value.entries()) {
			const keySuffix = findUndefinedPath(key);
			if (keySuffix !== null) return `{mapKey:${index}}${keySuffix}`;
			const valueSuffix = findUndefinedPath(item);
			if (valueSuffix !== null) return `{mapVal:${index}}${valueSuffix}`;
			index++;
		}
		return null;
	}
	if (t === 'object') {
		const keys = Object.keys(value as Record<string, unknown>);
		for (const key of keys) {
			const suffix = findUndefinedPath((value as Record<string, unknown>)[key]);
			if (suffix !== null) return `.${key}${suffix}`;
		}
	}
	return null;
}

export function assertNoUndefinedParams(params: Record<string, unknown>): void {
	const keys = Object.keys(params);
	for (let i = 0; i < keys.length; i++) {
		const k = keys[i]!;
		const suffix = findUndefinedPath(params[k]);
		if (suffix !== null) {
			throw new Error(
				`Undefined value at ":${k}${suffix}". This project forbids undefined in Cassandra params; use null explicitly or omit the column via PATCH.`,
			);
		}
	}
}

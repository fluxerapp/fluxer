/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import cassandra from 'cassandra-driver';
import {Config} from '~/Config';
import {Logger} from '~/Logger';

const IS_DEV = Config.nodeEnv === 'development';

const colors = {
	reset: '\x1b[0m',
	dim: '\x1b[2m',
	bold: '\x1b[1m',
	cyan: '\x1b[36m',
	yellow: '\x1b[33m',
	green: '\x1b[32m',
	magenta: '\x1b[35m',
	blue: '\x1b[34m',
	gray: '\x1b[90m',
	white: '\x1b[37m',
} as const;

function formatValue(value: unknown): string {
	if (value === null) return `${colors.dim}null${colors.reset}`;
	if (value === undefined) return `${colors.dim}undefined${colors.reset}`;
	if (typeof value === 'string') {
		const truncated = value.length > 50 ? `${value.slice(0, 50)}...` : value;
		return `${colors.green}"${truncated}"${colors.reset}`;
	}
	if (typeof value === 'number' || typeof value === 'bigint') {
		return `${colors.yellow}${value}${colors.reset}`;
	}
	if (typeof value === 'boolean') {
		return `${colors.magenta}${value}${colors.reset}`;
	}
	if (value instanceof Date) {
		return `${colors.cyan}Date(${value.toISOString()})${colors.reset}`;
	}
	if (value instanceof Buffer) {
		return `${colors.dim}Buffer(${value.length} bytes)${colors.reset}`;
	}
	if (value instanceof Set) {
		if (value.size === 0) return `${colors.dim}Set{}${colors.reset}`;
		if (value.size > 3) return `${colors.dim}Set{${value.size} items}${colors.reset}`;
		const items = [...value].map((v) => formatValue(v)).join(', ');
		return `Set{${items}}`;
	}
	if (value instanceof Map) {
		if (value.size === 0) return `${colors.dim}Map{}${colors.reset}`;
		return `${colors.dim}Map{${value.size} entries}${colors.reset}`;
	}
	if (Array.isArray(value)) {
		if (value.length === 0) return `${colors.dim}[]${colors.reset}`;
		if (value.length > 5) return `${colors.dim}[${value.length} items]${colors.reset}`;
		const items = value.map((v) => formatValue(v)).join(', ');
		return `[${items}]`;
	}
	if (typeof value === 'object') {
		const entries = Object.entries(value as Record<string, unknown>);
		if (entries.length === 0) return `${colors.dim}{}${colors.reset}`;
		if (entries.length > 5) return `${colors.dim}{${entries.length} keys}${colors.reset}`;
		const formatted = entries.map(([k, v]) => `${k}: ${formatValue(v)}`).join(', ');
		return `{${formatted}}`;
	}
	return String(value);
}

function formatParams(params: Record<string, unknown>): string {
	const entries = Object.entries(params);
	if (entries.length === 0) return `${colors.dim}(no params)${colors.reset}`;
	return entries.map(([k, v]) => `  ${colors.blue}:${k}${colors.reset} = ${formatValue(v)}`).join('\n');
}

function getQueryType(cql: string): string {
	const trimmed = cql.trim().toUpperCase();
	if (trimmed.startsWith('SELECT')) return 'SELECT';
	if (trimmed.startsWith('INSERT')) return 'INSERT';
	if (trimmed.startsWith('UPDATE')) return 'UPDATE';
	if (trimmed.startsWith('DELETE')) return 'DELETE';
	if (trimmed.startsWith('BEGIN BATCH')) return 'BATCH';
	return 'QUERY';
}

function formatCql(cql: string): string {
	return cql
		.replace(/\s+/g, ' ')
		.replace(/\s*;\s*$/, '')
		.trim();
}

function logQuery(
	queryType: string,
	cql: string,
	params: Record<string, unknown>,
	durationMs: number,
	rowCount?: number,
): void {
	if (!IS_DEV) return;

	const typeColors: Record<string, string> = {
		SELECT: colors.cyan,
		INSERT: colors.green,
		UPDATE: colors.yellow,
		DELETE: colors.magenta,
		BATCH: colors.blue,
		QUERY: colors.white,
	};

	const typeColor = typeColors[queryType] || colors.white;
	const durationColor = durationMs > 100 ? colors.yellow : durationMs > 50 ? colors.dim : colors.green;

	const lines = [
		`${colors.dim}┌──${colors.reset} ${typeColor}${colors.bold}${queryType}${colors.reset} ${colors.dim}──────────────────────────────────────${colors.reset}`,
		`${colors.dim}│${colors.reset} ${formatCql(cql)}`,
		`${colors.dim}│${colors.reset}`,
		...formatParams(params)
			.split('\n')
			.map((line) => `${colors.dim}│${colors.reset}${line}`),
		`${colors.dim}│${colors.reset}`,
		`${colors.dim}└──${colors.reset} ${durationColor}${durationMs.toFixed(2)}ms${colors.reset}${rowCount !== undefined ? ` ${colors.dim}(${rowCount} rows)${colors.reset}` : ''}`,
	];

	console.log(lines.join('\n'));
}

function logBatch(queries: Array<{query: string; params: object}>, durationMs: number): void {
	if (!IS_DEV) return;

	const lines = [
		`${colors.dim}┌──${colors.reset} ${colors.blue}${colors.bold}BATCH${colors.reset} ${colors.dim}(${queries.length} queries) ──────────────────────────${colors.reset}`,
	];

	for (let i = 0; i < queries.length; i++) {
		const {query, params} = queries[i];
		const queryType = getQueryType(query);
		const typeColors: Record<string, string> = {
			SELECT: colors.cyan,
			INSERT: colors.green,
			UPDATE: colors.yellow,
			DELETE: colors.magenta,
		};
		const typeColor = typeColors[queryType] || colors.white;

		lines.push(
			`${colors.dim}│${colors.reset} ${colors.dim}[${i + 1}]${colors.reset} ${typeColor}${queryType}${colors.reset} ${formatCql(query)}`,
		);

		const paramEntries = Object.entries(params as Record<string, unknown>);
		if (paramEntries.length > 0 && paramEntries.length <= 4) {
			const paramStr = paramEntries.map(([k, v]) => `${colors.blue}:${k}${colors.reset}=${formatValue(v)}`).join(' ');
			lines.push(`${colors.dim}│${colors.reset}     ${paramStr}`);
		} else if (paramEntries.length > 4) {
			lines.push(`${colors.dim}│${colors.reset}     ${colors.dim}(${paramEntries.length} params)${colors.reset}`);
		}
	}

	const durationColor = durationMs > 100 ? colors.yellow : durationMs > 50 ? colors.dim : colors.green;
	lines.push(`${colors.dim}└──${colors.reset} ${durationColor}${durationMs.toFixed(2)}ms${colors.reset}`);

	console.log(lines.join('\n'));
}

export type DbOp<T> = {kind: 'set'; value: T} | {kind: 'clear'};

export const Db = {
	set<T>(value: T): DbOp<T> {
		return {kind: 'set', value};
	},
	clear<T = never>(): DbOp<T> {
		return {kind: 'clear'};
	},
} as const;

export type ColumnName<Row> = Extract<keyof Row, string>;
type RowValue<Row, K extends ColumnName<Row>> = Row[K & keyof Row];

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

export interface PreparedQuery<P extends CassandraParams = CassandraParams> {
	cql: string;
	params: P;
}

export function prepared<P extends CassandraParams>(cql: string, params: P): PreparedQuery<P> {
	return {cql, params};
}

export interface QueryTemplate<P extends CassandraParams = CassandraParams> {
	cql: string;
	bind(params: P): PreparedQuery<P>;
}

const clientOptions: cassandra.ClientOptions = {
	contactPoints: Config.cassandra.hosts.split(','),
	keyspace: Config.cassandra.keyspace,
	localDataCenter: Config.cassandra.localDc,
	encoding: {
		map: Map,
		set: Set,
		useUndefinedAsUnset: false,
		useBigIntAsLong: true,
		useBigIntAsVarint: true,
	},
};

if (Config.cassandra.username && Config.cassandra.password) {
	clientOptions.credentials = {
		username: Config.cassandra.username,
		password: Config.cassandra.password,
	};
}

const client = new cassandra.Client(clientOptions);

const DEFAULT_MAX_PARTITION_KEYS_PER_QUERY = 100;

function chunkArray<T>(items: Array<T>, size: number): Array<Array<T>> {
	const chunks: Array<Array<T>> = [];
	if (size <= 0) {
		throw new Error('Chunk size must be greater than 0');
	}
	for (let i = 0; i < items.length; i += size) {
		chunks.push(items.slice(i, i + size));
	}
	return chunks;
}

function isUnsafePreparedStatement(query: string): boolean {
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

function assertNoUndefinedParams(params: Record<string, unknown>): void {
	for (const [k, v] of Object.entries(params)) {
		assertNoUndefinedDeep(v, `:${k}`);
	}
}

function normalizeExecuteArgs<P extends CassandraParams>(
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

async function executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
	queryOrPrepared: string | PreparedQuery<P>,
	params?: P,
): Promise<Array<T>> {
	const {cql, params: bound} = normalizeExecuteArgs(queryOrPrepared, params);

	if (isUnsafePreparedStatement(cql)) {
		throw new Error('Cannot prepare a statement that looks like `SELECT *`');
	}

	assertNoUndefinedParams(bound as Record<string, unknown>);

	const startTime = IS_DEV ? performance.now() : 0;

	try {
		const result = await client.execute(cql, bound, {prepare: true});
		const rows = (result.rows ?? []) as Array<T>;

		if (IS_DEV) {
			const durationMs = performance.now() - startTime;
			logQuery(getQueryType(cql), cql, bound as Record<string, unknown>, durationMs, rows.length);
		}

		return rows;
	} catch (err: unknown) {
		const paramSummary: Record<string, unknown> = {};
		for (const [k, v] of Object.entries(bound as Record<string, unknown>)) {
			if (typeof v === 'string') paramSummary[k] = {type: 'string', len: v.length};
			else if (typeof v === 'bigint') paramSummary[k] = {type: 'bigint'};
			else if (typeof v === 'number') paramSummary[k] = {type: 'number'};
			else if (typeof v === 'boolean') paramSummary[k] = {type: 'boolean'};
			else if (v instanceof Buffer) paramSummary[k] = {type: 'buffer', len: v.length};
			else if (v instanceof Set) paramSummary[k] = {type: 'set', size: (v as Set<unknown>).size};
			else if (v instanceof Map) paramSummary[k] = {type: 'map', size: (v as Map<unknown, unknown>).size};
			else if (v instanceof Date) paramSummary[k] = {type: 'date'};
			else if (Array.isArray(v)) paramSummary[k] = {type: 'array', len: v.length};
			else if (v === null) paramSummary[k] = {type: 'null'};
			else paramSummary[k] = {type: typeof v};
		}
		const errorMessage = err instanceof Error ? err.message : String(err);
		Logger.warn({error: errorMessage, query: cql, params: paramSummary}, 'Cassandra query failed');
		throw err;
	}
}

export async function fetchOne<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
	queryOrPrepared: PreparedQuery<P> | string,
	params?: P,
): Promise<T | null> {
	const [row] = await executeQuery<T, P>(queryOrPrepared, params);
	return row ?? null;
}

export async function fetchMany<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
	queryOrPrepared: PreparedQuery<P> | string,
	params?: P,
): Promise<Array<T>> {
	return executeQuery<T, P>(queryOrPrepared, params);
}

export async function fetchManyInChunks<
	T = Record<string, unknown>,
	V = unknown,
	P extends CassandraParams = CassandraParams,
>(
	query: QueryTemplate<P> | PreparedQuery<P> | string,
	values: Array<V>,
	paramsFactory: (chunk: Array<V>) => P,
	chunkSize = DEFAULT_MAX_PARTITION_KEYS_PER_QUERY,
): Promise<Array<T>> {
	if (values.length === 0) return [];

	const chunks = chunkArray(values, chunkSize);
	const results = await Promise.all(
		chunks.map(async (chunk) => {
			const params = paramsFactory(chunk);

			if (typeof query === 'string') {
				return executeQuery<T, P>(query, params);
			}

			if ((query as PreparedQuery<P>).params !== undefined) {
				return executeQuery<T, P>(query as PreparedQuery<P>);
			}

			return executeQuery<T, P>((query as QueryTemplate<P>).bind(params));
		}),
	);

	return results.flat();
}

export async function upsertOne<P extends CassandraParams = CassandraParams>(
	queryOrPrepared: PreparedQuery<P> | string,
	params?: P,
): Promise<void> {
	await executeQuery(queryOrPrepared, params);
}

export async function deleteOneOrMany<P extends CassandraParams = CassandraParams>(
	queryOrPrepared: PreparedQuery<P> | string,
	params?: P,
): Promise<void> {
	await executeQuery(queryOrPrepared, params);
}

export async function executeConditional<P extends CassandraParams = CassandraParams>(
	queryOrPrepared: PreparedQuery<P> | string,
	params?: P,
): Promise<{applied: boolean; rows: Array<Record<string, unknown>>}> {
	const {cql, params: bound} = normalizeExecuteArgs(queryOrPrepared, params);

	if (isUnsafePreparedStatement(cql)) {
		throw new Error('Cannot prepare a statement that looks like `SELECT *`');
	}

	assertNoUndefinedParams(bound as Record<string, unknown>);

	const startTime = IS_DEV ? performance.now() : 0;

	try {
		const result = await client.execute(cql, bound, {prepare: true});
		interface MaybeApplied {
			wasApplied?: () => boolean;
		}
		const applied =
			typeof (result as MaybeApplied).wasApplied === 'function' ? (result as MaybeApplied).wasApplied!() : false;

		if (IS_DEV) {
			const durationMs = performance.now() - startTime;
			logQuery(
				`${getQueryType(cql)} (LWT${applied ? ' applied' : ' not applied'})`,
				cql,
				bound as Record<string, unknown>,
				durationMs,
				result.rows.length,
			);
		}

		return {applied, rows: result.rows as Array<Record<string, unknown>>};
	} catch (err: unknown) {
		const errorMessage = err instanceof Error ? err.message : String(err);
		Logger.warn({error: errorMessage, query: cql}, 'Cassandra conditional query failed');
		throw err;
	}
}

interface BatchQuery {
	query: string;
	params: object;
}

async function executeBatch(queries: Array<BatchQuery>, atomic = true): Promise<void> {
	if (queries.length === 0) return;

	for (const {query} of queries) {
		if (isUnsafePreparedStatement(query)) {
			throw new Error('Cannot prepare a statement that looks like `SELECT *`');
		}
	}

	for (const {params} of queries) {
		assertNoUndefinedParams(params as Record<string, unknown>);
	}

	const options = {
		prepare: true,
		logged: atomic,
		counter: false,
	};

	const startTime = IS_DEV ? performance.now() : 0;

	await client.batch(
		queries.map(({query, params}) => ({query, params})),
		options,
	);

	if (IS_DEV) {
		const durationMs = performance.now() - startTime;
		logBatch(queries, durationMs);
	}
}

export class BatchBuilder {
	private queries: Array<BatchQuery> = [];

	add(query: string, params: object): this {
		this.queries.push({query, params});
		return this;
	}

	addPrepared(q: PreparedQuery): this {
		this.queries.push({query: q.cql, params: q.params});
		return this;
	}

	addIf(condition: boolean, query: string, params: object): this {
		if (condition) this.queries.push({query, params});
		return this;
	}

	addPreparedIf(condition: boolean, q: PreparedQuery): this {
		if (condition) this.queries.push({query: q.cql, params: q.params});
		return this;
	}

	async execute(atomic = true): Promise<void> {
		if (this.queries.length === 0) return;
		await executeBatch(this.queries, atomic);
	}

	getQueries(): Array<BatchQuery> {
		return this.queries;
	}
}

export type WhereExpr<Row extends object> =
	| {kind: 'eq'; col: ColumnName<Row>; param: string}
	| {kind: 'in'; col: ColumnName<Row>; param: string}
	| {kind: 'lt'; col: ColumnName<Row>; param: string}
	| {kind: 'gt'; col: ColumnName<Row>; param: string}
	| {kind: 'lte'; col: ColumnName<Row>; param: string}
	| {kind: 'gte'; col: ColumnName<Row>; param: string}
	| {kind: 'tokenGt'; col: ColumnName<Row>; param: string}
	| {kind: 'tupleGt'; cols: ReadonlyArray<ColumnName<Row>>; params: ReadonlyArray<string>};

export type OrderBy<Row extends object> = {col: ColumnName<Row>; direction?: 'ASC' | 'DESC'};

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
		patch: Partial<{[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>}>,
	): PreparedQuery;

	deleteCql(opts?: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>}): string;

	delete(opts?: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>}): QueryTemplate;

	deleteByPk(pk: Pick<Row, PK>): PreparedQuery;

	deletePartition(pk: Pick<Row, PartKey>): PreparedQuery;

	insertCql(opts?: {ttlParam?: string}): string;

	insert(row: Row): PreparedQuery;

	insertWithTtl(row: Row, ttlSeconds: number): PreparedQuery;

	insertWithTtlParam(row: Row, ttlParamName: string): PreparedQuery;

	insertIfNotExists(row: Row): PreparedQuery;

	selectCountCql(opts?: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>}): string;

	selectCount(opts?: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>}): QueryTemplate;

	insertWithNow<NowCol extends ColumnName<Row>>(row: Omit<Row, NowCol>, nowColumn: NowCol): PreparedQuery;

	patchByPkWithTtl(
		pk: Pick<Row, PK>,
		patch: Partial<{[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>}>,
		ttlSeconds: number,
	): PreparedQuery;

	patchByPkWithTtlParam(
		pk: Pick<Row, PK>,
		patch: Partial<{[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>}>,
		ttlParamName: string,
		ttlValue: number,
	): PreparedQuery;

	upsertAllWithTtl(row: Row, ttlSeconds: number): PreparedQuery;

	upsertAllWithTtlParam(row: Row, ttlParamName: string, ttlValue: number): PreparedQuery;

	patchByPkIf<CondCol extends Exclude<ColumnName<Row>, PK>>(
		pk: Pick<Row, PK>,
		patch: Partial<{[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>}>,
		condition: {col: CondCol; expectedParam: string; expectedValue: RowValue<Row, CondCol>},
	): PreparedQuery;

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

function compileWhere<Row extends object>(w: WhereExpr<Row>): string {
	switch (w.kind) {
		case 'eq':
			return `${w.col} = :${w.param}`;
		case 'in':
			return `${w.col} IN :${w.param}`;
		case 'lt':
			return `${w.col} < :${w.param}`;
		case 'gt':
			return `${w.col} > :${w.param}`;
		case 'lte':
			return `${w.col} <= :${w.param}`;
		case 'gte':
			return `${w.col} >= :${w.param}`;
		case 'tokenGt':
			return `TOKEN(${w.col}) > TOKEN(:${w.param})`;
		case 'tupleGt': {
			if (w.cols.length !== w.params.length || w.cols.length === 0) {
				throw new Error('tupleGt requires equal-length non-empty cols/params');
			}
			const cols = `(${w.cols.join(', ')})`;
			const params = `(${w.params.map((p) => `:${p}`).join(', ')})`;
			return `${cols} > ${params}`;
		}
		default: {
			const _exhaustive: never = w;
			return _exhaustive;
		}
	}
}

function opToValue(op: DbOp<unknown>): CassandraParam {
	return op.kind === 'clear' ? null : (op.value as CassandraParam);
}

export function defineTable<Row extends object, PK extends ColumnName<Row>, PartKey extends ColumnName<Row> = PK>(def: {
	name: string;
	columns: ReadonlyArray<ColumnName<Row>>;
	primaryKey: ReadonlyArray<PK>;
	partitionKey?: ReadonlyArray<PartKey>;
}): Table<Row, PK, PartKey> {
	const columns = [...def.columns];
	const pk = [...def.primaryKey];
	const partitionKey = [...(def.partitionKey ?? def.primaryKey)] as Array<PartKey>;

	const nonPkColumns = columns.filter((c) => !pk.includes(c as PK)) as Array<Exclude<ColumnName<Row>, PK>>;

	const updateAll =
		nonPkColumns.length > 0
			? `UPDATE ${def.name}
SET ${nonPkColumns.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};
`
			: `INSERT INTO ${def.name} (${columns.join(', ')}) VALUES (${columns.map((c) => `:${c}`).join(', ')});`;

	function paramsFromRow(row: Row, requireAll: boolean = true): CassandraParams {
		const params: CassandraParams = {};
		for (const c of columns) {
			const v = row[c as keyof Row];
			if (v === undefined) {
				if (requireAll) {
					throw new Error(
						`Row is missing value for "${def.name}.${c}". Full-row upserts require every column to be present (use patchByPk() for partial writes).`,
					);
				}
				continue;
			}
			params[c] = v as CassandraParam;
		}
		return params;
	}

	function buildDynamicUpsertCql(row: Row): {cql: string; params: CassandraParams} {
		const presentColumns: Array<string> = [];
		const params: CassandraParams = {};

		for (const c of columns) {
			const v = row[c as keyof Row];
			if (v !== undefined) {
				presentColumns.push(c);
				params[c] = v as CassandraParam;
			}
		}

		const nonPkColumns = presentColumns.filter((c) => !pk.includes(c as PK));
		const cql =
			nonPkColumns.length > 0
				? `UPDATE ${def.name}
SET ${nonPkColumns.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};
`
				: `INSERT INTO ${def.name} (${pk.join(', ')}) VALUES (${pk.map((c) => `:${c}`).join(', ')});`;

		return {cql, params};
	}

	function selectCql(
		opts: {
			columns?: ReadonlyArray<ColumnName<Row>>;
			where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
			orderBy?: OrderBy<Row>;
			limit?: number;
		} = {},
	): string {
		const selectCols = (opts.columns ?? columns).join(', ');

		let where = '';
		if (opts.where) {
			const clauses = Array.isArray(opts.where) ? opts.where : [opts.where];
			if (clauses.length > 0) {
				where = ` WHERE ${clauses.map((c) => compileWhere<Row>(c)).join(' AND ')}`;
			}
		}

		const orderBy = opts.orderBy != null ? ` ORDER BY ${opts.orderBy.col} ${opts.orderBy.direction ?? 'ASC'}` : '';
		const limit = typeof opts.limit === 'number' ? ` LIMIT ${opts.limit}` : '';

		return `SELECT ${selectCols} FROM ${def.name}${where}${orderBy}${limit};`;
	}

	function select(
		opts: {
			columns?: ReadonlyArray<ColumnName<Row>>;
			where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
			orderBy?: OrderBy<Row>;
			limit?: number;
		} = {},
	): QueryTemplate {
		const cql = selectCql(opts);
		return {
			cql,
			bind(params: CassandraParams) {
				return prepared(cql, params);
			},
		};
	}

	function patchByPk(
		pkValues: Pick<Row, PK>,
		patch: Partial<{[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>}>,
	): PreparedQuery {
		const patchKeys = Object.keys(patch) as Array<Exclude<ColumnName<Row>, PK>>;
		if (patchKeys.length === 0) {
			throw new Error(`Refusing to execute empty PATCH update on table "${def.name}"`);
		}

		patchKeys.sort((a, b) => columns.indexOf(a) - columns.indexOf(b));

		const cql = `UPDATE ${def.name}
SET ${patchKeys.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};
`;

		const params: CassandraParams = {};
		for (const k of pk) params[k] = pkValues[k] as CassandraParam;
		for (const c of patchKeys) params[c] = opToValue(patch[c] as DbOp<unknown>);

		return prepared(cql, params);
	}

	const deleteByPkCql = `DELETE FROM ${def.name} WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};`;

	function deleteCql(opts: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>} = {}): string {
		let where = '';
		if (opts.where) {
			const clauses = Array.isArray(opts.where) ? opts.where : [opts.where];
			if (clauses.length > 0) {
				where = ` WHERE ${clauses.map((c) => compileWhere<Row>(c)).join(' AND ')}`;
			}
		} else {
			where = ` WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')}`;
		}
		return `DELETE FROM ${def.name}${where};`;
	}

	function del(opts: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>} = {}): QueryTemplate {
		const cql = deleteCql(opts);
		return {
			cql,
			bind(params: CassandraParams) {
				return prepared(cql, params);
			},
		};
	}

	function deleteByPk(pkValues: Pick<Row, PK>): PreparedQuery {
		const params: CassandraParams = {};
		for (const k of pk) params[k] = pkValues[k] as CassandraParam;
		return prepared(deleteByPkCql, params);
	}

	function deletePartition(partKeyValues: Pick<Row, PartKey>): PreparedQuery {
		if (partitionKey.length === 0) {
			throw new Error(`Table "${def.name}" has empty partitionKey; cannot deletePartition()`);
		}
		const cql = `DELETE FROM ${def.name} WHERE ${partitionKey.map((k) => `${k} = :${k}`).join(' AND ')};`;
		const params: CassandraParams = {};
		for (const k of partitionKey) params[k] = (partKeyValues as Record<string, CassandraParam>)[k];
		return prepared(cql, params);
	}

	const insertBaseCql = `INSERT INTO ${def.name} (${columns.join(', ')}) VALUES (${columns.map((c) => `:${c}`).join(', ')})`;

	function insertCql(opts: {ttlParam?: string} = {}): string {
		if (opts.ttlParam) return `${insertBaseCql} USING TTL :${opts.ttlParam};`;
		return `${insertBaseCql};`;
	}

	function insert(row: Row): PreparedQuery {
		return prepared(`${insertBaseCql};`, paramsFromRow(row));
	}

	function insertWithTtl(row: Row, ttlSeconds: number): PreparedQuery {
		const cql = `${insertBaseCql} USING TTL ${ttlSeconds};`;
		return prepared(cql, paramsFromRow(row));
	}

	function insertWithTtlParam(row: Row, ttlParamName: string): PreparedQuery {
		const cql = `${insertBaseCql} USING TTL :${ttlParamName};`;
		const params = paramsFromRow(row);
		if (params[ttlParamName] === undefined) {
			params[ttlParamName] = row[ttlParamName as keyof Row] as CassandraParam;
		}
		return prepared(cql, params as CassandraParams);
	}

	function insertIfNotExists(row: Row): PreparedQuery {
		const cql = `${insertBaseCql} IF NOT EXISTS;`;
		return prepared(cql, paramsFromRow(row));
	}

	function selectCountCql(opts: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>} = {}): string {
		let where = '';
		if (opts.where) {
			const clauses = Array.isArray(opts.where) ? opts.where : [opts.where];
			if (clauses.length > 0) {
				where = ` WHERE ${clauses.map((c) => compileWhere<Row>(c)).join(' AND ')}`;
			}
		}
		return `SELECT COUNT(*) as count FROM ${def.name}${where};`;
	}

	function selectCount(opts: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>} = {}): QueryTemplate {
		const cql = selectCountCql(opts);
		return {
			cql,
			bind(params: CassandraParams) {
				return prepared(cql, params);
			},
		};
	}

	function insertWithNow<NowCol extends ColumnName<Row>>(row: Omit<Row, NowCol>, nowColumn: NowCol): PreparedQuery {
		const otherColumns = columns.filter((c) => c !== nowColumn);
		const allCols = [...otherColumns, nowColumn];
		const values = otherColumns.map((c) => `:${c}`).concat(['now()']);
		const cql = `INSERT INTO ${def.name} (${allCols.join(', ')}) VALUES (${values.join(', ')});`;

		const params: CassandraParams = {};
		for (const c of otherColumns) {
			if (c === nowColumn) continue;
			const v = (row as Record<string, unknown>)[c];
			if (v === undefined) {
				throw new Error(`Row is missing value for "${def.name}.${c}". INSERT requires every column to be present.`);
			}
			params[c] = v as CassandraParam;
		}
		return prepared(cql, params);
	}

	function patchByPkWithTtl(
		pkValues: Pick<Row, PK>,
		patch: Partial<{[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>}>,
		ttlSeconds: number,
	): PreparedQuery {
		const patchKeys = Object.keys(patch) as Array<Exclude<ColumnName<Row>, PK>>;
		if (patchKeys.length === 0) {
			throw new Error(`Refusing to execute empty PATCH update on table "${def.name}"`);
		}

		patchKeys.sort((a, b) => columns.indexOf(a) - columns.indexOf(b));

		const cql = `UPDATE ${def.name} USING TTL ${ttlSeconds}
SET ${patchKeys.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};
`;

		const params: CassandraParams = {};
		for (const k of pk) params[k] = pkValues[k] as CassandraParam;
		for (const c of patchKeys) params[c] = opToValue(patch[c] as DbOp<unknown>);

		return prepared(cql, params);
	}

	function patchByPkWithTtlParam(
		pkValues: Pick<Row, PK>,
		patch: Partial<{[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>}>,
		ttlParamName: string,
		ttlValue: number,
	): PreparedQuery {
		const patchKeys = Object.keys(patch) as Array<Exclude<ColumnName<Row>, PK>>;
		if (patchKeys.length === 0) {
			throw new Error(`Refusing to execute empty PATCH update on table "${def.name}"`);
		}

		patchKeys.sort((a, b) => columns.indexOf(a) - columns.indexOf(b));

		const cql = `UPDATE ${def.name} USING TTL :${ttlParamName}
SET ${patchKeys.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};
`;

		const params: CassandraParams = {};
		for (const k of pk) params[k] = pkValues[k] as CassandraParam;
		for (const c of patchKeys) params[c] = opToValue(patch[c] as DbOp<unknown>);
		params[ttlParamName] = ttlValue;

		return prepared(cql, params);
	}

	function upsertAllWithTtl(row: Row, ttlSeconds: number): PreparedQuery {
		const cql =
			nonPkColumns.length > 0
				? `UPDATE ${def.name} USING TTL ${ttlSeconds}
SET ${nonPkColumns.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};
`
				: `INSERT INTO ${def.name} (${columns.join(', ')}) VALUES (${columns.map((c) => `:${c}`).join(', ')}) USING TTL ${ttlSeconds};`;
		return prepared(cql, paramsFromRow(row));
	}

	function upsertAllWithTtlParam(row: Row, ttlParamName: string, ttlValue: number): PreparedQuery {
		const cql =
			nonPkColumns.length > 0
				? `UPDATE ${def.name} USING TTL :${ttlParamName}
SET ${nonPkColumns.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};
`
				: `INSERT INTO ${def.name} (${columns.join(', ')}) VALUES (${columns.map((c) => `:${c}`).join(', ')}) USING TTL :${ttlParamName};`;
		const params = paramsFromRow(row);
		params[ttlParamName] = ttlValue;
		return prepared(cql, params);
	}

	function patchByPkIf<CondCol extends Exclude<ColumnName<Row>, PK>>(
		pkValues: Pick<Row, PK>,
		patch: Partial<{[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>}>,
		condition: {col: CondCol; expectedParam: string; expectedValue: RowValue<Row, CondCol>},
	): PreparedQuery {
		const patchKeys = Object.keys(patch) as Array<Exclude<ColumnName<Row>, PK>>;
		if (patchKeys.length === 0) {
			throw new Error(`Refusing to execute empty conditional PATCH update on table "${def.name}"`);
		}

		patchKeys.sort((a, b) => columns.indexOf(a) - columns.indexOf(b));

		const cql = `UPDATE ${def.name}
SET ${patchKeys.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')}
IF ${condition.col} = :${condition.expectedParam};
`;

		const params: CassandraParams = {};
		for (const k of pk) params[k] = pkValues[k] as CassandraParam;
		for (const c of patchKeys) params[c] = opToValue(patch[c] as DbOp<unknown>);
		params[condition.expectedParam] = condition.expectedValue as CassandraParam;

		return prepared(cql, params);
	}

	return {
		name: def.name,
		columns: def.columns,
		primaryKey: def.primaryKey,
		partitionKey: partitionKey,

		selectCql,
		select,

		updateAllCql() {
			return updateAll;
		},

		paramsFromRow,

		upsertAll(row: Row) {
			const hasAllColumns = columns.every((c) => row[c as keyof Row] !== undefined);
			if (hasAllColumns) {
				return prepared(updateAll, paramsFromRow(row));
			}
			const {cql, params} = buildDynamicUpsertCql(row);
			return prepared(cql, params);
		},

		patchByPk,

		deleteCql,
		delete: del,
		deleteByPk,
		deletePartition,

		insertCql,
		insert,
		insertWithTtl,
		insertWithTtlParam,
		insertIfNotExists,

		selectCountCql,
		selectCount,

		insertWithNow,

		patchByPkWithTtl,
		patchByPkWithTtlParam,
		upsertAllWithTtl,
		upsertAllWithTtlParam,
		patchByPkIf,

		where: {
			eq: (col, param) => ({kind: 'eq', col, param: param ?? col}),
			in: (col, param) => ({kind: 'in', col, param}),
			lt: (col, param) => ({kind: 'lt', col, param: param ?? col}),
			gt: (col, param) => ({kind: 'gt', col, param: param ?? col}),
			lte: (col, param) => ({kind: 'lte', col, param: param ?? col}),
			gte: (col, param) => ({kind: 'gte', col, param: param ?? col}),
			tokenGt: (col, param) => ({kind: 'tokenGt', col, param}),
			tupleGt: (cols, params) => ({kind: 'tupleGt', cols, params}),
		},
	};
}

const DEFAULT_LWT_RETRIES = 8;

export type PatchObject = {[key: string]: DbOp<unknown>};

export async function executeVersionedUpdate<
	Row extends {version?: number | null},
	PK extends ColumnName<Row>,
	Patch extends PatchObject = PatchObject,
>(
	fetchCurrent: () => Promise<Row | null>,
	buildPatch: (current: Row | null) => {pk: Record<string, unknown>; patch: Patch},
	table: Table<Row, PK>,
	opts?: {maxRetries?: number; onFailure?: 'throw' | 'log' | 'silent'},
): Promise<{applied: boolean; finalVersion: number | null}> {
	const maxRetries = opts?.maxRetries ?? DEFAULT_LWT_RETRIES;

	for (let i = 0; i < maxRetries; i++) {
		const current = await fetchCurrent();
		const currentVersion = current?.version ?? null;
		const newVersion = (currentVersion ?? 0) + 1;

		const {pk, patch} = buildPatch(current);

		const q = table.patchByPkIf(
			pk as Pick<Row, PK>,
			{...patch, version: Db.set(newVersion)} as Partial<{[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>}>,
			{
				col: 'version' as Exclude<ColumnName<Row>, PK>,
				expectedParam: 'prev_version',
				expectedValue: currentVersion as RowValue<Row, Exclude<ColumnName<Row>, PK>>,
			},
		);

		const res = await executeConditional(q);
		if (res.applied) {
			return {applied: true, finalVersion: newVersion};
		}
	}

	if (opts?.onFailure === 'throw') {
		throw new Error('LWT update failed after max retries');
	} else if (opts?.onFailure !== 'silent') {
		Logger.warn({}, 'LWT update failed after max retries');
	}

	return {applied: false, finalVersion: null};
}

export function buildPatchFromData<Row extends object>(
	newData: Partial<Row>,
	oldData: Partial<Row> | null,
	columns: ReadonlyArray<keyof Row>,
	pkColumns: ReadonlyArray<keyof Row>,
): Record<string, DbOp<unknown>> {
	const patch: Record<string, DbOp<unknown>> = {};
	for (const col of columns) {
		if (pkColumns.includes(col)) continue;
		if (col === 'version') continue;

		const colName = col as string;
		const newVal = (newData as Record<string, unknown>)[colName];
		if (newVal === undefined) continue;

		const oldVal = oldData ? (oldData as Record<string, unknown>)[colName] : undefined;

		if (newVal === null) {
			if (oldData !== null && oldVal !== null && oldVal !== undefined) {
				patch[colName] = Db.clear();
			}
		} else {
			patch[colName] = Db.set(newVal);
		}
	}

	return patch;
}

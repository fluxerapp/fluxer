// SPDX-License-Identifier: AGPL-3.0-or-later

import {type IPostgresClient, type PostgresQueryable, quoteIdentifier} from '@pkgs/postgres/src/Client';
import cassandra from 'cassandra-driver';
import {getKvMeta, getTableMetadata} from './CassandraMetaRegistry';
import type {CassandraParams, ColumnName, KvQueryMeta, PreparedQuery, WhereExpr} from './CassandraTypes';

type Row = Record<string, unknown>;
type EqWhereExpr = Extract<WhereExpr<Row>, {kind: 'eq'}>;

interface StoredRow {
	row_key: string;
	row_data: unknown;
}

interface PageState {
	offset: number;
}

const VALUE_SEPARATOR = '\u001f';
const ENCODED_TYPE_KEY = '__fluxer_type';

function normalizeCql(cql: string): string {
	return cql.replace(/\s+/g, ' ').trim();
}

function isPlainObject(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object' && Object.getPrototypeOf(value) === Object.prototype;
}

function encodeValue(value: unknown): unknown {
	if (value === null || value === undefined) return null;
	if (typeof value === 'bigint') return {[ENCODED_TYPE_KEY]: 'bigint', value: value.toString()};
	if (value instanceof Date) return {[ENCODED_TYPE_KEY]: 'date', value: value.toISOString()};
	if (Buffer.isBuffer(value)) return {[ENCODED_TYPE_KEY]: 'buffer', value: value.toString('base64')};
	if (value instanceof Set) return {[ENCODED_TYPE_KEY]: 'set', value: [...value.values()].map(encodeValue)};
	if (value instanceof Map) {
		return {
			[ENCODED_TYPE_KEY]: 'map',
			value: [...value.entries()].map(([key, entry]) => [encodeValue(key), encodeValue(entry)]),
		};
	}
	if (typeof value === 'object' && value.constructor?.name === 'LocalDate') {
		return {[ENCODED_TYPE_KEY]: 'local_date', value: value.toString()};
	}
	if (Array.isArray(value)) return value.map(encodeValue);
	if (isPlainObject(value)) {
		const encoded: Record<string, unknown> = {};
		for (const [key, entry] of Object.entries(value)) {
			encoded[key] = encodeValue(entry);
		}
		return encoded;
	}
	return value;
}

function decodeValue(value: unknown): unknown {
	if (Array.isArray(value)) return value.map(decodeValue);
	if (!isPlainObject(value)) return value;
	const encodedType = value[ENCODED_TYPE_KEY];
	if (encodedType === 'bigint') return BigInt(String(value.value));
	if (encodedType === 'date') return new Date(String(value.value));
	if (encodedType === 'buffer') return Buffer.from(String(value.value), 'base64');
	if (encodedType === 'set') return new Set(((value.value as Array<unknown>) ?? []).map(decodeValue));
	if (encodedType === 'map') {
		return new Map(
			((value.value as Array<[unknown, unknown]>) ?? []).map(([key, entry]) => [decodeValue(key), decodeValue(entry)]),
		);
	}
	if (encodedType === 'local_date') return cassandra.types.LocalDate.fromString(String(value.value));
	const decoded: Record<string, unknown> = {};
	for (const [key, entry] of Object.entries(value)) {
		decoded[key] = decodeValue(entry);
	}
	return decoded;
}

function encodeRow(row: Row): Record<string, unknown> {
	const encoded: Record<string, unknown> = {};
	for (const [key, value] of Object.entries(row)) {
		encoded[key] = encodeValue(value);
	}
	return encoded;
}

function decodeRow(value: unknown): Row {
	const decoded = decodeValue(value);
	if (!isPlainObject(decoded)) {
		throw new Error('Postgres KV row payload is not an object');
	}
	return decoded;
}

function valueKey(value: unknown): string {
	return JSON.stringify(encodeValue(value));
}

function keyFromColumns(columns: ReadonlyArray<string>, row: Row): string {
	return columns.map((column) => valueKey(row[column])).join(VALUE_SEPARATOR);
}

function paramsRow(params: CassandraParams, columns: ReadonlyArray<string>): Row {
	const row: Row = {};
	for (const column of columns) {
		if (!(column in params)) {
			throw new Error(`Missing Postgres KV key parameter: ${column}`);
		}
		row[column] = params[column];
	}
	return row;
}

function rowFromParams(meta: KvQueryMeta, params: CassandraParams): Row {
	const row: Row = {};
	for (const column of meta.table.columns) {
		if (column in params) {
			row[column] = params[column];
		}
	}
	if (meta.nowColumn) {
		row[meta.nowColumn] = new Date();
	}
	return row;
}

function rowKey(meta: KvQueryMeta, row: Row): string {
	return keyFromColumns(meta.table.primaryKey as ReadonlyArray<string>, row);
}

function partitionKey(meta: KvQueryMeta, row: Row): string {
	return keyFromColumns(meta.table.partitionKey as ReadonlyArray<string>, row);
}

function rowKeyFromParams(meta: KvQueryMeta, params: CassandraParams): string {
	return rowKey(meta, paramsRow(params, (meta.pkColumns ?? meta.table.primaryKey) as ReadonlyArray<string>));
}

function partitionKeyFromParams(meta: KvQueryMeta, params: CassandraParams): string {
	return partitionKey(meta, paramsRow(params, meta.table.partitionKey as ReadonlyArray<string>));
}

function compareValues(left: unknown, right: unknown): number {
	if (typeof left === 'bigint' || typeof right === 'bigint') {
		const l = typeof left === 'bigint' ? left : BigInt(left as number | string);
		const r = typeof right === 'bigint' ? right : BigInt(right as number | string);
		if (l === r) return 0;
		return l < r ? -1 : 1;
	}
	const l = left instanceof Date ? left.getTime() : left?.constructor?.name === 'LocalDate' ? left.toString() : left;
	const r =
		right instanceof Date ? right.getTime() : right?.constructor?.name === 'LocalDate' ? right.toString() : right;
	if (Buffer.isBuffer(l) && Buffer.isBuffer(r)) return Buffer.compare(l, r);
	if (l === r) return 0;
	return (l as number | string) < (r as number | string) ? -1 : 1;
}

function valuesEqual(left: unknown, right: unknown): boolean {
	if (left == null && right == null) return true;
	if (left instanceof Date && right instanceof Date) return left.getTime() === right.getTime();
	if (Buffer.isBuffer(left) && Buffer.isBuffer(right)) return left.equals(right);
	if (left?.constructor?.name === 'LocalDate' || right?.constructor?.name === 'LocalDate') {
		return left?.toString() === right?.toString();
	}
	return left === right;
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
			case 'tokenGt':
				if (compareValues(row[clause.col], getParam(params, clause.param)) <= 0) return false;
				break;
			case 'gte':
				if (compareValues(row[clause.col], getParam(params, clause.param)) < 0) return false;
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
	if (!columns) return {...row};
	const projected: Row = {};
	for (const column of columns) {
		projected[column] = row[column];
	}
	return projected;
}

function sortRows(meta: KvQueryMeta, rows: Array<Row>): Array<Row> {
	if (meta.orderBy) {
		const direction = meta.orderBy.direction === 'DESC' ? -1 : 1;
		return rows.sort((left, right) => compareValues(left[meta.orderBy!.col], right[meta.orderBy!.col]) * direction);
	}
	return rows.sort((left, right) => {
		for (const column of meta.table.primaryKey) {
			const cmp = compareValues(left[column], right[column]);
			if (cmp !== 0) return cmp;
		}
		return 0;
	});
}

function equalityParam(where: ReadonlyArray<WhereExpr<Row>> | undefined, column: string): string | null {
	const clause = (where ?? []).find((entry) => entry.kind === 'eq' && entry.col === column);
	return clause && clause.kind === 'eq' ? clause.param : null;
}

function inParam(where: ReadonlyArray<WhereExpr<Row>> | undefined, column: string): string | null {
	const clause = (where ?? []).find((entry) => entry.kind === 'in' && entry.col === column);
	return clause && clause.kind === 'in' ? clause.param : null;
}

function fullRowKeysFromWhere(meta: KvQueryMeta, params: CassandraParams): Array<string> | null {
	const pk = meta.table.primaryKey as ReadonlyArray<string>;
	const eqParams = pk.map((column) => equalityParam(meta.where as ReadonlyArray<WhereExpr<Row>> | undefined, column));
	if (eqParams.every((param) => param !== null)) {
		const row: Row = {};
		for (let i = 0; i < pk.length; i += 1) row[pk[i]!] = params[eqParams[i]!];
		return [rowKey(meta, row)];
	}
	if (pk.length === 1) {
		const param = inParam(meta.where as ReadonlyArray<WhereExpr<Row>> | undefined, pk[0]!);
		if (param) {
			const values = params[param] as ReadonlyArray<unknown> | Set<unknown>;
			const haystack = values instanceof Set ? [...values] : values;
			return haystack.map((value) => rowKey(meta, {[pk[0]!]: value}));
		}
	}
	return null;
}

function hasFullPartition(meta: KvQueryMeta): boolean {
	return meta.table.partitionKey.every((column) =>
		equalityParam(meta.where as ReadonlyArray<WhereExpr<Row>> | undefined, column),
	);
}

function ttlExpiresAt(meta: KvQueryMeta, params: CassandraParams): Date | null | undefined {
	const ttlParam = meta.ttlParamName;
	if (!ttlParam) return undefined;
	const ttlRaw = params[ttlParam];
	if (typeof ttlRaw !== 'number') {
		throw new Error(`TTL parameter ${ttlParam} must be a number`);
	}
	return new Date(Date.now() + ttlRaw * 1000);
}

function encodePageState(pageState: PageState): string {
	return Buffer.from(JSON.stringify(pageState)).toString('base64url');
}

function decodePageState(pageState: string | null | undefined): PageState {
	if (!pageState) return {offset: 0};
	const decoded = JSON.parse(Buffer.from(pageState, 'base64url').toString('utf8')) as PageState;
	if (!Number.isInteger(decoded.offset) || decoded.offset < 0) {
		throw new Error('Invalid Postgres KV page state');
	}
	return decoded;
}

function parseRawMeta(cql: string): KvQueryMeta<Row> | null {
	const normalized = normalizeCql(cql).replace(/;$/, '');
	const update =
		/^UPDATE\s+([A-Za-z0-9_]+)(?:\s+USING\s+(?:TIMESTAMP|TTL)\s+:[A-Za-z0-9_]+)?\s+SET\s+(.+?)\s+WHERE\s+(.+)$/iu.exec(
			normalized,
		);
	if (update) {
		const table = tableSpec(update[1]!);
		const patchKeys = update[2]!.split(',').map((part) => {
			const [column, value] = part.trim().split(/\s*=\s*/u);
			if (!column || !value?.startsWith(':')) {
				throw new Error(`Postgres KV raw UPDATE only supports parameter assignments: ${cql}`);
			}
			return column;
		});
		const where = parseEqWhere(update[3]!, cql);
		return {
			action: 'patch',
			table,
			where,
			patchKeys,
			pkColumns: where.map((clause) => clause.col),
		};
	}
	const select =
		/^SELECT\s+(.+?)\s+FROM\s+([A-Za-z0-9_]+)(?:\s+WHERE\s+(.+?))?(?:\s+ALLOW\s+FILTERING)?(?:\s+LIMIT\s+(\d+))?$/iu.exec(
			normalized,
		);
	if (select) {
		const table = tableSpec(select[2]!);
		return {
			action: 'select',
			table,
			columns: select[1]!.split(',').map((part) => part.trim() as ColumnName<Row>),
			where: select[3] ? parseEqWhere(select[3], cql) : [],
			limit: select[4] ? Number.parseInt(select[4], 10) : undefined,
		};
	}
	return null;
}

function tableSpec(tableName: string): KvQueryMeta<Row>['table'] {
	const table = getTableMetadata(tableName);
	if (!table) {
		throw new Error(`Postgres KV metadata is missing for table: ${tableName}`);
	}
	return table;
}

function parseEqWhere(whereSql: string, cql: string): ReadonlyArray<EqWhereExpr> {
	return whereSql.split(/\s+AND\s+/iu).map((part) => {
		const match = /^\s*([A-Za-z0-9_]+)\s*=\s*:([A-Za-z0-9_]+)\s*$/u.exec(part.trim());
		if (!match) {
			throw new Error(`Postgres KV raw WHERE only supports equality predicates: ${cql}`);
		}
		return {kind: 'eq', col: match[1]! as ColumnName<Row>, param: match[2]!};
	});
}

export async function ensurePostgresKvSchema(client: IPostgresClient): Promise<void> {
	const table = quoteIdentifier(client.kvTable());
	await client.query(`
CREATE TABLE IF NOT EXISTS ${table} (
	table_name text NOT NULL,
	partition_key text NOT NULL,
	row_key text NOT NULL,
	row_data jsonb NOT NULL,
	expires_at timestamptz,
	updated_at timestamptz NOT NULL DEFAULT now(),
	PRIMARY KEY (table_name, row_key)
)`);
	await client.query(
		`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${client.kvTable()}_partition_row_idx`)} ON ${table} (table_name, partition_key, row_key)`,
	);
	await client.query(
		`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${client.kvTable()}_row_key_c_idx`)} ON ${table} (table_name, row_key COLLATE "C")`,
	);
	await client.query(
		`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${client.kvTable()}_expires_idx`)} ON ${table} (expires_at) WHERE expires_at IS NOT NULL`,
	);
	await client.query(
		`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${client.kvTable()}_messages_message_idx`)} ON ${table} (partition_key, ((CASE WHEN row_data -> 'message_id' ->> 'value' ~ '^-?[0-9]+$' THEN (row_data -> 'message_id' ->> 'value')::bigint END))) WHERE table_name = 'messages'`,
	);
	await client.query(
		`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${client.kvTable()}_message_reactions_message_idx`)} ON ${table} (partition_key, ((CASE WHEN row_data -> 'message_id' ->> 'value' ~ '^-?[0-9]+$' THEN (row_data -> 'message_id' ->> 'value')::bigint END))) WHERE table_name = 'message_reactions'`,
	);
	await client.query(`
UPDATE ${table}
SET partition_key = split_part(row_key, chr(31), 1) || chr(31) || split_part(row_key, chr(31), 2)
WHERE table_name = 'messages'
	AND partition_key = row_key
	AND split_part(row_key, chr(31), 3) <> ''`);
	await client.query(`DROP INDEX IF EXISTS ${quoteIdentifier(`${client.kvTable()}_partition_idx`)}`);
}

export async function pruneExpiredPostgresKvRows(client: IPostgresClient, batchSize = 5000): Promise<number> {
	if (!Number.isInteger(batchSize) || batchSize <= 0) {
		throw new Error('Postgres KV prune batch size must be a positive integer');
	}
	const table = quoteIdentifier(client.kvTable());
	const result = await client.query(
		`
WITH expired AS (
	SELECT table_name, row_key
	FROM ${table}
	WHERE expires_at IS NOT NULL AND expires_at <= now()
	ORDER BY expires_at
	LIMIT $1
	FOR UPDATE SKIP LOCKED
)
DELETE FROM ${table} kv
USING expired
WHERE kv.table_name = expired.table_name AND kv.row_key = expired.row_key`,
		[batchSize],
	);
	return result.rowCount ?? 0;
}

export class PostgresKvQueryExecutor {
	private readonly table: string;

	constructor(private readonly client: IPostgresClient) {
		this.table = quoteIdentifier(client.kvTable());
	}

	async executeQuery<T = Row, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
		db: PostgresQueryable = this.client,
	): Promise<Array<T>> {
		const meta = this.meta(query);
		switch (meta.action) {
			case 'select':
				return (await this.select(meta, query.params, db)) as Array<T>;
			case 'count':
				return [{count: (await this.select(meta, query.params, db)).length}] as Array<T>;
			case 'upsert':
				return (await this.upsert(meta, query.params, db)) as Array<T>;
			case 'insert':
				return (await this.upsert(meta, query.params, db)) as Array<T>;
			case 'patch':
				await this.patch(meta, query.params, db);
				return [];
			case 'delete':
				await this.delete(meta, query.params, db);
				return [];
			case 'batch':
				return [];
			default: {
				const _exhaustive: never = meta.action;
				throw new Error(`Unsupported Postgres KV action: ${_exhaustive}`);
			}
		}
	}

	async executePagedQuery<T = Row, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
		options: {pageSize: number; pageState?: string | null},
	): Promise<{rows: Array<T>; pageState: string | null}> {
		const state = decodePageState(options.pageState);
		const rows = await this.executeQuery<T, P>(query);
		const pageRows = rows.slice(state.offset, state.offset + options.pageSize);
		const nextOffset = state.offset + pageRows.length;
		return {
			rows: pageRows,
			pageState: nextOffset < rows.length ? encodePageState({offset: nextOffset}) : null,
		};
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		atomic = true,
	): Promise<void> {
		if (atomic) {
			await this.client.transaction(async (db) => {
				for (const query of queries) {
					await this.executeQuery({cql: query.query, params: query.params as CassandraParams, kvMeta: query.meta}, db);
				}
			});
			return;
		}
		for (const query of queries) {
			await this.executeQuery({cql: query.query, params: query.params as CassandraParams, kvMeta: query.meta});
		}
	}

	private meta(query: PreparedQuery): KvQueryMeta<Row> {
		const meta = (query.kvMeta ?? getKvMeta(query.cql) ?? parseRawMeta(query.cql)) as KvQueryMeta<Row> | null;
		if (!meta) {
			throw new Error(`Postgres KV does not understand query: ${query.cql}`);
		}
		return meta;
	}

	private async candidates(
		meta: KvQueryMeta,
		params: CassandraParams,
		db: PostgresQueryable,
	): Promise<Array<StoredRow>> {
		const rowKeys = fullRowKeysFromWhere(meta, params);
		if (rowKeys) {
			const result = await db.query<StoredRow>(
				`SELECT row_key, row_data FROM ${this.table} WHERE table_name = $1 AND row_key = ANY($2::text[]) AND (expires_at IS NULL OR expires_at > now())`,
				[meta.table.name, rowKeys],
			);
			return result.rows;
		}
		if (hasFullPartition(meta)) {
			const result = await db.query<StoredRow>(
				`SELECT row_key, row_data FROM ${this.table} WHERE table_name = $1 AND partition_key = $2 AND (expires_at IS NULL OR expires_at > now())`,
				[meta.table.name, partitionKeyFromParams(meta, params)],
			);
			return result.rows;
		}
		const result = await db.query<StoredRow>(
			`SELECT row_key, row_data FROM ${this.table} WHERE table_name = $1 AND (expires_at IS NULL OR expires_at > now())`,
			[meta.table.name],
		);
		return result.rows;
	}

	private async select(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<Array<Row>> {
		let rows = (await this.candidates(meta, params, db))
			.map((stored) => decodeRow(stored.row_data))
			.filter((row) => matchesWhere(row, meta.where as ReadonlyArray<WhereExpr<Row>> | undefined, params));
		rows = sortRows(meta, rows);
		if (typeof meta.limit === 'number') rows = rows.slice(0, meta.limit);
		return rows.map((row) => projectRow(row, meta.columns as ReadonlyArray<string> | undefined));
	}

	private async upsert(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<Array<Row>> {
		const incoming = rowFromParams(meta, params);
		const key = rowKey(meta, incoming);
		const existing = await this.getRow(meta, key, db);
		if (meta.ifNotExists && existing) {
			return [{'[applied]': false}];
		}
		if (meta.ifNotExists) {
			await db.query(
				`DELETE FROM ${this.table} WHERE table_name = $1 AND row_key = $2 AND expires_at IS NOT NULL AND expires_at <= now()`,
				[meta.table.name, key],
			);
		}
		const next = {...(existing ?? {}), ...incoming};
		const expiresAt = ttlExpiresAt(meta, params) ?? null;
		const result = await db.query(
			`INSERT INTO ${this.table} (table_name, partition_key, row_key, row_data, expires_at, updated_at)
VALUES ($1, $2, $3, $4::jsonb, $5, now())
ON CONFLICT (table_name, row_key)
DO UPDATE SET partition_key = EXCLUDED.partition_key, row_data = EXCLUDED.row_data, expires_at = EXCLUDED.expires_at, updated_at = now()
WHERE NOT $6`,
			[
				meta.table.name,
				partitionKey(meta, next),
				key,
				JSON.stringify(encodeRow(next)),
				expiresAt,
				meta.ifNotExists === true,
			],
		);
		if (meta.ifNotExists) {
			return [{'[applied]': result.rowCount === 1}];
		}
		return [];
	}

	private async patch(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<void> {
		const key = rowKeyFromParams(meta, params);
		const stored = await this.getStoredRow(meta, key, db);
		const base = stored?.row ?? paramsRow(params, (meta.pkColumns ?? meta.table.primaryKey) as ReadonlyArray<string>);
		const next = {...base};
		for (const column of meta.patchKeys ?? []) {
			next[column] = column in params ? params[column] : null;
		}
		const ttl = ttlExpiresAt(meta, params);
		const expiresAt = ttl === undefined ? (stored?.expiresAt ?? null) : ttl;
		await db.query(
			`INSERT INTO ${this.table} (table_name, partition_key, row_key, row_data, expires_at, updated_at)
VALUES ($1, $2, $3, $4::jsonb, $5, now())
ON CONFLICT (table_name, row_key)
DO UPDATE SET partition_key = EXCLUDED.partition_key, row_data = EXCLUDED.row_data, expires_at = EXCLUDED.expires_at, updated_at = now()`,
			[meta.table.name, partitionKey(meta, next), key, JSON.stringify(encodeRow(next)), expiresAt ?? null],
		);
	}

	private async delete(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<void> {
		const rows = await this.candidates(meta, params, db);
		const matchingKeys = rows
			.filter((stored) =>
				matchesWhere(decodeRow(stored.row_data), meta.where as ReadonlyArray<WhereExpr<Row>> | undefined, params),
			)
			.map((stored) => stored.row_key);
		if (matchingKeys.length === 0) return;
		await db.query(`DELETE FROM ${this.table} WHERE table_name = $1 AND row_key = ANY($2::text[])`, [
			meta.table.name,
			matchingKeys,
		]);
	}

	private async getStoredRow(
		meta: KvQueryMeta,
		key: string,
		db: PostgresQueryable,
	): Promise<{row: Row; expiresAt: Date | null} | null> {
		const result = await db.query<StoredRow & {expires_at: Date | null}>(
			`SELECT row_key, row_data, expires_at FROM ${this.table} WHERE table_name = $1 AND row_key = $2 AND (expires_at IS NULL OR expires_at > now()) LIMIT 1`,
			[meta.table.name, key],
		);
		const row = result.rows[0];
		return row ? {row: decodeRow(row.row_data), expiresAt: row.expires_at ?? null} : null;
	}

	private async getRow(meta: KvQueryMeta, key: string, db: PostgresQueryable): Promise<Row | null> {
		return (await this.getStoredRow(meta, key, db))?.row ?? null;
	}
}

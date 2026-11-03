// SPDX-License-Identifier: AGPL-3.0-or-later

import {getKvMeta, getTableMetadata} from '@app/api/database/CassandraMetaRegistry';
import type {
	CassandraParams,
	ColumnName,
	KvAdvance,
	KvColumnParam,
	KvQueryMeta,
	PreparedQuery,
	WhereExpr,
} from '@app/api/database/CassandraTypes';
import {isConditionalQuery, validateTtlSeconds} from '@app/api/database/CassandraTypes';
import {LocalDate} from '@app/api/database/LocalDate';
import {Logger} from '@app/api/Logger';
import {type IPostgresClient, type PostgresQueryable, quoteIdentifier} from '@pkgs/postgres/src/Client';

type Row = Record<string, unknown>;
type EqWhereExpr = Extract<WhereExpr<Row>, {kind: 'eq'}>;
type InWhereExpr = Extract<WhereExpr<Row>, {kind: 'in'}>;
type PinnedWhereExpr = EqWhereExpr | InWhereExpr;

interface StoredRow {
	row_key: string;
	row_data: unknown;
}

interface PageState {
	offset: number;
	after?: string;
	keyed?: boolean;
	byRowKey?: boolean;
	since?: string;
}

interface ScanWalk {
	numeric: boolean;
	after: string | null;
	since: string | null;
}

interface NumericFloor {
	value: string;
	inclusive: boolean;
}

interface PageEntry {
	key: string;
	row: Row;
}

interface RangeGroup {
	lowerBounds: Array<string>;
	upperBounds: Array<string>;
}

interface KeyBound {
	op: '<' | '<=' | '>' | '>=';
	value: string;
}

interface OrderedRange {
	lowerBound: string;
	upperBound: string;
	segment: number;
	descending: boolean;
	bounds: Array<KeyBound>;
	partition: string | null;
}

interface OrderedWalk {
	entries: Array<PageEntry>;
	after: string | null;
	more: boolean;
}

export type CandidatePlan =
	| {kind: 'none'}
	| {kind: 'rowKeys'; rowKeys: Array<string>}
	| {kind: 'range'; lowerBound: string; upperBound: string}
	| {kind: 'ranges'; lowerBounds: Array<string>; upperBounds: Array<string>}
	| {kind: 'rangeGroups'; groups: Array<RangeGroup>}
	| {kind: 'scan'};

interface QueryPlan {
	candidates: CandidatePlan;
	exact: boolean;
}

interface QueryShape {
	leadingClauses: ReadonlyArray<PinnedWhereExpr>;
	inClauses: ReadonlyArray<InWhereExpr>;
	whereColumns: ReadonlyArray<string>;
	requiredColumns: ReadonlyArray<string> | null;
	clauseCount: number;
	summary: string;
}

interface PlanFragments {
	using: string;
	predicate: string;
	params: Array<unknown>;
}

const VALUE_SEPARATOR = '\u001f';
const KEY_RANGE_UPPER = ' ';
const MAX_ROW_KEY_COMBINATIONS = 32_768;
const MAX_PREFIX_RANGES = 256;
const FULL_SCAN_LOG_INTERVAL_MS = 60_000;
const FULL_SCAN_LOG_KEY_LIMIT = 1024;
const ENCODED_TYPE_KEY = '__fluxer_type';
const POSTGRES_KV_SCHEMA_LOCK_NAMESPACE = 0x46584b56;
const POSTGRES_KV_SCHEMA_LOCK_TIMEOUT = '120s';
const POSTGRES_KV_SCHEMA_MIGRATION_TIMEOUT = '30min';
export const POSTGRES_KV_MIGRATION_TABLE = '__fluxer_schema_migrations';
const POSTGRES_KV_MESSAGES_PARTITION_MIGRATION = 'messages_partition_key_v1';
const POSTGRES_KV_SORT_ID_MIGRATION = 'sort_id_v1';
const POSTGRES_KV_SORT_ID_TABLES = ['messages', 'message_reactions'];
const POSTGRES_KV_SORT_ID_BACKFILL_BATCH = 5000;
const POSTGRES_KV_STORAGE_OPTIONS = [
	'fillfactor=85',
	'autovacuum_vacuum_scale_factor=0',
	'autovacuum_vacuum_threshold=20000',
	'autovacuum_vacuum_insert_scale_factor=0.05',
	'autovacuum_analyze_scale_factor=0.01',
];
const POSTGRES_KV_ALTER_LOCK_TIMEOUT = '10s';
const POSTGRES_KV_SCHEMA_ATTEMPTS = 3;
const KV_WRITE_PARALLELISM = 4;
const POSTGRES_KV_SCHEMA_RETRY_DELAY_MS = 250;
const POSTGRES_KV_CONCURRENT_DDL_CODES = new Set(['23505', '42P07', '42710', '55P03']);
const NUMERIC_ROW_KEY_BIGINT_PATTERN = '^\\{"__fluxer_type":"bigint","value":"(-?[0-9]+)"\\}$';
const NUMERIC_ROW_KEY_NUMBER_PATTERN = '^(-?[0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?)$';
const EXPIRED_STORED_ROW = 'kv.expires_at IS NOT NULL AND kv.expires_at <= now()';
const MERGED_ROW_DATA = `CASE WHEN ${EXPIRED_STORED_ROW} THEN EXCLUDED.row_data ELSE kv.row_data || EXCLUDED.row_data END`;
const KEPT_EXPIRES_AT = `CASE WHEN ${EXPIRED_STORED_ROW} THEN NULL ELSE kv.expires_at END`;
const NO_EXPIRY = 'infinity';
const BIGINT_ROW_KEY_PREFIX = `{"${ENCODED_TYPE_KEY}":"bigint","value":"`;
const INT64_MIN = -(2n ** 63n);
const INT64_MAX = 2n ** 63n - 1n;
const KEY_BOUND_OPERATORS: Partial<Record<WhereExpr<Row>['kind'], KeyBound['op']>> = {
	lt: '<',
	lte: '<=',
	gt: '>',
	gte: '>=',
};
const KEY_BOUND_NAMES: Record<KeyBound['op'], string> = {'<': 'lt', '<=': 'lte', '>': 'gt', '>=': 'gte'};
const NON_NUMERIC_ROW_KEY_RANGES = [
	`kv.row_key COLLATE "C" < '-'`,
	`kv.row_key COLLATE "C" >= ':' AND kv.row_key COLLATE "C" < '${BIGINT_ROW_KEY_PREFIX}'`,
	`kv.row_key COLLATE "C" >= '${BIGINT_ROW_KEY_PREFIX}:'`,
];

export async function postgresKvPassRecorded(client: IPostgresClient, marker: string): Promise<boolean> {
	const result = await client.query(
		`SELECT 1 FROM ${quoteIdentifier(client.kvTable())} WHERE table_name = $1 AND row_key = $2`,
		[POSTGRES_KV_MIGRATION_TABLE, marker],
	);
	return result.rows.length > 0;
}

export async function recordPostgresKvCleanPass(client: IPostgresClient, marker: string): Promise<void> {
	await client.query(
		`INSERT INTO ${quoteIdentifier(client.kvTable())} (table_name, partition_key, row_key, row_data)
VALUES ($1, $2, $2, jsonb_build_object('applied_at', now()))
ON CONFLICT (table_name, row_key) DO UPDATE SET row_data = EXCLUDED.row_data, updated_at = now()`,
		[POSTGRES_KV_MIGRATION_TABLE, marker],
	);
}

function numericRowKeyExpr(column: string): string {
	return `(COALESCE(substring(${column} from '${NUMERIC_ROW_KEY_BIGINT_PATTERN}'), substring(${column} from '${NUMERIC_ROW_KEY_NUMBER_PATTERN}'))::numeric)`;
}

const NUMERIC_ROW_KEY = numericRowKeyExpr('kv.row_key');

function numericKeySegmentExpr(segment: number): string {
	const value = `split_part(kv.row_key, chr(31), ${segment})`;
	const digits = `substr(${value}, ${BIGINT_ROW_KEY_PREFIX.length + 1}, greatest(length(${value}) - ${BIGINT_ROW_KEY_PREFIX.length + 2}, 0))`;
	return `(CASE WHEN left(${value}, ${BIGINT_ROW_KEY_PREFIX.length}) = '${BIGINT_ROW_KEY_PREFIX}' AND right(${value}, 2) = '"}' AND ${digits} ~ '^-?[0-9]+$' THEN ${digits}::numeric WHEN ${value} ~ '${NUMERIC_ROW_KEY_NUMBER_PATTERN}' THEN ${value}::numeric END)`;
}

function planStatementName(prefix: string, plan: CandidatePlan): string {
	switch (plan.kind) {
		case 'rowKeys':
			return `${prefix}_rowkeys`;
		case 'range':
			return `${prefix}_range`;
		case 'ranges':
		case 'rangeGroups':
			return `${prefix}_ranges`;
		default:
			return `${prefix}_scan`;
	}
}

export function normalizeCql(cql: string): string {
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
	if (value instanceof LocalDate) {
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
	if (encodedType === 'local_date') return LocalDate.fromString(String(value.value));
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

function decodeRowColumns(value: unknown, columns: ReadonlyArray<string>): Row {
	if (!isPlainObject(value)) {
		throw new Error('Postgres KV row payload is not an object');
	}
	const decoded: Row = {};
	for (const column of columns) {
		if (column in value) {
			decoded[column] = decodeValue(value[column]);
		}
	}
	return decoded;
}

function valueKey(value: unknown): string {
	return JSON.stringify(encodeValue(value));
}

export function keyFromColumns(columns: ReadonlyArray<string>, row: Row): string {
	return columns.map((column) => valueKey(row[column])).join(VALUE_SEPARATOR);
}

function sortId(meta: KvQueryMeta, row: Row): string | null {
	if (!POSTGRES_KV_SORT_ID_TABLES.includes(meta.table.name)) return null;
	const messageId = row['message_id'];
	return typeof messageId === 'bigint' ? messageId.toString() : null;
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

function boundColumns(params: CassandraParams, columns: ReadonlyArray<KvColumnParam>): CassandraParams {
	const row: CassandraParams = {};
	for (const {col, param} of columns) {
		if (!Object.hasOwn(params, param) || params[param] === undefined) {
			throw new Error(`Missing conditional write parameter: ${param}`);
		}
		row[col] = params[param];
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

function comparisonValue(value: unknown): unknown {
	if (value instanceof Date) {
		return value.getTime();
	}
	return value instanceof LocalDate ? value.toString() : value;
}

function compareValues(left: unknown, right: unknown): number {
	if (typeof left === 'bigint' || typeof right === 'bigint') {
		const l = typeof left === 'bigint' ? left : BigInt(left as number | string);
		const r = typeof right === 'bigint' ? right : BigInt(right as number | string);
		if (l === r) return 0;
		return l < r ? -1 : 1;
	}
	const l = comparisonValue(left);
	const r = comparisonValue(right);
	if (Buffer.isBuffer(l) && Buffer.isBuffer(r)) return Buffer.compare(l, r);
	if (l === r) return 0;
	return (l as number | string) < (r as number | string) ? -1 : 1;
}

export function valuesEqual(left: unknown, right: unknown): boolean {
	if (left == null && right == null) return true;
	if (left instanceof Date && right instanceof Date) return left.getTime() === right.getTime();
	if (Buffer.isBuffer(left) && Buffer.isBuffer(right)) return left.equals(right);
	if (left instanceof LocalDate && right instanceof LocalDate) {
		return left.toString() === right.toString();
	}
	return left === right;
}

function getParam(params: CassandraParams, param: string): unknown {
	return params[param];
}

export function matchesWhere(
	row: Row,
	where: ReadonlyArray<WhereExpr<Row>> | undefined,
	params: CassandraParams,
): boolean {
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

function compareColumns(columns: ReadonlyArray<string>, left: Row, right: Row): number {
	for (const column of columns) {
		const cmp = compareValues(left[column], right[column]);
		if (cmp !== 0) return cmp;
	}
	return 0;
}

export function rowComparator(meta: KvQueryMeta): (left: Row, right: Row) => number {
	const primaryKey = meta.table.primaryKey as ReadonlyArray<string>;
	if (!meta.orderBy) return (left, right) => compareColumns(primaryKey, left, right);
	const column = meta.orderBy.col as string;
	const columns = [column, ...primaryKey.slice(primaryKey.indexOf(column) + 1)];
	const direction = meta.orderBy.direction === 'DESC' ? -1 : 1;
	return (left, right) => compareColumns(columns, left, right) * direction;
}

function sortRows(meta: KvQueryMeta, rows: Array<Row>): Array<Row> {
	return rows.sort(rowComparator(meta));
}

function decodeRowKey(key: string, columns: number): Array<unknown> | null {
	const segments = key.split(VALUE_SEPARATOR);
	if (segments.length !== columns) return null;
	const values: Array<unknown> = [];
	for (const segment of segments) {
		try {
			values.push(decodeValue(JSON.parse(segment)));
		} catch {
			return null;
		}
	}
	return values;
}

function compareKeyValues(left: ReadonlyArray<unknown>, right: ReadonlyArray<unknown>): number {
	for (let index = 0; index < left.length; index += 1) {
		const cmp = compareValues(left[index], right[index]);
		if (cmp !== 0) return cmp;
	}
	return 0;
}

function compareRowToKeyValues(meta: KvQueryMeta, row: Row, values: ReadonlyArray<unknown>): number {
	return compareKeyValues(
		(meta.table.primaryKey as ReadonlyArray<string>).map((column) => row[column]),
		values,
	);
}

function whereClauses(meta: KvQueryMeta): ReadonlyArray<WhereExpr<Row>> {
	return (meta.where ?? []) as ReadonlyArray<WhereExpr<Row>>;
}

function pinnedClause(where: ReadonlyArray<WhereExpr<Row>>, column: string): PinnedWhereExpr | null {
	for (const clause of where) {
		if ((clause.kind === 'eq' || clause.kind === 'in') && clause.col === column) return clause;
	}
	return null;
}

function clauseColumns(clause: WhereExpr<Row>): ReadonlyArray<string> {
	return [clause.col as string];
}

function describeClause(clause: WhereExpr<Row>): string {
	return `${clauseColumns(clause).join('+')} ${clause.kind}`;
}

function requiredColumnsFor(meta: KvQueryMeta, whereColumns: ReadonlyArray<string>): ReadonlyArray<string> | null {
	if (!meta.columns) return null;
	const required = new Set<string>(meta.columns as ReadonlyArray<string>);
	for (const column of whereColumns) required.add(column);
	if (meta.orderBy) required.add(meta.orderBy.col as string);
	for (const column of meta.table.primaryKey as ReadonlyArray<string>) required.add(column);
	for (const column of meta.table.columns as ReadonlyArray<string>) {
		if (!required.has(column)) return [...required];
	}
	return null;
}

const QUERY_SHAPES = new WeakMap<KvQueryMeta, QueryShape>();

function queryShape(meta: KvQueryMeta): QueryShape {
	const cached = QUERY_SHAPES.get(meta);
	if (cached) return cached;
	const where = whereClauses(meta);
	const leadingClauses: Array<PinnedWhereExpr> = [];
	for (const column of meta.table.primaryKey as ReadonlyArray<string>) {
		const clause = pinnedClause(where, column);
		if (!clause) break;
		leadingClauses.push(clause);
	}
	const whereColumns = [...new Set(where.flatMap(clauseColumns))];
	const shape: QueryShape = {
		leadingClauses,
		inClauses: where.filter((clause): clause is InWhereExpr => clause.kind === 'in'),
		whereColumns,
		requiredColumns: requiredColumnsFor(meta, whereColumns),
		clauseCount: where.length,
		summary: where.map(describeClause).join(', '),
	};
	QUERY_SHAPES.set(meta, shape);
	return shape;
}

function clauseValues(clause: PinnedWhereExpr, params: CassandraParams): Array<unknown> | null {
	if (clause.kind === 'eq') return [getParam(params, clause.param)];
	const values = getParam(params, clause.param);
	if (values === null || values === undefined) return [];
	if (values instanceof Set) return [...values];
	if (Array.isArray(values)) return [...values];
	return null;
}

function isKeyComparableValue(value: unknown): boolean {
	if (value === null || value === undefined) return true;
	if (typeof value === 'string' || typeof value === 'boolean' || typeof value === 'bigint') return true;
	if (typeof value === 'number') return Number.isFinite(value);
	if (Buffer.isBuffer(value)) return true;
	if (value instanceof Date) return !Number.isNaN(value.getTime());
	return false;
}

function keySegments(values: ReadonlyArray<unknown>): Array<string> | null {
	const segments: Array<string> = [];
	for (const value of values) {
		let segment: string;
		try {
			segment = valueKey(value);
		} catch {
			return null;
		}
		if (typeof segment !== 'string') return null;
		segments.push(segment);
	}
	return segments;
}

function combinationCount(segmentLists: ReadonlyArray<ReadonlyArray<string>>): number {
	let total = 1;
	for (const segments of segmentLists) total *= segments.length;
	return total;
}

function keyPrefixes(segmentLists: ReadonlyArray<ReadonlyArray<string>>): Array<string> {
	let prefixes: Array<string> | null = null;
	for (const segments of segmentLists) {
		const next: Array<string> = [];
		for (const segment of segments) {
			if (prefixes === null) {
				next.push(segment);
				continue;
			}
			for (const prefix of prefixes) next.push(`${prefix}${VALUE_SEPARATOR}${segment}`);
		}
		prefixes = next;
	}
	return [...new Set(prefixes ?? [])];
}

function keyRangeLowerBound(prefix: string): string {
	return `${prefix}${VALUE_SEPARATOR}`;
}

function keyRangeUpperBound(prefix: string): string {
	return `${prefix}${KEY_RANGE_UPPER}`;
}

function prefixRangeGroups(prefixes: ReadonlyArray<string>): Array<RangeGroup> {
	const groups: Array<RangeGroup> = [];
	for (let start = 0; start < prefixes.length; start += MAX_PREFIX_RANGES) {
		const group = prefixes.slice(start, start + MAX_PREFIX_RANGES);
		groups.push({lowerBounds: group.map(keyRangeLowerBound), upperBounds: group.map(keyRangeUpperBound)});
	}
	return groups;
}

interface PinnedColumn {
	values: Array<unknown>;
	segments: Array<string>;
}

function pinnedColumns(clauses: ReadonlyArray<PinnedWhereExpr>, params: CassandraParams): Array<PinnedColumn> {
	const pinned: Array<PinnedColumn> = [];
	for (const clause of clauses) {
		const values = clauseValues(clause, params);
		if (values === null || values.length === 0) break;
		const segments = keySegments(values);
		if (segments === null) break;
		pinned.push({values, segments: [...new Set(segments)]});
	}
	return pinned;
}

function boundedLeading(pinned: ReadonlyArray<PinnedColumn>, primaryKeyLength: number): number {
	for (let leading = pinned.length; leading > 0; leading -= 1) {
		const cap = leading === primaryKeyLength ? MAX_ROW_KEY_COMBINATIONS : MAX_PREFIX_RANGES;
		if (combinationCount(pinned.slice(0, leading).map((column) => column.segments)) <= cap) return leading;
	}
	return 0;
}

function chunkableLeading(pinned: ReadonlyArray<PinnedColumn>): number {
	for (let leading = pinned.length; leading > 0; leading -= 1) {
		if (combinationCount(pinned.slice(0, leading).map((column) => column.segments)) <= MAX_ROW_KEY_COMBINATIONS) {
			return leading;
		}
	}
	return 0;
}

function planIsExact(meta: KvQueryMeta, shape: QueryShape, pinned: ReadonlyArray<PinnedColumn>): boolean {
	if (pinned.length !== shape.clauseCount) return false;
	if (meta.limit !== undefined || meta.orderBy !== undefined) return false;
	for (const column of pinned) {
		for (const value of column.values) {
			if (!isKeyComparableValue(value)) return false;
		}
	}
	return true;
}

export function buildCandidatePlan(meta: KvQueryMeta, params: CassandraParams): QueryPlan {
	const shape = queryShape(meta);
	for (const clause of shape.inClauses) {
		const values = clauseValues(clause, params);
		if (values !== null && values.length === 0) {
			return {candidates: {kind: 'none'}, exact: true};
		}
	}
	const primaryKey = meta.table.primaryKey as ReadonlyArray<string>;
	const pinned = pinnedColumns(shape.leadingClauses, params);
	let leading = boundedLeading(pinned, primaryKey.length);
	if (leading === 0) leading = chunkableLeading(pinned);
	if (leading > 0) {
		const columns = pinned.slice(0, leading);
		const prefixes = keyPrefixes(columns.map((column) => column.segments));
		const exact = planIsExact(meta, shape, columns);
		if (leading === primaryKey.length) {
			return {candidates: {kind: 'rowKeys', rowKeys: prefixes}, exact};
		}
		if (prefixes.length === 1) {
			const prefix = prefixes[0]!;
			return {
				candidates: {kind: 'range', lowerBound: keyRangeLowerBound(prefix), upperBound: keyRangeUpperBound(prefix)},
				exact,
			};
		}
		if (prefixes.length > MAX_PREFIX_RANGES) {
			return {candidates: {kind: 'rangeGroups', groups: prefixRangeGroups(prefixes)}, exact};
		}
		return {
			candidates: {
				kind: 'ranges',
				lowerBounds: prefixes.map(keyRangeLowerBound),
				upperBounds: prefixes.map(keyRangeUpperBound),
			},
			exact,
		};
	}
	return {candidates: {kind: 'scan'}, exact: planIsExact(meta, shape, [])};
}

function rangeFragments(group: RangeGroup): PlanFragments {
	return {
		using: 'unnest($2::text[], $3::text[]) AS r(lo, hi)',
		predicate: ' AND kv.row_key COLLATE "C" >= r.lo AND kv.row_key COLLATE "C" < r.hi',
		params: [group.lowerBounds, group.upperBounds],
	};
}

function fromPlan(table: string, fragments: PlanFragments): string {
	return fragments.using === '' ? `${table} kv` : `${table} kv, ${fragments.using}`;
}

function planFragments(plan: Exclude<CandidatePlan, {kind: 'rangeGroups'}>): PlanFragments {
	switch (plan.kind) {
		case 'rowKeys':
			return {using: '', predicate: ' AND kv.row_key = ANY($2::text[])', params: [plan.rowKeys]};
		case 'range':
			return {
				using: '',
				predicate: ' AND kv.row_key COLLATE "C" >= $2 AND kv.row_key COLLATE "C" < $3',
				params: [plan.lowerBound, plan.upperBound],
			};
		case 'ranges':
			return rangeFragments(plan);
		default:
			return {using: '', predicate: '', params: []};
	}
}

function planFragmentGroups(plan: CandidatePlan): Array<PlanFragments> {
	if (plan.kind === 'rangeGroups') return plan.groups.map(rangeFragments);
	return [planFragments(plan)];
}

function logWarn(details: Record<string, unknown>, message: string): void {
	try {
		Logger.warn(details, message);
	} catch {}
}

function logError(details: Record<string, unknown>, message: string): void {
	try {
		Logger.error(details, message);
	} catch {}
}

const fullScanLoggedAt = new Map<string, number>();

function logFullScan(meta: KvQueryMeta, walk?: 'numeric' | 'row_key'): void {
	const shape = queryShape(meta);
	const key = `${meta.table.name}|${meta.action}|${shape.summary}|${walk ?? 'load'}`;
	const now = Date.now();
	const last = fullScanLoggedAt.get(key);
	if (last !== undefined && now - last < FULL_SCAN_LOG_INTERVAL_MS) return;
	if (fullScanLoggedAt.size >= FULL_SCAN_LOG_KEY_LIMIT) fullScanLoggedAt.clear();
	fullScanLoggedAt.set(key, now);
	logWarn(
		{table: meta.table.name, action: meta.action, where: shape.summary || 'none', walk: walk ?? 'load'},
		'Postgres KV full table scan',
	);
}

function ttlExpiresAt(meta: KvQueryMeta, params: CassandraParams): Date | typeof NO_EXPIRY | null | undefined {
	const ttlParam = meta.ttlParamName;
	if (!ttlParam) return undefined;
	const ttlRaw = params[ttlParam];
	if (typeof ttlRaw !== 'number') {
		throw new Error(`TTL parameter ${ttlParam} must be a number`);
	}
	const ttlSeconds = validateTtlSeconds(ttlRaw);
	if (ttlSeconds === 0) return meta.table.defaultTtlSeconds === undefined ? null : NO_EXPIRY;
	return new Date(Date.now() + ttlSeconds * 1000);
}

function defaultExpiresAt(meta: KvQueryMeta): Date | undefined {
	const ttlSeconds = meta.table.defaultTtlSeconds;
	return ttlSeconds === undefined ? undefined : new Date(Date.now() + ttlSeconds * 1000);
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
	if (typeof decoded.after !== 'string') return {offset: decoded.offset};
	return {
		offset: decoded.offset,
		after: decoded.after,
		keyed: decoded.keyed === true,
		byRowKey: decoded.byRowKey === true,
		...(typeof decoded.since === 'string' ? {since: decoded.since} : {}),
	};
}

function pageableSelect(meta: KvQueryMeta, pageSize: number): boolean {
	return (
		meta.action === 'select' &&
		meta.orderBy === undefined &&
		meta.limit === undefined &&
		Number.isInteger(pageSize) &&
		pageSize > 0
	);
}

function numericKeyValue(value: unknown): string | null {
	if (typeof value === 'bigint') return value.toString();
	if (typeof value === 'number' && Number.isFinite(value)) return String(value);
	return null;
}

function numericRowKeyValue(key: string): string | null {
	const values = decodeRowKey(key, 1);
	return values === null ? null : numericKeyValue(values[0]);
}

function singlePrimaryKey(meta: KvQueryMeta): boolean {
	return (meta.table.primaryKey as ReadonlyArray<string>).length === 1;
}

function walksInPrimaryKeyOrder(meta: KvQueryMeta): boolean {
	if (!singlePrimaryKey(meta)) return false;
	if (meta.orderBy === undefined) return true;
	return meta.orderBy.col === meta.table.primaryKey[0] && meta.orderBy.direction !== 'DESC';
}

function numericFloor(meta: KvQueryMeta, params: CassandraParams): NumericFloor | null {
	const column = meta.table.primaryKey[0];
	for (const clause of whereClauses(meta)) {
		if (clause.col !== column) continue;
		if (clause.kind !== 'gt' && clause.kind !== 'tokenGt' && clause.kind !== 'gte') continue;
		const value = numericKeyValue(getParam(params, clause.param));
		if (value !== null) return {value, inclusive: clause.kind === 'gte'};
	}
	return null;
}

function pageStart(meta: KvQueryMeta, entries: ReadonlyArray<PageEntry>, state: PageState): number {
	if (state.after === undefined) return state.offset;
	const index = entries.findIndex((entry) => entry.key === state.after);
	if (index >= 0) return index + 1;
	const values = decodeRowKey(state.after, (meta.table.primaryKey as ReadonlyArray<string>).length);
	if (values === null) return state.offset;
	let start = 0;
	while (start < entries.length && compareRowToKeyValues(meta, entries[start]!.row, values) <= 0) start += 1;
	return start;
}

function integerKeyValue(value: unknown): string | null {
	if (typeof value === 'bigint') return value.toString();
	if (typeof value === 'number' && Number.isSafeInteger(value)) return String(value);
	return null;
}

function fitsInt64(value: string): boolean {
	if (!/^-?[0-9]+$/.test(value)) return false;
	const parsed = BigInt(value);
	return parsed >= INT64_MIN && parsed <= INT64_MAX;
}

function orderedRange(meta: KvQueryMeta, params: CassandraParams, plan: CandidatePlan): OrderedRange | null {
	if (plan.kind !== 'range') return null;
	const primaryKey = meta.table.primaryKey as ReadonlyArray<string>;
	const column = primaryKey[primaryKey.length - 1]!;
	if (plan.lowerBound.split(VALUE_SEPARATOR).length !== primaryKey.length) return null;
	if (meta.orderBy !== undefined && meta.orderBy.col !== column) return null;
	const bounds: Array<KeyBound> = [];
	for (const clause of whereClauses(meta)) {
		const op = KEY_BOUND_OPERATORS[clause.kind];
		if (clause.col !== column || op === undefined) continue;
		const value = integerKeyValue(getParam(params, clause.param));
		if (value !== null) bounds.push({op, value});
	}
	const shape = queryShape(meta);
	if (Math.min(shape.leadingClauses.length, primaryKey.length - 1) + bounds.length !== shape.clauseCount) return null;
	const sorted =
		column === 'message_id' &&
		POSTGRES_KV_SORT_ID_TABLES.includes(meta.table.name) &&
		meta.table.partitionKey.length === primaryKey.length - 1 &&
		bounds.every((bound) => fitsInt64(bound.value));
	return {
		lowerBound: plan.lowerBound,
		upperBound: plan.upperBound,
		segment: primaryKey.length,
		descending: meta.orderBy?.direction === 'DESC',
		bounds,
		partition: sorted ? plan.lowerBound.slice(0, -VALUE_SEPARATOR.length) : null,
	};
}

function lastKeySegmentValue(key: string): string | null {
	return numericRowKeyValue(key.slice(key.lastIndexOf(VALUE_SEPARATOR) + VALUE_SEPARATOR.length));
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

function sortIdIndexName(kvTable: string, tableName: string): string {
	return `${kvTable}_${tableName}_sort_idx`;
}

function sortIdIndexNames(kvTable: string): Array<string> {
	return POSTGRES_KV_SORT_ID_TABLES.map((tableName) => sortIdIndexName(kvTable, tableName));
}

function jsonbSortIndexNames(kvTable: string): Array<string> {
	return [`${kvTable}_messages_message_idx`, `${kvTable}_message_reactions_message_idx`];
}

function retiredIndexNames(kvTable: string): Array<string> {
	return [...jsonbSortIndexNames(kvTable), `${kvTable}_partition_row_idx`];
}

async function repairInvalidPostgresKvIndexes(db: PostgresQueryable, kvTable: string): Promise<void> {
	const invalid = await db.query<{index_name: string; index_def: string}>(
		`SELECT cls.relname AS index_name, pg_get_indexdef(idx.indexrelid) AS index_def
FROM pg_index idx
JOIN pg_class cls ON cls.oid = idx.indexrelid
WHERE idx.indrelid = to_regclass($1)
	AND NOT idx.indisvalid
	AND NOT idx.indisprimary
	AND NOT idx.indisunique
	AND cls.relname <> ALL($2::text[])`,
		[kvTable, [...sortIdIndexNames(kvTable), ...retiredIndexNames(kvTable)]],
	);
	for (const row of invalid.rows) {
		logError({table: kvTable, index: row.index_name}, 'Postgres KV index is invalid, rebuilding it');
		await db.query(`DROP INDEX IF EXISTS ${quoteIdentifier(row.index_name)}`);
		await db.query(row.index_def);
	}
}

async function postgresKvRowKeyIsCCollated(db: PostgresQueryable, kvTable: string): Promise<boolean> {
	const result = await db.query<{c_collated: boolean}>(
		`SELECT col.collname = 'C' AND col.collnamespace = 'pg_catalog'::regnamespace AS c_collated
FROM pg_attribute att
JOIN pg_collation col ON col.oid = att.attcollation
WHERE att.attrelid = to_regclass($1)
	AND att.attname = 'row_key'
	AND NOT att.attisdropped`,
		[kvTable],
	);
	return result.rows[0]?.c_collated === true;
}

async function postgresKvHasSortId(db: PostgresQueryable, kvTable: string): Promise<boolean> {
	const result = await db.query(
		`SELECT 1 FROM pg_attribute WHERE attrelid = to_regclass($1) AND attname = 'sort_id' AND NOT attisdropped`,
		[kvTable],
	);
	return result.rows.length > 0;
}

async function postgresKvHasSortIdFill(db: PostgresQueryable, kvTable: string): Promise<boolean> {
	const result = await db.query('SELECT 1 FROM pg_trigger WHERE tgrelid = to_regclass($1) AND tgname = $2', [
		kvTable,
		`${kvTable}_sort_id_fill`,
	]);
	return result.rows.length > 0;
}

async function createPostgresKvSortIdFill(db: PostgresQueryable, kvTable: string): Promise<void> {
	const fill = quoteIdentifier(`${kvTable}_sort_id_fill`);
	await db.query(`CREATE OR REPLACE FUNCTION ${fill}() RETURNS trigger LANGUAGE plpgsql AS $fill$
BEGIN
	IF NEW.row_data -> 'message_id' ->> 'value' ~ '^-?[0-9]+$' THEN
		NEW.sort_id := (NEW.row_data -> 'message_id' ->> 'value')::bigint;
	END IF;
	RETURN NEW;
END
$fill$`);
	await db.query(
		`CREATE TRIGGER ${fill} BEFORE INSERT OR UPDATE ON ${quoteIdentifier(kvTable)} FOR EACH ROW WHEN (NEW.sort_id IS NULL AND NEW.table_name IN ('messages', 'message_reactions')) EXECUTE FUNCTION ${fill}()`,
	);
}

async function postgresKvNeedsLz4(db: PostgresQueryable, kvTable: string): Promise<boolean> {
	const result = await db.query<{needs_lz4: boolean}>(
		`SELECT att.attcompression <> 'l' AND EXISTS (SELECT 1 FROM pg_settings WHERE name = 'default_toast_compression' AND 'lz4' = ANY(enumvals)) AS needs_lz4
FROM pg_attribute att
WHERE att.attrelid = to_regclass($1) AND att.attname = 'row_data' AND NOT att.attisdropped`,
		[kvTable],
	);
	return result.rows[0]?.needs_lz4 === true;
}

async function missingPostgresKvStorageOptions(db: PostgresQueryable, kvTable: string): Promise<Array<string>> {
	const result = await db.query<{options: Array<string> | null}>(
		'SELECT reloptions AS options FROM pg_class WHERE oid = to_regclass($1)',
		[kvTable],
	);
	const current = new Set(result.rows[0]?.options ?? []);
	return POSTGRES_KV_STORAGE_OPTIONS.filter((option) => !current.has(option));
}

async function ensurePostgresKvSchemaOnce(client: IPostgresClient): Promise<void> {
	const kvTable = client.kvTable();
	const table = quoteIdentifier(kvTable);
	await client.transaction(async (db) => {
		await db.query("SELECT set_config('statement_timeout', $1, true)", [POSTGRES_KV_SCHEMA_LOCK_TIMEOUT]);
		await db.query('SELECT pg_advisory_xact_lock($1, hashtext($2))', [POSTGRES_KV_SCHEMA_LOCK_NAMESPACE, kvTable]);
		await db.query("SELECT set_config('statement_timeout', $1, true)", [POSTGRES_KV_SCHEMA_MIGRATION_TIMEOUT]);
		await db.query(`
CREATE TABLE IF NOT EXISTS ${table} (
	table_name text NOT NULL,
	partition_key text COLLATE "C" NOT NULL,
	row_key text COLLATE "C" NOT NULL,
	row_data jsonb NOT NULL,
	expires_at timestamptz,
	updated_at timestamptz NOT NULL DEFAULT now(),
	sort_id bigint,
	PRIMARY KEY (table_name, row_key)
)`);
		const addSortId = !(await postgresKvHasSortId(db, kvTable));
		const addSortIdFill = !(await postgresKvHasSortIdFill(db, kvTable));
		const missingOptions = await missingPostgresKvStorageOptions(db, kvTable);
		const setLz4 = await postgresKvNeedsLz4(db, kvTable);
		if (addSortId || addSortIdFill || missingOptions.length > 0 || setLz4) {
			await db.query("SELECT set_config('lock_timeout', $1, true)", [POSTGRES_KV_ALTER_LOCK_TIMEOUT]);
			if (addSortId) await db.query(`ALTER TABLE ${table} ADD COLUMN sort_id bigint`);
			if (addSortIdFill) await createPostgresKvSortIdFill(db, kvTable);
			if (missingOptions.length > 0) await db.query(`ALTER TABLE ${table} SET (${missingOptions.join(', ')})`);
			if (setLz4) await db.query(`ALTER TABLE ${table} ALTER COLUMN row_data SET COMPRESSION lz4`);
			await db.query("SELECT set_config('lock_timeout', '0', true)");
		}
		if (!(await postgresKvRowKeyIsCCollated(db, kvTable))) {
			await db.query(
				`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${kvTable}_row_key_c_idx`)} ON ${table} (table_name, row_key COLLATE "C")`,
			);
		}
		await db.query(
			`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${kvTable}_row_key_numeric_idx`)} ON ${table} (table_name, ${numericRowKeyExpr('row_key')}) WHERE ${numericRowKeyExpr('row_key')} IS NOT NULL`,
		);
		await db.query(
			`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${kvTable}_expires_idx`)} ON ${table} (expires_at) WHERE expires_at IS NOT NULL`,
		);
		await repairInvalidPostgresKvIndexes(db, kvTable);
		const migrated = await db.query(`SELECT 1 FROM ${table} WHERE table_name = $1 AND row_key = $2 LIMIT 1`, [
			POSTGRES_KV_MIGRATION_TABLE,
			POSTGRES_KV_MESSAGES_PARTITION_MIGRATION,
		]);
		if (migrated.rows.length === 0) {
			const pending = await db.query(`
SELECT 1
FROM ${table}
WHERE table_name = 'messages'
	AND partition_key = row_key
	AND split_part(row_key, chr(31), 3) <> ''
LIMIT 1`);
			if (pending.rows.length > 0) {
				await db.query(`
UPDATE ${table}
SET partition_key = split_part(row_key, chr(31), 1) || chr(31) || split_part(row_key, chr(31), 2)
WHERE table_name = 'messages'
	AND partition_key = row_key
	AND split_part(row_key, chr(31), 3) <> ''`);
			}
			await db.query(
				`INSERT INTO ${table} (table_name, partition_key, row_key, row_data)
VALUES ($1, $2, $2, jsonb_build_object('applied_at', now()))
ON CONFLICT (table_name, row_key) DO NOTHING`,
				[POSTGRES_KV_MIGRATION_TABLE, POSTGRES_KV_MESSAGES_PARTITION_MIGRATION],
			);
		}
		await db.query(`DROP INDEX IF EXISTS ${quoteIdentifier(`${kvTable}_partition_idx`)}`);
	});
}

function isConcurrentDdlConflict(error: unknown): boolean {
	return (
		typeof error === 'object' &&
		error !== null &&
		'code' in error &&
		POSTGRES_KV_CONCURRENT_DDL_CODES.has(String((error as {code: unknown}).code))
	);
}

export async function ensurePostgresKvSchema(client: IPostgresClient): Promise<void> {
	for (let attempt = 1; attempt <= POSTGRES_KV_SCHEMA_ATTEMPTS; attempt += 1) {
		try {
			await ensurePostgresKvSchemaOnce(client);
			break;
		} catch (error) {
			if (attempt === POSTGRES_KV_SCHEMA_ATTEMPTS || !isConcurrentDdlConflict(error)) throw error;
			logWarn({table: client.kvTable(), attempt}, 'Postgres KV schema hit a concurrent DDL conflict, retrying');
			await new Promise((resolve) => setTimeout(resolve, POSTGRES_KV_SCHEMA_RETRY_DELAY_MS));
		}
	}
	await enableStatementStats(client);
	if (await postgresKvIndexesPending(client)) {
		void client
			.session((db) => migratePostgresKvIndexes(db, client.kvTable()))
			.catch((error: unknown) => logError({error, table: client.kvTable()}, 'Postgres KV sort_id migration failed'));
	}
}

async function enableStatementStats(client: IPostgresClient): Promise<void> {
	try {
		const missing = await client.query(
			`SELECT 1 FROM pg_available_extensions
WHERE name = 'pg_stat_statements'
	AND installed_version IS NULL
	AND 'pg_stat_statements' = ANY(string_to_array(replace(current_setting('shared_preload_libraries'), ' ', ''), ','))`,
		);
		if (missing.rows.length === 0) return;
		await client.query('CREATE EXTENSION IF NOT EXISTS pg_stat_statements');
	} catch (error) {
		if (isConcurrentDdlConflict(error)) return;
		logWarn({error}, 'Postgres KV could not enable pg_stat_statements');
	}
}

async function postgresKvIndexesPending(client: IPostgresClient): Promise<boolean> {
	const kvTable = client.kvTable();
	const result = await client.query<{marked: boolean; valid: number; retired: number}>(
		`SELECT
	EXISTS (SELECT 1 FROM ${quoteIdentifier(kvTable)} WHERE table_name = $1 AND row_key = $2) AS marked,
	(SELECT count(*)::int FROM pg_index idx JOIN pg_class cls ON cls.oid = idx.indexrelid
		WHERE idx.indrelid = to_regclass($3) AND idx.indisvalid AND cls.relname = ANY($4::text[])) AS valid,
	(SELECT count(*)::int FROM pg_index idx JOIN pg_class cls ON cls.oid = idx.indexrelid
		WHERE idx.indrelid = to_regclass($3) AND cls.relname = ANY($5::text[])) AS retired`,
		[
			POSTGRES_KV_MIGRATION_TABLE,
			POSTGRES_KV_SORT_ID_MIGRATION,
			kvTable,
			sortIdIndexNames(kvTable),
			retiredIndexNames(kvTable),
		],
	);
	const row = result.rows[0];
	return !row?.marked || row.valid !== POSTGRES_KV_SORT_ID_TABLES.length || row.retired > 0;
}

async function migratePostgresKvIndexes(db: PostgresQueryable, kvTable: string): Promise<void> {
	const table = quoteIdentifier(kvTable);
	const locked = await db.query<{locked: boolean}>('SELECT pg_try_advisory_lock($1, hashtext($2)) AS locked', [
		POSTGRES_KV_SCHEMA_LOCK_NAMESPACE,
		`${kvTable}_${POSTGRES_KV_SORT_ID_MIGRATION}`,
	]);
	if (locked.rows[0]?.locked !== true) return;
	await db.query("SELECT set_config('statement_timeout', '0', false)");
	const retired = await db.query<{index_name: string; valid: boolean}>(
		`SELECT cls.relname AS index_name, idx.indisvalid AS valid FROM pg_index idx JOIN pg_class cls ON cls.oid = idx.indexrelid
WHERE idx.indrelid = to_regclass($1) AND cls.relname = ANY($2::text[])`,
		[kvTable, retiredIndexNames(kvTable)],
	);
	const marked = await db.query(`SELECT 1 FROM ${table} WHERE table_name = $1 AND row_key = $2`, [
		POSTGRES_KV_MIGRATION_TABLE,
		POSTGRES_KV_SORT_ID_MIGRATION,
	]);
	const jsonbSortIndexes = jsonbSortIndexNames(kvTable);
	if (
		marked.rows.length === 0 ||
		retired.rows.some(({index_name, valid}) => valid && jsonbSortIndexes.includes(index_name))
	) {
		for (const tableName of POSTGRES_KV_SORT_ID_TABLES) {
			await backfillPostgresKvSortIds(db, table, tableName);
		}
	}
	for (const tableName of POSTGRES_KV_SORT_ID_TABLES) {
		const name = sortIdIndexName(kvTable, tableName);
		const existing = await db.query<{valid: boolean}>(
			`SELECT idx.indisvalid AS valid FROM pg_index idx JOIN pg_class cls ON cls.oid = idx.indexrelid
WHERE idx.indrelid = to_regclass($1) AND cls.relname = $2`,
			[kvTable, name],
		);
		if (existing.rows[0]?.valid === true) continue;
		if (existing.rows.length > 0) await db.query(`DROP INDEX CONCURRENTLY IF EXISTS ${quoteIdentifier(name)}`);
		await db.query(
			`CREATE INDEX CONCURRENTLY IF NOT EXISTS ${quoteIdentifier(name)} ON ${table} (partition_key, sort_id) WHERE table_name = '${tableName}'`,
		);
	}
	await db.query(
		`INSERT INTO ${table} (table_name, partition_key, row_key, row_data)
VALUES ($1, $2, $2, jsonb_build_object('applied_at', now()))
ON CONFLICT (table_name, row_key) DO UPDATE SET row_data = EXCLUDED.row_data, updated_at = now()`,
		[POSTGRES_KV_MIGRATION_TABLE, POSTGRES_KV_SORT_ID_MIGRATION],
	);
	for (const {index_name} of retired.rows) {
		await db.query(`DROP INDEX CONCURRENTLY IF EXISTS ${quoteIdentifier(index_name)}`);
	}
}

async function backfillPostgresKvSortIds(db: PostgresQueryable, table: string, tableName: string): Promise<void> {
	let after = '';
	for (;;) {
		const result = await db.query<{picked: number; last: string | null}>(
			`WITH batch AS (
	SELECT row_key FROM ${table}
	WHERE table_name = $1 AND row_key COLLATE "C" > $2 AND sort_id IS NULL
	ORDER BY row_key COLLATE "C"
	LIMIT $3
), filled AS (
	UPDATE ${table} kv
	SET sort_id = CASE WHEN kv.row_data -> 'message_id' ->> 'value' ~ '^-?[0-9]+$' THEN (kv.row_data -> 'message_id' ->> 'value')::bigint END
	FROM batch
	WHERE kv.table_name = $1 AND kv.row_key = batch.row_key
)
SELECT count(*)::int AS picked, max(row_key COLLATE "C") AS last FROM batch`,
			[tableName, after, POSTGRES_KV_SORT_ID_BACKFILL_BATCH],
		);
		const row = result.rows[0];
		if (!row?.last || row.picked < POSTGRES_KV_SORT_ID_BACKFILL_BATCH) return;
		after = row.last;
	}
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

function writtenRowKeys(meta: KvQueryMeta, params: CassandraParams): ReadonlyArray<string> | null {
	try {
		switch (meta.action) {
			case 'upsert':
			case 'insert':
				return [rowKey(meta, rowFromParams(meta, params))];
			case 'patch':
				return [rowKeyFromParams(meta, params)];
			case 'delete': {
				const plan = buildCandidatePlan(meta, params);
				if (plan.candidates.kind === 'none') return [];
				return plan.candidates.kind === 'rowKeys' ? plan.candidates.rowKeys : null;
			}
			default:
				return null;
		}
	} catch {
		return null;
	}
}

async function inParallel<T>(items: ReadonlyArray<T>, run: (item: T, index: number) => Promise<void>): Promise<void> {
	let next = 0;
	const errors: Array<unknown> = [];
	const worker = async (): Promise<void> => {
		while (errors.length === 0 && next < items.length) {
			const index = next;
			next += 1;
			try {
				await run(items[index]!, index);
			} catch (error) {
				errors.push(error);
			}
		}
	};
	await Promise.all(Array.from({length: Math.min(KV_WRITE_PARALLELISM, items.length)}, worker));
	if (errors.length > 0) throw errors[0];
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
		if (meta.conditions !== undefined) {
			return (await this.conditionalWrite(meta, query.params, db)) as Array<T>;
		}
		switch (meta.action) {
			case 'select':
				return (await this.select(meta, query.params, buildCandidatePlan(meta, query.params), db)) as Array<T>;
			case 'count':
				return (await this.count(meta, query.params, db)) as Array<T>;
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
				if (meta.batchEntries === undefined) return [];
				if (db !== this.client) {
					throw new Error('Conditional batches must own their database transaction');
				}
				return (await this.conditionalBatch(meta, query.params)) as Array<T>;
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
		const meta = this.meta(query);
		if (pageableSelect(meta, options.pageSize) && (state.after !== undefined || state.offset === 0)) {
			const plan = buildCandidatePlan(meta, query.params);
			const page =
				plan.candidates.kind === 'scan'
					? await this.scanPage(meta, query.params, await this.scanWalk(meta, state), state.offset, options.pageSize)
					: await this.sortedPage(meta, query.params, plan, state, options.pageSize);
			return {rows: page.rows as Array<T>, pageState: page.pageState};
		}
		const rows = await this.executeQuery<T, P>(query);
		const pageRows = rows.slice(state.offset, state.offset + options.pageSize);
		const nextOffset = state.offset + pageRows.length;
		return {
			rows: pageRows,
			pageState: nextOffset < rows.length ? encodePageState({offset: nextOffset}) : null,
		};
	}

	private async scanWalk(meta: KvQueryMeta, state: PageState): Promise<ScanWalk> {
		if (state.byRowKey) return {numeric: false, after: state.after ?? null, since: state.since ?? null};
		const after = state.after ?? null;
		const numericAfter = after === null || numericRowKeyValue(after) !== null;
		if (state.keyed && after !== null && numericAfter && singlePrimaryKey(meta)) {
			return {numeric: true, after, since: null};
		}
		const numeric = singlePrimaryKey(meta) && !(await this.hasNonNumericRows(meta, this.client));
		if (numeric && numericAfter) return {numeric: true, after, since: null};
		return {numeric: false, after: null, since: after};
	}

	private async hasNonNumericRows(meta: KvQueryMeta, db: PostgresQueryable): Promise<boolean> {
		const probes = NON_NUMERIC_ROW_KEY_RANGES.map(
			(range) =>
				`(SELECT kv.row_key FROM ${this.table} kv WHERE kv.table_name = $1 AND ${range} AND (kv.expires_at IS NULL OR kv.expires_at > now()) AND ${NUMERIC_ROW_KEY} IS NULL ORDER BY kv.row_key COLLATE "C" LIMIT 1) IS NOT NULL`,
		);
		const result = await db.query<{found: boolean}>(
			`SELECT ${probes.join(' OR ')} AS found`,
			[meta.table.name],
			'kv_non_numeric_probe',
		);
		return result.rows[0]?.found === true;
	}

	private async scanPage(
		meta: KvQueryMeta,
		params: CassandraParams,
		walk: ScanWalk,
		offset: number,
		pageSize: number,
	): Promise<{rows: Array<Row>; pageState: string | null}> {
		const page = await this.walkScan(meta, params, walk, pageSize, this.client);
		if (!page.more || page.after === null) return {rows: this.projected(meta, page.entries), pageState: null};
		const position = {offset: offset + page.entries.length, after: page.after};
		const rows = this.projected(meta, page.entries);
		let state: PageState;
		if (walk.numeric) {
			state = {...position, keyed: true};
		} else {
			state = {...position, byRowKey: true, ...(walk.since === null ? {} : {since: walk.since})};
		}
		return {rows, pageState: encodePageState(state)};
	}

	private async walkScan(
		meta: KvQueryMeta,
		params: CassandraParams,
		walk: ScanWalk,
		want: number,
		db: PostgresQueryable,
	): Promise<{entries: Array<PageEntry>; after: string | null; more: boolean}> {
		logFullScan(meta, walk.numeric ? 'numeric' : 'row_key');
		const floor = walk.numeric ? numericFloor(meta, params) : null;
		const since = walk.since === null ? null : decodeRowKey(walk.since, meta.table.primaryKey.length);
		if (walk.since !== null && since === null) throw new Error('Invalid Postgres KV page state');
		const limit = want + 1;
		const entries: Array<PageEntry> = [];
		let after = walk.after;
		while (entries.length < want) {
			const batch = await this.scanBatch(meta, walk.numeric, floor, after, limit, db);
			for (let index = 0; index < batch.length; index += 1) {
				const stored = batch[index]!;
				after = stored.row_key;
				const entry = this.matchingEntry(meta, stored, params);
				if (entry && (since === null || compareRowToKeyValues(meta, entry.row, since) > 0)) entries.push(entry);
				if (entries.length === want) {
					return {entries, after, more: index < batch.length - 1 || batch.length === limit};
				}
			}
			if (batch.length < limit) return {entries, after, more: false};
		}
		return {entries, after, more: false};
	}

	private async scanBatch(
		meta: KvQueryMeta,
		numeric: boolean,
		floor: NumericFloor | null,
		after: string | null,
		limit: number,
		db: PostgresQueryable,
	): Promise<Array<StoredRow>> {
		const params: Array<unknown> = [meta.table.name, limit];
		let predicate = '';
		let shape = 'key';
		if (numeric) {
			predicate = ` AND ${NUMERIC_ROW_KEY} IS NOT NULL`;
			shape = 'numeric';
			if (floor) {
				shape = floor.inclusive ? 'numeric_from' : 'numeric_above';
				params.push(floor.value);
				predicate += ` AND ${NUMERIC_ROW_KEY} ${floor.inclusive ? '>=' : '>'} $${params.length}::numeric`;
			}
			if (after !== null) {
				const value = numericRowKeyValue(after);
				if (value === null) throw new Error('Invalid Postgres KV page state');
				params.push(value, after);
				predicate += ` AND (${NUMERIC_ROW_KEY}, kv.row_key COLLATE "C") > ($${params.length - 1}::numeric, $${params.length})`;
			}
		} else if (after !== null) {
			params.push(after);
			predicate = ` AND kv.row_key COLLATE "C" > $${params.length}`;
		}
		const order = numeric ? `${NUMERIC_ROW_KEY}, kv.row_key COLLATE "C"` : 'kv.row_key COLLATE "C"';
		const result = await db.query<StoredRow>(
			`SELECT kv.row_key, kv.row_data FROM ${this.table} kv WHERE kv.table_name = $1${predicate} AND (kv.expires_at IS NULL OR kv.expires_at > now()) ORDER BY ${order} LIMIT $2`,
			params,
			`kv_scan_${shape}_${after === null ? 'start' : 'next'}`,
		);
		return result.rows;
	}

	private async sortedPage(
		meta: KvQueryMeta,
		params: CassandraParams,
		plan: QueryPlan,
		state: PageState,
		pageSize: number,
	): Promise<{rows: Array<Row>; pageState: string | null}> {
		const ordered = await this.orderedWalk(meta, params, plan.candidates, state.after ?? null, pageSize, this.client);
		if (ordered !== null) {
			const rows = this.projected(meta, ordered.entries);
			if (!ordered.more || ordered.after === null) return {rows, pageState: null};
			return {
				rows,
				pageState: encodePageState({offset: state.offset + ordered.entries.length, after: ordered.after}),
			};
		}
		const entries = this.matchingEntries(meta, await this.candidates(meta, plan, this.client), params);
		const compare = rowComparator(meta);
		entries.sort((left, right) => compare(left.row, right.row));
		const start = pageStart(meta, entries, state);
		const page = entries.slice(start, start + pageSize);
		const last = page[page.length - 1];
		const nextOffset = start + page.length;
		if (nextOffset >= entries.length || !last) return {rows: this.projected(meta, page), pageState: null};
		return {
			rows: this.projected(meta, page),
			pageState: encodePageState({offset: nextOffset, after: last.key}),
		};
	}

	private async orderedWalk(
		meta: KvQueryMeta,
		params: CassandraParams,
		plan: CandidatePlan,
		after: string | null,
		want: number,
		db: PostgresQueryable,
	): Promise<OrderedWalk | null> {
		const range = orderedRange(meta, params, plan);
		if (range === null) return null;
		const afterValue = after === null ? null : lastKeySegmentValue(after);
		if (after !== null && (afterValue === null || !after.startsWith(range.lowerBound))) return null;
		const probe = await this.probeOrderedRange(meta, range, db);
		const sorted = range.partition !== null && !probe.unsorted && (afterValue === null || fitsInt64(afterValue));
		if (!sorted && probe.mixed) return null;
		const limit = want + 1;
		const entries: Array<PageEntry> = [];
		let cursor = after;
		let cursorValue = afterValue;
		for (;;) {
			const batch = await this.orderedBatch(meta, range, sorted, cursor, cursorValue, limit, db);
			for (let index = 0; index < batch.length; index += 1) {
				const stored = batch[index]!;
				cursor = stored.row_key;
				const entry = this.matchingEntry(meta, stored, params);
				if (entry) entries.push(entry);
				if (entries.length === want) {
					return {entries, after: cursor, more: index < batch.length - 1 || batch.length === limit};
				}
			}
			if (batch.length < limit || cursor === null) return {entries, after: cursor, more: false};
			cursorValue = lastKeySegmentValue(cursor);
			if (cursorValue === null) throw new Error('Postgres KV ordered walk read a non-numeric key');
		}
	}

	private async probeOrderedRange(
		meta: KvQueryMeta,
		range: OrderedRange,
		db: PostgresQueryable,
	): Promise<{mixed: boolean; unsorted: boolean}> {
		const live = '(kv.expires_at IS NULL OR kv.expires_at > now())';
		const params: Array<unknown> = [
			meta.table.name,
			range.lowerBound,
			`${range.lowerBound}-`,
			`${range.lowerBound}:`,
			`${range.lowerBound}${BIGINT_ROW_KEY_PREFIX}`,
			`${range.lowerBound}${BIGINT_ROW_KEY_PREFIX}:`,
			range.upperBound,
		];
		const spans: Array<[number, number]> = [
			[2, 3],
			[4, 5],
			[6, 7],
		];
		const mixed = spans
			.map(
				([lo, hi]) =>
					`(SELECT 1 FROM ${this.table} kv WHERE kv.table_name = $1 AND kv.row_key COLLATE "C" >= $${lo} AND kv.row_key COLLATE "C" < $${hi} AND ${live} LIMIT 1) IS NOT NULL`,
			)
			.join(' OR ');
		let unsorted = 'false';
		let name = 'kv_ordered_probe';
		if (range.partition !== null) {
			params.push(range.partition);
			unsorted = `EXISTS (SELECT 1 FROM ${this.table} kv WHERE kv.table_name = $1 AND kv.table_name = '${meta.table.name}' AND kv.partition_key = $8 AND kv.row_key COLLATE "C" >= $2 AND kv.row_key COLLATE "C" < $7 AND kv.sort_id IS NULL AND ${live})`;
			name = `kv_ordered_probe_${meta.table.name}`;
		}
		const result = await db.query<{mixed: boolean; unsorted: boolean}>(
			`SELECT ${mixed} AS mixed, ${unsorted} AS unsorted`,
			params,
			name,
		);
		const row = result.rows[0];
		return {mixed: row?.mixed !== false, unsorted: row?.unsorted !== false};
	}

	private async orderedBatch(
		meta: KvQueryMeta,
		range: OrderedRange,
		sorted: boolean,
		after: string | null,
		afterValue: string | null,
		limit: number,
		db: PostgresQueryable,
	): Promise<Array<StoredRow>> {
		const key = sorted ? 'kv.sort_id' : numericKeySegmentExpr(range.segment);
		const cast = sorted ? 'bigint' : 'numeric';
		const direction = range.descending ? 'DESC' : 'ASC';
		const params: Array<unknown> = [meta.table.name, range.lowerBound, range.upperBound, limit];
		let predicate = '';
		const shape = [sorted ? meta.table.name : `seg${range.segment}`, direction.toLowerCase()];
		if (sorted) {
			params.push(range.partition);
			predicate += ` AND kv.table_name = '${meta.table.name}' AND kv.partition_key = $${params.length}`;
		}
		for (const bound of range.bounds) {
			params.push(bound.value);
			predicate += ` AND ${key} ${bound.op} $${params.length}::${cast}`;
			shape.push(KEY_BOUND_NAMES[bound.op]);
		}
		const op = range.descending ? '<' : '>';
		if (after !== null && afterValue !== null) {
			params.push(afterValue);
			if (sorted) {
				predicate += ` AND kv.sort_id ${op} $${params.length}::bigint`;
			} else {
				params.push(after);
				predicate += ` AND (${key}, kv.row_key COLLATE "C") ${op} ($${params.length - 1}::numeric, $${params.length})`;
			}
			shape.push('next');
		}
		const order = sorted ? `kv.sort_id ${direction}` : `${key} ${direction}, kv.row_key COLLATE "C" ${direction}`;
		const result = await db.query<StoredRow>(
			`SELECT kv.row_key, kv.row_data FROM ${this.table} kv WHERE kv.table_name = $1 AND kv.row_key COLLATE "C" >= $2 AND kv.row_key COLLATE "C" < $3${predicate} AND (kv.expires_at IS NULL OR kv.expires_at > now()) ORDER BY ${order} LIMIT $4`,
			params,
			`kv_ordered_${shape.join('_')}`,
		);
		return result.rows;
	}

	private projected(meta: KvQueryMeta, entries: ReadonlyArray<PageEntry>): Array<Row> {
		return entries.map((entry) => projectRow(entry.row, meta.columns as ReadonlyArray<string> | undefined));
	}

	async executeBatch(
		queries: Array<{query: string; params: object; meta?: KvQueryMeta}>,
		atomic = true,
	): Promise<void> {
		for (const query of queries) {
			if (isConditionalQuery({cql: query.query, params: query.params as CassandraParams, kvMeta: query.meta})) {
				throw new Error('Conditional writes must use executeConditional to preserve their result');
			}
		}
		if (atomic) {
			await this.client.transaction(async (db) => {
				for (const query of queries) {
					await this.executeQuery({cql: query.query, params: query.params as CassandraParams, kvMeta: query.meta}, db);
				}
			});
			return;
		}
		const prepared = queries.map((query) => ({
			cql: query.query,
			params: query.params as CassandraParams,
			kvMeta: query.meta,
		}));
		if (!this.writesCommute(prepared)) {
			for (const query of prepared) await this.executeQuery(query);
			return;
		}
		await inParallel(prepared, async (query) => {
			await this.executeQuery(query);
		});
	}

	async advanceRows(writes: ReadonlyArray<KvAdvance>): Promise<Array<boolean>> {
		const written = writes.map(() => false);
		await inParallel(writes, async (write, index) => {
			written[index] = await this.advanceRow(write);
		});
		return written;
	}

	private async advanceRow(write: KvAdvance): Promise<boolean> {
		const meta: KvQueryMeta = {action: 'patch', table: write.table};
		const always = write.always ?? {};
		const increments = Object.entries(write.increment ?? {});
		const params: Array<unknown> = [
			write.table.name,
			rowKey(meta, write.pk),
			write.guard.col,
			write.guard.value.toString(),
			JSON.stringify(encodeRow(always)),
			JSON.stringify(encodeRow(write.set)),
		];
		const guard = `COALESCE((kv.row_data -> $3::text ->> 'value')::numeric ${write.guard.orEqual ? '<=' : '<'} $4::numeric, true)`;
		let advanced = 'kv.row_data || $5::jsonb || $6::jsonb';
		for (const [column, amount] of increments) {
			params.push(column, amount);
			const name = `$${params.length - 1}::text`;
			advanced += ` || jsonb_build_object(${name}, COALESCE((kv.row_data ->> ${name})::numeric, 0) + $${params.length}::numeric)`;
		}
		const rowData = `CASE WHEN ${guard} THEN ${advanced} ELSE kv.row_data || $5::jsonb END`;
		const unconditional = Object.keys(always).length > 0;
		const fallbackTtl = defaultExpiresAt(meta);
		const shape = [
			write.guard.orEqual ? 'lte' : 'lt',
			increments.length,
			unconditional ? 'always' : 'guarded',
			fallbackTtl === undefined ? 'keep' : 'ttl',
		].join('_');
		if (write.insert === null) {
			const assignments = [`row_data = ${rowData}`, 'updated_at = now()'];
			if (fallbackTtl !== undefined) {
				params.push(fallbackTtl);
				assignments.push(`expires_at = GREATEST(kv.expires_at, $${params.length}::timestamptz)`);
			}
			const result = await this.client.query(
				`UPDATE ${this.table} kv SET ${assignments.join(', ')} WHERE kv.table_name = $1 AND kv.row_key = $2 AND (kv.expires_at IS NULL OR kv.expires_at > now())${unconditional ? '' : ` AND ${guard}`}`,
				params,
				`kv_advance_update_${shape}`,
			);
			return result.rowCount === 1;
		}
		params.push(partitionKey(meta, write.insert), JSON.stringify(encodeRow(write.insert)), fallbackTtl ?? null);
		const last = params.length;
		const expiresAt = fallbackTtl === undefined ? KEPT_EXPIRES_AT : 'GREATEST(kv.expires_at, EXCLUDED.expires_at)';
		const result = await this.client.query(
			`INSERT INTO ${this.table} AS kv (table_name, partition_key, row_key, row_data, expires_at, updated_at)
VALUES ($1, $${last - 2}, $2, $${last - 1}::jsonb, $${last}, now())
ON CONFLICT (table_name, row_key)
DO UPDATE SET partition_key = EXCLUDED.partition_key, row_data = CASE WHEN ${EXPIRED_STORED_ROW} THEN EXCLUDED.row_data ELSE ${rowData} END, expires_at = ${expiresAt}, updated_at = now()${unconditional ? '' : `\nWHERE ${EXPIRED_STORED_ROW} OR ${guard}`}`,
			params,
			`kv_advance_upsert_${shape}`,
		);
		return result.rowCount === 1;
	}

	private writesCommute(queries: ReadonlyArray<PreparedQuery>): boolean {
		if (queries.length < 2) return false;
		let metas: Array<KvQueryMeta>;
		try {
			metas = queries.map((query) => this.meta(query));
		} catch {
			return false;
		}
		if (metas.every((meta) => meta.action === 'delete')) return true;
		const touched = new Set<string>();
		for (let index = 0; index < queries.length; index += 1) {
			const meta = metas[index]!;
			const keys = writtenRowKeys(meta, queries[index]!.params);
			if (keys === null) return false;
			for (const key of keys) {
				const id = `${meta.table.name}${VALUE_SEPARATOR}${key}`;
				if (touched.has(id)) return false;
				touched.add(id);
			}
		}
		return true;
	}

	private meta(query: PreparedQuery): KvQueryMeta<Row> {
		const meta = (query.kvMeta ?? getKvMeta(query.cql) ?? parseRawMeta(query.cql)) as KvQueryMeta<Row> | null;
		if (!meta) {
			throw new Error(`Postgres KV does not understand query: ${query.cql}`);
		}
		return meta;
	}

	private async candidateGroup(
		meta: KvQueryMeta,
		fragments: PlanFragments,
		db: PostgresQueryable,
		name: string,
	): Promise<Array<StoredRow>> {
		const result = await db.query<StoredRow>(
			`SELECT kv.row_key, kv.row_data FROM ${fromPlan(this.table, fragments)} WHERE kv.table_name = $1${fragments.predicate} AND (kv.expires_at IS NULL OR kv.expires_at > now())`,
			[meta.table.name, ...fragments.params],
			name,
		);
		return result.rows;
	}

	private async candidates(meta: KvQueryMeta, plan: QueryPlan, db: PostgresQueryable): Promise<Array<StoredRow>> {
		if (plan.candidates.kind === 'none') return [];
		if (plan.candidates.kind === 'scan') logFullScan(meta);
		const groups = planFragmentGroups(plan.candidates);
		const name = planStatementName('kv_sel', plan.candidates);
		if (groups.length === 1) return this.candidateGroup(meta, groups[0]!, db, name);
		const byRowKey = new Map<string, StoredRow>();
		for (const fragments of groups) {
			for (const stored of await this.candidateGroup(meta, fragments, db, name)) byRowKey.set(stored.row_key, stored);
		}
		return [...byRowKey.values()];
	}

	private matchingEntries(
		meta: KvQueryMeta,
		stored: ReadonlyArray<StoredRow>,
		params: CassandraParams,
	): Array<PageEntry> {
		const entries: Array<PageEntry> = [];
		for (const entry of stored) {
			const matched = this.matchingEntry(meta, entry, params);
			if (matched) entries.push(matched);
		}
		return entries;
	}

	private matchingEntry(meta: KvQueryMeta, stored: StoredRow, params: CassandraParams): PageEntry | null {
		const required = queryShape(meta).requiredColumns;
		const row = required ? decodeRowColumns(stored.row_data, required) : decodeRow(stored.row_data);
		return matchesWhere(row, meta.where as ReadonlyArray<WhereExpr<Row>> | undefined, params)
			? {key: stored.row_key, row}
			: null;
	}

	private matchingRows(meta: KvQueryMeta, stored: ReadonlyArray<StoredRow>, params: CassandraParams): Array<Row> {
		return this.matchingEntries(meta, stored, params).map((entry) => entry.row);
	}

	private async unorderedScan(meta: KvQueryMeta, limit: number, db: PostgresQueryable): Promise<Array<Row>> {
		const result = await db.query<StoredRow>(
			`SELECT kv.row_key, kv.row_data FROM ${this.table} kv WHERE kv.table_name = $1 AND (kv.expires_at IS NULL OR kv.expires_at > now()) LIMIT $2`,
			[meta.table.name, limit],
			'kv_sel_unordered',
		);
		return this.matchingRows(meta, result.rows, {}).map((row) =>
			projectRow(row, meta.columns as ReadonlyArray<string> | undefined),
		);
	}

	private async select(
		meta: KvQueryMeta,
		params: CassandraParams,
		plan: QueryPlan,
		db: PostgresQueryable,
	): Promise<Array<Row>> {
		if (
			meta.unordered &&
			typeof meta.limit === 'number' &&
			plan.candidates.kind === 'scan' &&
			queryShape(meta).clauseCount === 0
		) {
			return this.unorderedScan(meta, meta.limit, db);
		}
		if (
			typeof meta.limit === 'number' &&
			Number.isInteger(meta.limit) &&
			plan.candidates.kind === 'scan' &&
			walksInPrimaryKeyOrder(meta) &&
			!(await this.hasNonNumericRows(meta, db))
		) {
			const walk = {numeric: true, after: null, since: null};
			return this.projected(meta, (await this.walkScan(meta, params, walk, meta.limit, db)).entries);
		}
		if (typeof meta.limit === 'number' && Number.isInteger(meta.limit) && meta.limit > 0) {
			const ordered = await this.orderedWalk(meta, params, plan.candidates, null, meta.limit, db);
			if (ordered !== null) return this.projected(meta, ordered.entries);
		}
		let rows = this.matchingRows(meta, await this.candidates(meta, plan, db), params);
		rows = sortRows(meta, rows);
		if (typeof meta.limit === 'number') rows = rows.slice(0, meta.limit);
		return rows.map((row) => projectRow(row, meta.columns as ReadonlyArray<string> | undefined));
	}

	private async count(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<Array<Row>> {
		const plan = buildCandidatePlan(meta, params);
		if (!plan.exact) {
			return [{count: (await this.select(meta, params, plan, db)).length}];
		}
		if (plan.candidates.kind === 'none') return [{count: 0}];
		if (plan.candidates.kind === 'scan') logFullScan(meta);
		let total = 0;
		for (const fragments of planFragmentGroups(plan.candidates)) {
			const result = await db.query<{count: string}>(
				`SELECT count(*) AS count FROM ${fromPlan(this.table, fragments)} WHERE kv.table_name = $1${fragments.predicate} AND (kv.expires_at IS NULL OR kv.expires_at > now())`,
				[meta.table.name, ...fragments.params],
				planStatementName('kv_count', plan.candidates),
			);
			total += Number(result.rows[0]?.count ?? 0);
		}
		return [{count: total}];
	}

	private async upsert(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<Array<Row>> {
		const incoming = rowFromParams(meta, params);
		const explicit = ttlExpiresAt(meta, params);
		const expiresAt = explicit === undefined ? (defaultExpiresAt(meta) ?? null) : explicit;
		const values = [
			meta.table.name,
			partitionKey(meta, incoming),
			rowKey(meta, incoming),
			JSON.stringify(encodeRow(incoming)),
			expiresAt,
			sortId(meta, incoming),
		];
		if (meta.ifNotExists) {
			const result = await db.query(
				`INSERT INTO ${this.table} AS kv (table_name, partition_key, row_key, row_data, expires_at, sort_id, updated_at)
VALUES ($1, $2, $3, $4::jsonb, $5, $6::bigint, now())
ON CONFLICT (table_name, row_key)
DO UPDATE SET partition_key = EXCLUDED.partition_key, row_data = EXCLUDED.row_data, expires_at = EXCLUDED.expires_at, sort_id = EXCLUDED.sort_id, updated_at = now()
WHERE ${EXPIRED_STORED_ROW}`,
				values,
				'kv_insert_absent',
			);
			return [{'[applied]': result.rowCount === 1}];
		}
		await db.query(
			`INSERT INTO ${this.table} AS kv (table_name, partition_key, row_key, row_data, expires_at, sort_id, updated_at)
VALUES ($1, $2, $3, $4::jsonb, $5, $6::bigint, now())
ON CONFLICT (table_name, row_key)
DO UPDATE SET partition_key = EXCLUDED.partition_key, row_data = ${MERGED_ROW_DATA}, expires_at = EXCLUDED.expires_at, sort_id = EXCLUDED.sort_id, updated_at = now()`,
			values,
			'kv_upsert',
		);
		return [];
	}

	private async conditionalWrite(
		meta: KvQueryMeta,
		params: CassandraParams,
		db: PostgresQueryable,
	): Promise<Array<Row>> {
		if (meta.action !== 'patch' && meta.action !== 'delete') {
			throw new Error(`Unsupported conditional write action: ${meta.action}`);
		}
		if (!meta.conditions?.length) {
			throw new Error('Conditional writes require expected values');
		}
		const bindings: Array<unknown> = [meta.table.name, rowKeyFromParams(meta, params)];
		let expiry = 'keep';
		const predicates = ['kv.table_name = $1', 'kv.row_key = $2', '(kv.expires_at IS NULL OR kv.expires_at > now())'];
		for (const {col, expectedParam} of meta.conditions) {
			if (!Object.hasOwn(params, expectedParam) || params[expectedParam] === undefined) {
				throw new Error(`Missing conditional write parameter: ${expectedParam}`);
			}
			bindings.push(col, JSON.stringify(encodeValue(params[expectedParam])));
			predicates.push(
				`COALESCE(kv.row_data -> $${bindings.length - 1}::text, 'null'::jsonb) = $${bindings.length}::jsonb`,
			);
		}
		const where = predicates.join(' AND ');
		let sql = `DELETE FROM ${this.table} kv WHERE ${where}`;
		if (meta.action === 'patch') {
			if (!meta.patchKeys?.length) {
				throw new Error('Conditional patches require at least one column');
			}
			bindings.push(JSON.stringify(encodeRow(paramsRow(params, meta.patchKeys))));
			const assignments = [`row_data = kv.row_data || $${bindings.length}::jsonb`, 'updated_at = now()'];
			const explicit = ttlExpiresAt(meta, params);
			const fallback = explicit === undefined ? defaultExpiresAt(meta) : undefined;
			if (explicit !== undefined) {
				bindings.push(explicit);
				assignments.push(`expires_at = $${bindings.length}`);
				expiry = 'set';
			} else if (fallback !== undefined) {
				bindings.push(fallback);
				assignments.push(`expires_at = GREATEST(kv.expires_at, $${bindings.length}::timestamptz)`);
				expiry = 'extend';
			}
			sql = `UPDATE ${this.table} kv SET ${assignments.join(', ')} WHERE ${where}`;
		}
		const result = await db.query(sql, bindings, `kv_cond_${meta.action}_${meta.conditions.length}_${expiry}`);
		if (result.rowCount !== 0 && result.rowCount !== 1) {
			throw new Error('Conditional write returned an invalid affected row count');
		}
		return [{'[applied]': result.rowCount === 1}];
	}

	private async conditionalBatch(meta: KvQueryMeta, params: CassandraParams): Promise<Array<Row>> {
		const entries = meta.batchEntries;
		if (!entries?.length) {
			throw new Error('Conditional batches require at least one row');
		}
		const writes = entries.map((entry) => {
			const values = boundColumns(params, entry.pk);
			const writeMeta: KvQueryMeta = {
				action: entry.action,
				table: meta.table,
				pkColumns: entry.pk.map(({col}) => col),
			};
			if (entry.action === 'insert') {
				Object.assign(values, boundColumns(params, entry.values));
				writeMeta.ifNotExists = true;
			} else {
				if (entry.conditions.length === 0) {
					throw new Error('Conditional batch updates and deletes require expected values');
				}
				if (entry.action === 'patch') {
					if (entry.patch.length === 0) {
						throw new Error('Conditional batch patches require at least one column');
					}
					Object.assign(values, boundColumns(params, entry.patch));
					writeMeta.patchKeys = entry.patch.map(({col}) => col);
				}
				const expected = boundColumns(
					params,
					entry.conditions.map(({col, expectedParam}) => ({col, param: expectedParam})),
				);
				writeMeta.conditions = Object.entries(expected).map(([col, value]) => {
					const expectedParam = `expected_${col}`;
					if (Object.hasOwn(values, expectedParam)) {
						throw new Error(`Conditional batch parameter conflicts with a column value: ${expectedParam}`);
					}
					values[expectedParam] = value;
					return {col, expectedParam};
				});
			}
			if (entry.action !== 'delete' && entry.ttlParamName !== undefined) {
				const ttl = params[entry.ttlParamName];
				if (typeof ttl !== 'number') {
					throw new Error(`TTL parameter ${entry.ttlParamName} must be a number`);
				}
				if (Object.hasOwn(values, entry.ttlParamName)) {
					throw new Error(`Conditional batch TTL parameter conflicts with a column value: ${entry.ttlParamName}`);
				}
				values[entry.ttlParamName] = validateTtlSeconds(ttl);
				writeMeta.ttlParamName = entry.ttlParamName;
			}
			const key = rowKey(meta, values);
			return {key, keyBytes: Buffer.from(key), partition: partitionKey(meta, values), meta: writeMeta, params: values};
		});
		if (new Set(writes.map(({key}) => key)).size !== writes.length) {
			throw new Error('Conditional batches cannot repeat a row');
		}
		if (new Set(writes.map(({partition}) => partition)).size !== 1) {
			throw new Error('Conditional batches must stay within one partition');
		}
		writes.sort((left, right) => Buffer.compare(left.keyBytes, right.keyBytes));
		const rejected = new Error('Conditional batch was not applied');
		try {
			await this.client.transaction(async (db) => {
				for (const write of writes) {
					const result =
						write.meta.action === 'insert'
							? await this.upsert(write.meta, write.params, db)
							: await this.conditionalWrite(write.meta, write.params, db);
					const applied = result[0]?.['[applied]'];
					if (result.length !== 1 || typeof applied !== 'boolean') {
						throw new Error('Conditional batch entry returned an invalid database result');
					}
					if (!applied) throw rejected;
				}
			});
		} catch (error) {
			if (error !== rejected) throw error;
			return [{'[applied]': false}];
		}
		return [{'[applied]': true}];
	}

	private async patch(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<void> {
		const key = rowKeyFromParams(meta, params);
		const incoming = paramsRow(params, (meta.pkColumns ?? meta.table.primaryKey) as ReadonlyArray<string>);
		for (const column of meta.patchKeys ?? []) {
			incoming[column] = column in params ? params[column] : null;
		}
		const explicit = ttlExpiresAt(meta, params);
		const fallback = explicit === undefined ? defaultExpiresAt(meta) : undefined;
		let patchExpiration: [string, string];
		if (explicit !== undefined) {
			patchExpiration = ['EXCLUDED.expires_at', 'kv_patch_set_ttl'];
		} else if (fallback !== undefined) {
			patchExpiration = ['GREATEST(kv.expires_at, EXCLUDED.expires_at)', 'kv_patch_default_ttl'];
		} else {
			patchExpiration = [KEPT_EXPIRES_AT, 'kv_patch_keep_ttl'];
		}
		const [expiresAtExpr, statementName] = patchExpiration;
		await db.query(
			`INSERT INTO ${this.table} AS kv (table_name, partition_key, row_key, row_data, expires_at, sort_id, updated_at)
VALUES ($1, $2, $3, $4::jsonb, $5, $6::bigint, now())
ON CONFLICT (table_name, row_key)
DO UPDATE SET partition_key = EXCLUDED.partition_key, row_data = ${MERGED_ROW_DATA}, expires_at = ${expiresAtExpr}, sort_id = EXCLUDED.sort_id, updated_at = now()`,
			[
				meta.table.name,
				partitionKey(meta, incoming),
				key,
				JSON.stringify(encodeRow(incoming)),
				explicit ?? fallback ?? null,
				sortId(meta, incoming),
			],
			statementName,
		);
	}

	private async delete(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<void> {
		const plan = buildCandidatePlan(meta, params);
		if (plan.candidates.kind === 'none') return;
		if (plan.exact) {
			if (plan.candidates.kind === 'scan') logFullScan(meta);
			for (const fragments of planFragmentGroups(plan.candidates)) {
				await db.query(
					`DELETE FROM ${this.table} kv${fragments.using === '' ? '' : ` USING ${fragments.using}`} WHERE kv.table_name = $1${fragments.predicate} AND (kv.expires_at IS NULL OR kv.expires_at > now())`,
					[meta.table.name, ...fragments.params],
					planStatementName('kv_del', plan.candidates),
				);
			}
			return;
		}
		const whereColumns = queryShape(meta).whereColumns;
		const rows = await this.candidates(meta, plan, db);
		const matchingKeys = rows
			.filter((stored) =>
				matchesWhere(
					decodeRowColumns(stored.row_data, whereColumns),
					meta.where as ReadonlyArray<WhereExpr<Row>> | undefined,
					params,
				),
			)
			.map((stored) => stored.row_key);
		if (matchingKeys.length === 0) return;
		await db.query(
			`DELETE FROM ${this.table} WHERE table_name = $1 AND row_key = ANY($2::text[])`,
			[meta.table.name, matchingKeys],
			'kv_del_keys',
		);
	}
}

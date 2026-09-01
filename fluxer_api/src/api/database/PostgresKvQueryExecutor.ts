// SPDX-License-Identifier: AGPL-3.0-or-later

import {type IPostgresClient, type PostgresQueryable, quoteIdentifier} from '@pkgs/postgres/src/Client';
import cassandra from 'cassandra-driver';
import {Logger} from '../Logger';
import {getKvMeta, getTableMetadata} from './CassandraMetaRegistry';
import type {CassandraParams, ColumnName, KvQueryMeta, PreparedQuery, WhereExpr} from './CassandraTypes';

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
}

interface PageEntry {
	key: string;
	row: Row;
}

interface RangeGroup {
	lowerBounds: Array<string>;
	upperBounds: Array<string>;
}

export type CandidatePlan =
	| {kind: 'none'}
	| {kind: 'rowKeys'; rowKeys: Array<string>}
	| {kind: 'range'; lowerBound: string; upperBound: string}
	| {kind: 'ranges'; lowerBounds: Array<string>; upperBounds: Array<string>}
	| {kind: 'rangeGroups'; groups: Array<RangeGroup>}
	| {kind: 'partitionKeys'; partitionKeys: Array<string>}
	| {kind: 'scan'};

interface QueryPlan {
	candidates: CandidatePlan;
	exact: boolean;
}

interface QueryShape {
	leadingClauses: ReadonlyArray<PinnedWhereExpr>;
	partitionClauses: ReadonlyArray<PinnedWhereExpr> | null;
	inClauses: ReadonlyArray<InWhereExpr>;
	whereColumns: ReadonlyArray<string>;
	requiredColumns: ReadonlyArray<string> | null;
	clauseCount: number;
	summary: string;
}

interface PlanFragments {
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
const POSTGRES_KV_SCHEMA_ATTEMPTS = 3;
const POSTGRES_KV_SCHEMA_RETRY_DELAY_MS = 250;
const POSTGRES_KV_CONCURRENT_DDL_CODES = new Set(['23505', '42P07', '42710']);
const NUMERIC_ROW_KEY_BIGINT_PATTERN = '^\\{"__fluxer_type":"bigint","value":"(-?[0-9]+)"\\}$';
const NUMERIC_ROW_KEY_NUMBER_PATTERN = '^(-?[0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?)$';
const EXPIRED_STORED_ROW = 'kv.expires_at IS NOT NULL AND kv.expires_at <= now()';
const MERGED_ROW_DATA = `CASE WHEN ${EXPIRED_STORED_ROW} THEN EXCLUDED.row_data ELSE kv.row_data || EXCLUDED.row_data END`;
const KEPT_EXPIRES_AT = `CASE WHEN ${EXPIRED_STORED_ROW} THEN NULL ELSE kv.expires_at END`;

function numericRowKeyExpr(column: string): string {
	return `(COALESCE(substring(${column} from '${NUMERIC_ROW_KEY_BIGINT_PATTERN}'), substring(${column} from '${NUMERIC_ROW_KEY_NUMBER_PATTERN}'))::numeric)`;
}

const NUMERIC_ROW_KEY = numericRowKeyExpr('kv.row_key');

function planStatementName(prefix: string, plan: CandidatePlan): string | undefined {
	switch (plan.kind) {
		case 'rowKeys':
			return `${prefix}_rowkeys`;
		case 'range':
			return `${prefix}_range`;
		default:
			return undefined;
	}
}

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
	if (left?.constructor?.name === 'LocalDate' && right?.constructor?.name === 'LocalDate') {
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

function rowComparator(meta: KvQueryMeta): (left: Row, right: Row) => number {
	if (meta.orderBy) {
		const column = meta.orderBy.col as string;
		const direction = meta.orderBy.direction === 'DESC' ? -1 : 1;
		return (left, right) => compareValues(left[column], right[column]) * direction;
	}
	const columns = meta.table.primaryKey as ReadonlyArray<string>;
	return (left, right) => {
		for (const column of columns) {
			const cmp = compareValues(left[column], right[column]);
			if (cmp !== 0) return cmp;
		}
		return 0;
	};
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
	return clause.kind === 'tupleGt' ? (clause.cols as ReadonlyArray<string>) : [clause.col as string];
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
	const partitionColumns = meta.table.partitionKey as ReadonlyArray<string>;
	const partitionClauses: Array<PinnedWhereExpr> = [];
	for (const column of partitionColumns) {
		const clause = pinnedClause(where, column);
		if (!clause) {
			partitionClauses.length = 0;
			break;
		}
		partitionClauses.push(clause);
	}
	const whereColumns = [...new Set(where.flatMap(clauseColumns))];
	const shape: QueryShape = {
		leadingClauses,
		partitionClauses:
			partitionColumns.length > 0 && partitionClauses.length === partitionColumns.length ? partitionClauses : null,
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
	if (shape.partitionClauses) {
		const partition = pinnedColumns(shape.partitionClauses, params);
		if (
			partition.length === shape.partitionClauses.length &&
			combinationCount(partition.map((column) => column.segments)) <= MAX_ROW_KEY_COMBINATIONS
		) {
			return {
				candidates: {kind: 'partitionKeys', partitionKeys: keyPrefixes(partition.map((column) => column.segments))},
				exact: planIsExact(meta, shape, partition),
			};
		}
	}
	return {candidates: {kind: 'scan'}, exact: planIsExact(meta, shape, [])};
}

function rangeFragments(group: RangeGroup): PlanFragments {
	const params: Array<unknown> = [];
	const arms = group.lowerBounds.map((lowerBound, index) => {
		params.push(lowerBound, group.upperBounds[index]);
		return `(kv.row_key COLLATE "C" >= $${params.length} AND kv.row_key COLLATE "C" < $${params.length + 1})`;
	});
	return {predicate: ` AND (${arms.join(' OR ')})`, params};
}

function planFragments(plan: Exclude<CandidatePlan, {kind: 'rangeGroups'}>): PlanFragments {
	switch (plan.kind) {
		case 'rowKeys':
			return {predicate: ' AND kv.row_key = ANY($2::text[])', params: [plan.rowKeys]};
		case 'range':
			return {
				predicate: ' AND kv.row_key COLLATE "C" >= $2 AND kv.row_key COLLATE "C" < $3',
				params: [plan.lowerBound, plan.upperBound],
			};
		case 'ranges':
			return rangeFragments(plan);
		case 'partitionKeys':
			return plan.partitionKeys.length === 1
				? {predicate: ' AND kv.partition_key = $2', params: [plan.partitionKeys[0]]}
				: {predicate: ' AND kv.partition_key = ANY($2::text[])', params: [plan.partitionKeys]};
		default:
			return {predicate: '', params: []};
	}
}

export function planFragmentGroups(plan: CandidatePlan): Array<PlanFragments> {
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

function logFullScan(meta: KvQueryMeta): void {
	const shape = queryShape(meta);
	const key = `${meta.table.name}|${meta.action}|${shape.summary}`;
	const now = Date.now();
	const last = fullScanLoggedAt.get(key);
	if (last !== undefined && now - last < FULL_SCAN_LOG_INTERVAL_MS) return;
	if (fullScanLoggedAt.size >= FULL_SCAN_LOG_KEY_LIMIT) fullScanLoggedAt.clear();
	fullScanLoggedAt.set(key, now);
	logWarn({table: meta.table.name, action: meta.action, where: shape.summary || 'none'}, 'Postgres KV full table scan');
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
	if (typeof decoded.after !== 'string') return {offset: decoded.offset};
	return {offset: decoded.offset, after: decoded.after, keyed: decoded.keyed === true};
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

function numericScanPlan(meta: KvQueryMeta, plan: QueryPlan): boolean {
	return plan.exact && plan.candidates.kind === 'scan' && (meta.table.primaryKey as ReadonlyArray<string>).length === 1;
}

function numericScanKeyed(meta: KvQueryMeta, plan: QueryPlan, entries: ReadonlyArray<PageEntry>): boolean {
	if (!numericScanPlan(meta, plan)) return false;
	const column = (meta.table.primaryKey as ReadonlyArray<string>)[0]!;
	return entries.every((entry) => numericKeyValue(entry.row[column]) !== null);
}

function numericScanCursor(state: PageState): {rowKey: string; value: string} | null {
	if (state.keyed !== true || state.after === undefined) return null;
	const values = decodeRowKey(state.after, 1);
	if (values === null) return null;
	const value = numericKeyValue(values[0]);
	return value === null ? null : {rowKey: state.after, value};
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

async function repairInvalidPostgresKvIndexes(db: PostgresQueryable, kvTable: string): Promise<void> {
	const invalid = await db.query<{index_name: string; index_def: string}>(
		`SELECT cls.relname AS index_name, pg_get_indexdef(idx.indexrelid) AS index_def
FROM pg_index idx
JOIN pg_class cls ON cls.oid = idx.indexrelid
WHERE idx.indrelid = to_regclass($1)
	AND NOT idx.indisvalid
	AND NOT idx.indisprimary
	AND NOT idx.indisunique`,
		[kvTable],
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
	PRIMARY KEY (table_name, row_key)
)`);
		await db.query(
			`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${kvTable}_partition_row_idx`)} ON ${table} (table_name, partition_key, row_key)`,
		);
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
		await db.query(
			`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${kvTable}_messages_message_idx`)} ON ${table} (partition_key, ((CASE WHEN row_data -> 'message_id' ->> 'value' ~ '^-?[0-9]+$' THEN (row_data -> 'message_id' ->> 'value')::bigint END))) WHERE table_name = 'messages'`,
		);
		await db.query(
			`CREATE INDEX IF NOT EXISTS ${quoteIdentifier(`${kvTable}_message_reactions_message_idx`)} ON ${table} (partition_key, ((CASE WHEN row_data -> 'message_id' ->> 'value' ~ '^-?[0-9]+$' THEN (row_data -> 'message_id' ->> 'value')::bigint END))) WHERE table_name = 'message_reactions'`,
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
			return;
		} catch (error) {
			if (attempt === POSTGRES_KV_SCHEMA_ATTEMPTS || !isConcurrentDdlConflict(error)) throw error;
			logWarn({table: client.kvTable(), attempt}, 'Postgres KV schema hit a concurrent DDL conflict, retrying');
			await new Promise((resolve) => setTimeout(resolve, POSTGRES_KV_SCHEMA_RETRY_DELAY_MS));
		}
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
		const meta = this.meta(query);
		if (pageableSelect(meta, options.pageSize) && (state.after !== undefined || state.offset === 0)) {
			const plan = buildCandidatePlan(meta, query.params);
			const cursor = numericScanPlan(meta, plan) ? numericScanCursor(state) : null;
			const page = cursor
				? await this.numericScanPage(meta, query.params, cursor, state.offset, options.pageSize)
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

	private async numericScanPage(
		meta: KvQueryMeta,
		params: CassandraParams,
		cursor: {rowKey: string; value: string},
		offset: number,
		pageSize: number,
	): Promise<{rows: Array<Row>; pageState: string | null}> {
		logFullScan(meta);
		const result = await this.client.query<StoredRow>(
			`SELECT kv.row_key, kv.row_data FROM ${this.table} kv WHERE kv.table_name = $1 AND ${NUMERIC_ROW_KEY} IS NOT NULL AND (${NUMERIC_ROW_KEY}, kv.row_key COLLATE "C") > ($2::numeric, $3) AND (kv.expires_at IS NULL OR kv.expires_at > now()) ORDER BY ${NUMERIC_ROW_KEY}, kv.row_key COLLATE "C" LIMIT $4`,
			[meta.table.name, cursor.value, cursor.rowKey, pageSize + 1],
		);
		const entries = this.matchingEntries(meta, result.rows.slice(0, pageSize), params);
		const last = entries[entries.length - 1];
		return {
			rows: this.projected(meta, entries),
			pageState:
				result.rows.length > pageSize && last
					? encodePageState({offset: offset + entries.length, after: last.key, keyed: true})
					: null,
		};
	}

	private async sortedPage(
		meta: KvQueryMeta,
		params: CassandraParams,
		plan: QueryPlan,
		state: PageState,
		pageSize: number,
	): Promise<{rows: Array<Row>; pageState: string | null}> {
		const entries = this.matchingEntries(meta, await this.candidates(meta, plan, this.client), params);
		const compare = rowComparator(meta);
		entries.sort((left, right) => compare(left.row, right.row));
		const start = pageStart(meta, entries, state);
		const page = entries.slice(start, start + pageSize);
		const last = page[page.length - 1];
		const nextOffset = start + page.length;
		if (nextOffset >= entries.length || !last) return {rows: this.projected(meta, page), pageState: null};
		const keyed = numericScanKeyed(meta, plan, entries);
		return {
			rows: this.projected(meta, page),
			pageState: encodePageState(
				keyed ? {offset: nextOffset, after: last.key, keyed: true} : {offset: nextOffset, after: last.key},
			),
		};
	}

	private projected(meta: KvQueryMeta, entries: ReadonlyArray<PageEntry>): Array<Row> {
		return entries.map((entry) => projectRow(entry.row, meta.columns as ReadonlyArray<string> | undefined));
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

	private async candidateGroup(
		meta: KvQueryMeta,
		fragments: PlanFragments,
		db: PostgresQueryable,
		name?: string,
	): Promise<Array<StoredRow>> {
		const result = await db.query<StoredRow>(
			`SELECT kv.row_key, kv.row_data FROM ${this.table} kv WHERE kv.table_name = $1${fragments.predicate} AND (kv.expires_at IS NULL OR kv.expires_at > now())`,
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
		const required = queryShape(meta).requiredColumns;
		const entries: Array<PageEntry> = [];
		for (const entry of stored) {
			const row = required ? decodeRowColumns(entry.row_data, required) : decodeRow(entry.row_data);
			if (matchesWhere(row, meta.where as ReadonlyArray<WhereExpr<Row>> | undefined, params)) {
				entries.push({key: entry.row_key, row});
			}
		}
		return entries;
	}

	private matchingRows(meta: KvQueryMeta, stored: ReadonlyArray<StoredRow>, params: CassandraParams): Array<Row> {
		return this.matchingEntries(meta, stored, params).map((entry) => entry.row);
	}

	private async select(
		meta: KvQueryMeta,
		params: CassandraParams,
		plan: QueryPlan,
		db: PostgresQueryable,
	): Promise<Array<Row>> {
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
				`SELECT count(*) AS count FROM ${this.table} kv WHERE kv.table_name = $1${fragments.predicate} AND (kv.expires_at IS NULL OR kv.expires_at > now())`,
				[meta.table.name, ...fragments.params],
				planStatementName('kv_count', plan.candidates),
			);
			total += Number(result.rows[0]?.count ?? 0);
		}
		return [{count: total}];
	}

	private async upsert(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<Array<Row>> {
		const incoming = rowFromParams(meta, params);
		const key = rowKey(meta, incoming);
		if (meta.ifNotExists) {
			if (await this.getRow(meta, key, db)) {
				return [{'[applied]': false}];
			}
			await db.query(
				`DELETE FROM ${this.table} WHERE table_name = $1 AND row_key = $2 AND expires_at IS NOT NULL AND expires_at <= now()`,
				[meta.table.name, key],
				'kv_del_expired',
			);
		}
		const expiresAt = ttlExpiresAt(meta, params) ?? null;
		const result = await db.query(
			`INSERT INTO ${this.table} AS kv (table_name, partition_key, row_key, row_data, expires_at, updated_at)
VALUES ($1, $2, $3, $4::jsonb, $5, now())
ON CONFLICT (table_name, row_key)
DO UPDATE SET partition_key = EXCLUDED.partition_key, row_data = ${MERGED_ROW_DATA}, expires_at = EXCLUDED.expires_at, updated_at = now()
WHERE NOT $6`,
			[
				meta.table.name,
				partitionKey(meta, incoming),
				key,
				JSON.stringify(encodeRow(incoming)),
				expiresAt,
				meta.ifNotExists === true,
			],
			'kv_upsert',
		);
		if (meta.ifNotExists) {
			return [{'[applied]': result.rowCount === 1}];
		}
		return [];
	}

	private async patch(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<void> {
		const key = rowKeyFromParams(meta, params);
		const incoming = paramsRow(params, (meta.pkColumns ?? meta.table.primaryKey) as ReadonlyArray<string>);
		for (const column of meta.patchKeys ?? []) {
			incoming[column] = column in params ? params[column] : null;
		}
		const ttl = ttlExpiresAt(meta, params);
		const expiresAtExpr = ttl === undefined ? KEPT_EXPIRES_AT : 'EXCLUDED.expires_at';
		await db.query(
			`INSERT INTO ${this.table} AS kv (table_name, partition_key, row_key, row_data, expires_at, updated_at)
VALUES ($1, $2, $3, $4::jsonb, $5, now())
ON CONFLICT (table_name, row_key)
DO UPDATE SET partition_key = EXCLUDED.partition_key, row_data = ${MERGED_ROW_DATA}, expires_at = ${expiresAtExpr}, updated_at = now()`,
			[meta.table.name, partitionKey(meta, incoming), key, JSON.stringify(encodeRow(incoming)), ttl ?? null],
			ttl === undefined ? 'kv_patch_keep_ttl' : 'kv_patch_set_ttl',
		);
	}

	private async delete(meta: KvQueryMeta, params: CassandraParams, db: PostgresQueryable): Promise<void> {
		const plan = buildCandidatePlan(meta, params);
		if (plan.candidates.kind === 'none') return;
		if (plan.exact) {
			if (plan.candidates.kind === 'scan') logFullScan(meta);
			for (const fragments of planFragmentGroups(plan.candidates)) {
				await db.query(
					`DELETE FROM ${this.table} kv WHERE kv.table_name = $1${fragments.predicate} AND (kv.expires_at IS NULL OR kv.expires_at > now())`,
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

	private async getRow(meta: KvQueryMeta, key: string, db: PostgresQueryable): Promise<Row | null> {
		const result = await db.query<{row_data: unknown}>(
			`SELECT row_data FROM ${this.table} WHERE table_name = $1 AND row_key = $2 AND (expires_at IS NULL OR expires_at > now()) LIMIT 1`,
			[meta.table.name, key],
			'kv_get_row',
		);
		const row = result.rows[0];
		return row ? decodeRow(row.row_data) : null;
	}
}

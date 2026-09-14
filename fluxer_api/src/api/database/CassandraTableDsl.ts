// SPDX-License-Identifier: AGPL-3.0-or-later

import {isDeepStrictEqual} from 'node:util';
import {registerKvMeta, registerTableSpec} from '@app/api/database/CassandraMetaRegistry';
import type {
	CassandraParam,
	CassandraParams,
	ColumnName,
	ConditionalWriteEntry,
	DbOp,
	KvConditionalBatchEntry,
	KvQueryCondition,
	KvQueryMeta,
	KvTableSpec,
	OrderBy,
	PreparedQuery,
	QueryTemplate,
	RowValue,
	Table,
	WhereExpr,
} from '@app/api/database/CassandraTypes';
import {prepared, validateTtlSeconds} from '@app/api/database/CassandraTypes';

const DEFAULT_TTL_PARAM_NAME = 'ttl_seconds_bind';
const DEFAULT_LIMIT_PARAM_NAME = 'limit_bind';

const CQL_IDENTIFIER_PATTERN = /^[A-Za-z_][A-Za-z0-9_]*$/;

function assertCqlIdentifier(value: string): string {
	if (!CQL_IDENTIFIER_PATTERN.test(value)) {
		throw new Error(`Unsafe CQL identifier: ${JSON.stringify(value)}`);
	}
	return value;
}

function compileWhere<Row extends object>(w: WhereExpr<Row>): string {
	if (w.kind === 'tupleGt') {
		if (w.cols.length !== w.params.length || w.cols.length === 0) {
			throw new Error('tupleGt requires equal-length non-empty cols/params');
		}
		const cols = `(${w.cols.map((c) => assertCqlIdentifier(c as string)).join(', ')})`;
		const params = `(${w.params.map((p) => `:${assertCqlIdentifier(p as string)}`).join(', ')})`;
		return `${cols} > ${params}`;
	}
	const col = assertCqlIdentifier(w.col as string);
	const param = assertCqlIdentifier(w.param as string);
	switch (w.kind) {
		case 'eq':
			return `${col} = :${param}`;
		case 'in':
			return `${col} IN :${param}`;
		case 'lt':
			return `${col} < :${param}`;
		case 'gt':
			return `${col} > :${param}`;
		case 'lte':
			return `${col} <= :${param}`;
		case 'gte':
			return `${col} >= :${param}`;
		case 'tokenGt':
			return `TOKEN(${col}) > TOKEN(:${param})`;
		default: {
			const _exhaustive: never = w;
			return _exhaustive;
		}
	}
}

function opToValue(op: DbOp<unknown>): CassandraParam {
	return op.kind === 'clear' ? null : (op.value as CassandraParam);
}

function assertConditionalValue(value: unknown, column: string): void {
	if (value === null || typeof value === 'string' || typeof value === 'boolean' || typeof value === 'bigint') return;
	if (typeof value === 'number' && Number.isFinite(value)) return;
	if (value instanceof Date && Number.isFinite(value.getTime())) return;
	if (Buffer.isBuffer(value)) return;
	throw new Error(`Conditional writes require a finite scalar, date or buffer for "${column}"`);
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
	assertCqlIdentifier(def.name);
	for (const c of columns) assertCqlIdentifier(c as string);
	for (const k of pk) assertCqlIdentifier(k as string);
	for (const k of partitionKey) assertCqlIdentifier(k as string);
	const tableSpec: KvTableSpec<Row> = {
		name: def.name,
		columns,
		primaryKey: pk as ReadonlyArray<ColumnName<Row>>,
		partitionKey: partitionKey as ReadonlyArray<ColumnName<Row>>,
	};
	registerTableSpec(tableSpec);
	const nonPkColumns = columns.filter((c) => !pk.includes(c as PK)) as Array<Exclude<ColumnName<Row>, PK>>;
	const normalizeWhereArray = (
		where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>,
	): ReadonlyArray<WhereExpr<Row>> => {
		if (!where) return [];
		if (Array.isArray(where)) return where;
		return [where as WhereExpr<Row>];
	};
	const updateAll =
		nonPkColumns.length > 0
			? `UPDATE ${def.name}
SET ${nonPkColumns.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};
`
			: `INSERT INTO ${def.name} (${columns.join(', ')}) VALUES (${columns.map((c) => `:${c}`).join(', ')});`;
	registerKvMeta(updateAll, {action: 'upsert', table: tableSpec} as KvQueryMeta<Record<string, unknown>>);
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
	function buildDynamicUpsertCql(row: Row): {
		cql: string;
		params: CassandraParams;
	} {
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
	function buildSelectCql(
		opts: {
			columns?: ReadonlyArray<ColumnName<Row>>;
			where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
			orderBy?: OrderBy<Row>;
			limit?: number;
		} = {},
		limitParamName?: string,
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
		const limit = typeof opts.limit === 'number' ? ` LIMIT ${limitParamName ? `:${limitParamName}` : opts.limit}` : '';
		return `SELECT ${selectCols} FROM ${def.name}${where}${orderBy}${limit};`;
	}
	function selectCql(
		opts: {
			columns?: ReadonlyArray<ColumnName<Row>>;
			where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
			orderBy?: OrderBy<Row>;
			limit?: number;
		} = {},
	): string {
		const cql = buildSelectCql(opts);
		const kvMeta: KvQueryMeta<Row> = {
			action: 'select',
			table: tableSpec,
			where: normalizeWhereArray(opts.where),
			orderBy: opts.orderBy,
			limit: opts.limit,
			columns: opts.columns ?? columns,
		};
		registerKvMeta(cql, kvMeta as KvQueryMeta<Record<string, unknown>>);
		return cql;
	}
	function select(
		opts: {
			columns?: ReadonlyArray<ColumnName<Row>>;
			where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
			orderBy?: OrderBy<Row>;
			limit?: number;
		} = {},
	): QueryTemplate {
		const limitParamName = typeof opts.limit === 'number' ? DEFAULT_LIMIT_PARAM_NAME : undefined;
		const cql = buildSelectCql(opts, limitParamName);
		const kvMeta: KvQueryMeta<Row> = {
			action: 'select',
			table: tableSpec,
			where: normalizeWhereArray(opts.where),
			orderBy: opts.orderBy,
			limit: opts.limit,
			columns: opts.columns ?? columns,
		};
		registerKvMeta(cql, kvMeta as KvQueryMeta<Record<string, unknown>>);
		return {
			cql,
			bind(params: CassandraParams) {
				const bound =
					limitParamName !== undefined ? {...params, [limitParamName]: opts.limit as CassandraParam} : params;
				return prepared(cql, bound, kvMeta as KvQueryMeta<Record<string, unknown>>);
			},
		};
	}
	function patchByPk(
		pkValues: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
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
		const kvMeta: KvQueryMeta<Row> = {
			action: 'patch',
			table: tableSpec,
			patch: patch as Partial<Record<ColumnName<Row>, DbOp<unknown>>>,
			patchKeys,
			pkColumns: pk,
		};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function withConditions(query: PreparedQuery, expected: Partial<Row>): PreparedQuery {
		const keys = Object.keys(expected) as Array<ColumnName<Row>>;
		if (keys.length === 0) {
			throw new Error(`Conditional writes require expected values for table "${def.name}"`);
		}
		if (!query.kvMeta) {
			throw new Error(`Conditional writes require query metadata for table "${def.name}"`);
		}
		const params = {...query.params};
		for (const col of pk) {
			if (!Object.hasOwn(params, col) || params[col] == null) {
				throw new Error(`Conditional writes require a primary key value for "${def.name}.${col}"`);
			}
		}
		const conditions: Array<KvQueryCondition<Row>> = [];
		let hasNonNullValue = false;
		keys.sort((a, b) => columns.indexOf(a) - columns.indexOf(b));
		for (const col of keys) {
			if (!columns.includes(col) || pk.includes(col as PK)) {
				throw new Error(`Invalid conditional write column "${def.name}.${col}"`);
			}
			const value = expected[col];
			if (value === undefined) {
				throw new Error(`Missing expected value for "${def.name}.${col}"`);
			}
			assertConditionalValue(value, `${def.name}.${col}`);
			const expectedParam = `expected_${col}`;
			if (Object.hasOwn(params, expectedParam)) {
				throw new Error(`Conditional write parameter "${expectedParam}" conflicts with a column value`);
			}
			params[expectedParam] = value as CassandraParam;
			conditions.push({col, expectedParam});
			hasNonNullValue ||= value !== null;
		}
		if (!hasNonNullValue) {
			throw new Error(`Conditional writes require a non-null expected value for table "${def.name}"`);
		}
		const conditionCql = conditions.map(({col, expectedParam}) => `${col} = :${expectedParam}`).join(' AND ');
		const cql = query.cql.replace(/;\s*$/, ` IF ${conditionCql};`);
		const kvMeta: KvQueryMeta<Row> = {
			action: query.kvMeta.action,
			table: tableSpec,
			pkColumns: pk,
			patchKeys: query.kvMeta.patchKeys as ReadonlyArray<ColumnName<Row>> | undefined,
			ttlParamName: query.kvMeta.ttlParamName,
			conditions,
		};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function assertConditionalPatchColumns(patch: object): void {
		for (const col of Object.keys(patch)) {
			if (!nonPkColumns.includes(col as Exclude<ColumnName<Row>, PK>)) {
				throw new Error(`Invalid conditional patch column "${def.name}.${col}"`);
			}
		}
	}
	function conditionalPatchByPk(
		pkValues: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		expected: Partial<Row>,
	): PreparedQuery {
		assertConditionalPatchColumns(patch);
		return withConditions(patchByPk(pkValues, patch), expected);
	}
	function conditionalPatchByPkWithTtl(
		pkValues: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		expected: Partial<Row>,
		ttlSeconds: number,
	): PreparedQuery {
		assertConditionalPatchColumns(patch);
		return withConditions(patchByPkWithTtl(pkValues, patch, validateTtlSeconds(ttlSeconds)), expected);
	}
	function conditionalBatch(entries: ReadonlyArray<ConditionalWriteEntry<Row, PK>>): PreparedQuery {
		const first = entries[0];
		if (!first) {
			throw new Error(`Conditional batches require at least one row for table "${def.name}"`);
		}
		const params: CassandraParams = {};
		const statements: Array<string> = [];
		const batchEntries: Array<KvConditionalBatchEntry<Row>> = [];
		const firstKey = first.action === 'insert' ? first.row : first.pk;
		const firstPartition = partitionKey.map((col) => Reflect.get(firstKey, col));
		const seenKeys: Array<Pick<Row, PK>> = [];
		for (const [index, entry] of entries.entries()) {
			const entryKey = entry.action === 'insert' ? entry.row : entry.pk;
			for (const col of pk) {
				if (entryKey[col] == null) {
					throw new Error(`Conditional batches require a primary key value for "${def.name}.${col}"`);
				}
			}
			if (
				!isDeepStrictEqual(
					partitionKey.map((col) => Reflect.get(entryKey, col)),
					firstPartition,
				)
			) {
				throw new Error(`Conditional batches must stay within one partition of table "${def.name}"`);
			}
			if (seenKeys.some((other) => pk.every((col) => isDeepStrictEqual(entryKey[col], other[col])))) {
				throw new Error(`Conditional batches cannot repeat a row of table "${def.name}"`);
			}
			seenKeys.push(entryKey);
			const query = conditionalEntryQuery(entry);
			const prefix = `entry_${index}_`;
			for (const [name, value] of Object.entries(query.params)) params[`${prefix}${name}`] = value;
			statements.push(query.cql.replace(/:([A-Za-z_][A-Za-z0-9_]*)/g, (_match, name: string) => `:${prefix}${name}`));
			batchEntries.push(conditionalEntryMeta(entry, query, prefix));
		}
		const kvMeta: KvQueryMeta<Row> = {
			action: 'batch',
			table: tableSpec,
			batchEntries,
		};
		return prepared(
			`BEGIN BATCH\n${statements.join('\n')}\nAPPLY BATCH;`,
			params,
			kvMeta as KvQueryMeta<Record<string, unknown>>,
		);
	}
	function conditionalEntryQuery(entry: ConditionalWriteEntry<Row, PK>): PreparedQuery {
		switch (entry.action) {
			case 'insert':
				return entry.ttlSeconds === undefined
					? insertIfNotExists(entry.row)
					: insertIfNotExistsWithTtl(entry.row, entry.ttlSeconds);
			case 'patch':
				return entry.ttlSeconds === undefined
					? conditionalPatchByPk(entry.pk, entry.patch, entry.expected)
					: conditionalPatchByPkWithTtl(entry.pk, entry.patch, entry.expected, entry.ttlSeconds);
			case 'delete':
				return conditionalDeleteByPk(entry.pk, entry.expected);
		}
	}
	function conditionalEntryMeta(
		entry: ConditionalWriteEntry<Row, PK>,
		query: PreparedQuery,
		prefix: string,
	): KvConditionalBatchEntry<Row> {
		const key = pk.map((col) => ({col, param: `${prefix}${col}`}));
		const ttlMetadata =
			query.kvMeta?.ttlParamName === undefined ? {} : {ttlParamName: `${prefix}${query.kvMeta.ttlParamName}`};
		if (entry.action === 'insert') {
			return {
				action: 'insert',
				pk: key,
				values: columns.map((col) => ({col, param: `${prefix}${col}`})),
				...ttlMetadata,
			};
		}
		if (!query.kvMeta?.conditions) {
			throw new Error(`Conditional batch entry has incomplete metadata for table "${def.name}"`);
		}
		const conditions = query.kvMeta.conditions.map(({col, expectedParam}) => ({
			col: col as ColumnName<Row>,
			expectedParam: `${prefix}${expectedParam}`,
		}));
		if (entry.action === 'delete') return {action: 'delete', pk: key, conditions};
		if (!query.kvMeta.patchKeys) {
			throw new Error(`Conditional batch patch has incomplete metadata for table "${def.name}"`);
		}
		return {
			action: 'patch',
			pk: key,
			patch: query.kvMeta.patchKeys.map((col) => ({col: col as ColumnName<Row>, param: `${prefix}${col}`})),
			conditions,
			...ttlMetadata,
		};
	}
	const deleteByPkCql = `DELETE FROM ${def.name} WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};`;
	registerKvMeta(deleteByPkCql, {
		action: 'delete',
		table: tableSpec,
		where: pk.map((col) => ({kind: 'eq', col, param: col})) as ReadonlyArray<WhereExpr<Row>>,
	} as KvQueryMeta<Record<string, unknown>>);
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
		const cql = `DELETE FROM ${def.name}${where};`;
		registerKvMeta(cql, {
			action: 'delete',
			table: tableSpec,
			where: normalizeWhereArray(opts.where),
		} as KvQueryMeta<Record<string, unknown>>);
		return cql;
	}
	function del(opts: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>} = {}): QueryTemplate {
		const cql = deleteCql(opts);
		const kvMeta: KvQueryMeta<Row> = {
			action: 'delete',
			table: tableSpec,
			where: normalizeWhereArray(opts.where),
		};
		return {
			cql,
			bind(params: CassandraParams) {
				return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
			},
		};
	}
	function deleteByPk(pkValues: Pick<Row, PK>): PreparedQuery {
		const params: CassandraParams = {};
		for (const k of pk) params[k] = pkValues[k] as CassandraParam;
		const kvMeta: KvQueryMeta<Row> = {
			action: 'delete',
			table: tableSpec,
			pkColumns: pk,
			where: pk.map((col) => ({kind: 'eq', col, param: col})) as ReadonlyArray<WhereExpr<Row>>,
		} as KvQueryMeta<Row>;
		return prepared(deleteByPkCql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function conditionalDeleteByPk(pkValues: Pick<Row, PK>, expected: Partial<Row>): PreparedQuery {
		return withConditions(deleteByPk(pkValues), expected);
	}
	function deletePartition(partKeyValues: Pick<Row, PartKey>): PreparedQuery {
		if (partitionKey.length === 0) {
			throw new Error(`Table "${def.name}" has empty partitionKey; cannot deletePartition()`);
		}
		const cql = `DELETE FROM ${def.name} WHERE ${partitionKey.map((k) => `${k} = :${k}`).join(' AND ')};`;
		const params: CassandraParams = {};
		for (const k of partitionKey) params[k] = (partKeyValues as Record<string, CassandraParam>)[k];
		const kvMeta: KvQueryMeta<Row> = {
			action: 'delete',
			table: tableSpec,
			where: partitionKey.map((col) => ({kind: 'eq', col, param: col})) as ReadonlyArray<WhereExpr<Row>>,
		};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	const insertBaseCql = `INSERT INTO ${def.name} (${columns.join(', ')}) VALUES (${columns.map((c) => `:${c}`).join(', ')})`;
	function insertCql(opts: {ttlParam?: string} = {}): string {
		const cql = opts.ttlParam ? `${insertBaseCql} USING TTL :${opts.ttlParam};` : `${insertBaseCql};`;
		const kvMeta: KvQueryMeta<Row> = {action: 'upsert', table: tableSpec, ttlParamName: opts.ttlParam};
		registerKvMeta(cql, kvMeta as KvQueryMeta<Record<string, unknown>>);
		return cql;
	}
	function insert(row: Row): PreparedQuery {
		const params = paramsFromRow(row);
		const kvMeta: KvQueryMeta<Row> = {action: 'upsert', table: tableSpec};
		return prepared(`${insertBaseCql};`, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function insertIfNotExists(row: Row): PreparedQuery {
		const params = paramsFromRow(row);
		const cql = `${insertBaseCql} IF NOT EXISTS;`;
		const kvMeta: KvQueryMeta<Row> = {action: 'upsert', table: tableSpec, ifNotExists: true};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function insertIfNotExistsWithTtl(row: Row, ttlSeconds: number): PreparedQuery {
		const cql = `${insertBaseCql} IF NOT EXISTS USING TTL :${DEFAULT_TTL_PARAM_NAME};`;
		const params = paramsFromRow(row);
		params[DEFAULT_TTL_PARAM_NAME] = validateTtlSeconds(ttlSeconds);
		const kvMeta: KvQueryMeta<Row> = {
			action: 'upsert',
			table: tableSpec,
			ifNotExists: true,
			ttlParamName: DEFAULT_TTL_PARAM_NAME,
		};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function insertWithTtl(row: Row, ttlSeconds: number): PreparedQuery {
		const cql = `${insertBaseCql} USING TTL :${DEFAULT_TTL_PARAM_NAME};`;
		const params = paramsFromRow(row);
		params[DEFAULT_TTL_PARAM_NAME] = ttlSeconds;
		const kvMeta: KvQueryMeta<Row> = {
			action: 'upsert',
			table: tableSpec,
			ttlParamName: DEFAULT_TTL_PARAM_NAME,
		};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function insertWithTtlParam(row: Row, ttlParamName: string): PreparedQuery {
		const cql = `${insertBaseCql} USING TTL :${ttlParamName};`;
		const params = paramsFromRow(row);
		if (params[ttlParamName] === undefined) {
			params[ttlParamName] = row[ttlParamName as keyof Row] as CassandraParam;
		}
		const kvMeta: KvQueryMeta<Row> = {action: 'upsert', table: tableSpec, ttlParamName};
		return prepared(cql, params as CassandraParams, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function selectCountCql(opts: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>} = {}): string {
		let where = '';
		if (opts.where) {
			const clauses = Array.isArray(opts.where) ? opts.where : [opts.where];
			if (clauses.length > 0) {
				where = ` WHERE ${clauses.map((c) => compileWhere<Row>(c)).join(' AND ')}`;
			}
		}
		const cql = `SELECT COUNT(*) as count FROM ${def.name}${where};`;
		registerKvMeta(cql, {
			action: 'count',
			table: tableSpec,
			where: normalizeWhereArray(opts.where),
		} as KvQueryMeta<Record<string, unknown>>);
		return cql;
	}
	function selectCount(opts: {where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>} = {}): QueryTemplate {
		const cql = selectCountCql(opts);
		const kvMeta: KvQueryMeta<Row> = {
			action: 'count',
			table: tableSpec,
			where: normalizeWhereArray(opts.where),
		};
		return {
			cql,
			bind(params: CassandraParams) {
				return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
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
		const kvMeta: KvQueryMeta<Row> = {
			action: 'upsert',
			table: tableSpec,
			nowColumn: nowColumn as ColumnName<Row>,
		};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function patchByPkWithTtl(
		pkValues: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		ttlSeconds: number,
	): PreparedQuery {
		const patchKeys = Object.keys(patch) as Array<Exclude<ColumnName<Row>, PK>>;
		if (patchKeys.length === 0) {
			throw new Error(`Refusing to execute empty PATCH update on table "${def.name}"`);
		}
		patchKeys.sort((a, b) => columns.indexOf(a) - columns.indexOf(b));
		const cql = `UPDATE ${def.name} USING TTL :${DEFAULT_TTL_PARAM_NAME}
SET ${patchKeys.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};
`;
		const params: CassandraParams = {};
		for (const k of pk) params[k] = pkValues[k] as CassandraParam;
		for (const c of patchKeys) params[c] = opToValue(patch[c] as DbOp<unknown>);
		params[DEFAULT_TTL_PARAM_NAME] = ttlSeconds;
		const kvMeta: KvQueryMeta<Row> = {
			action: 'patch',
			table: tableSpec,
			patch: patch as Partial<Record<ColumnName<Row>, DbOp<unknown>>>,
			patchKeys,
			pkColumns: pk,
			ttlParamName: DEFAULT_TTL_PARAM_NAME,
		};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function patchByPkWithTtlParam(
		pkValues: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
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
		const kvMeta: KvQueryMeta<Row> = {
			action: 'patch',
			table: tableSpec,
			patch: patch as Partial<Record<ColumnName<Row>, DbOp<unknown>>>,
			patchKeys,
			pkColumns: pk,
			ttlParamName,
		};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
	}
	function upsertAllWithTtl(row: Row, ttlSeconds: number): PreparedQuery {
		const cql =
			nonPkColumns.length > 0
				? `UPDATE ${def.name} USING TTL :${DEFAULT_TTL_PARAM_NAME}
SET ${nonPkColumns.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')};
`
				: `INSERT INTO ${def.name} (${columns.join(', ')}) VALUES (${columns.map((c) => `:${c}`).join(', ')}) USING TTL :${DEFAULT_TTL_PARAM_NAME};`;
		const params = paramsFromRow(row);
		params[DEFAULT_TTL_PARAM_NAME] = ttlSeconds;
		const kvMeta: KvQueryMeta<Row> = {
			action: 'upsert',
			table: tableSpec,
			ttlParamName: DEFAULT_TTL_PARAM_NAME,
		};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
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
		const kvMeta: KvQueryMeta<Row> = {action: 'upsert', table: tableSpec, ttlParamName};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
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
			const kvMeta: KvQueryMeta<Row> = {action: 'upsert', table: tableSpec};
			if (hasAllColumns) {
				return prepared(updateAll, paramsFromRow(row), kvMeta as KvQueryMeta<Record<string, unknown>>);
			}
			const {cql, params} = buildDynamicUpsertCql(row);
			registerKvMeta(cql, kvMeta as KvQueryMeta<Record<string, unknown>>);
			return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
		},
		patchByPk,
		conditionalPatchByPk,
		conditionalPatchByPkWithTtl,
		conditionalBatch,
		deleteCql,
		delete: del,
		deleteByPk,
		conditionalDeleteByPk,
		deletePartition,
		insertCql,
		insert,
		insertIfNotExists,
		insertIfNotExistsWithTtl,
		insertWithTtl,
		insertWithTtlParam,
		selectCountCql,
		selectCount,
		insertWithNow,
		patchByPkWithTtl,
		patchByPkWithTtlParam,
		upsertAllWithTtl,
		upsertAllWithTtlParam,
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

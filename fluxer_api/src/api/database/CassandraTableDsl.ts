// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerKvMeta, registerTableSpec} from './CassandraMetaRegistry';
import type {
	CassandraParam,
	CassandraParams,
	ColumnName,
	DbOp,
	KvQueryMeta,
	KvTableSpec,
	OrderBy,
	PreparedQuery,
	QueryTemplate,
	RowValue,
	Table,
	WhereExpr,
} from './CassandraTypes';
import {prepared} from './CassandraTypes';

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
	if (w.kind === 'tupleGt' || w.kind === 'tupleLt') {
		if (w.cols.length !== w.params.length || w.cols.length === 0) {
			throw new Error(`${w.kind} requires equal-length non-empty cols/params`);
		}
		const cols = `(${w.cols.map((c) => assertCqlIdentifier(c as string)).join(', ')})`;
		const params = `(${w.params.map((p) => `:${assertCqlIdentifier(p as string)}`).join(', ')})`;
		return `${cols} ${w.kind === 'tupleGt' ? '>' : '<'} ${params}`;
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
			orderBy?: OrderBy<Row> | ReadonlyArray<OrderBy<Row>>;
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
		const orderItems = opts.orderBy == null ? [] : Array.isArray(opts.orderBy) ? opts.orderBy : [opts.orderBy];
		const orderBy =
			orderItems.length > 0
				? ` ORDER BY ${orderItems.map((item) => `${item.col} ${item.direction ?? 'ASC'}`).join(', ')}`
				: '';
		const limit = typeof opts.limit === 'number' ? ` LIMIT ${limitParamName ? `:${limitParamName}` : opts.limit}` : '';
		return `SELECT ${selectCols} FROM ${def.name}${where}${orderBy}${limit};`;
	}
	function selectCql(
		opts: {
			columns?: ReadonlyArray<ColumnName<Row>>;
			where?: WhereExpr<Row> | ReadonlyArray<WhereExpr<Row>>;
			orderBy?: OrderBy<Row> | ReadonlyArray<OrderBy<Row>>;
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
			orderBy?: OrderBy<Row> | ReadonlyArray<OrderBy<Row>>;
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
	function patchByPkIf(
		pkValues: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		condition:
			| {col: ColumnName<Row>; expected: RowValue<Row, ColumnName<Row>>}
			| ReadonlyArray<{col: ColumnName<Row>; expected: RowValue<Row, ColumnName<Row>>}>,
	): PreparedQuery {
		const patchKeys = Object.keys(patch) as Array<Exclude<ColumnName<Row>, PK>>;
		if (patchKeys.length === 0) {
			throw new Error(`Refusing to execute empty conditional PATCH update on table "${def.name}"`);
		}
		patchKeys.sort((a, b) => columns.indexOf(a) - columns.indexOf(b));
		const conditions = Array.isArray(condition) ? condition : [condition];
		if (conditions.length === 0) throw new Error('Conditional PATCH requires at least one condition');
		const conditionParams = conditions.map((item) => `if_${item.col}_expected`);
		const cql = `UPDATE ${def.name}
SET ${patchKeys.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')}
IF ${conditions.map((item, index) => `${item.col} = :${conditionParams[index]}`).join(' AND ')};
`;
		const params: CassandraParams = {};
		for (const k of pk) params[k] = pkValues[k] as CassandraParam;
		for (const c of patchKeys) params[c] = opToValue(patch[c] as DbOp<unknown>);
		conditions.forEach((item, index) => {
			params[conditionParams[index] as string] = item.expected as CassandraParam;
		});
		const conditionMeta = conditions.map((item, index) => ({
			col: item.col,
			expectedParam: conditionParams[index] as string,
			expectedValue: item.expected,
		}));
		const kvMeta: KvQueryMeta<Row> = {
			action: 'patch',
			table: tableSpec,
			patch: patch as Partial<Record<ColumnName<Row>, DbOp<unknown>>>,
			patchKeys,
			pkColumns: pk,
			...(conditionMeta.length === 1 ? {condition: conditionMeta[0]} : {conditions: conditionMeta}),
		};
		return prepared(cql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
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
			where: pk.map((col) => ({kind: 'eq', col, param: col})) as ReadonlyArray<WhereExpr<Row>>,
		} as KvQueryMeta<Row>;
		return prepared(deleteByPkCql, params, kvMeta as KvQueryMeta<Record<string, unknown>>);
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
	function patchByPkWithTtlIf(
		pkValues: Pick<Row, PK>,
		patch: Partial<{
			[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
		}>,
		ttlSeconds: number,
		condition:
			| {col: ColumnName<Row>; expected: RowValue<Row, ColumnName<Row>>}
			| ReadonlyArray<{col: ColumnName<Row>; expected: RowValue<Row, ColumnName<Row>>}>,
	): PreparedQuery {
		const patchKeys = Object.keys(patch) as Array<Exclude<ColumnName<Row>, PK>>;
		if (patchKeys.length === 0) {
			throw new Error(`Refusing to execute empty conditional TTL PATCH update on table "${def.name}"`);
		}
		patchKeys.sort((a, b) => columns.indexOf(a) - columns.indexOf(b));
		const conditions = Array.isArray(condition) ? condition : [condition];
		if (conditions.length === 0) throw new Error('Conditional TTL PATCH requires at least one condition');
		const conditionParams = conditions.map((item) => `if_${item.col}_expected`);
		const cql = `UPDATE ${def.name} USING TTL :${DEFAULT_TTL_PARAM_NAME}
SET ${patchKeys.map((c) => `${c} = :${c}`).join(', ')}
WHERE ${pk.map((k) => `${k} = :${k}`).join(' AND ')}
IF ${conditions.map((item, index) => `${item.col} = :${conditionParams[index]}`).join(' AND ')};
`;
		const params: CassandraParams = {};
		for (const k of pk) params[k] = pkValues[k] as CassandraParam;
		for (const c of patchKeys) params[c] = opToValue(patch[c] as DbOp<unknown>);
		params[DEFAULT_TTL_PARAM_NAME] = ttlSeconds;
		conditions.forEach((item, index) => {
			params[conditionParams[index] as string] = item.expected as CassandraParam;
		});
		const conditionMeta = conditions.map((item, index) => ({
			col: item.col,
			expectedParam: conditionParams[index] as string,
			expectedValue: item.expected,
		}));
		const kvMeta: KvQueryMeta<Row> = {
			action: 'patch',
			table: tableSpec,
			patch: patch as Partial<Record<ColumnName<Row>, DbOp<unknown>>>,
			patchKeys,
			pkColumns: pk,
			ttlParamName: DEFAULT_TTL_PARAM_NAME,
			...(conditionMeta.length === 1 ? {condition: conditionMeta[0]} : {conditions: conditionMeta}),
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
		patchByPkIf,
		deleteCql,
		delete: del,
		deleteByPk,
		deletePartition,
		insertCql,
		insert,
		insertIfNotExists,
		insertWithTtl,
		insertWithTtlParam,
		selectCountCql,
		selectCount,
		insertWithNow,
		patchByPkWithTtl,
		patchByPkWithTtlIf,
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
			tupleLt: (cols, params) => ({kind: 'tupleLt', cols, params}),
		},
	};
}

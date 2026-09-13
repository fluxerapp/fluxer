// SPDX-License-Identifier: AGPL-3.0-or-later

import {logBatch, logQuery} from '@app/api/database/CassandraDevLogger';
import {getIsDev} from '@app/api/database/CassandraMetaRegistry';
import type {CassandraParams, KvQueryMeta, PreparedQuery, QueryTemplate} from '@app/api/database/CassandraTypes';
import {
	assertNoUndefinedParams,
	getStatementMeta,
	isConditionalQuery,
	normalizeExecuteArgs,
	normalizeInParams,
} from '@app/api/database/CassandraTypes';
import {Logger} from '@app/api/Logger';
import {chunkArray} from '@app/api/utils/ArrayUtils';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {getClient} from '@pkgs/cassandra/src/Client';
import cassandra from 'cassandra-driver';

const DEFAULT_MAX_PARTITION_KEYS_PER_QUERY = 100;

function isDriverOverloadError(err: unknown): boolean {
	if (err instanceof cassandra.errors.BusyConnectionError) {
		return true;
	}
	if (err instanceof cassandra.errors.NoHostAvailableError && err.innerErrors) {
		return Object.values(err.innerErrors as Record<string, unknown>).some(
			(innerError) => innerError instanceof cassandra.errors.BusyConnectionError,
		);
	}
	return false;
}

export function mapCassandraDriverError(err: unknown): unknown {
	return isDriverOverloadError(err) ? new ServiceUnavailableError() : err;
}

export interface CassandraQueryExecutorForTesting {
	executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
	): Promise<Array<T>>;
	executePagedQuery?<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
		query: PreparedQuery<P>,
		options: {
			pageSize: number;
			pageState?: string | null;
		},
	): Promise<PagedQueryResult<T>>;
	executeBatch(queries: Array<{query: string; params: object; meta?: KvQueryMeta}>, atomic?: boolean): Promise<void>;
	reset?(): void;
	shutdown?(): Promise<void>;
}

let injectedExecutorForTesting: CassandraQueryExecutorForTesting | null = null;
let configuredExecutor: CassandraQueryExecutorForTesting | null = null;

function activeExecutor(): CassandraQueryExecutorForTesting | null {
	return injectedExecutorForTesting ?? configuredExecutor;
}

export function setDatabaseQueryExecutor(executor: CassandraQueryExecutorForTesting | null): void {
	configuredExecutor = executor;
}

export function hasDatabaseQueryExecutor(): boolean {
	return activeExecutor() !== null;
}

export function setCassandraQueryExecutorForTesting(executor: CassandraQueryExecutorForTesting | null): void {
	injectedExecutorForTesting = executor;
}

export function resetCassandraQueryExecutorForTesting(): void {
	injectedExecutorForTesting?.reset?.();
}

export async function shutdownCassandraQueryExecutorForTesting(): Promise<void> {
	await injectedExecutorForTesting?.shutdown?.();
	injectedExecutorForTesting = null;
}

export interface PagedQueryResult<T> {
	rows: Array<T>;
	pageState: string | null;
}

interface CassandraReadOptions {
	consistency: 'all' | 'serial';
}

async function collectSelectRows<T>(queryType: string, result: cassandra.types.ResultSet): Promise<Array<T>> {
	if (queryType !== 'SELECT' || !result.pageState) {
		return (result.rows ?? []) as Array<T>;
	}
	const rows: Array<T> = [];
	for await (const row of result) {
		rows.push(row as T);
	}
	return rows;
}

export async function executeQuery<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
	queryOrPrepared: string | PreparedQuery<P>,
	params?: P,
	readOptions?: CassandraReadOptions,
): Promise<Array<T>> {
	const {cql, params: boundRaw} = normalizeExecuteArgs(queryOrPrepared, params);
	const meta = getStatementMeta(cql);
	const bound = normalizeInParams(meta, boundRaw);
	if (meta.unsafe) {
		throw new Error('Cannot prepare a statement that looks like `SELECT *`');
	}
	if (readOptions && meta.type !== 'SELECT') {
		throw new Error('Read consistency options require a SELECT query');
	}
	assertNoUndefinedParams(bound as Record<string, unknown>);
	const executor = activeExecutor();
	if (executor) {
		return executor.executeQuery<T, P>({
			cql,
			params: bound as P,
			kvMeta: typeof queryOrPrepared === 'string' ? undefined : queryOrPrepared.kvMeta,
		});
	}
	const isDev = getIsDev();
	const startTime = isDev ? performance.now() : 0;
	try {
		const result = await getClient().execute(cql, bound, {
			prepare: true,
			...(readOptions ? {consistency: cassandra.types.consistencies[readOptions.consistency]} : {}),
		});
		const rows = await collectSelectRows<T>(meta.type, result);
		if (isDev) {
			const durationMs = performance.now() - startTime;
			logQuery(meta.type, cql, bound as Record<string, unknown>, durationMs, rows.length);
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
		throw mapCassandraDriverError(err);
	}
}

export async function executeConditional(query: PreparedQuery): Promise<boolean> {
	if (!isConditionalQuery(query)) {
		throw new Error('executeConditional requires a conditional query');
	}
	const rows = await executeQuery(query);
	const applied = rows[0]?.['[applied]'];
	const isBatch = query.kvMeta?.action === 'batch';
	if (
		typeof applied !== 'boolean' ||
		(!isBatch && rows.length !== 1) ||
		rows.some((row) => row['[applied]'] !== applied)
	) {
		throw new Error('Conditional write returned an invalid database result');
	}
	return applied;
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
	readOptions?: CassandraReadOptions,
): Promise<Array<T>> {
	return executeQuery<T, P>(queryOrPrepared, params, readOptions);
}

export async function fetchPage<T = Record<string, unknown>, P extends CassandraParams = CassandraParams>(
	queryOrPrepared: PreparedQuery<P> | string,
	params: P | undefined,
	options: {
		pageSize: number;
		pageState?: string | null;
		readTimeout?: number | undefined;
	},
): Promise<PagedQueryResult<T>> {
	const {cql, params: boundRaw} = normalizeExecuteArgs(queryOrPrepared, params);
	const meta = getStatementMeta(cql);
	const bound = normalizeInParams(meta, boundRaw);
	if (meta.unsafe) {
		throw new Error('Cannot prepare a statement that looks like `SELECT *`');
	}
	assertNoUndefinedParams(bound as Record<string, unknown>);
	const executor = activeExecutor();
	if (executor) {
		const preparedQuery = {
			cql,
			params: bound as P,
			kvMeta: typeof queryOrPrepared === 'string' ? undefined : queryOrPrepared.kvMeta,
		};
		if (executor.executePagedQuery) {
			return executor.executePagedQuery<T, P>(preparedQuery, options);
		}
		const rows = await executor.executeQuery<T, P>(preparedQuery);
		return {
			rows: rows.slice(0, options.pageSize),
			pageState: null,
		};
	}
	const result = await getClient().execute(cql, bound, {
		prepare: true,
		fetchSize: options.pageSize,
		pageState: options.pageState ?? undefined,
		readTimeout: options.readTimeout,
	});
	return {
		rows: (result.rows as Array<T>) ?? [],
		pageState: result.pageState ?? null,
	};
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
	assertUnconditional(normalizeExecuteArgs(queryOrPrepared, params));
	await executeQuery(queryOrPrepared, params);
}

export async function deleteOneOrMany<P extends CassandraParams = CassandraParams>(
	queryOrPrepared: PreparedQuery<P> | string,
	params?: P,
): Promise<void> {
	assertUnconditional(normalizeExecuteArgs(queryOrPrepared, params));
	await executeQuery(queryOrPrepared, params);
}

function assertUnconditional(query: PreparedQuery): void {
	if (isConditionalQuery(query)) {
		throw new Error('Conditional writes must use executeConditional to preserve their result');
	}
}

interface BatchQuery {
	query: string;
	params: object;
	meta?: KvQueryMeta;
}

async function executeBatch(queries: Array<BatchQuery>, atomic = true): Promise<void> {
	if (queries.length === 0) return;
	for (const {query, params, meta} of queries) {
		assertUnconditional({cql: query, params: params as CassandraParams, kvMeta: meta});
		if (getStatementMeta(query).unsafe) {
			throw new Error('Cannot prepare a statement that looks like `SELECT *`');
		}
	}
	for (const {params} of queries) {
		assertNoUndefinedParams(params as Record<string, unknown>);
	}
	const executor = activeExecutor();
	if (executor) {
		await executor.executeBatch(queries, atomic);
		return;
	}
	const options = {
		prepare: true,
		logged: atomic,
		counter: false,
	};
	const isDev = getIsDev();
	const startTime = isDev ? performance.now() : 0;
	try {
		await getClient().batch(
			queries.map(({query, params}) => ({
				query,
				params: normalizeInParams(getStatementMeta(query), params as CassandraParams),
			})),
			options,
		);
	} catch (err: unknown) {
		throw mapCassandraDriverError(err);
	}
	if (isDev) {
		const durationMs = performance.now() - startTime;
		logBatch(queries, durationMs);
	}
}

export class BatchBuilder {
	private queries: Array<BatchQuery> = [];

	add(query: string, params: object, meta?: KvQueryMeta): this {
		this.queries.push({query, params, meta});
		return this;
	}

	addPrepared(q: PreparedQuery): this {
		this.queries.push({query: q.cql, params: q.params, meta: q.kvMeta});
		return this;
	}

	addIf(condition: boolean, query: string, params: object, meta?: KvQueryMeta): this {
		if (condition) this.queries.push({query, params, meta});
		return this;
	}

	addPreparedIf(condition: boolean, q: PreparedQuery): this {
		if (condition) this.queries.push({query: q.cql, params: q.params, meta: q.kvMeta});
		return this;
	}

	async execute(atomic = true): Promise<void> {
		if (this.queries.length === 0) return;
		await executeBatch(this.queries, atomic);
	}

	async executeChunked(chunkSize: number, atomic = false): Promise<void> {
		if (this.queries.length === 0) return;
		for (let i = 0; i < this.queries.length; i += chunkSize) {
			await executeBatch(this.queries.slice(i, i + chunkSize), atomic);
		}
	}

	getQueries(): Array<BatchQuery> {
		return this.queries;
	}
}

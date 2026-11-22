// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BatchBuilder,
	executeConditional,
	setCassandraQueryExecutorForTesting,
	setCassandraWriteCapture,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {defineTable} from '@app/api/database/CassandraTableDsl';
import type {CapturedStatement, WriteCaptureHook} from '@app/api/database/capture/CassandraWriteCapture';
import {
	startCassandraWriteCapture,
	stopCassandraWriteCapture,
} from '@app/api/database/capture/CassandraWriteCaptureLifecycle';
import {initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import cassandra from 'cassandra-driver';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface DriverCall {
	method: 'execute' | 'batch';
	query: unknown;
	params?: unknown;
	options: Record<string, unknown>;
}

const calls: Array<DriverCall> = [];
let conditionalRows: Array<Record<string, unknown>> = [];

interface LogRow {
	user_id: bigint;
	event_id: unknown;
	field: string;
}

const Logs = defineTable<LogRow, 'user_id' | 'event_id'>({
	name: 'driver_path_logs',
	columns: ['user_id', 'event_id', 'field'],
	primaryKey: ['user_id', 'event_id'],
	partitionKey: ['user_id'],
});

interface CodeRow {
	code: string;
	uses: number;
}

const Codes = defineTable<CodeRow, 'code'>({
	name: 'driver_path_codes',
	columns: ['code', 'uses'],
	primaryKey: ['code'],
});

const CAPTURED_TIMESTAMP = 1_700_000_000_000_000;

class RecordingHook implements WriteCaptureHook {
	readonly plainWrites: Array<{tables: Array<string>; logged: boolean}> = [];
	readonly conditionalWrites: Array<{table: string; applied: boolean | undefined}> = [];

	async plain<T>(
		statements: ReadonlyArray<CapturedStatement>,
		logged: boolean,
		send: (timestamp: number) => Promise<T>,
	): Promise<T> {
		this.plainWrites.push({tables: statements.map((statement) => statement.meta?.table.name ?? ''), logged});
		return send(CAPTURED_TIMESTAMP);
	}

	async conditional<T>(
		statement: CapturedStatement,
		send: () => Promise<T>,
		appliedOf: (result: T) => boolean | undefined,
	): Promise<T> {
		const result = await send();
		this.conditionalWrites.push({table: statement.meta?.table.name ?? '', applied: appliedOf(result)});
		return result;
	}

	async close(): Promise<void> {}
}

beforeAll(async () => {
	vi.spyOn(cassandra.Client.prototype, 'connect').mockResolvedValue(undefined);
	vi.spyOn(cassandra.Client.prototype, 'shutdown').mockResolvedValue(undefined);
	vi.spyOn(cassandra.Client.prototype, 'execute').mockImplementation((async (
		query: string,
		params: unknown,
		options: Record<string, unknown>,
	) => {
		calls.push({method: 'execute', query, params, options});
		return {rows: /\bIF\b/.test(query) ? conditionalRows : [], pageState: null};
	}) as never);
	vi.spyOn(cassandra.Client.prototype, 'batch').mockImplementation((async (
		queries: unknown,
		options: Record<string, unknown>,
	) => {
		calls.push({method: 'batch', query: queries, options});
	}) as never);
	await initCassandra({hosts: ['127.0.0.1'], keyspace: 'fluxer', localDc: 'dc1'});
	setCassandraQueryExecutorForTesting(null);
});

afterAll(async () => {
	await shutdownCassandra();
	vi.restoreAllMocks();
});

beforeEach(() => {
	calls.length = 0;
	conditionalRows = [{'[applied]': true}];
});

afterEach(() => {
	setCassandraWriteCapture(null);
});

async function writeEachKind(): Promise<void> {
	await upsertOne(Codes.insert({code: 'abc', uses: 1}));
	await executeConditional(Codes.insertIfNotExists({code: 'abc', uses: 1}));
	await new BatchBuilder()
		.addPrepared(Codes.insert({code: 'a', uses: 1}))
		.addPrepared(Codes.deleteByPk({code: 'b'}))
		.execute();
}

describe('driver path', () => {
	it('generates the now column as a client timeuuid', async () => {
		await upsertOne(Logs.insertWithNow({user_id: 1n, field: 'email'}, 'event_id'));
		expect(calls).toHaveLength(1);
		const [{query, params}] = calls as [DriverCall];
		expect(query).not.toMatch(/now\(\)/i);
		expect((params as Record<string, unknown>).event_id).toBeInstanceOf(cassandra.types.TimeUuid);
	});

	it('sends exactly the options it always sent while capture is off', async () => {
		await writeEachKind();
		expect(calls.map(({method, options}) => [method, options])).toEqual([
			['execute', {prepare: true}],
			['execute', {prepare: true}],
			['batch', {prepare: true, logged: true, counter: false}],
		]);
		expect(calls[2]?.query).toEqual([
			{query: expect.any(String), params: {code: 'a', uses: 1}},
			{query: expect.any(String), params: {code: 'b'}},
		]);
	});

	it('opens no connection and leaves the driver call untouched when capture starts in mode off', async () => {
		const connect = vi.spyOn(JetStreamConnectionManager.prototype, 'connect');
		await startCassandraWriteCapture({mode: 'off', natsUrl: '', authToken: '', stream: 'CASSANDRA_CDC'}, 'api');
		await writeEachKind();
		await stopCassandraWriteCapture();
		expect(connect).not.toHaveBeenCalled();
		expect(calls.map(({method, options}) => [method, options])).toEqual([
			['execute', {prepare: true}],
			['execute', {prepare: true}],
			['batch', {prepare: true, logged: true, counter: false}],
		]);
		expect(calls.map(({params}) => params)).toEqual([{code: 'abc', uses: 1}, {code: 'abc', uses: 1}, undefined]);
		connect.mockRestore();
	});

	it('sends plain writes and batches at the captured timestamp and leaves conditional writes alone', async () => {
		const hook = new RecordingHook();
		setCassandraWriteCapture(hook);
		await writeEachKind();
		expect(calls.map(({options}) => options)).toEqual([
			{prepare: true, timestamp: CAPTURED_TIMESTAMP},
			{prepare: true},
			{prepare: true, logged: true, counter: false, timestamp: CAPTURED_TIMESTAMP},
		]);
		expect(hook.plainWrites).toEqual([
			{tables: ['driver_path_codes'], logged: false},
			{tables: ['driver_path_codes', 'driver_path_codes'], logged: true},
		]);
		expect(hook.conditionalWrites).toEqual([{table: 'driver_path_codes', applied: true}]);
	});
});

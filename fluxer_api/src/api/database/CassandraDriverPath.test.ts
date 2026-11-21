// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BatchBuilder,
	executeConditional,
	setCassandraQueryExecutorForTesting,
	setCassandraWriteCapture,
	upsertOne,
} from '@app/api/database/CassandraQueryExecution';
import {defineTable} from '@app/api/database/CassandraTableDsl';
import {CaptureCodec} from '@app/api/database/capture/CaptureCodec';
import type {IntentRecord, LwtIntentRecord} from '@app/api/database/capture/CaptureRecords';
import {type CaptureSink, WriteCapture} from '@app/api/database/capture/CassandraWriteCapture';
import {
	startCassandraWriteCapture,
	stopCassandraWriteCapture,
} from '@app/api/database/capture/CassandraWriteCaptureLifecycle';
import {initCassandra, nextWriteTimestamp, shutdownCassandra} from '@pkgs/cassandra/src/Client';
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

class RecordingSink implements CaptureSink {
	readonly records: Array<Record<string, unknown>> = [];

	async publish(_subject: string, body: string): Promise<void> {
		this.records.push(JSON.parse(body) as Record<string, unknown>);
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
		await startCassandraWriteCapture({mode: 'off', natsUrl: '', authToken: '', stream: 'CASSANDRA_CAPTURE'}, 'api');
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
		const sink = new RecordingSink();
		setCassandraWriteCapture(
			new WriteCapture({
				process: 'api',
				producer: 'api-0:boot',
				codec: new CaptureCodec(cassandra.types),
				sink,
				nextTimestamp: nextWriteTimestamp,
			}),
		);
		await writeEachKind();
		const [plain, lwt, batch] = sink.records as unknown as [IntentRecord, LwtIntentRecord, IntentRecord];
		expect(calls.map(({options}) => options)).toEqual([
			{prepare: true, timestamp: plain.ts},
			{prepare: true},
			{prepare: true, logged: true, counter: false, timestamp: batch.ts},
		]);
		expect(lwt).toMatchObject({kind: 'l', rows: [{t: 'driver_path_codes', c: {code: 'abc'}, insert: true}]});
		expect(batch).toMatchObject({kind: 'i', logged: true});
		expect(batch.statements.map((statement) => statement.keys)).toEqual([
			[{s: 'row', c: {code: 'a'}}],
			[{s: 'row', c: {code: 'b'}}],
		]);
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomUUID} from 'node:crypto';
import {loadTableLayouts, UnreadableKey} from '@app/api/capture/CaptureTables';
import {BatchReader} from '@app/api/capture/cdc/CdcApply';
import {delay, pause, Stopped, withBackoff} from '@app/api/capture/cdc/CdcBackoff';
import {Comparer, compareColumns, countsAgree, type TableComparison} from '@app/api/capture/cdc/CdcCompare';
import type {CdcConfig, CdcNatsConfig} from '@app/api/capture/cdc/CdcConfig';
import {
	AppliedFloor,
	initialAppliedFloor,
	type Loss,
	LossTracker,
	type ProducerProgress,
	streamWasReset,
	trimmedBeforeAck,
} from '@app/api/capture/cdc/CdcLoss';
import {
	cassandraErrorsTotal,
	clickhouseErrorsTotal,
	gauges,
	keysReadTotal,
	lossyProducersTotal,
	observeBatch,
	producerGapsTotal,
	producersDisabledTotal,
	producersUnclosedTotal,
	recordsTotal,
	renderMetrics,
	repairFinished,
	repairRowsRead,
	repairScanStarted,
	repairTombstones,
	rowsWrittenTotal,
	schemaMismatch,
	streamLossTotal,
	tombstonesTotal,
} from '@app/api/capture/cdc/CdcMetrics';
import {PendingKeys} from '@app/api/capture/cdc/CdcPending';
import {
	nextRepair,
	PROGRESS_PREFIX,
	type RepairProgress,
	type RepairRecord,
	repairTable,
	resumableProgress,
	TableLocks,
} from '@app/api/capture/cdc/CdcRepair';
import {RowEncoder, UnmappableValue} from '@app/api/capture/cdc/CdcRows';
import {
	bookkeepingDdl,
	CDC_DATABASE,
	type CdcTable,
	cdcTable,
	cdcTableName,
	loadColumnTypes,
	schemaPlan,
	viewDdl,
} from '@app/api/capture/cdc/CdcSchema';
import {checkSetup} from '@app/api/capture/cdc/CdcSetup';
import {StateStore} from '@app/api/capture/cdc/CdcState';
import {ClickHouseHttp, quoteIdentifier} from '@app/api/capture/cdc/ClickHouseHttp';
import {CaptureCodec, type CaptureValue} from '@app/api/database/capture/CaptureCodec';
import type {CaptureScope} from '@app/api/database/capture/CaptureRecords';
import {CDC_SKIPPED, CDC_TABLES, type CdcRecord} from '@app/api/database/capture/CdcRecords';
import {settleWithin} from '@app/api/utils/ConcurrencyUtils';
import {createServer, setupGracefulShutdown} from '@fluxer/hono/src/Server';
import {createLogger} from '@fluxer/logger/src/Logger';
import type {Consumer, JsMsg} from '@nats-io/jetstream';
import {getClient, initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import type cassandra from 'cassandra-driver';
import {Hono} from 'hono';

const Logger = createLogger('clickhouse-cdc');

const FETCH_LIMIT = 2000;
const MAX_PENDING_KEYS = 20_000;
const MAX_HELD_MESSAGES = 10_000;
const FETCH_EXPIRES_MS = 1000;
const RECHECK_LIMIT = 2000;
const REREAD_LIMIT = 20_000;
const HOUSEKEEPING_MS = 5 * 60_000;
const STATUS_MS = 30_000;
const CDC_DRAIN_MS = 30_000;
const APPLIED_STATE_KEY = 'stream:applied';
const CDC_SHUTDOWN_MS = 50_000;
const WORKING_MS = 60_000;
const IDLE_REPAIR_MS = 15_000;
const SUMMARY_MS = 86_400_000;

export interface CompareOptions {
	syncDatabase: string;
	sinceUs: number;
	sample: number;
}

interface Runtime {
	config: CdcConfig;
	client: cassandra.Client;
	clickhouse: ClickHouseHttp;
	codec: CaptureCodec;
	encoder: RowEncoder;
	state: StateStore;
	consistencies: {quorum: number; serial: number};
}

interface CdcStatus {
	broken: string | null;
	bootstrapped: boolean;
	paused: boolean;
	repairing: string | null;
	unlisted: Array<string>;
	lag_seconds: number;
	pending_records: number;
	last_batch_at: string | null;
}

async function openRuntime(config: CdcConfig): Promise<Runtime> {
	await initCassandra({
		hosts: config.cassandra.hosts,
		port: config.cassandra.port,
		keyspace: config.cassandra.keyspace,
		localDc: config.cassandra.localDc,
		consistency: 'localQuorum',
		serialConsistency: 'localSerial',
		username: config.cassandra.username || undefined,
		password: config.cassandra.password || undefined,
	});
	const {default: driver} = await import('cassandra-driver');
	const clickhouse = new ClickHouseHttp(config.clickhouse);
	return {
		config,
		client: getClient(),
		clickhouse,
		codec: new CaptureCodec(driver.types),
		encoder: new RowEncoder(driver.types),
		state: new StateStore(clickhouse),
		consistencies: {
			quorum: driver.types.consistencies.localQuorum,
			serial: driver.types.consistencies.localSerial,
		},
	};
}

async function connectNats(nats: CdcNatsConfig): Promise<JetStreamConnectionManager> {
	const connection = new JetStreamConnectionManager({
		url: nats.url,
		token: nats.token || undefined,
		name: 'clickhouse-cdc',
	});
	await connection.connect();
	return connection;
}

async function loadTables(runtime: Runtime): Promise<{tables: Map<string, CdcTable>; keyspace: Set<string>}> {
	const keyspace = runtime.config.cassandra.keyspace;
	const [layouts, types] = await Promise.all([
		loadTableLayouts(runtime.client, keyspace),
		loadColumnTypes(runtime.client, keyspace),
	]);
	const tables = new Map<string, CdcTable>();
	for (const name of CDC_TABLES) {
		const layout = layouts.get(name);
		if (layout !== undefined) tables.set(name, cdcTable(layout, types.get(name) ?? new Map()));
	}
	return {tables, keyspace: new Set(layouts.keys())};
}

async function columnsOf(clickhouse: ClickHouseHttp, database: string): Promise<Map<string, Map<string, string>>> {
	const rows = await clickhouse.select<{table: string; name: string; type: string}>(
		'SELECT table, name, type FROM system.columns WHERE database = {db:String} ORDER BY table, position',
		{params: {db: database}},
	);
	const columns = new Map<string, Map<string, string>>();
	for (const row of rows) {
		const table = columns.get(row.table) ?? new Map<string, string>();
		table.set(row.name, row.type);
		columns.set(row.table, table);
	}
	return columns;
}

interface SyncedSchema {
	tables: Map<string, CdcTable>;
	unlisted: Array<string>;
}

async function syncSchema(runtime: Runtime): Promise<SyncedSchema> {
	const {clickhouse, config} = runtime;
	for (const statement of bookkeepingDdl()) await clickhouse.command(statement);
	await clickhouse.command(`CREATE DATABASE IF NOT EXISTS ${quoteIdentifier(config.viewDatabase)}`);
	const {tables, keyspace} = await loadTables(runtime);
	const existing = new Map(
		[...(await columnsOf(clickhouse, CDC_DATABASE))].filter(
			([name]) => !name.startsWith('_') && !name.includes('__dropped_'),
		),
	);
	const views = new Map(
		[...(await columnsOf(clickhouse, config.viewDatabase))].map(([name, c]) => [name, [...c.keys()]]),
	);
	const plan = schemaPlan({
		desired: tables,
		existing,
		views,
		viewDatabase: config.viewDatabase,
		definer: config.clickhouse.user,
		cassandraTables: keyspace,
		today: new Date().toISOString().slice(0, 10).replaceAll('-', ''),
	});
	for (const statement of plan.statements) {
		Logger.info({statement}, 'Applying ClickHouse schema change');
		await clickhouse.command(statement);
	}
	const marked = [...plan.created, ...plan.widened];
	if (marked.length > 0) {
		const nowMs = Date.now();
		await runtime.state.write(new Map(marked.map((name) => [`due:${name}`, nowMs])));
	}
	schemaMismatch.clear();
	for (const name of plan.mismatched) {
		schemaMismatch.add(name);
		tables.delete(name);
	}
	const unlisted = [...keyspace].filter((name) => !CDC_TABLES.has(name) && !CDC_SKIPPED.has(name)).sort();
	gauges.unlisted_tables = unlisted.length;
	return {tables, unlisted};
}

async function notify(url: string, content: string): Promise<void> {
	if (url === '') return;
	try {
		await fetch(url, {
			method: 'POST',
			headers: {'Content-Type': 'application/json'},
			body: JSON.stringify({content}),
			signal: AbortSignal.timeout(10_000),
		});
	} catch (error) {
		Logger.warn({error}, 'ClickHouse CDC webhook failed');
	}
}

function streamMs(msg: JsMsg): number {
	return Number(msg.timestampNanos / 1_000_000n);
}

function serialisedProgress(progress: ProducerProgress | null): unknown {
	return progress === null ? null : {...progress, above: [...progress.above]};
}

export class CdcConsumer {
	readonly status: CdcStatus = {
		broken: null,
		bootstrapped: false,
		paused: false,
		repairing: null,
		unlisted: [],
		lag_seconds: 0,
		pending_records: 0,
		last_batch_at: null,
	};
	private readonly runtime: Runtime;
	private readonly reader: BatchReader;
	private readonly pending = new PendingKeys();
	private readonly locks = new TableLocks();
	private readonly processId = randomUUID();
	private readonly ttlTables = new Set<string>();
	private readonly markedAt = new Map<string, number>();
	private tables: Map<string, CdcTable>;
	private loss = new LossTracker();
	private held: Array<JsMsg> = [];
	private nextStreamSeq = 1;
	private trimReportedThrough = 0;
	private applied = new AppliedFloor(0);
	private latestStreamMs = 0;
	private batches = 0;
	private stopping = false;
	private lastHousekeepingMs = Date.now();
	private lastSummaryMs = Date.now();
	private summary = {rows: 0, tombstones: 0, repairs: [] as Array<string>};

	constructor(runtime: Runtime, schema: SyncedSchema) {
		this.runtime = runtime;
		this.tables = schema.tables;
		this.status.unlisted = schema.unlisted;
		this.reader = new BatchReader({
			client: runtime.client,
			clickhouse: runtime.clickhouse,
			encoder: runtime.encoder,
			codec: runtime.codec,
			consistencies: runtime.consistencies,
		});
	}

	stop(): void {
		this.stopping = true;
	}

	async restore(consumer: Consumer, connection: JetStreamConnectionManager): Promise<void> {
		const {state} = this.runtime;
		const producers = await state.read<ProducerProgress & {above: Array<number>}>('producer:');
		this.loss = new LossTracker(
			new Map(
				[...producers].map(([name, progress]) => [
					name.slice('producer:'.length),
					{...progress, above: new Set(progress.above)},
				]),
			),
		);
		for (const name of (await state.read('ttl:')).keys()) this.ttlTables.add(name.slice('ttl:'.length));
		for (const [name, table] of this.tables) if (table.layout.defaultTtl > 0) this.ttlTables.add(name);
		const repairs = await state.read<RepairRecord>('repair:');
		for (const name of this.tables.keys()) {
			const record = repairs.get(`repair:${name}`);
			repairFinished.set(name, record === undefined ? 0 : record.finishedMs / 1000);
			repairTombstones.set(name, record?.tombstones ?? 0);
			repairScanStarted.set(name, 0);
			repairRowsRead.set(name, 0);
		}
		this.status.bootstrapped = (await state.read('bootstrap')).has('bootstrap');
		gauges.bootstrapped = this.status.bootstrapped ? 1 : 0;
		const stored = (await state.read<{through: number}>(APPLIED_STATE_KEY)).get(APPLIED_STATE_KEY);
		const info = await consumer.info();
		const jsm = await connection.getJetStreamManager();
		const stream = await jsm.streams.info(this.runtime.config.nats.stream);
		this.nextStreamSeq = info.delivered.stream_seq + 1;
		const through = typeof stored?.through === 'number' ? stored.through : undefined;
		if (streamWasReset(through, stream.state.last_seq)) {
			this.markAll([
				{
					kind: 'gap',
					producer: 'stream',
					detail: `stream restarted at seq ${stream.state.last_seq} below the saved floor ${through}`,
				},
			]);
		}
		const floor = initialAppliedFloor(
			through,
			info.ack_floor.stream_seq,
			info.delivered.stream_seq,
			stream.state.first_seq,
			stream.state.last_seq,
		);
		this.applied = new AppliedFloor(floor);
		this.trimReportedThrough = floor;
	}

	async bootstrap(): Promise<void> {
		if (this.status.bootstrapped) return;
		Logger.info({tables: this.tables.size}, 'ClickHouse CDC bootstrap started');
		while (!this.stopping) {
			try {
				if (!(await this.repairOnce(true))) break;
			} catch (error) {
				if (error instanceof Stopped) return;
				clickhouseErrorsTotal.inc();
				Logger.warn({error}, 'ClickHouse CDC bootstrap step failed, retrying');
				await pause(IDLE_REPAIR_MS, () => this.stopping);
			}
		}
		if (this.stopping) return;
		await this.retry(() => this.runtime.state.write(new Map([['bootstrap', {doneMs: Date.now()}]])), 'clickhouse');
		this.status.bootstrapped = true;
		gauges.bootstrapped = 1;
		Logger.info('ClickHouse CDC bootstrap finished');
	}

	async apply(consumer: Consumer): Promise<void> {
		while (!this.stopping && this.status.broken === null) {
			const windowStartMs = Date.now();
			while (
				!this.stopping &&
				this.status.broken === null &&
				this.pending.size < MAX_PENDING_KEYS &&
				this.held.length < MAX_HELD_MESSAGES &&
				Date.now() - windowStartMs < this.runtime.config.flushSeconds * 1000
			) {
				await this.fetch(consumer);
			}
			if (this.stopping || this.status.broken !== null) break;
			try {
				await this.flush(windowStartMs);
				await this.housekeeping();
			} catch (error) {
				if (error instanceof Stopped) break;
				this.setBroken(`batch failed: ${String(error)}`);
			}
		}
		if (this.stopping && this.status.broken === null) this.handBack();
	}

	private handBack(): void {
		if (this.held.length === 0) return;
		for (const msg of this.held) msg.nak();
		Logger.info({messages: this.held.length}, 'ClickHouse CDC handed back unapplied messages');
		this.held = [];
	}

	async repairs(): Promise<void> {
		while (!this.stopping && this.status.broken === null) {
			try {
				if (!(await this.repairOnce(false))) await pause(IDLE_REPAIR_MS, () => this.stopping);
			} catch (error) {
				if (error instanceof Stopped) return;
				clickhouseErrorsTotal.inc();
				Logger.warn({error}, 'ClickHouse CDC repair failed, trying again later');
				await pause(IDLE_REPAIR_MS, () => this.stopping);
			}
		}
	}

	async refreshStatus(consumer: Consumer, connection: JetStreamConnectionManager): Promise<void> {
		const info = await consumer.info();
		const jsm = await connection.getJetStreamManager();
		const stream = await jsm.streams.info(this.runtime.config.nats.stream);
		const trimmed = trimmedBeforeAck(
			stream.state.first_seq,
			this.applied.preview(this.held.map((msg) => msg.seq)),
			this.trimReportedThrough,
		);
		if (trimmed !== null) {
			this.trimReportedThrough = trimmed.to;
			this.applied.skipTo(trimmed.to);
			streamLossTotal.inc('', trimmed.to - trimmed.from + 1);
			this.markAll([
				{kind: 'gap', producer: 'stream', detail: `stream seq ${trimmed.from} to ${trimmed.to} trimmed before ack`},
			]);
		}
		gauges.ack_pending = info.num_ack_pending;
		gauges.max_ack_pending = info.config.max_ack_pending ?? 0;
		gauges.pending_records = info.num_pending + info.num_ack_pending;
		gauges.stream_bytes = stream.state.bytes;
		gauges.stream_max_bytes = stream.config.max_bytes;
		gauges.stream_first_timestamp_seconds = new Date(stream.state.first_ts).getTime() / 1000;
		gauges.stream_last_timestamp_seconds = new Date(stream.state.last_ts).getTime() / 1000;
		let oldestMs: number | null = null;
		if (gauges.pending_records > 0) {
			const next = await jsm.streams
				.getMessage(this.runtime.config.nats.stream, {seq: info.ack_floor.stream_seq + 1})
				.catch(() => null);
			oldestMs = next?.time.getTime() ?? new Date(stream.state.first_ts).getTime();
		}
		gauges.lag_seconds = oldestMs === null ? 0 : Math.max(0, (Date.now() - oldestMs) / 1000);
		const [recheck] = await this.runtime.clickhouse.select<{queued: string; overdue: string}>(
			`SELECT count() AS queued, countIf(due_at <= now64(3) - INTERVAL 5 MINUTE) AS overdue FROM ${CDC_DATABASE}._recheck FINAL WHERE _deleted = 0`,
		);
		gauges.recheck_queued = Number(recheck?.queued ?? 0);
		gauges.recheck_overdue = Number(recheck?.overdue ?? 0);
		this.status.lag_seconds = gauges.lag_seconds;
		this.status.pending_records = gauges.pending_records;
		gauges.status_refreshed_timestamp_seconds = Date.now() / 1000;
	}

	private async fetch(consumer: Consumer): Promise<void> {
		try {
			const messages = await consumer.fetch({max_messages: FETCH_LIMIT, expires: FETCH_EXPIRES_MS});
			for await (const msg of messages) {
				this.handle(msg);
				if (this.status.broken !== null) return;
			}
		} catch (error) {
			Logger.warn({error}, 'ClickHouse CDC fetch failed');
			await delay(FETCH_EXPIRES_MS);
		}
	}

	private handle(msg: JsMsg): void {
		const atMs = streamMs(msg);
		this.latestStreamMs = Math.max(this.latestStreamMs, atMs);
		if (!msg.redelivered) {
			if (msg.seq > this.nextStreamSeq) {
				Logger.info(
					{from: this.nextStreamSeq, to: msg.seq - 1},
					'ClickHouse CDC delivery skipped stream records, waiting for their redelivery',
				);
			}
			this.nextStreamSeq = Math.max(this.nextStreamSeq, msg.seq + 1);
		}
		let record: CdcRecord;
		try {
			record = msg.json<CdcRecord>();
		} catch (error) {
			this.setBroken(`stream seq ${msg.seq} is not a capture record: ${String(error)}`);
			return;
		}
		switch (record.kind) {
			case 'k':
				recordsTotal.inc('kind="k"');
				this.loss.key(record.producer, record.seq, atMs);
				for (const key of record.keys) {
					const table = this.tables.get(key.t);
					if (table === undefined) continue;
					const unknown = record.out === 'unknown';
					this.pending.add({
						table: key.t,
						scope: key.s,
						key: key.c,
						primaryKey: table.layout.primaryKey,
						ts: record.ts,
						serial: unknown && record.lwt === true,
						unknown,
					});
				}
				break;
			case 'life':
				recordsTotal.inc(`kind="life_${record.state}"`);
				if (record.state === 'disabled') this.captureOff(record.producer);
				this.markAll(this.loss.life(record, atMs));
				break;
			default:
				this.setBroken(`stream seq ${msg.seq} on ${msg.subject} is not a CDC record`);
				return;
		}
		this.held.push(msg);
	}

	private markAll(losses: ReadonlyArray<Loss>): void {
		for (const loss of losses) {
			if (loss.producer === 'stream') {
				Logger.error({loss}, 'CASSANDRA_CDC discarded records before they were applied');
			} else if (loss.kind === 'gap') {
				producerGapsTotal.inc();
			} else if (loss.kind === 'lossy') {
				lossyProducersTotal.inc();
			} else {
				producersUnclosedTotal.inc();
			}
			Logger.warn({loss}, 'Capture records were lost, marking every table for repair');
			void notify(
				this.runtime.config.webhookUrl,
				`ClickHouse CDC lost records (${loss.detail}), repairing every table`,
			);
		}
		if (losses.length === 0) return;
		const nowMs = Date.now();
		for (const name of this.tables.keys()) this.markedAt.set(name, nowMs);
		gauges.repair_due = 1;
	}

	private captureOff(producer: string): void {
		producersDisabledTotal.inc();
		Logger.error({producer}, 'A capture producer started with capture off, ClickHouse misses its writes');
		void notify(
			this.runtime.config.webhookUrl,
			`ClickHouse CDC: ${producer} started with Cassandra write capture off, ClickHouse misses its writes until capture is back and \`cdc repair --all\` has run`,
		);
	}

	private async loadRechecks(): Promise<void> {
		const rows = await this.retry(
			() =>
				this.runtime.clickhouse.select<{
					table: string;
					scope: CaptureScope;
					key: string;
					lwt: number;
					due_text: string;
				}>(
					`SELECT table, scope, key, lwt, toString(due_at) AS due_text FROM ${CDC_DATABASE}._recheck FINAL WHERE _deleted = 0 AND due_at <= now64(3) LIMIT ${RECHECK_LIMIT}`,
				),
			'clickhouse',
		);
		for (const row of rows) {
			const table = this.tables.get(row.table);
			if (table === undefined) continue;
			this.pending.add({
				table: row.table,
				scope: row.scope,
				key: JSON.parse(row.key) as Record<string, CaptureValue>,
				primaryKey: table.layout.primaryKey,
				ts: 0,
				serial: Number(row.lwt) === 1,
				unknown: false,
				recheckDue: row.due_text,
			});
		}
	}

	private async flush(windowStartMs: number): Promise<void> {
		await this.loadRechecks();
		const keys = this.pending.take();
		const producers = this.loss.takeChanges();
		const marks = new Map(this.markedAt);
		if (keys.length === 0 && this.held.length === 0 && producers.size === 0 && marks.size === 0) return;
		const batchId = `${this.processId}:${++this.batches}`;
		const heldSeqs = this.held.map((msg) => msg.seq);
		const appliedThrough = this.applied.preview(heldSeqs);
		const {body, newTtl} = await this.locks.hold(
			keys.map((key) => key.table),
			async () => {
				const read = await this.retry(() => this.reader.read(keys, this.tables, Date.now()), 'cassandra');
				const ttl = [...read.ttlTables].filter((name) => !this.ttlTables.has(name));
				if (read.oversized.size > 0) {
					Logger.warn({tables: [...read.oversized]}, 'A key holds too many rows to read back, repairing its table');
					gauges.repair_due = 1;
				}
				const state = new Map<string, unknown>([
					...[...producers].map(([id, progress]): [string, unknown] => [
						`producer:${id}`,
						serialisedProgress(progress),
					]),
					...[...marks].map(([name, atMs]): [string, unknown] => [`due:${name}`, atMs]),
					...[...read.oversized].map((name): [string, unknown] => [`due:${name}`, Date.now()]),
					...ttl.map((name): [string, unknown] => [`ttl:${name}`, true]),
					[APPLIED_STATE_KEY, {through: appliedThrough}],
				]);
				await this.retry(async () => {
					for (const [table, lines] of read.lines) {
						await this.runtime.clickhouse.insert(cdcTableName(table), lines, `${table}:${batchId}`);
					}
					await this.runtime.clickhouse.insert(`${CDC_DATABASE}._recheck`, read.recheck, `_recheck:${batchId}`);
					await this.runtime.state.write(state);
				}, 'clickhouse');
				return {body: read, newTtl: ttl};
			},
		);
		for (const msg of this.held) msg.ack();
		this.held = [];
		this.applied.commit(heldSeqs);
		for (const name of newTtl) this.ttlTables.add(name);
		for (const [name, atMs] of marks) if (this.markedAt.get(name) === atMs) this.markedAt.delete(name);
		for (const [table, count] of body.keysRead) keysReadTotal.inc(`table="${table}"`, count);
		for (const [table, lines] of body.lines) {
			rowsWrittenTotal.inc(`table="${table}"`, lines.length);
			this.summary.rows += lines.length;
		}
		for (const [table, count] of body.tombstones) {
			tombstonesTotal.inc(`table="${table}",source="stream"`, count);
			this.summary.tombstones += count;
		}
		observeBatch((Date.now() - windowStartMs) / 1000);
		this.status.last_batch_at = new Date().toISOString();
	}

	private async housekeeping(): Promise<void> {
		this.markAll(this.loss.check(this.latestStreamMs));
		const nowMs = Date.now();
		if (nowMs - this.lastHousekeepingMs >= HOUSEKEEPING_MS) {
			this.lastHousekeepingMs = nowMs;
			const schema = await this.retry(() => syncSchema(this.runtime), 'clickhouse');
			this.tables = schema.tables;
			this.status.unlisted = schema.unlisted;
			for (const name of this.tables.keys()) {
				if (!repairFinished.has(name)) repairFinished.set(name, 0);
				if (!repairScanStarted.has(name)) repairScanStarted.set(name, 0);
				if (!repairRowsRead.has(name)) repairRowsRead.set(name, 0);
			}
			await this.sweep();
		}
		if (nowMs - this.lastSummaryMs >= SUMMARY_MS) {
			this.lastSummaryMs = nowMs;
			const {rows, tombstones, repairs} = this.summary;
			this.summary = {rows: 0, tombstones: 0, repairs: []};
			await notify(
				this.runtime.config.webhookUrl,
				`ClickHouse CDC daily: lag ${Math.round(gauges.lag_seconds)} s, ${rows} rows and ${tombstones} tombstones written, repairs: ${repairs.join(', ') || 'none'}`,
			);
		}
	}

	private async sweep(): Promise<void> {
		const {clickhouse, codec, encoder} = this.runtime;
		for (const name of this.ttlTables) {
			const table = this.tables.get(name);
			if (table === undefined) continue;
			const keys = table.keyColumns.map((column) => quoteIdentifier(column.name)).join(', ');
			const expired = await this.retry(
				() =>
					clickhouse.command(
						[
							`INSERT INTO ${cdcTableName(name)} (${keys}, _version, _deleted)`,
							`SELECT ${keys}, _version + 1, 1 FROM ${cdcTableName(name)} FINAL`,
							'WHERE _deleted = 0 AND _expires_at > 0 AND _expires_at <= now64(3)',
						].join(' '),
					),
				'clickhouse',
			);
			if (expired.writtenRows > 0) tombstonesTotal.inc(`table="${name}",source="ttl"`, expired.writtenRows);
			const rereads = await this.retry(
				() =>
					clickhouse.select<Record<string, unknown>>(
						`SELECT ${keys} FROM ${cdcTableName(name)} FINAL WHERE _deleted = 0 AND _reread_at > 0 AND _reread_at <= now64(3) LIMIT ${REREAD_LIMIT}`,
					),
				'clickhouse',
			);
			for (const row of rereads) {
				const key = Object.fromEntries(
					table.keyColumns.map((column) => [column.name, encoder.fromClickHouse(column.cqlType, row[column.name])]),
				);
				this.pending.add({
					table: name,
					scope: 'row',
					key: codec.encodeRecord(key),
					primaryKey: table.layout.primaryKey,
					ts: 0,
					serial: false,
					unknown: false,
				});
			}
		}
	}

	private async repairOnce(bootstrap: boolean): Promise<boolean> {
		const {state} = this.runtime;
		const strip = <T>(entries: Map<string, T>, prefix: string) =>
			new Map([...entries].map(([name, value]) => [name.slice(prefix.length), value]));
		const repairs = strip(await state.read<RepairRecord>('repair:'), 'repair:');
		const due = strip(await state.read<number>('due:'), 'due:');
		const progress = strip(await state.read<RepairProgress>(PROGRESS_PREFIX), PROGRESS_PREFIX);
		const resumed = [...progress].find(([name, saved]) => {
			const known = this.tables.get(name);
			return known !== undefined && resumableProgress(saved, known) !== undefined;
		});
		const choice =
			resumed === undefined
				? nextRepair(this.tables.keys(), repairs, due, Date.now())
				: {table: resumed[0], marked: resumed[1].marked};
		gauges.repair_due = choice?.marked === true ? 1 : 0;
		const table = choice === null ? undefined : this.tables.get(choice.table);
		if (choice === null || table === undefined || (bootstrap && !choice.marked)) return false;
		this.status.repairing = choice.table;
		try {
			const record = await repairTable(
				{
					client: this.runtime.client,
					clickhouse: this.runtime.clickhouse,
					encoder: this.runtime.encoder,
					state,
					locks: this.locks,
					consistency: this.runtime.consistencies.quorum,
					stopping: () => this.stopping,
					onError: (error) => this.countError(error, 'cassandra'),
					onRows: (name, rows) => rowsWrittenTotal.inc(`table="${name}"`, rows),
					onProgress: (name, scanStartMs, read) => {
						repairScanStarted.set(name, scanStartMs / 1000);
						repairRowsRead.set(name, read);
					},
				},
				table,
				choice.marked ? this.runtime.config.markedRowsPerSecond : this.runtime.config.routineRowsPerSecond,
				{marked: choice.marked, ...(resumed === undefined ? {} : {resume: resumed[1]})},
			);
			repairScanStarted.set(choice.table, 0);
			repairRowsRead.set(choice.table, 0);
			repairFinished.set(choice.table, record.finishedMs / 1000);
			repairTombstones.set(choice.table, record.tombstones);
			tombstonesTotal.inc(`table="${choice.table}",source="repair"`, record.tombstones);
			this.summary.repairs.push(`${choice.table} (${record.tombstones} tombstones)`);
			Logger.info({table: choice.table, ...record}, 'ClickHouse CDC repair finished');
			if (choice.marked && !bootstrap && record.tombstones > 0) {
				await notify(
					this.runtime.config.webhookUrl,
					`ClickHouse CDC repair of ${choice.table} after a loss wrote ${record.tombstones} tombstones`,
				);
			}
		} finally {
			this.status.repairing = null;
		}
		return true;
	}

	private countError(error: unknown, source: 'cassandra' | 'clickhouse'): void {
		(source === 'cassandra' ? cassandraErrorsTotal : clickhouseErrorsTotal).inc();
		Logger.warn({error, source}, 'ClickHouse CDC call failed, retrying');
	}

	private async retry<T>(run: () => Promise<T>, source: 'cassandra' | 'clickhouse'): Promise<T> {
		let lastWorkingMs = Date.now();
		try {
			return await withBackoff(run, {
				stopping: () => this.stopping,
				fatal: (error) => error instanceof UnmappableValue || error instanceof UnreadableKey,
				onError: (error) => {
					this.countError(error, source);
					this.status.paused = true;
					gauges.paused = 1;
					if (Date.now() - lastWorkingMs >= WORKING_MS) {
						lastWorkingMs = Date.now();
						for (const msg of this.held) msg.working();
					}
				},
			});
		} finally {
			this.status.paused = false;
			gauges.paused = 0;
		}
	}

	private setBroken(reason: string): void {
		if (this.status.broken !== null) return;
		this.status.broken = reason;
		gauges.broken = 1;
		Logger.error({reason}, 'ClickHouse CDC stopped');
		void notify(this.runtime.config.webhookUrl, `ClickHouse CDC stopped: ${reason}`);
	}
}

export async function runCdcSetup(nats: CdcNatsConfig, create: boolean): Promise<number> {
	const connection = await connectNats(nats);
	try {
		const differences = await checkSetup(await connection.getJetStreamManager(), nats, create);
		for (const difference of differences) process.stderr.write(`${difference}\n`);
		return differences.length === 0 ? 0 : 1;
	} finally {
		await connection.drain();
	}
}

export async function runCdc(config: CdcConfig): Promise<number> {
	if ((await runCdcSetup(config.nats, false)) !== 0) return 1;
	const runtime = await openRuntime(config);
	const connection = await connectNats(config.nats);
	const consumer = await connection.getJetStreamClient().consumers.get(config.nats.stream, config.nats.consumer);
	const cdc = new CdcConsumer(runtime, await syncSchema(runtime));
	await cdc.restore(consumer, connection);
	const app = new Hono();
	app.get('/status', (c) => c.json(cdc.status, cdc.status.broken === null ? 200 : 503));
	app.get('/_metrics', (c) =>
		c.text(renderMetrics(), 200, {'Content-Type': 'text/plain; version=0.0.4; charset=utf-8'}),
	);
	const server = createServer(app, {
		port: config.port,
		onListen: (info) => Logger.info(info, 'ClickHouse CDC status listening'),
	});
	const statusTimer = setInterval(() => {
		void cdc.refreshStatus(consumer, connection).catch((error: unknown) => {
			Logger.warn({error}, 'ClickHouse CDC status refresh failed');
		});
	}, STATUS_MS);
	const finished = (async () => {
		await cdc.bootstrap();
		await Promise.all([cdc.apply(consumer), cdc.repairs()]);
	})();
	setupGracefulShutdown(
		async () => {
			cdc.stop();
			await settleWithin(
				finished.catch(() => undefined),
				CDC_DRAIN_MS,
			);
			clearInterval(statusTimer);
			server.close();
			await connection.drain();
			await shutdownCassandra();
		},
		{logger: Logger, timeoutMs: CDC_SHUTDOWN_MS},
	);
	await finished;
	Logger.info({status: cdc.status}, 'ClickHouse CDC loop ended');
	return cdc.status.broken === null ? 0 : 1;
}

export async function runCdcRepairMark(config: CdcConfig, tables: ReadonlyArray<string> | 'all'): Promise<number> {
	const clickhouse = new ClickHouseHttp(config.clickhouse);
	const names = tables === 'all' ? [...CDC_TABLES] : tables;
	const unknown = names.filter((name) => !CDC_TABLES.has(name));
	if (unknown.length > 0) {
		process.stderr.write(`not in CDC_TABLES: ${unknown.join(', ')}\n`);
		return 1;
	}
	const nowMs = Date.now();
	await new StateStore(clickhouse).write(new Map(names.map((name) => [`due:${name}`, nowMs])));
	process.stdout.write(`marked ${names.length} tables for repair\n`);
	return 0;
}

export async function runCdcViews(config: CdcConfig, database: string, replaceTables: boolean): Promise<number> {
	const runtime = await openRuntime(config);
	try {
		const {clickhouse} = runtime;
		const {tables} = await loadTables(runtime);
		const present = await columnsOf(clickhouse, CDC_DATABASE);
		const engines = new Map(
			(
				await clickhouse.select<{name: string; engine: string}>(
					'SELECT name, engine FROM system.tables WHERE database = {db:String}',
					{params: {db: database}},
				)
			).map((row) => [row.name, row.engine]),
		);
		await clickhouse.command(`CREATE DATABASE IF NOT EXISTS ${quoteIdentifier(database)}`);
		const dropTable = (name: string) =>
			clickhouse.command(`DROP TABLE ${quoteIdentifier(database)}.${quoteIdentifier(name)}`);
		for (const [name, table] of tables) {
			if (!present.has(name)) continue;
			const engine = engines.get(name);
			if (engine !== undefined && engine !== 'View') {
				if (!replaceTables) {
					process.stderr.write(`${database}.${name} is a ${engine} table, pass --replace-tables to drop it\n`);
					return 1;
				}
				await dropTable(name);
			}
			await clickhouse.command(viewDdl(database, config.clickhouse.user, table));
		}
		for (const [name, engine] of engines) {
			if (engine === 'View' || (tables.has(name) && present.has(name))) continue;
			if (replaceTables && CDC_SKIPPED.has(name)) {
				await dropTable(name);
				process.stdout.write(`dropped ${database}.${name}, a table the CDC leaves out\n`);
			} else if (CDC_SKIPPED.has(name)) {
				process.stderr.write(`${database}.${name} is a table the CDC leaves out, pass --replace-tables to drop it\n`);
			} else if (CDC_TABLES.has(name)) {
				process.stderr.write(`${database}.${name} stays a ${engine} table, ${CDC_DATABASE}.${name} does not exist\n`);
			} else {
				process.stderr.write(
					`${database}.${name} is a ${engine} table in neither CDC_TABLES nor CDC_SKIPPED, left in place\n`,
				);
			}
		}
		process.stdout.write(`views in ${database} point at ${CDC_DATABASE}\n`);
		return 0;
	} finally {
		await shutdownCassandra();
	}
}

export async function runCdcCompare(config: CdcConfig, options: CompareOptions): Promise<number> {
	const runtime = await openRuntime(config);
	try {
		const {tables} = await loadTables(runtime);
		const keyspace = config.cassandra.keyspace;
		const types = await loadColumnTypes(runtime.client, keyspace);
		const comparer = new Comparer({
			clickhouse: runtime.clickhouse,
			syncDatabase: options.syncDatabase,
			cdcDatabase: config.viewDatabase,
			sinceUs: options.sinceUs,
			sample: options.sample,
		});
		const syncColumns = await comparer.columns(options.syncDatabase);
		const cdcColumns = await comparer.columns(config.viewDatabase);
		let failed = 0;
		for (const [name, table] of tables) {
			if (table.derived !== undefined) continue;
			const sync = syncColumns.get(name);
			const cdc = cdcColumns.get(name);
			if (sync === undefined || cdc === undefined) {
				process.stdout.write(`${JSON.stringify({table: name, missing: sync === undefined ? 'sync' : 'cdc'})}\n`);
				failed++;
				continue;
			}
			const result: TableComparison = await comparer.table(
				table,
				compareColumns(name, sync, cdc, types.get(name) ?? new Map()),
			);
			const ok = result.count_ok && result.different === 0 && result.collection_different === 0;
			if (!ok) failed++;
			process.stdout.write(`${JSON.stringify({...result, ok})}\n`);
		}
		const [syncUsers, cdcUsers] = await Promise.all([
			comparer.userCounter(options.syncDatabase),
			comparer.userCounter(config.viewDatabase),
		]);
		const usersOk = countsAgree(syncUsers, cdcUsers);
		if (!usersOk) failed++;
		process.stdout.write(`${JSON.stringify({user_counter: {sync: syncUsers, cdc: cdcUsers, ok: usersOk}})}\n`);
		process.stdout.write(`${JSON.stringify({failed})}\n`);
		return failed === 0 ? 0 : 1;
	} finally {
		await shutdownCassandra();
	}
}

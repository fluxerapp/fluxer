// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {loadTableLayouts, readRows} from '@app/api/capture/CaptureTables';
import {fetchOnce} from '@app/api/capture/ReplayFetch';
import {type LedgerAction, ReplayLedger} from '@app/api/capture/ReplayLedger';
import {ReplayRefusal, replayStatement} from '@app/api/capture/ReplayStatements';
import {type ReplayProgress, type ReplayStatus, replayStatus} from '@app/api/capture/ReplayStatus';
import {
	type ImageRecord,
	MARK_SUBJECT,
	type MarkRecord,
	type PassRecord,
	SETTLE_SUBJECT,
} from '@app/api/capture/SettleRecords';
import {differencesBelowHorizon, type RowState, settleWrites, type TableLayout} from '@app/api/capture/SettleRules';
import {CaptureCodec} from '@app/api/database/capture/CaptureCodec';
import type {IntentRecord, ProducerRecord, ProducerState} from '@app/api/database/capture/CaptureRecords';
import {Logger} from '@app/api/Logger';
import {
	createRegisteredMetricsHandler,
	registerCounter,
	registerGauge,
	registerMetricsSection,
} from '@fluxer/hono/src/middleware/Metrics';
import {createServer, setupGracefulShutdown} from '@fluxer/hono/src/Server';
import type {Consumer, JsMsg} from '@nats-io/jetstream';
import {getClient, initCassandra, shutdownCassandra} from '@pkgs/cassandra/src/Client';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import type cassandra from 'cassandra-driver';
import {Hono} from 'hono';

const MAX_APPLIES_IN_FLIGHT = 256;
const FETCH_BATCH = 1000;
const STATUS_INTERVAL_MS = 5000;
const MARK_INTERVAL_MS = 30_000;
const IMAGE_WAIT_MS = 5000;
const RETRY_DELAYS_MS = [100, 250, 500, 1000, 2000, 5000];

const TRANSIENT_RESPONSE_CODES = new Set([0x0000, 0x1000, 0x1001, 0x1002, 0x1100, 0x1200, 0x1500]);
const TRANSIENT_ERROR_NAMES = new Set(['OperationTimedOutError', 'NoHostAvailableError', 'BusyConnectionError']);

export interface ReplayOptions {
	consumer: string;
	imagesConsumer: string;
	port: number;
}

const appliedTotal = registerCounter('fluxer_cassandra_replay_applied_total', 'Records applied to Cassandra, by kind');
const droppedTotal = registerCounter('fluxer_cassandra_replay_dropped_total', 'Intents dropped, by reason');
const settleKeysTotal = registerCounter('fluxer_cassandra_replay_settle_keys', 'Settle keys published, by reason');
const verifyDifferencesTotal = registerCounter(
	'fluxer_cassandra_replay_verify_differences_total',
	'Keys a verify pass found different below the horizon',
);

let status: ReplayStatus = replayStatus({
	progress: {pending: 0, ackPending: 0, oldestUnackedMs: null, latestStreamMs: null},
	nowMs: Date.now(),
	awaitingOutcome: 0,
	producersFinished: false,
	broken: null,
});
let lastPassSeconds = 0;

registerGauge('fluxer_cassandra_replay_lag_seconds', 'Age of the oldest capture record not yet resolved', () => {
	return status.lag_seconds;
});
registerGauge('fluxer_cassandra_replay_pending_records', 'Capture records not yet acknowledged', () => {
	return status.pending_records;
});
registerGauge('fluxer_cassandra_replay_awaiting_outcome', 'Intents held without an outcome', () => {
	return status.awaiting_outcome;
});
registerGauge('fluxer_cassandra_replay_watermark_seconds', 'Timestamp below which every write is resolved', () => {
	return (status.watermark ?? 0) / 1e6;
});
registerGauge('fluxer_cassandra_replay_broken', 'Whether the replayer stopped on a record it cannot apply', () => {
	return status.broken === null ? 0 : 1;
});
registerGauge('fluxer_cassandra_replay_drained', 'Whether every producer is finished and every record resolved', () => {
	return status.drained ? 1 : 0;
});
registerGauge('fluxer_cassandra_replay_settle_pass_seconds', 'Duration of the last settle pass', () => lastPassSeconds);

function delay(ms: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, ms).unref();
	});
}

function isTransient(error: unknown): boolean {
	if (!(error instanceof Error)) return false;
	if (TRANSIENT_ERROR_NAMES.has(error.name)) return true;
	const code = (error as {code?: unknown}).code;
	return error.name === 'ResponseError' && typeof code === 'number' && TRANSIENT_RESPONSE_CODES.has(code);
}

async function withRetry<T>(run: () => Promise<T>): Promise<T> {
	for (let attempt = 0; ; attempt++) {
		try {
			return await run();
		} catch (error) {
			if (!isTransient(error)) throw error;
			await delay(RETRY_DELAYS_MS[Math.min(attempt, RETRY_DELAYS_MS.length - 1)] ?? 5000);
		}
	}
}

function streamMs(msg: JsMsg): number {
	return Number(msg.timestampNanos / 1_000_000n);
}

class Replayer {
	private readonly ledger = new ReplayLedger<JsMsg>();
	private readonly applies = new Set<Promise<void>>();
	private readonly codec: CaptureCodec;
	private readonly layouts: Map<string, TableLayout>;
	private readonly client: cassandra.Client;
	private readonly connection: JetStreamConnectionManager;
	private broken: string | null = null;
	private latestStreamMs: number | null = null;
	private stopping = false;

	constructor(options: {
		codec: CaptureCodec;
		layouts: Map<string, TableLayout>;
		client: cassandra.Client;
		connection: JetStreamConnectionManager;
	}) {
		this.codec = options.codec;
		this.layouts = options.layouts;
		this.client = options.client;
		this.connection = options.connection;
	}

	producerStates(): ReadonlyMap<string, ProducerState> {
		return this.ledger.producerStates();
	}

	stop(): void {
		this.stopping = true;
	}

	async replay(consumer: Consumer): Promise<void> {
		while (!this.stopping && this.broken === null) {
			await fetchOnce(consumer, {max_messages: FETCH_BATCH, expires: 5000}, async (msg) => {
				if (this.broken !== null) return false;
				await this.handle(msg);
				return true;
			});
			await this.run(this.ledger.orphans(this.latestStreamMs ?? 0));
		}
		await Promise.all(this.applies);
	}

	async settleImages(consumer: Consumer): Promise<void> {
		while (!this.stopping && this.broken === null) {
			await fetchOnce(consumer, {max_messages: 32, expires: 5000}, async (msg) => {
				if (this.broken !== null) return false;
				const record = msg.json<ImageRecord | PassRecord>();
				if (record.kind === 'img') {
					await this.applyImage(record);
				} else if (record.state === 'done' && record.seconds !== undefined) {
					lastPassSeconds = record.seconds;
				}
				if (this.broken === null) msg.ack();
				return true;
			});
		}
	}

	async refreshStatus(consumer: Consumer, stream: string): Promise<ReplayStatus> {
		const info = await consumer.info();
		let oldestUnackedMs: number | null = null;
		if (info.num_pending > 0 || info.num_ack_pending > 0) {
			const jsm = await this.connection.getJetStreamManager();
			const next = await jsm.streams.getMessage(stream, {seq: info.ack_floor.stream_seq + 1});
			oldestUnackedMs = next?.time.getTime() ?? this.latestStreamMs;
		}
		const progress: ReplayProgress = {
			pending: info.num_pending,
			ackPending: info.num_ack_pending,
			oldestUnackedMs,
			latestStreamMs: this.latestStreamMs,
		};
		status = replayStatus({
			progress,
			nowMs: Date.now(),
			awaitingOutcome: this.ledger.awaitingOutcome(),
			producersFinished: this.ledger.allProducersFinished(),
			broken: this.broken,
		});
		return status;
	}

	async publishMark(): Promise<void> {
		const record: MarkRecord = {kind: 'mark', watermark: status.watermark, drained: status.drained};
		await this.connection.getJetStreamClient().publish(MARK_SUBJECT, JSON.stringify(record));
	}

	private async handle(msg: JsMsg): Promise<void> {
		const at = streamMs(msg);
		this.latestStreamMs = Math.max(this.latestStreamMs ?? at, at);
		let record: ProducerRecord;
		try {
			record = msg.json<ProducerRecord>();
		} catch (error) {
			this.setBroken(`stream seq ${msg.seq} is not a capture record: ${String(error)}`);
			return;
		}
		switch (record.kind) {
			case 'life':
				await this.run(this.ledger.life(record));
				msg.ack();
				return;
			case 'o':
				await this.run(this.ledger.outcome(record));
				msg.ack();
				return;
			case 'i':
			case 'l':
				await this.run(this.ledger.intent(record, at, msg));
				return;
			default:
				this.setBroken(
					`stream seq ${msg.seq} on ${msg.subject} is not a producer record, so the consumer filter is wrong`,
				);
		}
	}

	private async run(actions: Array<LedgerAction<JsMsg>>): Promise<void> {
		for (const action of actions) {
			switch (action.kind) {
				case 'broken':
					this.setBroken(action.reason);
					return;
				case 'drop':
					droppedTotal.inc(`reason="${action.reason}"`);
					action.handle.ack();
					break;
				case 'settle':
					for (const [index, key] of action.keys.entries()) {
						await this.connection.getJetStreamClient().publish(SETTLE_SUBJECT, JSON.stringify(key), {
							msgID: `${action.handle.seq}:settle:${index}`,
						});
						settleKeysTotal.inc(`reason="${key.reason}"`);
					}
					action.handle.ack();
					break;
				case 'apply':
					await this.schedule(action.intent, action.handle);
					break;
			}
		}
	}

	private async schedule(intent: IntentRecord, msg: JsMsg): Promise<void> {
		while (this.applies.size >= MAX_APPLIES_IN_FLIGHT) {
			await Promise.race(this.applies);
		}
		const apply = this.applyIntent(intent)
			.then(() => {
				appliedTotal.inc('kind="intent"');
				msg.ack();
			})
			.catch((error: unknown) => {
				this.setBroken(`${intent.producer} seq ${intent.seq}: ${String(error)}`);
			})
			.finally(() => {
				this.applies.delete(apply);
			});
		this.applies.add(apply);
	}

	private layout(table: string): TableLayout {
		const layout = this.layouts.get(table);
		if (!layout) throw new ReplayRefusal(`table ${table} does not exist on this cluster`);
		return layout;
	}

	private async applyIntent(intent: IntentRecord): Promise<void> {
		await withRetry(async () => {
			const statements = intent.statements.map((statement) =>
				replayStatement(
					statement,
					this.codec.decodeRecord(statement.p),
					intent.ts,
					Date.now(),
					this.layout(statement.t).defaultTtl,
				),
			);
			const [single] = statements;
			if (statements.length === 1 && single && !intent.logged) {
				await this.client.execute(single.cql, single.params, {prepare: true, timestamp: intent.ts});
				return;
			}
			await this.client.batch(
				statements.map(({cql, params}) => ({query: cql, params})),
				{prepare: true, logged: intent.logged, timestamp: intent.ts},
			);
		});
	}

	private async applyImage(record: ImageRecord): Promise<void> {
		if (record.unreadable !== undefined) {
			this.setBroken(`settle pass ${record.pass} could not read ${record.t}: ${record.unreadable}`);
			return;
		}
		while (!this.stopping && !status.drained && (status.watermark === null || status.watermark < record.h)) {
			await delay(IMAGE_WAIT_MS);
		}
		if (this.stopping) return;
		try {
			const layout = this.layout(record.t);
			const key = this.codec.decodeRecord(record.c);
			const source: Array<RowState> = record.rows.map((row) => ({
				key: this.codec.decodeRecord(row.c),
				cells: Object.fromEntries(
					Object.entries(row.cells).map(([column, cell]) => [
						column,
						{v: this.codec.decode(cell.v), wt: cell.wt, ttl: cell.ttl},
					]),
				),
			}));
			const canonical = (value: unknown) => JSON.stringify(this.codec.encode(value));
			const current = await withRetry(() => readRows(this.client, layout, key));
			if (record.verify) {
				const differences = differencesBelowHorizon(layout, source, current, record.h, canonical);
				if (differences.length > 0) {
					verifyDifferencesTotal.inc('', differences.length);
					Logger.error({pass: record.pass, differences}, 'Capture verify found a difference');
				}
				return;
			}
			const writes = settleWrites(layout, source, current, {
				horizon: record.h,
				elapsedSeconds: Math.ceil((Date.now() - record.read_at) / 1000),
				insert: record.insert === true,
				canonical,
			});
			for (const write of writes) {
				await withRetry(() =>
					this.client.execute(write.cql, write.params, {prepare: true, timestamp: write.timestamp}),
				);
			}
			appliedTotal.inc('kind="image"');
			const after = await withRetry(() => readRows(this.client, layout, key));
			const [difference] = differencesBelowHorizon(layout, source, after, record.h, canonical, true);
			if (difference !== undefined) this.setBroken(`settle pass ${record.pass} left ${difference}`);
		} catch (error) {
			this.setBroken(`settle pass ${record.pass} on ${record.t}: ${String(error)}`);
		}
	}

	private setBroken(reason: string): void {
		if (this.broken !== null) return;
		this.broken = reason;
		status = {...status, broken: reason, drained: false};
		Logger.error({reason}, 'Cassandra replay stopped');
	}
}

function renderProducers(replayer: Replayer): string {
	const counts: Record<ProducerState, number> = {open: 0, closed: 0, sealed: 0, disabled: 0};
	for (const state of replayer.producerStates().values()) counts[state]++;
	return [
		'# HELP fluxer_cassandra_replay_producers Capture producers seen, by state',
		'# TYPE fluxer_cassandra_replay_producers gauge',
		...Object.entries(counts).map(([state, count]) => `fluxer_cassandra_replay_producers{state="${state}"} ${count}`),
	].join('\n');
}

export async function runReplay(options: ReplayOptions): Promise<void> {
	const capture = Config.cassandraWriteCapture;
	await initCassandra({
		hosts: Config.cassandra.hosts.split(',').filter(Boolean),
		port: Config.cassandra.port,
		keyspace: Config.cassandra.keyspace,
		localDc: Config.cassandra.localDc,
		consistency: Config.cassandra.consistency,
		serialConsistency: Config.cassandra.serialConsistency,
		username: Config.cassandra.username || undefined,
		password: Config.cassandra.password || undefined,
	});
	const client = getClient();
	const {default: driver} = await import('cassandra-driver');
	const connection = new JetStreamConnectionManager({
		url: capture.natsUrl,
		token: capture.authToken || undefined,
		name: 'cassandra-replay',
	});
	await connection.connect();
	const js = connection.getJetStreamClient();
	const intents = await js.consumers.get(capture.stream, options.consumer);
	const images = await js.consumers.get(capture.stream, options.imagesConsumer);
	const replayer = new Replayer({
		codec: new CaptureCodec(driver.types),
		layouts: await loadTableLayouts(client, Config.cassandra.keyspace),
		client,
		connection,
	});
	const unregister = registerMetricsSection(() => renderProducers(replayer));
	const app = new Hono();
	app.get('/status', (c) => c.json(status));
	app.get('/_metrics', createRegisteredMetricsHandler());
	const server = createServer(app, {
		port: options.port,
		onListen: (info) => Logger.info(info, 'Replay status listening'),
	});
	let lastMarkAt = 0;
	const statusTimer = setInterval(() => {
		void replayer
			.refreshStatus(intents, capture.stream)
			.then(async () => {
				if (Date.now() - lastMarkAt < MARK_INTERVAL_MS) return;
				lastMarkAt = Date.now();
				await replayer.publishMark();
			})
			.catch((error: unknown) => Logger.warn({error}, 'Replay status refresh failed'));
	}, STATUS_INTERVAL_MS);
	const finished = Promise.all([replayer.replay(intents), replayer.settleImages(images)]);
	setupGracefulShutdown(
		async () => {
			replayer.stop();
			await finished;
			clearInterval(statusTimer);
			unregister();
			server.close();
			await connection.drain();
			await shutdownCassandra();
		},
		{logger: Logger, timeoutMs: 60_000},
	);
	await finished;
	Logger.info({status}, 'Cassandra replay loop ended');
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KvQueryMeta} from '@app/api/database/CassandraTypes';
import type {CaptureCodec} from '@app/api/database/capture/CaptureCodec';
import {lwtRows, statementTtl, writeKeys} from '@app/api/database/capture/CaptureKeys';
import {classifyWriteError} from '@app/api/database/capture/CaptureOutcomes';
import {
	type CaptureOutcome,
	type CaptureProcess,
	type CaptureStatement,
	captureSubject,
	type IntentRecord,
	type LifeRecord,
	type LwtIntentRecord,
	type OutcomeRecord,
} from '@app/api/database/capture/CaptureRecords';
import {Logger} from '@app/api/Logger';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {registerCounter, registerHistogram} from '@fluxer/hono/src/middleware/Metrics';
import type {JetStreamClient} from '@nats-io/jetstream';
import {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';

const PUBLISH_TIMEOUT_MS = 2000;
const MAX_TIMESTAMP_AGE_MS = 5000;
const OUTCOME_BATCH_SIZE = 512;
const OUTCOME_FLUSH_DELAY_MS = 5;
const OUTCOME_RETRY_DELAY_MS = 100;
const CLOSE_POLL_MS = 10;

const DEFAULT_LIMITS: WriteCaptureLimits = {maxInFlight: 4096, maxBufferedOutcomes: 50_000, slotWaitMs: 2000};

const recordsTotal = registerCounter('fluxer_cassandra_capture_records_total', 'Capture records built, by kind');
const bytesTotal = registerCounter('fluxer_cassandra_capture_bytes_total', 'Bytes of capture records built');
const recordBytes = registerHistogram(
	'fluxer_cassandra_capture_record_bytes',
	'Size of each capture record in bytes',
	[256, 1024, 4096, 16_384, 65_536, 262_144, 1_048_576],
);
const unkeyedTotal = registerCounter(
	'fluxer_cassandra_capture_unkeyed_total',
	'Captured writes whose rows capture cannot key, by table',
);
const lwtKeysTotal = registerCounter(
	'fluxer_cassandra_capture_lwt_keys_total',
	'Row keys recorded for conditional writes, by table',
);
const intentPublishSeconds = registerHistogram(
	'fluxer_cassandra_capture_intent_publish_seconds',
	'Time from sending an intent to its stream ack, retry included, in mode on',
	[0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5],
);
const measureFailuresTotal = registerCounter(
	'fluxer_cassandra_capture_measure_failures_total',
	'Records measure mode could not build, so the write went ahead uncaptured, by kind',
);
const publishFailuresTotal = registerCounter(
	'fluxer_cassandra_capture_publish_failures_total',
	'Capture publishes that failed, by kind',
);
const rejectedWritesTotal = registerCounter(
	'fluxer_cassandra_capture_rejected_writes_total',
	'Writes capture refused before they were sent to Cassandra, by reason',
);

export interface CaptureSink {
	publish(subject: string, body: string, msgID: string, timeoutMs: number): Promise<void>;
	close(): Promise<void>;
}

export interface CapturedStatement {
	cql: string;
	type: string;
	params: Readonly<Record<string, unknown>>;
	meta: KvQueryMeta | undefined;
}

export interface WriteCaptureLimits {
	maxInFlight: number;
	maxBufferedOutcomes: number;
	slotWaitMs: number;
}

interface WriteCaptureOptions {
	process: CaptureProcess;
	producer: string;
	codec: CaptureCodec;
	sink: CaptureSink | null;
	nextTimestamp: () => number;
	now?: () => number;
	limits?: WriteCaptureLimits;
}

function delay(ms: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, ms).unref();
	});
}

function tableOf(statement: CapturedStatement): string {
	return (
		statement.meta?.table.name ??
		/^\s*(?:INSERT\s+INTO|UPDATE|DELETE\s+FROM)\s+([A-Za-z0-9_]+)/i.exec(statement.cql)?.[1] ??
		'unknown'
	);
}

export class WriteCapture {
	private readonly process: CaptureProcess;
	private readonly producer: string;
	private readonly codec: CaptureCodec;
	private readonly sink: CaptureSink | null;
	private readonly nextTimestamp: () => number;
	private readonly now: () => number;
	private readonly limits: WriteCaptureLimits;
	private readonly waiters: Array<() => void> = [];
	private readonly outcomes: Array<[number, CaptureOutcome]> = [];
	private seq = 0;
	private inFlight = 0;
	private outcomeBatches = 0;
	private flushTimer: NodeJS.Timeout | null = null;
	private flushing: Promise<void> | null = null;
	private closing = false;
	private stopped = false;

	constructor(options: WriteCaptureOptions) {
		this.process = options.process;
		this.producer = options.producer;
		this.codec = options.codec;
		this.sink = options.sink;
		this.nextTimestamp = options.nextTimestamp;
		this.now = options.now ?? Date.now;
		this.limits = options.limits ?? DEFAULT_LIMITS;
	}

	async open(): Promise<void> {
		await this.publishLife({kind: 'life', producer: this.producer, state: 'open'});
	}

	async plain<T>(
		statements: ReadonlyArray<CapturedStatement>,
		logged: boolean,
		send: (timestamp: number) => Promise<T>,
	): Promise<T> {
		await this.acquire();
		try {
			const seq = ++this.seq;
			const ts = this.nextTimestamp();
			await this.begin(
				'i',
				seq,
				(): IntentRecord => ({
					kind: 'i',
					producer: this.producer,
					seq,
					ts,
					logged,
					statements: statements.map((statement) => this.statement(statement)),
				}),
			);
			this.assertFresh(seq, ts);
			return await this.finish(seq, send(ts), () => 'applied');
		} finally {
			this.release();
		}
	}

	async conditional<T>(
		statement: CapturedStatement,
		send: () => Promise<T>,
		appliedOf: (result: T) => boolean | undefined,
	): Promise<T> {
		await this.acquire();
		try {
			const seq = ++this.seq;
			await this.begin('l', seq, (): LwtIntentRecord => {
				const table = tableOf(statement);
				const rows = lwtRows(statement.meta, statement.params);
				if (rows === undefined) {
					unkeyedTotal.inc(`table="${table}"`);
				} else {
					lwtKeysTotal.inc(`table="${table}"`, rows.length);
				}
				return {
					kind: 'l',
					producer: this.producer,
					seq,
					ts: this.nextTimestamp(),
					rows: (rows ?? []).map(({t, c, insert}) => ({t, c: this.codec.encodeRecord(c), ...(insert ? {insert} : {})})),
				};
			});
			return await this.finish(seq, send(), (result) => {
				const applied = appliedOf(result);
				if (applied === undefined) return 'unknown';
				return applied ? 'applied' : 'not_applied';
			});
		} finally {
			this.release();
		}
	}

	async close(timeoutMs: number): Promise<void> {
		if (this.sink === null || this.closing) return;
		this.closing = true;
		const deadline = this.now() + timeoutMs;
		while ((this.inFlight > 0 || this.outcomes.length > 0 || this.flushing !== null) && this.now() < deadline) {
			this.scheduleFlush(0);
			await delay(CLOSE_POLL_MS);
		}
		this.stopped = true;
		if (this.inFlight > 0 || this.outcomes.length > 0) {
			Logger.error(
				{inFlight: this.inFlight, outcomes: this.outcomes.length},
				'Cassandra write capture closed with unpublished outcomes',
			);
		} else {
			try {
				await this.publishLife({kind: 'life', producer: this.producer, state: 'closed', last_seq: this.seq});
			} catch (error) {
				publishFailuresTotal.inc('kind="life"');
				Logger.error({error}, 'Cassandra write capture could not publish its closed record');
			}
		}
		await this.sink.close();
	}

	private statement(statement: CapturedStatement): CaptureStatement {
		const keys = writeKeys(statement.meta, statement.type, statement.params);
		const ttl = statementTtl(statement.meta, statement.type, statement.params);
		const t = tableOf(statement);
		if (keys === undefined) unkeyedTotal.inc(`table="${t}"`);
		return {
			t,
			q: statement.cql,
			p: this.codec.encodeRecord(statement.params),
			...(ttl === undefined ? {} : {ttl}),
			...(keys === undefined ? {} : {keys: keys.map(({s, c}) => ({s, c: this.codec.encodeRecord(c)}))}),
		};
	}

	private async begin(kind: 'i' | 'l', seq: number, build: () => IntentRecord | LwtIntentRecord): Promise<void> {
		let body: string;
		try {
			body = JSON.stringify(build());
		} catch (error) {
			if (this.sink !== null) throw error;
			measureFailuresTotal.inc(`kind="${kind}"`);
			Logger.warn({error}, 'Cassandra write capture could not build a record in measure mode');
			return;
		}
		const bytes = Buffer.byteLength(body);
		recordsTotal.inc(`kind="${kind}"`);
		bytesTotal.inc('', bytes);
		recordBytes.observe(bytes);
		if (this.sink === null) return;
		const subject = captureSubject(this.process, kind);
		const msgID = `${this.producer}:${seq}:${kind}`;
		const sink = this.sink;
		const startedAt = performance.now();
		try {
			await sink
				.publish(subject, body, msgID, PUBLISH_TIMEOUT_MS)
				.catch(() => sink.publish(subject, body, msgID, PUBLISH_TIMEOUT_MS));
			intentPublishSeconds.observe((performance.now() - startedAt) / 1000);
		} catch {
			publishFailuresTotal.inc(`kind="${kind}"`);
			this.recordOutcome(seq, 'failed');
			throw new ServiceUnavailableError();
		}
	}

	private assertFresh(seq: number, ts: number): void {
		if (this.sink === null || this.now() - ts / 1000 < MAX_TIMESTAMP_AGE_MS) return;
		rejectedWritesTotal.inc('reason="stale"');
		this.recordOutcome(seq, 'failed');
		throw new ServiceUnavailableError();
	}

	private async finish<T>(seq: number, sent: Promise<T>, outcomeOf: (result: T) => CaptureOutcome): Promise<T> {
		let result: T;
		try {
			result = await sent;
		} catch (error) {
			this.recordOutcome(seq, classifyWriteError(error));
			throw error;
		}
		this.recordOutcome(seq, outcomeOf(result));
		return result;
	}

	private hasRoom(): boolean {
		return this.inFlight < this.limits.maxInFlight && this.outcomes.length < this.limits.maxBufferedOutcomes;
	}

	private async acquire(): Promise<void> {
		if (this.sink === null) return;
		if (this.closing) {
			rejectedWritesTotal.inc('reason="closed"');
			throw new ServiceUnavailableError();
		}
		if (this.waiters.length === 0 && this.hasRoom()) {
			this.inFlight++;
			return;
		}
		await new Promise<void>((resolve, reject) => {
			const waiter = () => {
				clearTimeout(timer);
				this.inFlight++;
				resolve();
			};
			const timer = setTimeout(() => {
				this.waiters.splice(this.waiters.indexOf(waiter), 1);
				rejectedWritesTotal.inc('reason="busy"');
				reject(new ServiceUnavailableError());
			}, this.limits.slotWaitMs);
			this.waiters.push(waiter);
		});
	}

	private release(): void {
		if (this.sink === null) return;
		this.inFlight--;
		this.wake();
	}

	private wake(): void {
		while (this.waiters.length > 0 && this.hasRoom()) {
			this.waiters.shift()?.();
		}
	}

	private recordOutcome(seq: number, outcome: CaptureOutcome): void {
		if (this.sink === null) return;
		this.outcomes.push([seq, outcome]);
		this.scheduleFlush(this.outcomes.length >= OUTCOME_BATCH_SIZE ? 0 : OUTCOME_FLUSH_DELAY_MS);
	}

	private scheduleFlush(delayMs: number): void {
		if (this.flushing !== null || this.outcomes.length === 0) return;
		if (this.flushTimer !== null) {
			if (delayMs > 0) return;
			clearTimeout(this.flushTimer);
		}
		this.flushTimer = setTimeout(() => {
			this.flushTimer = null;
			this.flushing = this.flushOutcomes().finally(() => {
				this.flushing = null;
				this.scheduleFlush(OUTCOME_FLUSH_DELAY_MS);
			});
		}, delayMs);
	}

	private async flushOutcomes(): Promise<void> {
		const sink = this.sink;
		while (sink !== null && this.outcomes.length > 0 && !this.stopped) {
			const batch = this.outcomes.slice(0, OUTCOME_BATCH_SIZE);
			const record: OutcomeRecord = {kind: 'o', producer: this.producer, outcomes: batch};
			const msgID = `${this.producer}:${++this.outcomeBatches}:o`;
			const body = JSON.stringify(record);
			while (!this.stopped) {
				try {
					await sink.publish(captureSubject(this.process, 'o'), body, msgID, PUBLISH_TIMEOUT_MS);
					this.outcomes.splice(0, batch.length);
					break;
				} catch {
					publishFailuresTotal.inc('kind="o"');
					await delay(OUTCOME_RETRY_DELAY_MS);
				}
			}
			this.wake();
		}
	}

	private async publishLife(record: LifeRecord): Promise<void> {
		if (this.sink === null) return;
		await this.sink.publish(
			captureSubject(this.process, 'life'),
			JSON.stringify(record),
			`${this.producer}:${record.state}`,
			PUBLISH_TIMEOUT_MS,
		);
	}
}

export async function announceCaptureDisabled(
	sink: CaptureSink,
	process: CaptureProcess,
	producer: string,
): Promise<void> {
	const record: LifeRecord = {kind: 'life', producer, state: 'disabled'};
	await sink.publish(
		captureSubject(process, 'life'),
		JSON.stringify(record),
		`${producer}:disabled`,
		PUBLISH_TIMEOUT_MS,
	);
}

export class JetStreamCaptureSink implements CaptureSink {
	private readonly connection: JetStreamConnectionManager;
	private readonly stream: string;
	private client: JetStreamClient | null = null;

	constructor(options: {url: string; token: string; stream: string}) {
		this.connection = new JetStreamConnectionManager({
			url: options.url,
			token: options.token || undefined,
			name: 'cassandra-capture',
		});
		this.stream = options.stream;
	}

	async connect(): Promise<void> {
		await this.connection.connect();
		const jsm = await this.connection.getJetStreamManager();
		await jsm.streams.info(this.stream);
		this.client = this.connection.getJetStreamClient();
	}

	async publish(subject: string, body: string, msgID: string, timeoutMs: number): Promise<void> {
		if (this.client === null) {
			throw new Error('Cassandra capture sink is not connected');
		}
		await this.client.publish(subject, body, {msgID, timeout: timeoutMs, expect: {streamName: this.stream}});
	}

	async close(): Promise<void> {
		await this.connection.drain();
	}
}

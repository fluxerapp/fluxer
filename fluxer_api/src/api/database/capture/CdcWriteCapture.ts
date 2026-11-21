// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CaptureCodec} from '@app/api/database/capture/CaptureCodec';
import {lwtRows, type RawKey, writeKeys} from '@app/api/database/capture/CaptureKeys';
import {classifyWriteError} from '@app/api/database/capture/CaptureOutcomes';
import type {CaptureProcess} from '@app/api/database/capture/CaptureRecords';
import {
	bytesTotal,
	type CapturedStatement,
	type CaptureSink,
	PUBLISH_TIMEOUT_MS,
	publishFailuresTotal,
	recordBytes,
	recordsTotal,
	tableOf,
	unkeyedTotal,
	type WriteCaptureHook,
} from '@app/api/database/capture/CassandraWriteCapture';
import {
	CDC_TABLES,
	type CdcClosedRecord,
	type CdcLifeRecord,
	type CdcOutcome,
	type CdcSubjectKind,
	cdcSubject,
	type KeyRecord,
} from '@app/api/database/capture/CdcRecords';
import {Logger} from '@app/api/Logger';
import {registerCounter} from '@fluxer/hono/src/middleware/Metrics';

const PUBLISH_WINDOW = 64;
const CLOSE_POLL_MS = 10;

const DEFAULT_LIMITS: CdcCaptureLimits = {maxQueued: 20_000, retryMs: 250, aliveMs: 300_000};

const droppedTotal = registerCounter(
	'fluxer_cassandra_capture_dropped_total',
	'CDC key records dropped before they were published, by table',
);

export interface CdcCaptureLimits {
	maxQueued: number;
	retryMs: number;
	aliveMs: number;
}

interface CdcWriteCaptureOptions {
	process: CaptureProcess;
	producer: string;
	pod: string;
	codec: CaptureCodec;
	sink: CaptureSink;
	nextTimestamp: () => number;
	now?: () => number;
	limits?: CdcCaptureLimits;
}

interface TableKey extends RawKey {
	t: string;
}

interface QueuedKeyRecord {
	seq: number;
	body: string;
	tables: ReadonlyArray<string>;
}

interface Outgoing {
	kind: CdcSubjectKind;
	body: string;
	msgID: string;
	done: () => void;
}

interface Loss {
	from: number;
	to: number;
}

function delay(ms: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, ms).unref();
	});
}

function statementKeys(statement: CapturedStatement): Array<TableKey> {
	const t = tableOf(statement);
	if (!CDC_TABLES.has(t)) return [];
	const keys = writeKeys(statement.meta, statement.type, statement.params);
	if (keys === undefined) {
		unkeyedTotal.inc(`table="${t}"`);
		return [];
	}
	return keys.map(({s, c}) => ({t, s, c}));
}

function conditionalKeys(statement: CapturedStatement): Array<TableKey> {
	const t = tableOf(statement);
	if (!CDC_TABLES.has(t)) return [];
	const rows = lwtRows(statement.meta, statement.params);
	if (rows === undefined) {
		unkeyedTotal.inc(`table="${t}"`);
		return [];
	}
	return rows.map(({t: table, c}) => ({t: table, s: 'row', c}));
}

export class CdcWriteCapture implements WriteCaptureHook {
	private readonly process: CaptureProcess;
	private readonly producer: string;
	private readonly pod: string;
	private readonly codec: CaptureCodec;
	private readonly sink: CaptureSink;
	private readonly nextTimestamp: () => number;
	private readonly now: () => number;
	private readonly limits: CdcCaptureLimits;
	private queue: Array<QueuedKeyRecord> = [];
	private seq = 0;
	private dropped = 0;
	private loss: Loss | null = null;
	private openDue = false;
	private aliveDue = false;
	private alives = 0;
	private aliveTimer: NodeJS.Timeout | null = null;
	private retryTimer: NodeJS.Timeout | null = null;
	private pumping: Promise<void> | null = null;
	private failing = false;
	private closing = false;
	private stopped = false;

	constructor(options: CdcWriteCaptureOptions) {
		this.process = options.process;
		this.producer = options.producer;
		this.pod = options.pod;
		this.codec = options.codec;
		this.sink = options.sink;
		this.nextTimestamp = options.nextTimestamp;
		this.now = options.now ?? Date.now;
		this.limits = options.limits ?? DEFAULT_LIMITS;
	}

	open(): void {
		this.openDue = true;
		this.aliveTimer = setInterval(() => {
			this.aliveDue = true;
			this.kick();
		}, this.limits.aliveMs);
		this.aliveTimer.unref();
		this.kick();
	}

	async plain<T>(
		statements: ReadonlyArray<CapturedStatement>,
		_logged: boolean,
		send: (timestamp: number) => Promise<T>,
	): Promise<T> {
		const ts = this.nextTimestamp();
		const keysOf = () => statements.flatMap(statementKeys);
		let result: T;
		try {
			result = await send(ts);
		} catch (error) {
			if (classifyWriteError(error) === 'unknown') this.record(ts, 'unknown', false, keysOf);
			throw error;
		}
		this.record(ts, 'applied', false, keysOf);
		return result;
	}

	async conditional<T>(
		statement: CapturedStatement,
		send: () => Promise<T>,
		appliedOf: (result: T) => boolean | undefined,
	): Promise<T> {
		const ts = this.nextTimestamp();
		const keysOf = () => conditionalKeys(statement);
		let result: T;
		try {
			result = await send();
		} catch (error) {
			if (classifyWriteError(error) === 'unknown') this.record(ts, 'unknown', true, keysOf);
			throw error;
		}
		const applied = appliedOf(result);
		if (applied !== false) this.record(ts, applied ? 'applied' : 'unknown', true, keysOf);
		return result;
	}

	async close(timeoutMs: number): Promise<void> {
		if (this.closing) return;
		this.closing = true;
		if (this.aliveTimer !== null) clearInterval(this.aliveTimer);
		this.aliveDue = false;
		const deadline = this.now() + timeoutMs;
		while (this.hasWork() && this.now() < deadline) {
			this.kick();
			await delay(CLOSE_POLL_MS);
		}
		this.stopped = true;
		if (this.retryTimer !== null) clearTimeout(this.retryTimer);
		await this.pumping;
		for (const entry of this.queue) this.lose(entry.seq, entry.tables);
		this.queue = [];
		const closed: CdcClosedRecord = {
			kind: 'life',
			producer: this.producer,
			state: 'closed',
			last_seq: this.seq,
			dropped: this.dropped,
		};
		try {
			await this.sink.publish(
				cdcSubject(this.process, 'life'),
				JSON.stringify(closed),
				`${this.producer}:closed`,
				PUBLISH_TIMEOUT_MS,
			);
		} catch (error) {
			publishFailuresTotal.inc('kind="life"');
			Logger.error({error, dropped: this.dropped}, 'Cassandra CDC capture could not publish its closed record');
		}
		await this.sink.close();
	}

	private record(ts: number, out: CdcOutcome, lwt: boolean, keysOf: () => Array<TableKey>): void {
		let tables: Array<string> = [];
		let keys: KeyRecord['keys'];
		try {
			const raw = keysOf();
			if (raw.length === 0) return;
			tables = [...new Set(raw.map(({t}) => t))];
			if (this.stopped) {
				this.lose(++this.seq, tables);
				return;
			}
			keys = raw.map(({t, s, c}) => ({t, s, c: this.codec.encodeRecord(c)}));
		} catch (error) {
			Logger.warn({error, tables}, 'Cassandra CDC capture could not encode a key record');
			this.lose(++this.seq, tables);
			this.kick();
			return;
		}
		const seq = ++this.seq;
		const record: KeyRecord = {
			kind: 'k',
			producer: this.producer,
			seq,
			ts,
			out,
			...(lwt ? {lwt: true as const} : {}),
			keys,
		};
		const body = JSON.stringify(record);
		const bytes = Buffer.byteLength(body);
		recordsTotal.inc('kind="k"');
		bytesTotal.inc('', bytes);
		recordBytes.observe(bytes);
		if (this.queue.length >= this.limits.maxQueued) {
			const oldest = this.queue.shift();
			if (oldest !== undefined) this.lose(oldest.seq, oldest.tables);
		}
		this.queue.push({seq, body, tables});
		this.kick();
	}

	private lose(seq: number, tables: ReadonlyArray<string>): void {
		this.dropped++;
		for (const table of tables) droppedTotal.inc(`table="${table}"`);
		this.loss =
			this.loss === null
				? {from: seq, to: seq}
				: {from: Math.min(this.loss.from, seq), to: Math.max(this.loss.to, seq)};
	}

	private hasWork(): boolean {
		return this.queue.length > 0 || this.openDue || this.loss !== null || this.pumping !== null;
	}

	private kick(): void {
		if (this.pumping !== null || this.retryTimer !== null || this.stopped) return;
		this.pumping = new Promise<void>((resolve) => setImmediate(resolve))
			.then(() => this.pump())
			.finally(() => {
				this.pumping = null;
			});
	}

	private async pump(): Promise<void> {
		while (!this.stopped) {
			const delivered = new Set<QueuedKeyRecord>();
			const batch = this.nextBatch(delivered);
			if (batch.length === 0) return;
			const results = await Promise.allSettled(
				batch.map(({kind, body, msgID}) =>
					this.sink.publish(cdcSubject(this.process, kind), body, msgID, PUBLISH_TIMEOUT_MS),
				),
			);
			let failures = 0;
			for (const [index, result] of results.entries()) {
				const outgoing = batch[index];
				if (outgoing === undefined) continue;
				if (result.status === 'fulfilled') {
					outgoing.done();
				} else {
					failures++;
					publishFailuresTotal.inc(`kind="${outgoing.kind}"`);
				}
			}
			if (delivered.size > 0) this.queue = this.queue.filter((entry) => !delivered.has(entry));
			if (failures > 0) {
				this.retryLater(results.find((result) => result.status === 'rejected')?.reason);
				return;
			}
			if (this.failing) {
				this.failing = false;
				Logger.info({queued: this.queue.length}, 'Cassandra CDC capture publishes again');
			}
		}
	}

	private retryLater(error: unknown): void {
		if (!this.failing) {
			this.failing = true;
			Logger.warn({error, queued: this.queue.length}, 'Cassandra CDC capture cannot publish, queueing key records');
		}
		this.retryTimer = setTimeout(() => {
			this.retryTimer = null;
			this.kick();
		}, this.limits.retryMs);
		this.retryTimer.unref();
	}

	private nextBatch(delivered: Set<QueuedKeyRecord>): Array<Outgoing> {
		const batch: Array<Outgoing> = [];
		if (this.openDue) {
			batch.push(
				this.life({kind: 'life', producer: this.producer, state: 'open', pod: this.pod}, 'open', () => {
					this.openDue = false;
				}),
			);
		}
		const loss = this.loss;
		if (loss !== null) {
			batch.push(
				this.life(
					{kind: 'life', producer: this.producer, state: 'lossy', from_seq: loss.from, to_seq: loss.to},
					`lossy:${loss.from}-${loss.to}`,
					() => {
						if (this.loss?.from === loss.from && this.loss.to === loss.to) this.loss = null;
					},
				),
			);
		}
		if (this.aliveDue) {
			batch.push(
				this.life(
					{kind: 'life', producer: this.producer, state: 'alive', last_seq: this.seq},
					`alive:${this.alives + 1}`,
					() => {
						this.aliveDue = false;
						this.alives++;
					},
				),
			);
		}
		for (const entry of this.queue.slice(0, PUBLISH_WINDOW - batch.length)) {
			batch.push({
				kind: 'k',
				body: entry.body,
				msgID: `${this.producer}:${entry.seq}:k`,
				done: () => {
					delivered.add(entry);
				},
			});
		}
		return batch;
	}

	private life(record: CdcLifeRecord, id: string, done: () => void): Outgoing {
		return {kind: 'life', body: JSON.stringify(record), msgID: `${this.producer}:${id}`, done};
	}
}

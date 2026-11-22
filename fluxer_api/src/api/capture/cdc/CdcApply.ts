// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RowState} from '@app/api/capture/CaptureTables';
import {readRows, TooManyRows} from '@app/api/capture/CaptureTables';
import type {PendingKey} from '@app/api/capture/cdc/CdcPending';
import {type RowEncoder, rowExpiry, rowVersion, timestampText} from '@app/api/capture/cdc/CdcRows';
import {type CdcTable, cdcTableName} from '@app/api/capture/cdc/CdcSchema';
import {type ClickHouseHttp, quoteIdentifier} from '@app/api/capture/cdc/ClickHouseHttp';
import type {CaptureCodec, CaptureValue} from '@app/api/database/capture/CaptureCodec';
import type {CaptureScope} from '@app/api/database/capture/CaptureRecords';
import type cassandra from 'cassandra-driver';

const READS_IN_FLIGHT = 32;
const MAX_ROWS_PER_KEY = 2000;
const MAX_LIVE_KEYS = 100_000;
const RECHECK_DELAY_MS = 15 * 60_000;

export interface BatchBody {
	lines: Map<string, Array<string>>;
	recheck: Array<string>;
	keysRead: Map<string, number>;
	tombstones: Map<string, number>;
	ttlTables: Set<string>;
	oversized: Set<string>;
}

export function recheckKeyText(primaryKey: ReadonlyArray<string>, key: Readonly<Record<string, CaptureValue>>): string {
	return JSON.stringify(Object.fromEntries(primaryKey.filter((column) => column in key).map((c) => [c, key[c]])));
}

export function linesForKey(options: {
	encoder: RowEncoder;
	table: CdcTable;
	scope: CaptureScope;
	ts: number;
	key: Readonly<Record<string, unknown>>;
	rows: ReadonlyArray<RowState>;
	live: ReadonlyArray<Readonly<Record<string, unknown>>>;
	readStartMs: number;
}): {lines: Array<string>; tombstones: number} {
	const {encoder, table, rows, readStartMs} = options;
	const readStartUs = readStartMs * 1000;
	const lines = rows.map((row) =>
		encoder.rowLine(
			table.columns,
			row,
			rowVersion(options.ts, row, readStartUs),
			rowExpiry(table.layout, row, readStartMs),
		),
	);
	const tombstoneVersion = Math.max(options.ts, readStartUs);
	if (options.scope === 'row') {
		if (rows.length > 0) return {lines, tombstones: 0};
		return {lines: [encoder.tombstoneLine(table.keyColumns, options.key, tombstoneVersion)], tombstones: 1};
	}
	const identity = (key: Readonly<Record<string, unknown>>) =>
		JSON.stringify(table.keyColumns.map((column) => encoder.keyText(column, key[column.name])));
	const found = new Set(rows.map((row) => identity(row.key)));
	const vanished = options.live.filter((key) => !found.has(identity(key)));
	for (const key of vanished) lines.push(encoder.tombstoneLine(table.keyColumns, key, tombstoneVersion));
	return {lines, tombstones: vanished.length};
}

function increment(counts: Map<string, number>, table: string, amount = 1): void {
	counts.set(table, (counts.get(table) ?? 0) + amount);
}

export class BatchReader {
	private readonly client: cassandra.Client;
	private readonly clickhouse: ClickHouseHttp;
	private readonly encoder: RowEncoder;
	private readonly codec: CaptureCodec;
	private readonly consistencies: {quorum: number; serial: number};

	constructor(options: {
		client: cassandra.Client;
		clickhouse: ClickHouseHttp;
		encoder: RowEncoder;
		codec: CaptureCodec;
		consistencies: {quorum: number; serial: number};
	}) {
		this.client = options.client;
		this.clickhouse = options.clickhouse;
		this.encoder = options.encoder;
		this.codec = options.codec;
		this.consistencies = options.consistencies;
	}

	async read(
		keys: ReadonlyArray<PendingKey>,
		tables: ReadonlyMap<string, CdcTable>,
		nowMs: number,
	): Promise<BatchBody> {
		const body: BatchBody = {
			lines: new Map(),
			recheck: [],
			keysRead: new Map(),
			tombstones: new Map(),
			ttlTables: new Set(),
			oversized: new Set(),
		};
		const queue = keys.filter((key) => tables.has(key.table));
		const worker = async () => {
			for (let pending = queue.pop(); pending !== undefined; pending = queue.pop()) {
				const table = tables.get(pending.table);
				if (table !== undefined) await this.readKey(table, pending, body);
			}
		};
		await Promise.all(Array.from({length: READS_IN_FLIGHT}, worker));
		const dueAt = timestampText(nowMs + RECHECK_DELAY_MS) ?? '';
		for (const pending of keys) {
			const table = tables.get(pending.table);
			if (table === undefined) continue;
			const key = recheckKeyText(table.layout.primaryKey, pending.key);
			const lwt = pending.serial ? 1 : 0;
			if (pending.recheckDue !== null) {
				body.recheck.push(
					JSON.stringify({
						table: pending.table,
						scope: pending.scope,
						key,
						lwt,
						due_at: pending.recheckDue,
						_deleted: 1,
					}),
				);
			}
			if (pending.unknown) {
				body.recheck.push(
					JSON.stringify({table: pending.table, scope: pending.scope, key, lwt, due_at: dueAt, _deleted: 0}),
				);
			}
		}
		return body;
	}

	private async readKey(table: CdcTable, pending: PendingKey, body: BatchBody): Promise<void> {
		const key = this.codec.decodeRecord(pending.key);
		const readStartMs = Date.now();
		const consistency = pending.serial ? this.consistencies.serial : this.consistencies.quorum;
		const rows = await readRows(this.client, table.layout, key, consistency, MAX_ROWS_PER_KEY).catch(
			(error: unknown) => {
				if (error instanceof TooManyRows) return null;
				throw error;
			},
		);
		const live = pending.scope === 'row' || rows === null ? [] : await this.liveKeys(table, key);
		if (rows === null || live.length > MAX_LIVE_KEYS) {
			body.oversized.add(table.layout.name);
			return;
		}
		const {lines, tombstones} = linesForKey({
			encoder: this.encoder,
			table,
			scope: pending.scope,
			ts: pending.ts,
			key,
			rows,
			live,
			readStartMs,
		});
		const tableLines = body.lines.get(table.layout.name) ?? [];
		tableLines.push(...lines);
		body.lines.set(table.layout.name, tableLines);
		increment(body.keysRead, table.layout.name);
		if (tombstones > 0) increment(body.tombstones, table.layout.name, tombstones);
		if (rows.some((row) => Object.values(row.cells).some((cell) => cell.ttl !== null)) || table.layout.defaultTtl > 0) {
			body.ttlTables.add(table.layout.name);
		}
	}

	private async liveKeys(
		table: CdcTable,
		key: Readonly<Record<string, unknown>>,
	): Promise<Array<Record<string, unknown>>> {
		const bound = table.keyColumns.filter((column) => column.name in key);
		const where = bound.map((column, index) => `${quoteIdentifier(column.name)} = {k${index}:${column.chType}}`);
		const params = Object.fromEntries(
			bound.map((column, index) => [`k${index}`, this.encoder.keyText(column, key[column.name])]),
		);
		const selected = table.keyColumns.map((column) => quoteIdentifier(column.name)).join(', ');
		const rows = await this.clickhouse.select<Record<string, unknown>>(
			`SELECT ${selected} FROM ${cdcTableName(table.layout.name)} FINAL WHERE ${where.join(' AND ')} AND _deleted = 0 LIMIT ${MAX_LIVE_KEYS + 1}`,
			{params},
		);
		return rows.map((row) =>
			Object.fromEntries(
				table.keyColumns.map((column) => [column.name, this.encoder.fromClickHouse(column.cqlType, row[column.name])]),
			),
		);
	}
}

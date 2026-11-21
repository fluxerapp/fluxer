// SPDX-License-Identifier: AGPL-3.0-or-later

import {rowState, selectedColumns} from '@app/api/capture/CaptureTables';
import {delay, withBackoff} from '@app/api/capture/cdc/CdcBackoff';
import {type RowEncoder, rowExpiry, rowVersion} from '@app/api/capture/cdc/CdcRows';
import {type CdcTable, cdcTableName} from '@app/api/capture/cdc/CdcSchema';
import type {StateStore} from '@app/api/capture/cdc/CdcState';
import {type ClickHouseHttp, quoteIdentifier} from '@app/api/capture/cdc/ClickHouseHttp';
import type cassandra from 'cassandra-driver';

export const REPAIR_INTERVAL_MS = 7 * 86_400_000;
export const MARKED_ROWS_PER_SECOND = 2000;
export const ROUTINE_ROWS_PER_SECOND = 500;
const PAGE_SIZE = 500;
const OPTIMIZE_TIMEOUT_MS = 30 * 60_000;

export interface RepairRecord {
	scanStartMs: number;
	finishedMs: number;
	rows: number;
	tombstones: number;
}

export interface RepairChoice {
	table: string;
	marked: boolean;
}

export function nextRepair(
	tables: Iterable<string>,
	repairs: ReadonlyMap<string, RepairRecord>,
	due: ReadonlyMap<string, number>,
	nowMs: number,
): RepairChoice | null {
	let marked: {table: string; dueMs: number} | null = null;
	let routine: {table: string; finishedMs: number} | null = null;
	for (const table of tables) {
		const last = repairs.get(table);
		const dueMs = last === undefined ? 0 : (due.get(table) ?? -1);
		if (last === undefined || dueMs > last.scanStartMs) {
			if (marked === null || dueMs < marked.dueMs) marked = {table, dueMs};
		} else if (
			nowMs - last.finishedMs >= REPAIR_INTERVAL_MS &&
			(routine === null || last.finishedMs < routine.finishedMs)
		) {
			routine = {table, finishedMs: last.finishedMs};
		}
	}
	if (marked !== null) return {table: marked.table, marked: true};
	return routine === null ? null : {table: routine.table, marked: false};
}

export function vanishedTombstoneSql(table: CdcTable, scanStartUs: number): string {
	const keys = table.keyColumns.map((column) => quoteIdentifier(column.name)).join(', ');
	return [
		`INSERT INTO ${cdcTableName(table.layout.name)} (${keys}, _version, _deleted)`,
		`SELECT ${keys}, toUInt64(${scanStartUs}), 1 FROM ${cdcTableName(table.layout.name)} FINAL`,
		`WHERE _deleted = 0 AND _version < ${scanStartUs}`,
	].join(' ');
}

export class TableLocks {
	private readonly tails = new Map<string, Promise<void>>();

	async hold<T>(tables: Iterable<string>, run: () => Promise<T>): Promise<T> {
		const names = [...new Set(tables)];
		const earlier = names.map((name) => this.tails.get(name));
		const {promise, resolve} = Promise.withResolvers<void>();
		for (const name of names) this.tails.set(name, promise);
		await Promise.all(earlier);
		try {
			return await run();
		} finally {
			resolve();
			for (const name of names) if (this.tails.get(name) === promise) this.tails.delete(name);
		}
	}
}

export interface RepairDeps {
	client: cassandra.Client;
	clickhouse: ClickHouseHttp;
	encoder: RowEncoder;
	state: StateStore;
	locks: TableLocks;
	consistency: number;
	stopping: () => boolean;
	onError: (error: unknown) => void;
	onRows: (table: string, rows: number) => void;
}

export async function repairTable(deps: RepairDeps, table: CdcTable, rowsPerSecond: number): Promise<RepairRecord> {
	const scanStartMs = Date.now();
	const name = table.layout.name;
	const retry = <T>(run: () => Promise<T>) =>
		withBackoff(run, {stopping: deps.stopping, onError: (error) => deps.onError(error)});
	const cql = `SELECT ${selectedColumns(table.layout).join(', ')} FROM ${name}`;
	let pageState: string | undefined;
	let rows = 0;
	do {
		const page = await deps.locks.hold([name], async () => {
			const readStartMs = Date.now();
			const read = await retry(() =>
				deps.client.execute(cql, [], {
					prepare: true,
					fetchSize: PAGE_SIZE,
					consistency: deps.consistency,
					...(pageState === undefined ? {} : {pageState}),
				}),
			);
			const lines = read.rows.map((row) => {
				const state = rowState(table.layout, row);
				return deps.encoder.rowLine(
					table.columns,
					state,
					rowVersion(0, state, readStartMs * 1000),
					rowExpiry(table.layout, state, readStartMs),
				);
			});
			await retry(() => deps.clickhouse.insert(cdcTableName(name), lines));
			return {written: lines.length, next: read.pageState ?? undefined};
		});
		rows += page.written;
		deps.onRows(name, page.written);
		pageState = page.next;
		const aheadMs = (rows / rowsPerSecond) * 1000 - (Date.now() - scanStartMs);
		if (aheadMs > 0) await delay(aheadMs);
	} while (pageState !== undefined);
	const summary = await retry(() => deps.clickhouse.command(vanishedTombstoneSql(table, scanStartMs * 1000)));
	const record: RepairRecord = {scanStartMs, finishedMs: Date.now(), rows, tombstones: summary.writtenRows};
	await retry(() => deps.state.write(new Map([[`repair:${name}`, record]])));
	await retry(() =>
		deps.clickhouse.command(`OPTIMIZE TABLE ${cdcTableName(name)} FINAL CLEANUP`, {timeoutMs: OPTIMIZE_TIMEOUT_MS}),
	);
	return record;
}

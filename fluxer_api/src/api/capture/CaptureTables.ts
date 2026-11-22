// SPDX-License-Identifier: AGPL-3.0-or-later

import type cassandra from 'cassandra-driver';

export interface TableLayout {
	name: string;
	primaryKey: ReadonlyArray<string>;
	partitionKey: ReadonlyArray<string>;
	regular: ReadonlyArray<string>;
	multiCell: ReadonlySet<string>;
	defaultTtl: number;
}

interface CellState {
	v: unknown;
	wt: number;
	ttl: number | null;
}

export interface RowState {
	key: Record<string, unknown>;
	cells: Record<string, CellState>;
}

const MAX_IMAGE_ROWS = 5000;

interface SchemaColumn {
	table_name: string;
	column_name: string;
	kind: string;
	position: number;
	type: string;
}

export class UnreadableKey extends Error {
	override readonly name = 'UnreadableKey';
}

export class TooManyRows extends UnreadableKey {}

export async function loadTableLayouts(client: cassandra.Client, keyspace: string): Promise<Map<string, TableLayout>> {
	const columns = await client.execute(
		'SELECT table_name, column_name, kind, position, type FROM system_schema.columns WHERE keyspace_name = ?',
		[keyspace],
		{prepare: true},
	);
	const tables = await client.execute(
		'SELECT table_name, default_time_to_live FROM system_schema.tables WHERE keyspace_name = ?',
		[keyspace],
		{prepare: true},
	);
	const byTable = new Map<string, Array<SchemaColumn>>();
	for (const row of columns.rows as unknown as Array<SchemaColumn>) {
		byTable.set(row.table_name, [...(byTable.get(row.table_name) ?? []), row]);
	}
	const layouts = new Map<string, TableLayout>();
	for (const table of tables.rows) {
		const name = table.get('table_name') as string;
		const all = byTable.get(name) ?? [];
		const ordered = (kind: string) =>
			all
				.filter((column) => column.kind === kind)
				.sort((a, b) => a.position - b.position)
				.map((column) => column.column_name);
		const regular = all.filter((column) => column.kind === 'regular');
		layouts.set(name, {
			name,
			primaryKey: [...ordered('partition_key'), ...ordered('clustering')],
			partitionKey: ordered('partition_key'),
			regular: regular.map((column) => column.column_name).sort(),
			multiCell: new Set(
				regular.filter((column) => /^(?:set|map|list)</.test(column.type)).map((column) => column.column_name),
			),
			defaultTtl: Number(table.get('default_time_to_live') ?? 0),
		});
	}
	return layouts;
}

function keyColumns(layout: TableLayout, key: Readonly<Record<string, unknown>>): Array<string> {
	const columns = layout.primaryKey.slice(0, Object.keys(key).length);
	const coversPartition = layout.partitionKey.every((column) => columns.includes(column));
	if (!coversPartition || columns.some((column) => !(column in key))) {
		throw new UnreadableKey(`${layout.name} cannot be read by ${Object.keys(key).join(', ')}`);
	}
	return columns;
}

export function selectedColumns(layout: TableLayout): Array<string> {
	return [
		...layout.primaryKey,
		...layout.regular.flatMap((column) => [column, `WRITETIME(${column})`, `TTL(${column})`]),
	];
}

function selectCql(layout: TableLayout, columns: ReadonlyArray<string>): string {
	const where = columns.map((column) => `${column} = :${column}`).join(' AND ');
	return `SELECT ${selectedColumns(layout).join(', ')} FROM ${layout.name} WHERE ${where}`;
}

function firstOrMax(value: unknown, pick: (values: Array<number>) => number): number | null {
	if (value === null || value === undefined) return null;
	if (Array.isArray(value)) {
		const present = value.filter((entry) => entry !== null && entry !== undefined).map(Number);
		return present.length === 0 ? null : pick(present);
	}
	return Number(value);
}

export function rowState(layout: TableLayout, row: cassandra.types.Row): RowState {
	const key = Object.fromEntries(layout.primaryKey.map((column) => [column, row.get(column)]));
	const cells: Record<string, CellState> = {};
	const selected = row.values();
	for (const [index, column] of layout.regular.entries()) {
		const offset = layout.primaryKey.length + index * 3;
		const value = selected[offset];
		const wt = firstOrMax(selected[offset + 1], (values) => Math.max(...values));
		if (value === null || value === undefined || wt === null) continue;
		cells[column] = {v: value, wt, ttl: firstOrMax(selected[offset + 2], (values) => Math.min(...values))};
	}
	return {key, cells};
}

export async function readRows(
	client: cassandra.Client,
	layout: TableLayout,
	key: Readonly<Record<string, unknown>>,
	consistency?: number,
	maxRows = MAX_IMAGE_ROWS,
): Promise<Array<RowState>> {
	const columns = keyColumns(layout, key);
	const result = await client.execute(
		selectCql(layout, columns),
		Object.fromEntries(columns.map((column) => [column, key[column]])),
		{prepare: true, fetchSize: 1000, ...(consistency === undefined ? {} : {consistency})},
	);
	const rows: Array<RowState> = [];
	for await (const row of result) {
		if (rows.length === maxRows) {
			throw new TooManyRows(`${layout.name} has more than ${maxRows} rows under one key`);
		}
		rows.push(rowState(layout, row));
	}
	return rows;
}

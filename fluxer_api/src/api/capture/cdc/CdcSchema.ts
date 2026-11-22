// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TableLayout} from '@app/api/capture/CaptureTables';
import {type CdcColumn, clickHouseType} from '@app/api/capture/cdc/CdcRows';
import {quoteIdentifier} from '@app/api/capture/cdc/ClickHouseHttp';
import {CDC_DROPPED_COLUMNS} from '@app/api/database/capture/CdcRecords';
import type cassandra from 'cassandra-driver';

export const CDC_DATABASE = 'fluxer_cdc';
const INTERNAL_COLUMNS: ReadonlyArray<string> = ['_version', '_deleted', '_expires_at', '_reread_at'];

export interface CdcTable {
	layout: TableLayout;
	columns: ReadonlyArray<CdcColumn>;
	keyColumns: ReadonlyArray<CdcColumn>;
}

export interface SchemaPlan {
	statements: Array<string>;
	created: Array<string>;
	widened: Array<string>;
	mismatched: Array<string>;
}

export function cdcTableName(table: string): string {
	return `${CDC_DATABASE}.${quoteIdentifier(table)}`;
}

export async function loadColumnTypes(
	client: cassandra.Client,
	keyspace: string,
): Promise<Map<string, Map<string, string>>> {
	const result = await client.execute(
		'SELECT table_name, column_name, type FROM system_schema.columns WHERE keyspace_name = ?',
		[keyspace],
		{prepare: true, fetchSize: 1000},
	);
	const types = new Map<string, Map<string, string>>();
	for await (const row of result) {
		const table = row.get('table_name') as string;
		const columns = types.get(table) ?? new Map<string, string>();
		columns.set(row.get('column_name') as string, row.get('type') as string);
		types.set(table, columns);
	}
	return types;
}

export function cdcTable(layout: TableLayout, types: ReadonlyMap<string, string>): CdcTable {
	const dropped = new Set(CDC_DROPPED_COLUMNS[layout.name] ?? []);
	const all = [...layout.primaryKey, ...layout.regular];
	if (all.length !== types.size || all.some((column) => !types.has(column))) {
		throw new Error(`the schema of ${layout.name} changed while it was read`);
	}
	const column = (name: string): CdcColumn => {
		const cqlType = types.get(name) ?? '';
		return {name, cqlType, chType: clickHouseType(cqlType)};
	};
	const regular = layout.regular.filter((name) => !dropped.has(name));
	return {
		layout: {...layout, regular, multiCell: new Set([...layout.multiCell].filter((name) => !dropped.has(name)))},
		columns: [...layout.primaryKey, ...regular].sort().map(column),
		keyColumns: layout.primaryKey.map(column),
	};
}

const TABLE_SETTINGS =
	'non_replicated_deduplication_window = 1000, allow_nullable_key = 1, allow_experimental_replacing_merge_with_cleanup = 1';

export function tableDdl(table: CdcTable): string {
	const columns = table.columns.map((column) => `${quoteIdentifier(column.name)} ${column.chType}`);
	return [
		`CREATE TABLE IF NOT EXISTS ${cdcTableName(table.layout.name)} (`,
		[
			...columns,
			'_version UInt64',
			'_deleted UInt8',
			"_expires_at DateTime64(3, 'UTC') DEFAULT 0",
			"_reread_at DateTime64(3, 'UTC') DEFAULT 0",
		].join(', '),
		') ENGINE = ReplacingMergeTree(_version, _deleted)',
		`ORDER BY (${table.keyColumns.map((column) => quoteIdentifier(column.name)).join(', ')})`,
		`SETTINGS ${TABLE_SETTINGS}`,
	].join(' ');
}

export function viewDdl(database: string, definer: string, table: CdcTable): string {
	const columns = table.columns.map((column) => quoteIdentifier(column.name)).join(', ');
	return [
		`CREATE OR REPLACE VIEW ${quoteIdentifier(database)}.${quoteIdentifier(table.layout.name)}`,
		`DEFINER = ${quoteIdentifier(definer)} SQL SECURITY DEFINER AS`,
		`SELECT ${columns} FROM ${cdcTableName(table.layout.name)} FINAL`,
		'WHERE _deleted = 0 AND (_expires_at = 0 OR _expires_at > now64(3))',
	].join(' ');
}

export function bookkeepingDdl(): Array<string> {
	return [
		`CREATE DATABASE IF NOT EXISTS ${CDC_DATABASE}`,
		[
			`CREATE TABLE IF NOT EXISTS ${CDC_DATABASE}._state`,
			"(name String, value String, updated_at DateTime64(6, 'UTC'), _deleted UInt8 DEFAULT 0)",
			'ENGINE = ReplacingMergeTree(updated_at, _deleted) ORDER BY name',
		].join(' '),
		[
			`CREATE TABLE IF NOT EXISTS ${CDC_DATABASE}._recheck`,
			"(table String, scope String, key String, lwt UInt8, due_at DateTime64(3, 'UTC'), _deleted UInt8 DEFAULT 0)",
			'ENGINE = ReplacingMergeTree(due_at, _deleted) ORDER BY (table, scope, key)',
		].join(' '),
	];
}

export function schemaPlan(options: {
	desired: ReadonlyMap<string, CdcTable>;
	existing: ReadonlyMap<string, ReadonlyMap<string, string>>;
	views: ReadonlyMap<string, ReadonlyArray<string>>;
	viewDatabase: string;
	definer: string;
	cassandraTables: ReadonlySet<string>;
	today: string;
}): SchemaPlan {
	const plan: SchemaPlan = {statements: [], created: [], widened: [], mismatched: []};
	for (const [name, table] of options.desired) {
		const current = options.existing.get(name);
		if (current === undefined) {
			plan.statements.push(tableDdl(table));
			plan.created.push(name);
		} else {
			const wanted = new Map(table.columns.map((column) => [column.name, column.chType]));
			if ([...wanted].some(([column, type]) => current.has(column) && current.get(column) !== type)) {
				plan.mismatched.push(name);
				continue;
			}
			const added = [...wanted].filter(([column]) => !current.has(column));
			for (const [column, type] of added) {
				plan.statements.push(`ALTER TABLE ${cdcTableName(name)} ADD COLUMN ${quoteIdentifier(column)} ${type}`);
			}
			if (added.length > 0) plan.widened.push(name);
			for (const column of current.keys()) {
				if (!wanted.has(column) && !INTERNAL_COLUMNS.includes(column)) {
					plan.statements.push(`ALTER TABLE ${cdcTableName(name)} DROP COLUMN ${quoteIdentifier(column)}`);
				}
			}
		}
		const view = options.views.get(name);
		const columns = table.columns.map((column) => column.name);
		if (view === undefined || view.join(',') !== columns.join(',')) {
			plan.statements.push(viewDdl(options.viewDatabase, options.definer, table));
		}
	}
	for (const name of options.existing.keys()) {
		if (options.desired.has(name) || options.cassandraTables.has(name)) continue;
		plan.statements.push(`DROP VIEW IF EXISTS ${quoteIdentifier(options.viewDatabase)}.${quoteIdentifier(name)}`);
		plan.statements.push(`RENAME TABLE ${cdcTableName(name)} TO ${cdcTableName(`${name}__dropped_${options.today}`)}`);
	}
	return plan;
}

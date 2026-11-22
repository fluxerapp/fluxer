// SPDX-License-Identifier: AGPL-3.0-or-later

import type {KvQueryMeta} from '@app/api/database/CassandraTypes';
import type {CaptureScope} from '@app/api/database/capture/CaptureRecords';

export interface RawKey {
	s: CaptureScope;
	c: Record<string, unknown>;
}

export interface RawLwtRow {
	t: string;
	c: Record<string, unknown>;
	insert: boolean;
}

type Params = Readonly<Record<string, unknown>>;

function rowValues(columns: ReadonlyArray<string>, params: Params, names?: ReadonlyArray<string>): RawKey['c'] | null {
	const values: RawKey['c'] = {};
	for (const [index, column] of columns.entries()) {
		const value = params[names?.[index] ?? column];
		if (value === undefined || value === null) return null;
		values[column] = value;
	}
	return values;
}

function deleteScope(covered: number, primaryKeyLength: number, partitionKeyLength: number): CaptureScope {
	if (covered === primaryKeyLength) return 'row';
	return covered === partitionKeyLength ? 'partition' : 'prefix';
}

function deleteKeys(meta: KvQueryMeta, params: Params): Array<RawKey> | undefined {
	const {primaryKey, partitionKey} = meta.table;
	if (!meta.where?.length) {
		const row = rowValues(primaryKey, params);
		return row ? [{s: 'row', c: row}] : undefined;
	}
	let combos: Array<RawKey['c']> = [{}];
	let covered = 0;
	for (const column of primaryKey) {
		const clause = meta.where.find((where) => where.col === column && (where.kind === 'eq' || where.kind === 'in'));
		if (!clause) break;
		const value = params[clause.param];
		const choices = clause.kind === 'in' ? value : [value];
		if (!Array.isArray(choices) || choices.some((choice) => choice === undefined || choice === null)) return undefined;
		combos = combos.flatMap((combo) => choices.map((choice) => ({...combo, [column]: choice})));
		covered++;
	}
	if (covered === 0) return undefined;
	return combos.map((c) => ({s: deleteScope(covered, primaryKey.length, partitionKey.length), c}));
}

export function writeKeys(meta: KvQueryMeta | undefined, type: string, params: Params): Array<RawKey> | undefined {
	if (!meta) return undefined;
	if (type === 'DELETE') return deleteKeys(meta, params);
	if (type !== 'INSERT' && type !== 'UPDATE') return undefined;
	const row = rowValues(meta.pkColumns ?? meta.table.primaryKey, params);
	return row ? [{s: 'row', c: row}] : undefined;
}

export function lwtRows(meta: KvQueryMeta | undefined, params: Params): Array<RawLwtRow> | undefined {
	if (!meta) return undefined;
	const t = meta.table.name;
	if (meta.batchEntries) {
		const rows: Array<RawLwtRow> = [];
		for (const entry of meta.batchEntries) {
			const c = rowValues(
				entry.pk.map(({col}) => col),
				params,
				entry.pk.map(({param}) => param),
			);
			if (!c) return undefined;
			rows.push({t, c, insert: entry.action === 'insert'});
		}
		return rows;
	}
	const c = rowValues(meta.pkColumns ?? meta.table.primaryKey, params);
	return c ? [{t, c, insert: meta.ifNotExists === true}] : undefined;
}

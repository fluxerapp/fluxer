// SPDX-License-Identifier: AGPL-3.0-or-later

import {upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {ColumnName, DbOp, PatchObject, RowValue, Table} from '@app/api/database/CassandraTypes';
import {Db, nextVersion} from '@app/api/database/CassandraTypes';

export async function executeVersionedUpdate<
	Row extends {
		version?: number | null;
	},
	PK extends ColumnName<Row>,
	Patch extends PatchObject = PatchObject,
>(
	fetchCurrent: () => Promise<Row | null>,
	buildPatch: (current: Row | null) => {
		pk: Record<string, unknown>;
		patch: Patch;
	},
	table: Table<Row, PK>,
	opts?: {
		initialData?: Row | null;
	},
): Promise<{
	finalVersion: number | null;
	previousData: Row | null;
}> {
	const current = opts?.initialData !== undefined ? opts.initialData : await fetchCurrent();
	const currentVersion = current?.version ?? null;
	const newVersion = nextVersion(currentVersion);
	const {pk, patch} = buildPatch(current);
	await upsertOne(
		table.patchByPk(
			pk as Pick<Row, PK>,
			{...patch, version: Db.set(newVersion)} as Partial<{
				[K in Exclude<ColumnName<Row>, PK>]: DbOp<RowValue<Row, K>>;
			}>,
		),
	);
	return {finalVersion: newVersion, previousData: current};
}

export function applyPatchToRow<Row extends object>(current: Partial<Row> | null, patch: PatchObject): Partial<Row> {
	const next = {...(current ?? {})} as Record<string, unknown>;
	for (const [column, op] of Object.entries(patch)) {
		if (op.kind === 'clear') {
			next[column] = null;
			continue;
		}
		next[column] = op.value;
	}
	return next as Partial<Row>;
}

function valuesEqual(a: unknown, b: unknown): boolean {
	if (a === b) return true;
	if (a == null || b == null) return false;
	if (a instanceof Date && b instanceof Date) {
		return a.getTime() === b.getTime();
	}
	if (a instanceof Set && b instanceof Set) {
		if (a.size !== b.size) return false;
		for (const item of a) {
			if (!b.has(item)) return false;
		}
		return true;
	}
	if (a instanceof Map && b instanceof Map) {
		if (a.size !== b.size) return false;
		for (const [key, val] of a) {
			if (!b.has(key) || !valuesEqual(val, b.get(key))) return false;
		}
		return true;
	}
	if (Buffer.isBuffer(a) && Buffer.isBuffer(b)) {
		return a.equals(b);
	}
	if (typeof a === 'object' && typeof b === 'object') {
		const keysA = Object.keys(a as Record<string, unknown>);
		const keysB = Object.keys(b as Record<string, unknown>);
		if (keysA.length !== keysB.length) return false;
		for (const key of keysA) {
			if (!keysB.includes(key)) return false;
			if (!valuesEqual((a as Record<string, unknown>)[key], (b as Record<string, unknown>)[key])) {
				return false;
			}
		}
		return true;
	}
	return a === b;
}

export function buildPatchFromData<Row extends object>(
	newData: Partial<Row>,
	oldData: Partial<Row> | null,
	columns: ReadonlyArray<keyof Row>,
	pkColumns: ReadonlyArray<keyof Row>,
): Record<string, DbOp<unknown>> {
	const patch: Record<string, DbOp<unknown>> = {};
	for (const col of columns) {
		if (pkColumns.includes(col)) continue;
		if (col === 'version') continue;
		const colName = col as string;
		const newVal = (newData as Record<string, unknown>)[colName];
		if (newVal === undefined) continue;
		const oldVal = oldData ? (oldData as Record<string, unknown>)[colName] : undefined;
		if (valuesEqual(newVal, oldVal)) continue;
		if (newVal === null) {
			if (oldData !== null && oldVal !== null && oldVal !== undefined) {
				patch[colName] = Db.clear();
			}
		} else {
			patch[colName] = Db.set(newVal);
		}
	}
	return patch;
}

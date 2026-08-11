// SPDX-License-Identifier: AGPL-3.0-or-later

import {upsertOne} from '../../database/CassandraQueryExecution';
import type {ColumnName, DbOp, PatchObject, RowValue, Table} from '../../database/CassandraTypes';
import {Db} from '../../database/CassandraTypes';
import {buildPatchFromData} from '../../database/CassandraVersionedUpdate';

export function restoreReferenceOrder<Row extends {provider_id: string}>(
	refs: ReadonlyArray<{provider_id: string}>,
	rows: ReadonlyArray<Row>,
): Array<Row> {
	const byId = new Map(rows.map((row) => [row.provider_id, row]));
	return refs.flatMap((ref) => {
		const row = byId.get(ref.provider_id);
		return row ? [row] : [];
	});
}

export const BILLING_REVERSE_CHRONOLOGICAL_ORDER = [
	{col: 'stripe_created_at', direction: 'DESC'},
	{col: 'provider_id', direction: 'ASC'},
] as const;

function deepEqual(a: unknown, b: unknown): boolean {
	if (a === b) return true;
	if (a === null || a === undefined || b === null || b === undefined) {
		return a === b;
	}
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
			if (!b.has(key) || !deepEqual(val, b.get(key))) return false;
		}
		return true;
	}
	if (Buffer.isBuffer(a) && Buffer.isBuffer(b)) {
		return a.equals(b);
	}
	if (Array.isArray(a) && Array.isArray(b)) {
		if (a.length !== b.length) return false;
		for (let i = 0; i < a.length; i++) {
			if (!deepEqual(a[i], b[i])) return false;
		}
		return true;
	}
	if (typeof a === 'object' && typeof b === 'object') {
		const ao = a as Record<string, unknown>;
		const bo = b as Record<string, unknown>;
		const keysA = Object.keys(ao);
		const keysB = Object.keys(bo);
		if (keysA.length !== keysB.length) return false;
		for (const key of keysA) {
			if (!Object.hasOwn(bo, key)) return false;
			if (!deepEqual(ao[key], bo[key])) return false;
		}
		return true;
	}
	return false;
}

export function rowsEquivalent<T extends object>(a: T, b: T, ignoreFields: ReadonlyArray<keyof T>): boolean {
	const ignore = new Set<keyof T>(ignoreFields);
	const keys = new Set<keyof T>([...(Object.keys(a) as Array<keyof T>), ...(Object.keys(b) as Array<keyof T>)]);
	for (const key of keys) {
		if (ignore.has(key)) continue;
		if (!deepEqual(a[key], b[key])) return false;
	}
	return true;
}

export function buildPatchFromRow<Row extends object, PK extends keyof Row>(
	incoming: Row,
	current: Row | null,
	columns: ReadonlyArray<keyof Row>,
	pkColumns: ReadonlyArray<PK>,
): Record<string, DbOp<unknown>> {
	return buildPatchFromData<Row>(
		incoming as Partial<Row>,
		current as Partial<Row> | null,
		columns,
		pkColumns as ReadonlyArray<keyof Row>,
	);
}

export function isExistingNewer(
	existing: {
		stripe_updated_at?: Date | null;
	} | null,
	incoming: {
		stripe_updated_at?: Date | null;
	},
): boolean {
	if (!existing) return false;
	const e = existing.stripe_updated_at ?? null;
	const i = incoming.stripe_updated_at ?? null;
	if (e === null || i === null) return false;
	return e.getTime() > i.getTime();
}

export async function executeBillingVersionedUpdate<
	Row extends {
		version: bigint | null;
	},
	PK extends Exclude<ColumnName<Row>, 'version'>,
>(
	fetchCurrent: () => Promise<Row | null>,
	buildPatch: (current: Row | null) => {
		pk: Record<string, unknown>;
		patch: PatchObject;
	},
	table: Table<Row, PK>,
	opts?: {
		initialData?: Row | null;
	},
): Promise<{
	finalVersion: bigint | null;
	previousData: Row | null;
}> {
	const current = opts?.initialData !== undefined ? opts.initialData : await fetchCurrent();
	const currentVersion: bigint | null = current?.version ?? null;
	const newVersion: bigint = (currentVersion ?? 0n) + 1n;
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

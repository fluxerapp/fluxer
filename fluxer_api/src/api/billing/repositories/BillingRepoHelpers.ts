// SPDX-License-Identifier: AGPL-3.0-or-later

import {fetchMany, upsertOne} from '@app/api/database/CassandraQueryExecution';
import type {CassandraParams, ColumnName, PreparedQuery, Table} from '@app/api/database/CassandraTypes';
import {buildPatchFromData, executeVersionedUpdate, valuesEqual} from '@app/api/database/CassandraVersionedUpdate';

function rowsEquivalent<T extends object>(a: T, b: T, ignoreFields: ReadonlyArray<keyof T>): boolean {
	const ignore = new Set<keyof T>(ignoreFields);
	const keys = new Set<keyof T>([...(Object.keys(a) as Array<keyof T>), ...(Object.keys(b) as Array<keyof T>)]);
	for (const key of keys) {
		if (ignore.has(key)) continue;
		if (!valuesEqual(a[key], b[key])) return false;
	}
	return true;
}

function isExistingNewer(
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

interface MirrorRow {
	provider_id: string;
	stripe_updated_at?: Date | null;
	mirrored_at?: Date | null;
}
type MirrorSideRow = readonly [table: {upsertAll(row: object): PreparedQuery}, row: object | null];

export async function upsertMirrorRow<Row extends MirrorRow, PK extends ColumnName<Row>>(
	table: Table<Row, PK>,
	existing: Row | null,
	primary: Row,
	sideRows: ReadonlyArray<MirrorSideRow> = [],
): Promise<{changed: boolean; row: Row}> {
	if (isExistingNewer(existing, primary)) return {changed: false, row: existing!};
	await upsertOne(table.upsertAll(primary));
	for (const [sideTable, row] of sideRows) {
		if (row) await upsertOne(sideTable.upsertAll(row));
	}
	return {changed: true, row: primary};
}

export async function upsertVersionedMirrorRow<
	Row extends MirrorRow & {version: bigint | null},
	PK extends ColumnName<Row>,
>(
	table: Table<Row, PK>,
	columns: ReadonlyArray<keyof Row>,
	existing: Row | null,
	primary: Row,
): Promise<{changed: boolean; row: Row}> {
	if (isExistingNewer(existing, primary)) return {changed: false, row: existing!};
	if (existing && rowsEquivalent(existing, primary, ['mirrored_at', 'version'])) return {changed: false, row: existing};
	const result = await executeVersionedUpdate<Row, PK>(
		() => Promise.resolve(existing),
		(current) => ({
			pk: {provider_id: primary.provider_id},
			patch: buildPatchFromData(primary, current, columns, ['provider_id']),
		}),
		table,
		{initialData: existing, bumpVersion: (current) => ((current ?? 0n) + 1n) as NonNullable<Row['version']>},
	);
	return {changed: true, row: {...primary, version: result.finalVersion}};
}

export async function hydrateByRefs<Row>(
	refsQuery: string,
	params: CassandraParams,
	byIdsQuery: string,
): Promise<Array<Row>> {
	const refs = await fetchMany<{provider_id: string}>(refsQuery, params);
	if (refs.length === 0) return [];
	return fetchMany<Row>(byIdsQuery, {provider_ids: refs.map((row) => row.provider_id)});
}

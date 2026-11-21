// SPDX-License-Identifier: AGPL-3.0-or-later

export interface TableLayout {
	name: string;
	primaryKey: ReadonlyArray<string>;
	partitionKey: ReadonlyArray<string>;
	regular: ReadonlyArray<string>;
	multiCell: ReadonlySet<string>;
	defaultTtl: number;
}

export interface CellState {
	v: unknown;
	wt: number;
	ttl: number | null;
}

export interface RowState {
	key: Record<string, unknown>;
	cells: Record<string, CellState>;
}

export interface SettleWrite {
	cql: string;
	params: Record<string, unknown>;
	timestamp: number;
}

interface SettleOptions {
	horizon: number;
	elapsedSeconds: number;
	insert: boolean;
	canonical: (value: unknown) => string;
}

const TTL_PARAM = 'settle_ttl';

function rowsByKey(layout: TableLayout, rows: ReadonlyArray<RowState>, canonical: SettleOptions['canonical']) {
	return new Map(rows.map((row) => [canonical(layout.primaryKey.map((column) => row.key[column])), row]));
}

function unionKeys<T>(left: Map<string, T>, right: Map<string, T>): Set<string> {
	return new Set([...left.keys(), ...right.keys()]);
}

function whereKey(layout: TableLayout): string {
	return layout.primaryKey.map((column) => `${column} = :${column}`).join(' AND ');
}

function sameCell(left: CellState, right: CellState, canonical: SettleOptions['canonical']): boolean {
	return (
		left.wt === right.wt && (left.ttl === null) === (right.ttl === null) && canonical(left.v) === canonical(right.v)
	);
}

function hasCellAtOrAbove(row: RowState | undefined, horizon: number): boolean {
	return row !== undefined && Object.values(row.cells).some((cell) => cell.wt >= horizon);
}

function belowHorizon(cell: CellState | undefined, horizon: number): cell is CellState {
	return cell !== undefined && cell.wt < horizon;
}

function touchesHorizon(left: CellState | undefined, right: CellState | undefined, horizon: number): boolean {
	return (left !== undefined && left.wt >= horizon) || (right !== undefined && right.wt >= horizon);
}

export function settleWrites(
	layout: TableLayout,
	db1: ReadonlyArray<RowState>,
	lim1: ReadonlyArray<RowState>,
	options: SettleOptions,
): Array<SettleWrite> {
	const {horizon, elapsedSeconds, insert, canonical} = options;
	const source = rowsByKey(layout, db1, canonical);
	const target = rowsByKey(layout, lim1, canonical);
	const where = whereKey(layout);
	const keyColumns = layout.primaryKey.join(', ');
	const keyParams = layout.primaryKey.map((column) => `:${column}`).join(', ');
	const writes: Array<SettleWrite> = [];
	for (const id of unionKeys(source, target)) {
		const from = source.get(id);
		const to = target.get(id);
		const key = (from ?? to)?.key ?? {};
		if (!from) {
			writes.push({cql: `DELETE FROM ${layout.name} WHERE ${where};`, params: key, timestamp: horizon - 1});
			continue;
		}
		const cells = Object.values(from.cells);
		if (cells.length === 0 && !to) {
			writes.push({
				cql: `INSERT INTO ${layout.name} (${keyColumns}) VALUES (${keyParams});`,
				params: key,
				timestamp: horizon - 1,
			});
		}
		const oldest = Math.min(...cells.map((cell) => cell.wt));
		if (insert && cells.length > 0 && oldest < horizon) {
			writes.push({
				cql: `INSERT INTO ${layout.name} (${keyColumns}) VALUES (${keyParams});`,
				params: key,
				timestamp: oldest,
			});
		}
		for (const column of layout.regular) {
			const wanted = from.cells[column];
			const present = to?.cells[column];
			if (touchesHorizon(wanted, present, horizon)) continue;
			if (belowHorizon(wanted, horizon)) {
				if (present && sameCell(wanted, present, canonical)) continue;
				const ttl = wanted.ttl === null ? 0 : Math.max(1, wanted.ttl - elapsedSeconds);
				writes.push({
					cql: `UPDATE ${layout.name} USING TTL :${TTL_PARAM} SET ${column} = :${column} WHERE ${where};`,
					params: {...key, [column]: wanted.v, [TTL_PARAM]: ttl},
					timestamp: wanted.wt,
				});
			} else if (belowHorizon(present, horizon)) {
				writes.push({cql: `DELETE ${column} FROM ${layout.name} WHERE ${where};`, params: key, timestamp: present.wt});
			}
		}
	}
	return writes;
}

export function differencesBelowHorizon(
	layout: TableLayout,
	db1: ReadonlyArray<RowState>,
	lim1: ReadonlyArray<RowState>,
	horizon: number,
	canonical: SettleOptions['canonical'],
): Array<string> {
	const source = rowsByKey(layout, db1, canonical);
	const target = rowsByKey(layout, lim1, canonical);
	const differences: Array<string> = [];
	for (const id of unionKeys(source, target)) {
		const from = source.get(id);
		const to = target.get(id);
		const settledRow = !hasCellAtOrAbove(from, horizon) && !hasCellAtOrAbove(to, horizon);
		if (settledRow && (from === undefined) !== (to === undefined)) {
			differences.push(`${layout.name} ${id}: row ${from ? 'missing' : 'unexpected'}`);
			continue;
		}
		for (const column of layout.regular) {
			const wanted = from?.cells[column];
			const present = to?.cells[column];
			if (touchesHorizon(wanted, present, horizon) || (wanted === undefined && present === undefined)) continue;
			if (wanted === undefined || present === undefined || !sameCell(wanted, present, canonical)) {
				differences.push(`${layout.name} ${id}: ${column}`);
			}
		}
	}
	return differences;
}

// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type CellState,
	differencesBelowHorizon,
	type RowState,
	settleWrites,
	type TableLayout,
} from '@app/api/capture/SettleRules';
import {describe, expect, it} from 'vitest';

const layout: TableLayout = {
	name: 'codes',
	primaryKey: ['code', 'n'],
	partitionKey: ['code'],
	regular: ['note', 'roles'],
	multiCell: new Set(['roles']),
	defaultTtl: 0,
};

const H = 1_000_000;
function plain(_key: string, entry: unknown): unknown {
	if (entry instanceof Set) return [...entry];
	return typeof entry === 'bigint' ? `${entry}` : entry;
}

const canonical = (value: unknown) => JSON.stringify(value, plain);

function row(n: number, cells: Record<string, CellState> = {}): RowState {
	return {key: {code: 'c', n}, cells};
}

function cell(v: unknown, wt: number, ttl: number | null = null): CellState {
	return {v, wt, ttl};
}

function settle(db1: Array<RowState>, lim1: Array<RowState>, insert = false, elapsedSeconds = 0) {
	return settleWrites(layout, db1, lim1, {horizon: H, elapsedSeconds, insert, canonical});
}

const where = 'WHERE code = :code AND n = :n;';

describe('settle writes', () => {
	it('copies a cell lim1 lacks or holds differently, at db1 timestamp and remaining ttl', () => {
		expect(settle([row(1, {note: cell('a', 10, 100)})], [], false, 30)).toEqual([
			{
				cql: `UPDATE codes USING TTL :settle_ttl SET note = :note ${where}`,
				params: {code: 'c', n: 1, note: 'a', settle_ttl: 70},
				timestamp: 10,
			},
		]);
		expect(settle([row(1, {note: cell('a', 10, 5)})], [row(1, {note: cell('b', 9)})], false, 30)).toEqual([
			{
				cql: `UPDATE codes USING TTL :settle_ttl SET note = :note ${where}`,
				params: {code: 'c', n: 1, note: 'a', settle_ttl: 1},
				timestamp: 10,
			},
		]);
		expect(settle([row(1, {note: cell('a', 10)})], [row(1, {note: cell('a', 9)})])[0]?.params.settle_ttl).toBe(0);
	});

	it('writes nothing for a cell that already matches', () => {
		expect(settle([row(1, {note: cell('a', 10)})], [row(1, {note: cell('a', 10)})])).toEqual([]);
	});

	it('deletes a cell db1 lacks at the timestamp of the lim1 cell', () => {
		expect(
			settle(
				[row(1, {roles: cell(new Set(['x']), 5)})],
				[row(1, {note: cell('a', 7), roles: cell(new Set(['x']), 5)})],
			),
		).toEqual([{cql: `DELETE note FROM codes ${where}`, params: {code: 'c', n: 1}, timestamp: 7}]);
	});

	it('copies a whole collection at its element timestamp', () => {
		const writes = settle([row(1, {roles: cell(new Set(['x', 'y']), 12)})], [row(1, {roles: cell(new Set(['x']), 8)})]);
		expect(writes).toEqual([
			{
				cql: `UPDATE codes USING TTL :settle_ttl SET roles = :roles ${where}`,
				params: {code: 'c', n: 1, roles: new Set(['x', 'y']), settle_ttl: 0},
				timestamp: 12,
			},
		]);
	});

	it('creates a key-only row lim1 lacks and deletes a row db1 lacks, just below the horizon', () => {
		expect(settle([row(1)], [])).toEqual([
			{cql: 'INSERT INTO codes (code, n) VALUES (:code, :n);', params: {code: 'c', n: 1}, timestamp: H - 1},
		]);
		expect(settle([], [row(2, {note: cell('a', 3)})])).toEqual([
			{cql: `DELETE FROM codes ${where}`, params: {code: 'c', n: 2}, timestamp: H - 1},
		]);
	});

	it('leaves cells at or above the horizon to a later pass', () => {
		expect(settle([row(1, {note: cell('a', H)})], [])).toEqual([]);
		expect(settle([row(1, {note: cell('a', 10)})], [row(1, {note: cell('b', H + 5)})])).toEqual([]);
	});

	it('writes the row marker at the oldest db1 cell for a row an insert created', () => {
		const writes = settle([row(1, {note: cell('a', 20), roles: cell(new Set(['x']), 15)})], [], true);
		expect(writes[0]).toEqual({
			cql: 'INSERT INTO codes (code, n) VALUES (:code, :n);',
			params: {code: 'c', n: 1},
			timestamp: 15,
		});
		expect(writes).toHaveLength(3);
	});
});

describe('differences below the horizon', () => {
	it('finds none when both sides hold the same rows and cells', () => {
		const rows = [row(1, {note: cell('a', 10, 50)}), row(2)];
		expect(differencesBelowHorizon(layout, rows, [row(1, {note: cell('a', 10, 40)}), row(2)], H, canonical)).toEqual(
			[],
		);
	});

	it('reports a missing row, an extra row and a differing cell', () => {
		expect(differencesBelowHorizon(layout, [row(1)], [], H, canonical)).toHaveLength(1);
		expect(differencesBelowHorizon(layout, [], [row(1, {note: cell('a', 2)})], H, canonical)).toHaveLength(1);
		expect(
			differencesBelowHorizon(layout, [row(1, {note: cell('a', 2)})], [row(1, {note: cell('a', 3)})], H, canonical),
		).toEqual(['codes ["c",1]: note']);
	});

	it('ignores rows and cells with writes at or above the horizon', () => {
		expect(differencesBelowHorizon(layout, [], [row(1, {note: cell('a', H)})], H, canonical)).toEqual([]);
		expect(
			differencesBelowHorizon(layout, [row(1, {note: cell('a', 2)})], [row(1, {note: cell('b', H + 1)})], H, canonical),
		).toEqual([]);
	});
});

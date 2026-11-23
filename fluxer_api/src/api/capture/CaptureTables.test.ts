// SPDX-License-Identifier: AGPL-3.0-or-later

import {rowState, type TableLayout} from '@app/api/capture/CaptureTables';
import type cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';

const layout: TableLayout = {
	name: 'channels',
	primaryKey: ['channel_id'],
	partitionKey: ['channel_id'],
	regular: ['name', 'recipient_ids'],
	multiCell: new Set(['recipient_ids']),
	defaultTtl: 0,
};

function row(values: Array<unknown>): cassandra.types.Row {
	return {
		get: (column: string) => (column === 'channel_id' ? values[0] : undefined),
		values: () => values,
	} as unknown as cassandra.types.Row;
}

describe('rowState', () => {
	it('reads a collection whose elements have no TTL as a cell without a TTL', () => {
		const state = rowState(layout, row([1n, 'group', 100n, null, new Set([1n, 2n]), [200n, 300n], [null, null]]));
		expect(state.cells['recipient_ids']).toEqual({v: new Set([1n, 2n]), wt: 300, ttl: null});
		expect(state.cells['name']).toEqual({v: 'group', wt: 100, ttl: null});
	});

	it('takes the smallest TTL among the elements that have one', () => {
		const state = rowState(
			layout,
			row([1n, null, null, null, new Set([1n, 2n, 3n]), [200n, 300n, 250n], [null, 60, 30]]),
		);
		expect(state.cells['recipient_ids']).toEqual({v: new Set([1n, 2n, 3n]), wt: 300, ttl: 30});
		expect(state.cells['name']).toBeUndefined();
	});

	it('keeps scalar TTLs as they are', () => {
		const state = rowState(layout, row([1n, 'group', 100n, 45, null, null, null]));
		expect(state.cells['name']).toEqual({v: 'group', wt: 100, ttl: 45});
	});
});

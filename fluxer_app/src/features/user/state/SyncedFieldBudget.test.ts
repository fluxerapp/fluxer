// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {decideOversizePush} from './SyncedFieldBudget';

const MAX = 1000;

describe('decideOversizePush', () => {
	it('accepts a payload inside the budget', () => {
		expect(decideOversizePush({encodedBytes: 999, maxEncodedBytes: MAX, lastPreparedBytes: null})).toBe(
			'within-budget',
		);
		expect(decideOversizePush({encodedBytes: MAX, maxEncodedBytes: MAX, lastPreparedBytes: null})).toBe(
			'within-budget',
		);
	});

	it('drops the first over-budget payload', () => {
		expect(decideOversizePush({encodedBytes: 1001, maxEncodedBytes: MAX, lastPreparedBytes: null})).toBe('drop');
	});

	it('drops an over-budget payload that grows', () => {
		expect(decideOversizePush({encodedBytes: 1200, maxEncodedBytes: MAX, lastPreparedBytes: 1100})).toBe('drop');
	});

	it('drops an over-budget payload that stays the same size', () => {
		expect(decideOversizePush({encodedBytes: 1100, maxEncodedBytes: MAX, lastPreparedBytes: 1100})).toBe('drop');
	});

	it('pushes an over-budget payload that shrinks so removals persist', () => {
		expect(decideOversizePush({encodedBytes: 1050, maxEncodedBytes: MAX, lastPreparedBytes: 1100})).toBe(
			'push-shrinks',
		);
	});

	it('lets a run of removals drain back under the budget', () => {
		let last: number | null = null;
		const decisions: Array<string> = [];
		for (const bytes of [1300, 1200, 1100, 900]) {
			decisions.push(decideOversizePush({encodedBytes: bytes, maxEncodedBytes: MAX, lastPreparedBytes: last}));
			last = bytes;
		}
		expect(decisions).toEqual(['drop', 'push-shrinks', 'push-shrinks', 'within-budget']);
	});
});

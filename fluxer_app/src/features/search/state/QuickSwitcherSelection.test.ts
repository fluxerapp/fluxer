// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveRecomputedSelectedIndex} from '@app/features/search/state/QuickSwitcherSelection';
import type {QuickSwitcherResult} from '@app/features/search/state/QuickSwitcherTypes';
import {QuickSwitcherResultTypes} from '@fluxer/constants/src/QuickSwitcherConstants';
import {describe, expect, it} from 'vitest';

const result = (type: QuickSwitcherResult['type'], id: string) => ({type, id}) as QuickSwitcherResult;

describe('resolveRecomputedSelectedIndex', () => {
	it('follows the focused result when the list is reordered', () => {
		const previous = result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c3');
		const results = [
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c3'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c2'),
		];
		expect(resolveRecomputedSelectedIndex(previous, results, 0)).toBe(0);
	});

	it('keeps the focused result when entries are inserted above it', () => {
		const previous = result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c2');
		const results = [
			result(QuickSwitcherResultTypes.HEADER, 'h1'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c9'),
			result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c2'),
		];
		expect(resolveRecomputedSelectedIndex(previous, results, 1)).toBe(2);
	});

	it('falls back when the focused result disappears', () => {
		const previous = result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'gone');
		const results = [result(QuickSwitcherResultTypes.TEXT_CHANNEL, 'c1')];
		expect(resolveRecomputedSelectedIndex(previous, results, 0)).toBe(0);
	});

	it('does not confuse ids that repeat across result types', () => {
		const previous = result(QuickSwitcherResultTypes.GUILD, '42');
		const results = [result(QuickSwitcherResultTypes.TEXT_CHANNEL, '42'), result(QuickSwitcherResultTypes.GUILD, '42')];
		expect(resolveRecomputedSelectedIndex(previous, results, 0)).toBe(1);
	});

	it('uses the fallback when nothing was focused', () => {
		expect(resolveRecomputedSelectedIndex(undefined, [result(QuickSwitcherResultTypes.GUILD, 'g')], 0)).toBe(0);
	});
});

// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {toIdString, toSortedIdArray} from '../IdUtils';

describe('toIdString', () => {
	it.each([
		[null, null],
		[undefined, null],
		[123456789012345678n, '123456789012345678'],
		['123456789012345678', '123456789012345678'],
		[0n, '0'],
		[999999999999999999999999999999n, '999999999999999999999999999999'],
		[-1n, '-1'],
		['0', '0'],
		['00123', '00123'],
	])('preserves the exact ID representation of %s', (input, expected) => {
		expect(toIdString(input)).toBe(expected);
	});
});

describe('toSortedIdArray', () => {
	it.each([
		{input: null, expected: []},
		{input: undefined, expected: []},
		{input: [], expected: []},
		{input: new Set<bigint>(), expected: []},
		{input: [300n, 100n, 200n], expected: ['100', '200', '300']},
		{input: ['300', '100', '200'], expected: ['100', '200', '300']},
		{input: new Set([300n, 100n, 200n]), expected: ['100', '200', '300']},
		{input: [42n], expected: ['42']},
		{input: new Set([123n]), expected: ['123']},
		{input: [100n, 200n, 300n], expected: ['100', '200', '300']},
		{input: [300n, 200n, 100n], expected: ['100', '200', '300']},
		{input: [2n, 10n, 1n], expected: ['1', '10', '2']},
		{input: [100n, 100n, 200n], expected: ['100', '100', '200']},
	])('stringifies and lexicographically sorts case %#', ({input, expected}) => {
		expect(toSortedIdArray<bigint | string>(input)).toEqual(expected);
	});

	it('does not reorder the caller-owned array', () => {
		const input = [300n, 100n, 200n];
		toSortedIdArray(input);
		expect(input).toEqual([300n, 100n, 200n]);
	});
});

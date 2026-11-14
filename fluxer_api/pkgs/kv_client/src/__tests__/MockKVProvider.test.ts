// SPDX-License-Identifier: AGPL-3.0-or-later

import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {describe, expect, it} from 'vitest';

describe('MockKVProvider sorted-set score bounds', () => {
	it.each([
		{name: 'numeric inclusive bounds', min: -2, max: 5, expected: ['negative', 'zero', 'alpha', 'beta', 'positive']},
		{name: 'string inclusive bounds', min: '-2', max: '2', expected: ['negative', 'zero', 'alpha', 'beta']},
		{name: 'exclusive minimum', min: '(-2', max: 2, expected: ['zero', 'alpha', 'beta']},
		{name: 'exclusive maximum', min: -2, max: '(2', expected: ['negative', 'zero']},
		{name: 'both exclusive', min: '(-2', max: '(5', expected: ['zero', 'alpha', 'beta']},
		{name: 'equal inclusive bounds', min: 2, max: 2, expected: ['alpha', 'beta']},
		{name: 'equal exclusive minimum', min: '(2', max: 2, expected: []},
		{name: 'equal exclusive maximum', min: 2, max: '(2', expected: []},
		{name: 'reversed bounds', min: 5, max: -2, expected: []},
		{name: 'literal infinities', min: '-inf', max: '+inf', expected: ['negative', 'zero', 'alpha', 'beta', 'positive']},
		{name: 'literal positive infinite minimum', min: '+inf', max: '+inf', expected: []},
		{name: 'literal negative infinite maximum', min: '-inf', max: '-inf', expected: []},
		{
			name: 'numeric infinities',
			min: Number.NEGATIVE_INFINITY,
			max: Number.POSITIVE_INFINITY,
			expected: ['negative', 'zero', 'alpha', 'beta', 'positive'],
		},
		{name: 'numeric positive infinite minimum', min: Number.POSITIVE_INFINITY, max: 5, expected: []},
		{name: 'numeric negative infinite maximum', min: -2, max: Number.NEGATIVE_INFINITY, expected: []},
		{name: 'numeric NaN minimum', min: Number.NaN, max: 5, expected: []},
		{name: 'numeric NaN maximum', min: -2, max: Number.NaN, expected: []},
		{name: 'string NaN fallbacks', min: 'NaN', max: 'NaN', expected: ['negative', 'zero', 'alpha', 'beta', 'positive']},
		{
			name: 'string Infinity fallbacks',
			min: 'Infinity',
			max: '-Infinity',
			expected: ['negative', 'zero', 'alpha', 'beta', 'positive'],
		},
		{
			name: 'malformed inclusive fallbacks',
			min: 'missing',
			max: 'missing',
			expected: ['negative', 'zero', 'alpha', 'beta', 'positive'],
		},
		{
			name: 'malformed exclusive fallbacks',
			min: '(missing',
			max: '(missing',
			expected: ['negative', 'zero', 'alpha', 'beta', 'positive'],
		},
		{
			name: 'nonfinite exclusive fallbacks',
			min: '(Infinity',
			max: '(-Infinity',
			expected: ['negative', 'zero', 'alpha', 'beta', 'positive'],
		},
		{name: 'empty inclusive minimum', min: '', max: 2, expected: ['zero', 'alpha', 'beta']},
		{name: 'empty inclusive maximum', min: -2, max: '', expected: ['negative', 'zero']},
		{name: 'empty exclusive minimum', min: '(', max: 5, expected: ['alpha', 'beta', 'positive']},
		{name: 'empty exclusive maximum', min: -2, max: '(', expected: ['negative']},
		{name: 'whitespace numeric strings', min: ' 0 ', max: ' 2 ', expected: ['zero', 'alpha', 'beta']},
		{name: 'hexadecimal numeric strings', min: '0x2', max: '0x5', expected: ['alpha', 'beta', 'positive']},
	])('$name', async ({min, max, expected}) => {
		const provider = new MockKVProvider();
		await provider.zadd('scores', 5, 'positive', 2, 'beta', -2, 'negative', 2, 'alpha', 0, 'zero');

		await expect(provider.zrangebyscore('scores', min, max)).resolves.toEqual(expected);
	});
});

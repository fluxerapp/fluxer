// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {selectFrozenUnreadChannels} from './UnreadChannelOrder';

function ids(channels: ReadonlyArray<{id: string}>): Array<string> {
	return channels.map((channel) => channel.id);
}

describe('selectFrozenUnreadChannels', () => {
	it('keeps channels in their frozen position while the list is open', () => {
		const order = new Map([
			['a', 0],
			['b', 1],
			['c', 2],
		]);
		expect(ids(selectFrozenUnreadChannels(order, [{id: 'c'}, {id: 'a'}, {id: 'b'}]))).toEqual(['a', 'b', 'c']);
	});

	it('hides a channel that becomes unread after the order was frozen', () => {
		const order = new Map([['a', 0]]);
		expect(ids(selectFrozenUnreadChannels(order, [{id: 'a'}, {id: 'late'}]))).toEqual(['a']);
	});

	it('never grows the list when every live channel is new to the order', () => {
		const order = new Map([['a', 0]]);
		expect(ids(selectFrozenUnreadChannels(order, [{id: 'x'}, {id: 'y'}, {id: 'z'}]))).toEqual([]);
	});

	it('yields nothing for an empty order', () => {
		expect(ids(selectFrozenUnreadChannels(new Map<string, number>(), [{id: 'x'}]))).toEqual([]);
	});

	it('does not resurrect channels that are no longer unread', () => {
		const order = new Map([
			['a', 0],
			['gone', 1],
		]);
		expect(ids(selectFrozenUnreadChannels(order, [{id: 'a'}]))).toEqual(['a']);
	});

	it('does not mutate the order it was given', () => {
		const order = new Map([['a', 0]]);
		selectFrozenUnreadChannels(order, [{id: 'a'}, {id: 'late'}]);
		expect([...order.keys()]).toEqual(['a']);
	});
});

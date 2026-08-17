// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {mergeFrozenUnreadOrder} from './UnreadChannelOrder';

function ids(channels: ReadonlyArray<{id: string}>): Array<string> {
	return channels.map((channel) => channel.id);
}

describe('mergeFrozenUnreadOrder', () => {
	it('keeps channels in their frozen position while the list is open', () => {
		const order = new Map([
			['a', 0],
			['b', 1],
			['c', 2],
		]);
		const merged = mergeFrozenUnreadOrder(order, [{id: 'c'}, {id: 'a'}, {id: 'b'}]);
		expect(ids(merged)).toEqual(['a', 'b', 'c']);
	});

	it('shows a channel that becomes unread after the order was frozen', () => {
		const order = new Map([['a', 0]]);
		const merged = mergeFrozenUnreadOrder(order, [{id: 'a'}, {id: 'late'}]);
		expect(ids(merged)).toEqual(['a', 'late']);
	});

	it('appends late arrivals after existing entries rather than reshuffling', () => {
		const order = new Map([
			['a', 0],
			['b', 1],
		]);
		const merged = mergeFrozenUnreadOrder(order, [{id: 'late'}, {id: 'b'}, {id: 'a'}]);
		expect(ids(merged)).toEqual(['a', 'b', 'late']);
	});

	it('remembers a late arrival position once assigned', () => {
		const order = new Map([['a', 0]]);
		mergeFrozenUnreadOrder(order, [{id: 'a'}, {id: 'late'}]);
		const merged = mergeFrozenUnreadOrder(order, [{id: 'late'}, {id: 'a'}]);
		expect(ids(merged)).toEqual(['a', 'late']);
	});

	it('drops nothing when every channel is new', () => {
		const order = new Map<string, number>();
		const merged = mergeFrozenUnreadOrder(order, [{id: 'x'}, {id: 'y'}, {id: 'z'}]);
		expect(ids(merged)).toEqual(['x', 'y', 'z']);
	});

	it('does not resurrect channels that are no longer unread', () => {
		const order = new Map([
			['a', 0],
			['gone', 1],
		]);
		const merged = mergeFrozenUnreadOrder(order, [{id: 'a'}]);
		expect(ids(merged)).toEqual(['a']);
	});
});

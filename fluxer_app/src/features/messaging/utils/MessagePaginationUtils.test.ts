// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {calculateAroundPaginationState, getAroundWindowCounts, mergeAscendingById} from './MessagePaginationUtils';

describe('MessagePaginationUtils', () => {
	it('splits around windows with the newer side receiving the extra item for even limits', () => {
		expect(getAroundWindowCounts(0)).toEqual({newer: 0, older: 0});
		expect(getAroundWindowCounts(1)).toEqual({newer: 0, older: 0});
		expect(getAroundWindowCounts(2)).toEqual({newer: 1, older: 0});
		expect(getAroundWindowCounts(50)).toEqual({newer: 25, older: 24});
		expect(getAroundWindowCounts(51)).toEqual({newer: 25, older: 25});
	});

	it('keeps newer pagination open when an oldest-edge around response fills the newer side', () => {
		const state = calculateAroundPaginationState({
			limit: 50,
			messageCount: 26,
			targetIndex: 25,
			newestFetchedMessageId: '26',
			knownLatestMessageId: '100',
		});
		expect(state).toMatchObject({
			expectedNewer: 25,
			expectedOlder: 24,
			messagesNewer: 25,
			messagesOlder: 0,
			hasMoreBefore: false,
			hasMoreAfter: true,
		});
	});

	it('closes newer pagination when the fetched page reaches the known latest message', () => {
		const state = calculateAroundPaginationState({
			limit: 50,
			messageCount: 25,
			targetIndex: 0,
			newestFetchedMessageId: '100',
			knownLatestMessageId: '100',
		});
		expect(state.hasMoreBefore).toBe(true);
		expect(state.hasMoreAfter).toBe(false);
	});
});

describe('mergeAscendingById', () => {
	const ID = {a: '1519773906704011264', b: '1519773906708205568', c: '1519773906712399872'};

	it('appends when every incoming message is newer than the tail', () => {
		const existing = [{id: ID.a}];
		expect(mergeAscendingById(existing, [{id: ID.b}, {id: ID.c}])).toEqual([{id: ID.a}, {id: ID.b}, {id: ID.c}]);
	});

	it('returns the existing list untouched for an empty page', () => {
		const existing = [{id: ID.a}];
		expect(mergeAscendingById(existing, [])).toBe(existing);
	});

	it('interleaves a recovered message that is older than a live message already at the tail', () => {
		expect(mergeAscendingById([{id: ID.a}, {id: ID.c}], [{id: ID.b}])).toEqual([{id: ID.a}, {id: ID.b}, {id: ID.c}]);
	});

	it('keeps the whole page in order when it spans a live message already at the tail', () => {
		expect(mergeAscendingById([{id: ID.a}, {id: ID.b}], [{id: ID.c}])).toEqual([{id: ID.a}, {id: ID.b}, {id: ID.c}]);
		expect(mergeAscendingById([{id: ID.b}], [{id: ID.a}, {id: ID.c}])).toEqual([{id: ID.a}, {id: ID.b}, {id: ID.c}]);
	});

	it('appends onto an empty window', () => {
		expect(mergeAscendingById([], [{id: ID.a}])).toEqual([{id: ID.a}]);
	});
});

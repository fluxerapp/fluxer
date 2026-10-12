// SPDX-License-Identifier: AGPL-3.0-or-later

import {calculateAroundPaginationState, mergeAscendingById} from '@app/features/messaging/utils/MessagePaginationUtils';
import {describe, expect, it} from 'vitest';

describe('MessagePaginationUtils', () => {
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

	it('interleaves a recovered message that is older than a live message already at the tail', () => {
		expect(mergeAscendingById([{id: ID.a}, {id: ID.c}], [{id: ID.b}])).toEqual([{id: ID.a}, {id: ID.b}, {id: ID.c}]);
	});

	it('keeps the whole page in order when it spans a live message already at the tail', () => {
		expect(mergeAscendingById([{id: ID.a}, {id: ID.b}], [{id: ID.c}])).toEqual([{id: ID.a}, {id: ID.b}, {id: ID.c}]);
		expect(mergeAscendingById([{id: ID.b}], [{id: ID.a}, {id: ID.c}])).toEqual([{id: ID.a}, {id: ID.b}, {id: ID.c}]);
	});
});

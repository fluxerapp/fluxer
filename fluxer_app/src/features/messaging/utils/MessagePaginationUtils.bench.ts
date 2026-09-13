// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	calculateAroundPaginationState,
	getAroundWindowCounts,
} from '@app/features/messaging/utils/MessagePaginationUtils';
import {bench, describe} from 'vitest';

const PAGINATION_INPUTS = Array.from({length: 100_000}, (_value, index) => ({
	limit: 50 + (index % 25),
	messageCount: 25 + (index % 100),
	targetIndex: index % 50,
	newestFetchedMessageId: index % 3 === 0 ? 'latest' : `${index}`,
	knownLatestMessageId: 'latest',
}));

describe('MessagePaginationUtils benchmarks', () => {
	bench('calculate 100k around pagination states', () => {
		for (const input of PAGINATION_INPUTS) {
			calculateAroundPaginationState(input);
		}
	});

	bench('calculate 100k around window counts', () => {
		let total = 0;
		for (let index = 0; index < 100_000; index += 1) {
			const {newer, older} = getAroundWindowCounts(index % 100);
			total += newer + older;
		}
		(globalThis as {__messagePaginationBenchSink?: number}).__messagePaginationBenchSink = total;
	});
});

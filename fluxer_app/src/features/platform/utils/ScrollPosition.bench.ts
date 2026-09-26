// SPDX-License-Identifier: AGPL-3.0-or-later

import {evaluateScrollPinning} from '@app/features/platform/utils/ScrollPosition';
import {test} from 'vitest';

const SCROLL_METRICS = Array.from({length: 100_000}, (_value, index) => ({
	scrollTop: (index * 37) % 200_000,
	scrollHeight: 220_000 + (index % 1_000),
	offsetHeight: 800 + (index % 200),
}));

test('ScrollPosition benchmarks', async ({bench}) => {
	await bench('evaluate 100k message-list scroll pinning states', () => {
		let pinned = 0;
		for (let index = 0; index < SCROLL_METRICS.length; index += 1) {
			if (
				evaluateScrollPinning(SCROLL_METRICS[index], {
					hasMoreAfter: index % 7 === 0,
					wasPinned: index % 5 === 0,
				}).isPinned
			) {
				pinned += 1;
			}
		}
		(globalThis as {__scrollPositionBenchSink?: number}).__scrollPositionBenchSink = pinned;
	}).run();
});

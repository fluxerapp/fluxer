// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	areMemberListRangesCovered,
	areMemberListRangesEqual,
	areNormalizedMemberListRangesCovered,
	areNormalizedMemberListRangesEqual,
	buildMemberListRangeWindow,
	buildMemberListRenderWindow,
	normalizeMemberListRanges,
} from '@app/features/member/utils/MemberListRangeUtils';
import {test} from 'vitest';

const ROW_HEIGHT = 44;
const VIEWPORT_HEIGHT = ROW_HEIGHT * 18;
const LARGE_TOTAL_ROWS = 100_000;
const SCROLL_OFFSETS = Array.from({length: 1_000}, (_, index) => ((index * 997) % LARGE_TOTAL_ROWS) * ROW_HEIGHT);
const OVERLAPPING_RANGES = Array.from({length: 1_000}, (_, index): [number, number] => {
	const start = (index * 37) % 20_000;
	return [start, start + ((index * 13) % 150)];
});
const COVERING_RANGES = Array.from({length: 1_000}, (_, index): [number, number] => [index * 100, index * 100 + 99]);
const INNER_RANGES = Array.from({length: 1_000}, (_, index): [number, number] => [index * 100 + 25, index * 100 + 75]);
const COVERING_RANGES_COPY = COVERING_RANGES.map((range): [number, number] => [range[0], range[1]]);
const NORMALIZED_COVERING_RANGES = normalizeMemberListRanges(COVERING_RANGES);
const NORMALIZED_COVERING_RANGES_COPY = normalizeMemberListRanges(COVERING_RANGES_COPY);
const NORMALIZED_INNER_RANGES = normalizeMemberListRanges(INNER_RANGES);

test('MemberListRangeUtils benchmarks', async ({bench}) => {
	await bench('normalize 1k mixed overlapping ranges', () => {
		normalizeMemberListRanges(OVERLAPPING_RANGES);
	}).run();

	await bench('build subscription windows while fast scrolling', () => {
		for (const scrollTop of SCROLL_OFFSETS) {
			buildMemberListRangeWindow({
				scrollTop,
				clientHeight: VIEWPORT_HEIGHT,
				rowHeight: ROW_HEIGHT,
				bufferRows: 12,
				overscanPages: 0,
				totalRows: LARGE_TOTAL_ROWS,
			});
		}
	}).run();

	await bench('build render windows while fast scrolling', () => {
		for (const scrollTop of SCROLL_OFFSETS) {
			buildMemberListRenderWindow({
				scrollTop,
				clientHeight: VIEWPORT_HEIGHT,
				rowHeight: ROW_HEIGHT,
				bufferRows: 6,
				totalRows: LARGE_TOTAL_ROWS,
			});
		}
	}).run();

	await bench('coverage checks for 1k normalized pages', () => {
		areMemberListRangesCovered(INNER_RANGES, COVERING_RANGES);
	}).run();

	await bench('normalized coverage checks without re-normalizing', () => {
		areNormalizedMemberListRangesCovered(NORMALIZED_INNER_RANGES, NORMALIZED_COVERING_RANGES);
	}).run();

	await bench('range equality for raw normalized-shaped windows', () => {
		areMemberListRangesEqual(COVERING_RANGES, COVERING_RANGES_COPY);
	}).run();

	await bench('normalized range equality without re-normalizing', () => {
		areNormalizedMemberListRangesEqual(NORMALIZED_COVERING_RANGES, NORMALIZED_COVERING_RANGES_COPY);
	}).run();
});

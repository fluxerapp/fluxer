// SPDX-License-Identifier: AGPL-3.0-or-later

import {MEMBER_LIST_RANGE_MAX_SPAN} from '@fluxer/constants/src/GatewayConstants';
import {describe, expect, it} from 'vitest';
import {
	areMemberListRangesCovered,
	areMemberListRangesEqual,
	areNormalizedMemberListRangesCovered,
	areNormalizedMemberListRangesEqual,
	buildMemberListRangeWindow,
	buildMemberListRenderWindow,
	normalizeMemberListRanges,
} from './MemberListRangeUtils';

describe('MemberListRangeUtils', () => {
	it('compares ranges after normalization and page splitting', () => {
		expect(areMemberListRangesEqual([[0, MEMBER_LIST_RANGE_MAX_SPAN]], [[0, MEMBER_LIST_RANGE_MAX_SPAN]])).toBe(true);
		expect(areMemberListRangesEqual([[0, MEMBER_LIST_RANGE_MAX_SPAN + 1]], [[0, MEMBER_LIST_RANGE_MAX_SPAN]])).toBe(
			false,
		);
	});

	it('detects whether a requested window is still covered by local ranges', () => {
		expect(
			areMemberListRangesCovered(
				[
					[0, 99],
					[100, 199],
				],
				[[0, 199]],
			),
		).toBe(true);
		expect(areMemberListRangesCovered([[100, 199]], [[0, 299]])).toBe(true);
		expect(areMemberListRangesCovered([[0, 299]], [[100, 399]])).toBe(false);
	});

	it('compares branded normalized ranges without re-normalizing them', () => {
		const requested = normalizeMemberListRanges([[100, 199]]);
		const subscribed = normalizeMemberListRanges([[0, 299]]);
		expect(areNormalizedMemberListRangesEqual(requested, subscribed)).toBe(false);
		expect(areNormalizedMemberListRangesEqual(requested, normalizeMemberListRanges([[100, 199]]))).toBe(true);
		expect(areNormalizedMemberListRangesCovered(requested, subscribed)).toBe(true);
	});

	it('builds a bounded, overscanned page window from scroll metrics and keeps the head page subscribed', () => {
		expect(
			buildMemberListRangeWindow({
				scrollTop: 44 * 250,
				clientHeight: 44 * 10,
				rowHeight: 44,
				bufferRows: 24,
				totalRows: 500,
			}),
		).toEqual([
			[0, 99],
			[100, 199],
			[200, 299],
			[300, 399],
		]);
	});

	it('keeps scrollbar-dragged bottom windows clamped to the last known page', () => {
		expect(
			buildMemberListRangeWindow({
				scrollTop: 44 * 490,
				clientHeight: 44 * 10,
				rowHeight: 44,
				bufferRows: 24,
				totalRows: 500,
			}),
		).toEqual([
			[0, 99],
			[300, 399],
			[400, 499],
		]);
	});

	it('allows callers to disable page overscan for memory-constrained member-list surfaces', () => {
		expect(
			buildMemberListRangeWindow({
				scrollTop: 44 * 250,
				clientHeight: 44 * 10,
				rowHeight: 44,
				bufferRows: 12,
				overscanPages: 0,
				totalRows: 500,
			}),
		).toEqual([
			[0, 99],
			[200, 299],
		]);
	});

	it('omits a duplicate head page when the window already starts at the top of the list', () => {
		expect(
			buildMemberListRangeWindow({
				scrollTop: 0,
				clientHeight: 44 * 10,
				rowHeight: 44,
				bufferRows: 12,
				overscanPages: 0,
				totalRows: 500,
			}),
		).toEqual([[0, 99]]);
	});

	it('keeps page ends aligned so a changing member count does not resubscribe', () => {
		const options = {
			scrollTop: 44 * 250,
			clientHeight: 44 * 10,
			rowHeight: 44,
			bufferRows: 12,
			overscanPages: 0,
		};
		expect(buildMemberListRangeWindow({...options, totalRows: 450})).toEqual([
			[0, 99],
			[200, 299],
		]);
		expect(buildMemberListRangeWindow({...options, totalRows: 451})).toEqual([
			[0, 99],
			[200, 299],
		]);
	});

	it('builds a tight render window instead of rendering whole subscription pages', () => {
		expect(
			buildMemberListRenderWindow({
				scrollTop: 44 * 250,
				clientHeight: 44 * 10,
				rowHeight: 44,
				bufferRows: 8,
				totalRows: 500,
			}),
		).toEqual([[242, 268]]);
	});

	it('returns no render rows for an empty known list', () => {
		expect(
			buildMemberListRenderWindow({
				scrollTop: 0,
				clientHeight: 44 * 10,
				rowHeight: 44,
				bufferRows: 8,
				totalRows: 0,
			}),
		).toEqual([]);
	});

	it('resolves the visible row range from variable-height offsets when rowOffsets is provided', () => {
		const rowOffsets = [0, 30, 74, 118, 162, 206, 236, 280, 324, 368, 412];
		expect(
			buildMemberListRenderWindow({
				scrollTop: 206,
				clientHeight: 88,
				rowHeight: 44,
				rowOffsets,
				bufferRows: 0,
				totalRows: 10,
			}),
		).toEqual([[5, 7]]);
	});

	it('does not let a uniform rowHeight skew the variable-height window', () => {
		const rowOffsets = [0, 30, 74, 118, 162, 206, 236, 280, 324, 368, 412];
		const offsetWindow = buildMemberListRenderWindow({
			scrollTop: 206,
			clientHeight: 44,
			rowHeight: 44,
			rowOffsets,
			bufferRows: 0,
			totalRows: 10,
		});
		const uniformWindow = buildMemberListRenderWindow({
			scrollTop: 206,
			clientHeight: 44,
			rowHeight: 44,
			bufferRows: 0,
			totalRows: 10,
		});
		expect(offsetWindow).toEqual([[5, 6]]);
		expect(uniformWindow).toEqual([[4, 6]]);
	});
});

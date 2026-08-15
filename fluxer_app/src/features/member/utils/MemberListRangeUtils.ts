// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MEMBER_LIST_RANGE_MAX_END,
	MEMBER_LIST_RANGE_MAX_SPAN,
	MEMBER_LIST_RANGE_PAGE_SIZE,
	MEMBER_LIST_RANGE_WINDOW_OVERSCAN_PAGES,
} from '@fluxer/constants/src/GatewayConstants';

const MEMBER_LIST_RANGE_MAX_RANGES = 10;

declare const normalizedMemberListRangesBrand: unique symbol;

export type MemberListRange = [number, number];
export type MemberListRanges = Array<MemberListRange>;
export type NormalizedMemberListRanges = MemberListRanges & {
	readonly [normalizedMemberListRangesBrand]: 'NormalizedMemberListRanges';
};

export interface MemberListRangeWindowOptions {
	scrollTop: number;
	clientHeight: number;
	rowHeight: number;
	bufferRows: number;
	totalRows?: number | null;
	overscanPages?: number;
	rowOffsets?: ReadonlyArray<number> | null;
}

interface VisibleRowBounds {
	startIndex: number;
	unclampedEnd: number;
}

function findRowIndexForOffset(rowOffsets: ReadonlyArray<number>, pixel: number): number {
	const lastRow = rowOffsets.length - 2;
	if (lastRow < 0) {
		return 0;
	}
	if (pixel <= 0) {
		return 0;
	}
	if (pixel >= rowOffsets[lastRow + 1]!) {
		return lastRow;
	}
	let low = 0;
	let high = lastRow;
	while (low < high) {
		const mid = (low + high + 1) >> 1;
		if (rowOffsets[mid]! <= pixel) {
			low = mid;
		} else {
			high = mid - 1;
		}
	}
	return low;
}

function resolveVisibleRowBounds(options: MemberListRangeWindowOptions): VisibleRowBounds {
	const {scrollTop, clientHeight, rowHeight, bufferRows, rowOffsets} = options;
	if (rowOffsets != null && rowOffsets.length >= 2) {
		const visibleStart = findRowIndexForOffset(rowOffsets, scrollTop);
		const visibleEnd = findRowIndexForOffset(rowOffsets, scrollTop + clientHeight);
		const startIndex = Math.max(0, visibleStart - bufferRows);
		const unclampedEnd = Math.max(startIndex, visibleEnd + bufferRows);
		return {startIndex, unclampedEnd};
	}
	const startIndex = Math.max(0, Math.floor(scrollTop / rowHeight) - bufferRows);
	const unclampedEnd = Math.max(startIndex, Math.ceil((scrollTop + clientHeight) / rowHeight) + bufferRows);
	return {startIndex, unclampedEnd};
}

export function areMemberListRangesEqual(left: MemberListRanges, right: MemberListRanges): boolean {
	if (left === right) {
		return true;
	}
	return areNormalizedMemberListRangesEqual(normalizeMemberListRanges(left), normalizeMemberListRanges(right));
}

export function areNormalizedMemberListRangesEqual(
	left: NormalizedMemberListRanges,
	right: NormalizedMemberListRanges,
): boolean {
	if (left === right) {
		return true;
	}
	if (left.length !== right.length) {
		return false;
	}
	for (let index = 0; index < left.length; index += 1) {
		const [leftStart, leftEnd] = left[index];
		const [rightStart, rightEnd] = right[index];
		if (leftStart !== rightStart || leftEnd !== rightEnd) {
			return false;
		}
	}
	return true;
}

export function areMemberListRangesCovered(innerRanges: MemberListRanges, outerRanges: MemberListRanges): boolean {
	return areNormalizedMemberListRangesCovered(
		normalizeMemberListRanges(innerRanges),
		normalizeMemberListRanges(outerRanges),
	);
}

export function areNormalizedMemberListRangesCovered(
	innerRanges: NormalizedMemberListRanges,
	outerRanges: NormalizedMemberListRanges,
): boolean {
	if (innerRanges.length === 0) {
		return true;
	}
	if (outerRanges.length === 0) {
		return false;
	}
	let outerIndex = 0;
	for (const [innerStart, innerEnd] of innerRanges) {
		while (outerIndex < outerRanges.length && outerRanges[outerIndex]![1] < innerStart) {
			outerIndex += 1;
		}
		if (outerIndex >= outerRanges.length) {
			return false;
		}
		const [outerStart, outerEnd] = outerRanges[outerIndex]!;
		if (outerStart > innerStart || outerEnd < innerEnd) {
			return false;
		}
	}
	return true;
}

export function normalizeMemberListRanges(inputRanges: MemberListRanges): NormalizedMemberListRanges {
	if (inputRanges.length === 0) {
		return asNormalizedMemberListRanges([]);
	}
	if (inputRanges.length === 1) {
		const sanitizedRange = sanitizeMemberListRange(inputRanges[0]);
		return sanitizedRange ? splitMemberListRangeBySpan(sanitizedRange) : asNormalizedMemberListRanges([]);
	}
	const sanitizedRanges: MemberListRanges = [];
	for (const range of inputRanges) {
		const sanitizedRange = sanitizeMemberListRange(range);
		if (sanitizedRange) {
			sanitizedRanges.push(sanitizedRange);
		}
	}
	if (sanitizedRanges.length === 0) {
		return asNormalizedMemberListRanges([]);
	}
	sanitizedRanges.sort(([leftStart], [rightStart]) => leftStart - rightStart);
	const normalizedRanges: MemberListRanges = [];
	let [currentStart, currentEnd] = sanitizedRanges[0];
	for (let index = 1; index < sanitizedRanges.length; index += 1) {
		const [start, end] = sanitizedRanges[index];
		if (start <= currentEnd + 1) {
			currentEnd = Math.max(currentEnd, end);
			continue;
		}
		pushMemberListRangeBySpan(normalizedRanges, currentStart, currentEnd);
		currentStart = start;
		currentEnd = end;
	}
	pushMemberListRangeBySpan(normalizedRanges, currentStart, currentEnd);
	return asNormalizedMemberListRanges(normalizedRanges);
}

export function buildMemberListRangeWindow(options: MemberListRangeWindowOptions): NormalizedMemberListRanges {
	const {rowHeight, totalRows, rowOffsets} = options;
	const overscanPages = Math.max(0, Math.floor(options.overscanPages ?? MEMBER_LIST_RANGE_WINDOW_OVERSCAN_PAGES));
	const hasRowOffsets = rowOffsets != null && rowOffsets.length >= 2;
	if (!hasRowOffsets && rowHeight <= 0) {
		return asNormalizedMemberListRanges([[0, MEMBER_LIST_RANGE_MAX_SPAN]]);
	}
	const {startIndex, unclampedEnd} = resolveVisibleRowBounds(options);
	const safeTotalRows = totalRows != null && Number.isFinite(totalRows) ? Math.max(0, Math.floor(totalRows)) : null;
	if (safeTotalRows === 0) {
		return asNormalizedMemberListRanges([]);
	}
	const endIndex = safeTotalRows != null ? Math.min(unclampedEnd, safeTotalRows - 1) : unclampedEnd;
	const firstVisiblePage = Math.floor(startIndex / MEMBER_LIST_RANGE_PAGE_SIZE);
	const lastVisiblePage = Math.floor(endIndex / MEMBER_LIST_RANGE_PAGE_SIZE);
	let firstPage = Math.max(0, firstVisiblePage - overscanPages);
	let lastPage = Math.max(firstPage, lastVisiblePage + overscanPages);
	if (safeTotalRows != null) {
		const maxPage = Math.floor((safeTotalRows - 1) / MEMBER_LIST_RANGE_PAGE_SIZE);
		firstPage = Math.min(firstPage, maxPage);
		lastPage = Math.min(lastPage, maxPage);
	}
	const ranges: MemberListRanges = [];
	for (let page = firstPage; page <= lastPage; page += 1) {
		const pageStart = page * MEMBER_LIST_RANGE_PAGE_SIZE;
		const maxPageEnd = pageStart + MEMBER_LIST_RANGE_MAX_SPAN;
		const pageEnd = safeTotalRows != null ? Math.min(maxPageEnd, safeTotalRows - 1) : maxPageEnd;
		ranges.push([pageStart, pageEnd]);
	}
	return normalizeMemberListRanges(ranges);
}

export function buildMemberListRenderWindow(options: MemberListRangeWindowOptions): NormalizedMemberListRanges {
	const {rowHeight, totalRows, rowOffsets} = options;
	const hasRowOffsets = rowOffsets != null && rowOffsets.length >= 2;
	if (!hasRowOffsets && rowHeight <= 0) {
		return asNormalizedMemberListRanges([[0, MEMBER_LIST_RANGE_MAX_SPAN]]);
	}
	const safeTotalRows = totalRows != null && Number.isFinite(totalRows) ? Math.max(0, Math.floor(totalRows)) : null;
	if (safeTotalRows === 0) {
		return asNormalizedMemberListRanges([]);
	}
	const {startIndex, unclampedEnd} = resolveVisibleRowBounds(options);
	const endIndex = safeTotalRows != null ? Math.min(unclampedEnd, safeTotalRows - 1) : unclampedEnd;
	return asNormalizedMemberListRanges([[startIndex, endIndex]]);
}

export function isIndexInMemberListRanges(index: number, ranges: MemberListRanges): boolean {
	for (const [start, end] of ranges) {
		if (index >= start && index <= end) {
			return true;
		}
	}
	return false;
}

function splitMemberListRangeBySpan(range: MemberListRange): NormalizedMemberListRanges {
	const [start, end] = range;
	const chunks: MemberListRanges = [];
	pushMemberListRangeBySpan(chunks, start, end);
	return asNormalizedMemberListRanges(chunks);
}

function pushMemberListRangeBySpan(chunks: MemberListRanges, start: number, end: number): void {
	let chunkStart = start;
	while (chunkStart <= end && chunks.length < MEMBER_LIST_RANGE_MAX_RANGES) {
		const chunkEnd = Math.min(end, chunkStart + MEMBER_LIST_RANGE_MAX_SPAN);
		chunks.push([chunkStart, chunkEnd]);
		chunkStart = chunkEnd + 1;
	}
}

function sanitizeMemberListRange([start, end]: MemberListRange): MemberListRange | null {
	if (!Number.isFinite(start) || !Number.isFinite(end)) {
		return null;
	}
	const safeStart = Math.max(0, Math.floor(start));
	if (safeStart > MEMBER_LIST_RANGE_MAX_END) {
		return null;
	}
	const safeEnd = Math.min(MEMBER_LIST_RANGE_MAX_END, Math.max(safeStart, Math.floor(end)));
	return [safeStart, safeEnd];
}

function asNormalizedMemberListRanges(ranges: MemberListRanges): NormalizedMemberListRanges {
	return ranges as NormalizedMemberListRanges;
}

// SPDX-License-Identifier: AGPL-3.0-or-later

export interface MemberListGroupSnapshot {
	id: string;
	count: number;
}

export interface MemberListGroupLayout {
	id: string;
	count: number;
	headerRowIndex: number;
	memberStartIndex: number;
	memberEndIndex: number;
	rowEndIndex: number;
}

export function buildMemberListLayout(groups: ReadonlyArray<MemberListGroupSnapshot>): Array<MemberListGroupLayout> {
	const layouts: Array<MemberListGroupLayout> = [];
	let rowIndex = 0;
	let memberIndex = 0;
	for (const group of groups) {
		const effectiveCount = Math.max(0, group.count);
		if (effectiveCount === 0) {
			continue;
		}
		const headerRowIndex = rowIndex;
		const memberStartIndex = memberIndex;
		const memberEndIndex = memberIndex + effectiveCount - 1;
		const rowEndIndex = headerRowIndex + effectiveCount;
		layouts.push({
			id: group.id,
			count: effectiveCount,
			headerRowIndex,
			memberStartIndex,
			memberEndIndex,
			rowEndIndex,
		});
		rowIndex = rowEndIndex + 1;
		memberIndex = memberEndIndex + 1;
	}
	return layouts;
}

export function getTotalRowsFromLayout(layouts: ReadonlyArray<MemberListGroupLayout>): number {
	if (layouts.length === 0) {
		return 0;
	}
	return layouts[layouts.length - 1]!.rowEndIndex + 1;
}

export interface MemberListRowHeights {
	memberHeight: number;
	headerHeight: number;
}

export function buildMemberListRowOffsets(
	layouts: ReadonlyArray<MemberListGroupLayout>,
	totalRows: number,
	{memberHeight, headerHeight}: MemberListRowHeights,
): Array<number> {
	const safeTotalRows = Math.max(0, Math.floor(totalRows));
	const offsets = new Array<number>(safeTotalRows + 1);
	offsets[0] = 0;
	if (safeTotalRows === 0) {
		return offsets;
	}
	let rowIndex = 0;
	let runningOffset = 0;
	for (const layout of layouts) {
		if (rowIndex >= safeTotalRows) {
			break;
		}
		while (rowIndex < layout.headerRowIndex && rowIndex < safeTotalRows) {
			runningOffset += memberHeight;
			rowIndex += 1;
			offsets[rowIndex] = runningOffset;
		}
		if (rowIndex === layout.headerRowIndex && rowIndex < safeTotalRows) {
			runningOffset += headerHeight;
			rowIndex += 1;
			offsets[rowIndex] = runningOffset;
		}
		const lastMemberRow = Math.min(layout.rowEndIndex, safeTotalRows - 1);
		while (rowIndex <= lastMemberRow) {
			runningOffset += memberHeight;
			rowIndex += 1;
			offsets[rowIndex] = runningOffset;
		}
	}
	while (rowIndex < safeTotalRows) {
		runningOffset += memberHeight;
		rowIndex += 1;
		offsets[rowIndex] = runningOffset;
	}
	return offsets;
}

export function getTotalMemberCount(groups: ReadonlyArray<MemberListGroupSnapshot>): number {
	let count = 0;
	for (const group of groups) {
		count += Math.max(0, group.count);
	}
	return count;
}

export function getGroupLayoutForRow(
	layouts: ReadonlyArray<MemberListGroupLayout>,
	rowIndex: number,
): MemberListGroupLayout | null {
	let low = 0;
	let high = layouts.length - 1;
	let candidate: MemberListGroupLayout | null = null;
	while (low <= high) {
		const mid = (low + high) >> 1;
		const layout = layouts[mid]!;
		if (layout.headerRowIndex <= rowIndex) {
			candidate = layout;
			low = mid + 1;
		} else {
			high = mid - 1;
		}
	}
	if (candidate == null || rowIndex > candidate.rowEndIndex) {
		return null;
	}
	return candidate;
}

export interface MemberListScrollSpan {
	top: number;
	height: number;
}

export function quantizeMemberListScrollSpan(
	scrollTop: number,
	viewportHeight: number,
	chunkPx: number,
): MemberListScrollSpan {
	const safeChunkPx = Number.isFinite(chunkPx) && chunkPx > 0 ? chunkPx : 1;
	const safeScrollTop = Number.isFinite(scrollTop) ? Math.max(0, scrollTop) : 0;
	const safeViewportHeight = Number.isFinite(viewportHeight) ? Math.max(0, viewportHeight) : 0;
	const firstChunk = Math.floor(safeScrollTop / safeChunkPx) - 1;
	const lastChunk = Math.ceil((safeScrollTop + safeViewportHeight) / safeChunkPx) + 1;
	const top = Math.max(0, firstChunk * safeChunkPx);
	return {top, height: Math.max(0, lastChunk * safeChunkPx - top)};
}

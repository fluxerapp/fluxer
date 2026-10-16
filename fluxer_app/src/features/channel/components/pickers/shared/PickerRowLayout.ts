// SPDX-License-Identifier: AGPL-3.0-or-later
export const PICKER_CATEGORY_HEADER_HEIGHT = 32;
export const PICKER_CATEGORY_HEADER_GAP = 8;
export function findRowForOffset(rowOffsets: ReadonlyArray<number>, pixel: number): number {
	const lastRow = rowOffsets.length - 2;
	if (lastRow < 0 || pixel <= 0) {
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
export function buildRowOffsets(
	rowKinds: ReadonlyArray<string>,
	{headerHeightPx, rowHeightPx, sectionGap}: {headerHeightPx: number; rowHeightPx: number; sectionGap: number},
): Array<number> {
	const offsets = new Array<number>(rowKinds.length + 1);
	offsets[0] = 0;
	let runningOffset = 0;
	for (let i = 0; i < rowKinds.length; i++) {
		if (rowKinds[i] === 'header') runningOffset += headerHeightPx;
		else {
			runningOffset += rowHeightPx;
			if (rowKinds[i + 1] === 'header') runningOffset += sectionGap;
		}
		offsets[i + 1] = runningOffset;
	}
	return offsets;
}
export function getRowWindow(
	offsets: ReadonlyArray<number>,
	scrollTop: number,
	viewportHeight: number,
	leadRows: number,
) {
	const totalRows = offsets.length - 1;
	if (totalRows <= 0 || !(viewportHeight > 0)) return {firstRow: 0, lastRow: -1};
	const safeScrollTop = Number.isFinite(scrollTop) ? Math.max(0, scrollTop) : 0;
	return {
		firstRow: Math.max(0, findRowForOffset(offsets, safeScrollTop) - leadRows),
		lastRow: Math.min(totalRows - 1, findRowForOffset(offsets, safeScrollTop + viewportHeight) + leadRows),
	};
}

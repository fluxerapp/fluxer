// SPDX-License-Identifier: AGPL-3.0-or-later

export const STICKERS_PER_ROW = 4;
export const STICKERS_PER_ROW_MOBILE = 5;
export const STICKER_CATEGORY_HEADER_HEIGHT = 32;
export const STICKER_SECTION_GAP = 12;

const STICKER_CATEGORY_HEADER_GAP = 8;
const STICKER_GRID_GAP = 8;
const STICKER_GRID_BLOCK_END_PADDING = 8;
const STICKER_ROW_WINDOW_LEAD_ROWS = 2;

export type StickerRowKind = 'header' | 'sticker-row';

export interface StickerRowMetrics {
	remScale: number;
	stickerRowHeight: number;
	sectionGap: number;
}

export interface StickerRowWindow {
	firstRow: number;
	lastRow: number;
}

export function getStickerRowHeight(gridWidth: number, gridColumns: number, remScale: number): number {
	if (!(gridWidth > 0) || gridColumns <= 0) {
		return 0;
	}
	const cellWidth = Math.max(0, (gridWidth - STICKER_GRID_GAP * remScale * (gridColumns - 1)) / gridColumns);
	return cellWidth + STICKER_GRID_BLOCK_END_PADDING * remScale;
}

export function buildStickerRowOffsets(
	rowKinds: ReadonlyArray<StickerRowKind>,
	{remScale, stickerRowHeight, sectionGap}: StickerRowMetrics,
): Array<number> {
	const rowOffsets = new Array<number>(rowKinds.length + 1);
	rowOffsets[0] = 0;
	let runningOffset = 0;
	for (let rowIndex = 0; rowIndex < rowKinds.length; rowIndex += 1) {
		if (rowKinds[rowIndex] === 'header') {
			runningOffset += (STICKER_CATEGORY_HEADER_HEIGHT + STICKER_CATEGORY_HEADER_GAP) * remScale;
		} else {
			runningOffset += stickerRowHeight;
			if (rowKinds[rowIndex + 1] === 'header') {
				runningOffset += sectionGap;
			}
		}
		rowOffsets[rowIndex + 1] = runningOffset;
	}
	return rowOffsets;
}

export function findStickerRowForOffset(rowOffsets: ReadonlyArray<number>, pixel: number): number {
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

export function getStickerRowWindow(
	rowOffsets: ReadonlyArray<number>,
	scrollTop: number,
	viewportHeight: number,
): StickerRowWindow {
	const totalRows = rowOffsets.length - 1;
	if (totalRows <= 0 || !(viewportHeight > 0)) {
		return {firstRow: 0, lastRow: -1};
	}
	const safeScrollTop = Number.isFinite(scrollTop) ? Math.max(0, scrollTop) : 0;
	const leadingVisibleRow = findStickerRowForOffset(rowOffsets, safeScrollTop);
	const trailingVisibleRow = findStickerRowForOffset(rowOffsets, safeScrollTop + viewportHeight);
	return {
		firstRow: Math.max(0, leadingVisibleRow - STICKER_ROW_WINDOW_LEAD_ROWS),
		lastRow: Math.min(totalRows - 1, trailingVisibleRow + STICKER_ROW_WINDOW_LEAD_ROWS),
	};
}

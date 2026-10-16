// SPDX-License-Identifier: AGPL-3.0-or-later

import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';

const STICKERS_PER_ROW = 4;
export const STICKERS_PER_ROW_MOBILE = 5;
export const STICKER_SECTION_GAP = 12;
export const STICKER_GRID_TRACK_WIDTH = 120;

const STICKER_GRID_GAP = 8;
const STICKER_GRID_BLOCK_END_PADDING = 8;
export type StickerRowKind = 'header' | 'sticker-row';

export function getStickerGridColumns(containerWidth: number): number {
	if (containerWidth <= 0) {
		return STICKERS_PER_ROW;
	}
	const remScale = getRemScaleForDocument(typeof document === 'undefined' ? null : document);
	const trackWidth = STICKER_GRID_TRACK_WIDTH * remScale;
	const gap = STICKER_GRID_GAP * remScale;
	return Math.max(1, Math.floor((containerWidth + gap) / (trackWidth + gap)));
}

export function getFixedStickerRowHeight(remScale: number): number {
	return (STICKER_GRID_TRACK_WIDTH + STICKER_GRID_BLOCK_END_PADDING) * remScale;
}

export function getStickerRowHeight(gridWidth: number, gridColumns: number, remScale: number): number {
	if (!(gridWidth > 0) || gridColumns <= 0) {
		return 0;
	}
	const cellWidth = Math.max(0, (gridWidth - STICKER_GRID_GAP * remScale * (gridColumns - 1)) / gridColumns);
	return cellWidth + STICKER_GRID_BLOCK_END_PADDING * remScale;
}

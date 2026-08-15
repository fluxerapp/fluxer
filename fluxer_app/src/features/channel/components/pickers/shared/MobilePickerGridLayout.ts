// SPDX-License-Identifier: AGPL-3.0-or-later

import {EMOJIS_PER_ROW} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';
import {STICKERS_PER_ROW_MOBILE} from '@app/features/channel/components/sticker_picker/StickerPickerConstants';
import {getAppZoomFactor} from '@app/features/ui/utils/AppZoomUtils';

const MOBILE_LIST_INLINE_PADDING = 8;
const MOBILE_EMOJI_MIN_CELL_WIDTH = 32;
const MOBILE_STICKER_MIN_CELL_WIDTH = 56;
const MOBILE_STICKER_GAP = 8;

function getAvailableGridWidth(containerWidth: number): number {
	if (containerWidth <= 0) {
		return 0;
	}
	return Math.max(0, containerWidth / getAppZoomFactor() - MOBILE_LIST_INLINE_PADDING);
}

function getPreferredGridColumns(params: {
	availableWidth: number;
	preferredColumns: number;
	minCellWidth: number;
	gap?: number;
}): number {
	const {availableWidth, preferredColumns, minCellWidth, gap = 0} = params;
	if (availableWidth <= 0) {
		return preferredColumns;
	}
	const preferredCellWidth = (availableWidth - gap * (preferredColumns - 1)) / preferredColumns;
	if (preferredCellWidth >= minCellWidth) {
		return preferredColumns;
	}
	return Math.max(1, Math.floor((availableWidth + gap) / (minCellWidth + gap)));
}

export function getMobileEmojiGridColumns(containerWidth: number): number {
	return getPreferredGridColumns({
		availableWidth: getAvailableGridWidth(containerWidth),
		preferredColumns: EMOJIS_PER_ROW,
		minCellWidth: MOBILE_EMOJI_MIN_CELL_WIDTH,
	});
}

export function getMobileStickerGridColumns(containerWidth: number): number {
	return getPreferredGridColumns({
		availableWidth: getAvailableGridWidth(containerWidth),
		preferredColumns: STICKERS_PER_ROW_MOBILE,
		minCellWidth: MOBILE_STICKER_MIN_CELL_WIDTH,
		gap: MOBILE_STICKER_GAP,
	});
}

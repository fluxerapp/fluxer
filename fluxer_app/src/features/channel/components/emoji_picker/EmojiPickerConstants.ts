// SPDX-License-Identifier: AGPL-3.0-or-later

import {convertToCodePoints} from '@app/features/expressions/utils/EmojiCodepointUtils';
import {getRemScaleForDocument, remFromPx} from '@app/features/theme/layout/RemFromPx';
import sprite1f3fb2x from '@app/media/images/emoji-sprites/spritesheet-1f3fb@2x.png';
import sprite1f3fb1x from '@app/media/images/emoji-sprites/spritesheet-1f3fb.png';
import sprite1f3fc2x from '@app/media/images/emoji-sprites/spritesheet-1f3fc@2x.png';
import sprite1f3fc1x from '@app/media/images/emoji-sprites/spritesheet-1f3fc.png';
import sprite1f3fd2x from '@app/media/images/emoji-sprites/spritesheet-1f3fd@2x.png';
import sprite1f3fd1x from '@app/media/images/emoji-sprites/spritesheet-1f3fd.png';
import sprite1f3fe2x from '@app/media/images/emoji-sprites/spritesheet-1f3fe@2x.png';
import sprite1f3fe1x from '@app/media/images/emoji-sprites/spritesheet-1f3fe.png';
import sprite1f3ff2x from '@app/media/images/emoji-sprites/spritesheet-1f3ff@2x.png';
import sprite1f3ff1x from '@app/media/images/emoji-sprites/spritesheet-1f3ff.png';
import spriteDefault2x from '@app/media/images/emoji-sprites/spritesheet-emoji@2x.png';
import spriteDefault1x from '@app/media/images/emoji-sprites/spritesheet-emoji.png';
import type {CSSProperties} from 'react';

export const EMOJI_CLAP = '\u{1F44F}';
const EMOJI_SPRITE_SIZE = 32;
export const EMOJI_ROW_HEIGHT = 48;
export const CATEGORY_HEADER_HEIGHT = 32;
export const EMOJIS_PER_ROW = 9;
const EMOJIS_PER_ROW_MIN = 5;
export const EMOJI_SECTION_GAP = 12;
const CATEGORY_HEADER_GAP = 8;

const EMOJI_GRID_MIN_CELL_WIDTH = EMOJI_ROW_HEIGHT;
export const EMOJI_GRID_TRACK_WIDTH = EMOJI_GRID_MIN_CELL_WIDTH;
export const EMOJI_GRID_SCROLLER_PADDING_WIDTH = 8;

export function getEmojiGridColumns(containerWidth: number): number {
	if (containerWidth <= 0) return EMOJIS_PER_ROW;
	const remScale = getRemScaleForDocument(typeof document === 'undefined' ? null : document);
	const columns = Math.floor(containerWidth / (EMOJI_GRID_MIN_CELL_WIDTH * remScale));
	return Math.max(EMOJIS_PER_ROW_MIN, columns);
}

export type EmojiRowKind = 'header' | 'emoji-row';

export interface EmojiRowMetrics {
	remScale: number;
	sectionGap: number;
}

export interface EmojiRowWindow {
	firstRow: number;
	lastRow: number;
}

export function buildEmojiRowOffsets(
	rowKinds: ReadonlyArray<EmojiRowKind>,
	{remScale, sectionGap}: EmojiRowMetrics,
): Array<number> {
	const rowOffsets = new Array<number>(rowKinds.length + 1);
	rowOffsets[0] = 0;
	let runningOffset = 0;
	for (let rowIndex = 0; rowIndex < rowKinds.length; rowIndex += 1) {
		if (rowKinds[rowIndex] === 'header') {
			runningOffset += (CATEGORY_HEADER_HEIGHT + CATEGORY_HEADER_GAP) * remScale;
		} else {
			runningOffset += EMOJI_ROW_HEIGHT * remScale;
			if (rowKinds[rowIndex + 1] === 'header') {
				runningOffset += sectionGap;
			}
		}
		rowOffsets[rowIndex + 1] = runningOffset;
	}
	return rowOffsets;
}

export function findEmojiRowForOffset(rowOffsets: ReadonlyArray<number>, pixel: number): number {
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

export function getEmojiRowWindow(
	rowOffsets: ReadonlyArray<number>,
	scrollTop: number,
	viewportHeight: number,
): EmojiRowWindow {
	const totalRows = rowOffsets.length - 1;
	if (totalRows <= 0 || !(viewportHeight > 0)) {
		return {firstRow: 0, lastRow: -1};
	}
	const safeScrollTop = Number.isFinite(scrollTop) ? Math.max(0, scrollTop) : 0;
	return {
		firstRow: findEmojiRowForOffset(rowOffsets, safeScrollTop),
		lastRow: findEmojiRowForOffset(rowOffsets, safeScrollTop + viewportHeight),
	};
}

export const getEmojiSpriteSheetLayout = (index: number, perRow: number, rows: number): CSSProperties => {
	const column = index % perRow;
	const row = Math.floor(index / perRow);
	return {
		backgroundPosition: `${remFromPx(-column * EMOJI_SPRITE_SIZE)} ${remFromPx(-row * EMOJI_SPRITE_SIZE)}`,
		backgroundSize: `${remFromPx(perRow * EMOJI_SPRITE_SIZE)} ${remFromPx(rows * EMOJI_SPRITE_SIZE)}`,
	};
};

interface SpriteSheetOptions {
	retina?: boolean;
}

interface SpriteSheetVariant {
	standard: string;
	retina: string;
}

const SPRITE_SHEET_RESOURCES: Record<string, SpriteSheetVariant> = {
	default: {standard: spriteDefault1x, retina: spriteDefault2x},
	'1f3fb': {standard: sprite1f3fb1x, retina: sprite1f3fb2x},
	'1f3fc': {standard: sprite1f3fc1x, retina: sprite1f3fc2x},
	'1f3fd': {standard: sprite1f3fd1x, retina: sprite1f3fd2x},
	'1f3fe': {standard: sprite1f3fe1x, retina: sprite1f3fe2x},
	'1f3ff': {standard: sprite1f3ff1x, retina: sprite1f3ff2x},
};
const getSpriteSheetKey = (skinTone?: string): string => {
	if (!skinTone) {
		return 'default';
	}
	const codepoint = convertToCodePoints(skinTone);
	return SPRITE_SHEET_RESOURCES[codepoint] ? codepoint : 'default';
};
export const getSpriteSheetPath = (skinTone?: string, options?: SpriteSheetOptions): string => {
	const key = getSpriteSheetKey(skinTone);
	const sheet = SPRITE_SHEET_RESOURCES[key];
	return options?.retina ? sheet.retina : sheet.standard;
};

let supportsImageSetCache: boolean | null = null;

const supportsImageSet = (): boolean => {
	if (supportsImageSetCache !== null) {
		return supportsImageSetCache;
	}
	if (!window.CSS?.supports) {
		return false;
	}
	supportsImageSetCache = window.CSS.supports(
		'background-image',
		"image-set(url('data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEA') 1x)",
	);
	return supportsImageSetCache;
};
export const getSpriteSheetBackground = (skinTone?: string): string => {
	const basePath = getSpriteSheetPath(skinTone);
	if (supportsImageSet()) {
		const retinaPath = getSpriteSheetPath(skinTone, {retina: true});
		return `image-set(url(${basePath}) 1x, url(${retinaPath}) 2x)`;
	}
	return `url(${basePath})`;
};

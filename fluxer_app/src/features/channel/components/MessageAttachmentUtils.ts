// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export const ATTACHMENT_CARD_WIDTH = 400;

const IMAGE_TYPES = ['image/png', 'image/jpeg', 'image/jpg', 'image/gif', 'image/webp', 'image/avif'];
const VIDEO_TYPES = ['video/mp4', 'video/webm', 'video/quicktime'];

function isImageType(contentType?: string): boolean {
	return contentType ? IMAGE_TYPES.includes(contentType) : false;
}

function isVideoType(contentType?: string): boolean {
	return contentType ? VIDEO_TYPES.includes(contentType) : false;
}

const hasRenderableDimensions = (attachment: MessageAttachment): boolean =>
	typeof attachment.width === 'number' &&
	attachment.width > 0 &&
	typeof attachment.height === 'number' &&
	attachment.height > 0;
export const isMediaAttachment = (attachment: MessageAttachment): boolean =>
	hasRenderableDimensions(attachment) && (isImageType(attachment.content_type) || isVideoType(attachment.content_type));

export function splitMediaAndFileAttachments(attachments: ReadonlyArray<MessageAttachment>): {
	mediaAttachments: Array<MessageAttachment>;
	fileAttachments: Array<MessageAttachment>;
} {
	const mediaAttachments: Array<MessageAttachment> = [];
	const fileAttachments: Array<MessageAttachment> = [];
	for (const attachment of attachments) {
		if (isMediaAttachment(attachment)) {
			mediaAttachments.push(attachment);
		} else {
			fileAttachments.push(attachment);
		}
	}
	return {mediaAttachments, fileAttachments};
}

const MOSAIC_TILE_GAP = 4;

interface MosaicTileSlot {
	columns: number;
	span: number;
	aspectWidth: number;
	aspectHeight: number;
}

const MOSAIC_TILE_SLOTS: Record<number, (index: number) => MosaicTileSlot> = {
	2: () => ({columns: 2, span: 1, aspectWidth: 1, aspectHeight: 1}),
	3: (index) => ({columns: 3, span: index === 0 ? 2 : 1, aspectWidth: 1, aspectHeight: 1}),
	4: () => ({columns: 2, span: 1, aspectWidth: 3, aspectHeight: 2}),
	5: (index) =>
		index < 2
			? {columns: 6, span: 3, aspectWidth: 3, aspectHeight: 2}
			: {columns: 6, span: 2, aspectWidth: 1, aspectHeight: 1},
	6: () => ({columns: 3, span: 1, aspectWidth: 1, aspectHeight: 1}),
	7: (index) =>
		index === 0
			? {columns: 1, span: 1, aspectWidth: 16, aspectHeight: 9}
			: {columns: 3, span: 1, aspectWidth: 1, aspectHeight: 1},
	8: (index) =>
		index < 2
			? {columns: 2, span: 1, aspectWidth: 3, aspectHeight: 2}
			: {columns: 3, span: 1, aspectWidth: 1, aspectHeight: 1},
	9: () => ({columns: 3, span: 1, aspectWidth: 1, aspectHeight: 1}),
	10: (index) =>
		index === 0
			? {columns: 1, span: 1, aspectWidth: 16, aspectHeight: 9}
			: {columns: 3, span: 1, aspectWidth: 1, aspectHeight: 1},
};

const FALLBACK_MOSAIC_TILE_SLOT: MosaicTileSlot = {columns: 2, span: 1, aspectWidth: 3, aspectHeight: 2};

export function getMosaicTileBox(
	count: number,
	index: number,
	mosaicWidth: number,
): {width: number; height: number; aspectRatio: string} {
	const slot = MOSAIC_TILE_SLOTS[count]?.(index) ?? FALLBACK_MOSAIC_TILE_SLOT;
	const track = (mosaicWidth - (slot.columns - 1) * MOSAIC_TILE_GAP) / slot.columns;
	const width = Math.max(1, track * slot.span + (slot.span - 1) * MOSAIC_TILE_GAP);
	return {
		width,
		height: Math.max(1, (width * slot.aspectHeight) / slot.aspectWidth),
		aspectRatio: `${slot.aspectWidth} / ${slot.aspectHeight}`,
	};
}

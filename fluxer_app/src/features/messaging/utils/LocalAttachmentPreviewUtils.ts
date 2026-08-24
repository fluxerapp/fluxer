// SPDX-License-Identifier: AGPL-3.0-or-later

import {isEmbeddableImageFile} from '@app/features/messaging/utils/EmbeddableImageTypes';

export const LOCAL_MEDIA_PREVIEW_MAX_BYTES = 10 * 1024 * 1024;
export const LOCAL_VIDEO_THUMBNAIL_MAX_EDGE = 640;

type LocalPreviewFile = Pick<File, 'name' | 'size' | 'type'>;

export function canEagerlyPreviewLocalMedia(file: Pick<File, 'size'>): boolean {
	return file.size <= LOCAL_MEDIA_PREVIEW_MAX_BYTES;
}

export function shouldEagerlyPreviewLocalImage(file: LocalPreviewFile): boolean {
	return isEmbeddableImageFile(file) && canEagerlyPreviewLocalMedia(file);
}

export function shouldEagerlyPreviewLocalVideo(file: LocalPreviewFile): boolean {
	return file.type.toLowerCase().startsWith('video/') && canEagerlyPreviewLocalMedia(file);
}

export function getScaledMediaDimensions(
	width: number,
	height: number,
	maxEdge = LOCAL_VIDEO_THUMBNAIL_MAX_EDGE,
): {width: number; height: number} {
	if (!Number.isFinite(width) || !Number.isFinite(height) || width <= 0 || height <= 0 || maxEdge <= 0) {
		return {width: 0, height: 0};
	}
	const largestEdge = Math.max(width, height);
	if (largestEdge <= maxEdge) {
		return {width, height};
	}
	const scale = maxEdge / largestEdge;
	return {
		width: Math.max(1, Math.round(width * scale)),
		height: Math.max(1, Math.round(height * scale)),
	};
}

export function hasRenderableLocalPreview(attachment: {
	file: LocalPreviewFile;
	previewURL: string | null;
	thumbnailURL: string | null;
}): boolean {
	if (attachment.file.type.startsWith('video/')) {
		return attachment.thumbnailURL !== null;
	}
	if (isEmbeddableImageFile(attachment.file)) {
		return attachment.previewURL !== null;
	}
	return false;
}

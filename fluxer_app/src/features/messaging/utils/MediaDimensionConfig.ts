// SPDX-License-Identifier: AGPL-3.0-or-later

import {fitMediaWithinBounds} from '@app/features/ui/utils/DimensionUtils';
import type {MediaDimensions} from '@app/lib/branded-types';

interface MediaDimensionConstraints extends MediaDimensions {}

export const ATTACHMENT_MAX_WIDTH = 550;
export const ATTACHMENT_MAX_HEIGHT = 350;
export const EMBED_MAX_WIDTH = 400;
export const EMBED_MAX_HEIGHT = 300;
export const EMBED_TALL_MAX_HEIGHT = 450;
export const MIN_USEFUL_MEDIA_SIZE = 40;

const ATTACHMENT_DIMENSIONS: MediaDimensionConstraints = {
	maxWidth: ATTACHMENT_MAX_WIDTH,
	maxHeight: ATTACHMENT_MAX_HEIGHT,
};

const EMBED_DIMENSIONS: MediaDimensionConstraints = {
	maxWidth: EMBED_MAX_WIDTH,
	maxHeight: EMBED_MAX_HEIGHT,
};

const EMBED_TALL_DIMENSIONS: MediaDimensionConstraints = {
	maxWidth: EMBED_MAX_WIDTH,
	maxHeight: EMBED_TALL_MAX_HEIGHT,
};

export function getAttachmentMediaDimensions(): MediaDimensionConstraints {
	return ATTACHMENT_DIMENSIONS;
}

export function getEmbedMediaDimensions(isTallMedia = false): MediaDimensionConstraints {
	return isTallMedia ? EMBED_TALL_DIMENSIONS : EMBED_DIMENSIONS;
}

export function getMosaicMediaDimensions(): MediaDimensionConstraints {
	return ATTACHMENT_DIMENSIONS;
}

export function isUsefulVisualMediaSize(
	naturalWidth: number,
	naturalHeight: number,
	constraints: MediaDimensionConstraints = ATTACHMENT_DIMENSIONS,
): boolean {
	if (!(naturalWidth > 0) || !(naturalHeight > 0)) return false;
	const fitted = fitMediaWithinBounds({
		width: naturalWidth,
		height: naturalHeight,
		maxWidth: constraints.maxWidth,
		maxHeight: constraints.maxHeight,
	});
	return fitted.width >= MIN_USEFUL_MEDIA_SIZE && fitted.height >= MIN_USEFUL_MEDIA_SIZE;
}

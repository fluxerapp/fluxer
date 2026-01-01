/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {MessageFlags} from '~/Constants';
import type {MessageRecord} from '~/records/MessageRecord';
import AccessibilityStore, {MediaDimensionSize} from '~/stores/AccessibilityStore';

export interface MediaDimensions {
	maxWidth: number;
	maxHeight: number;
}

const DIMENSION_PRESETS = {
	SMALL: {
		maxWidth: 400,
		maxHeight: 300,
	},
	LARGE: {
		maxWidth: 550,
		maxHeight: 400,
	},
} as const;

export const getAttachmentMediaDimensions = (message?: MessageRecord): MediaDimensions => {
	if (message && (message.flags & MessageFlags.COMPACT_ATTACHMENTS) !== 0) {
		return DIMENSION_PRESETS.SMALL;
	}

	const size = AccessibilityStore.attachmentMediaDimensionSize;
	return size === MediaDimensionSize.SMALL ? DIMENSION_PRESETS.SMALL : DIMENSION_PRESETS.LARGE;
};

export const getEmbedMediaDimensions = (): MediaDimensions => {
	const size = AccessibilityStore.embedMediaDimensionSize;
	return size === MediaDimensionSize.SMALL ? DIMENSION_PRESETS.SMALL : DIMENSION_PRESETS.LARGE;
};

export const getMosaicMediaDimensions = (message?: MessageRecord): MediaDimensions => {
	return getAttachmentMediaDimensions(message);
};
